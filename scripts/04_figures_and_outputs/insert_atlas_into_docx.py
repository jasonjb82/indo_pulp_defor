#!/usr/bin/env python3
"""Insert the concession atlas into the Word SM as a new numbered section.

Reads outputs/atlas/atlas_data.typ (the same data the typst PDF is built from)
plus the per-concession tile PNGs, and splices an equivalent section into a
.docx immediately before the References heading.

Layout mirrors typst/concession_atlas.typ: 2 columns x 3 rows of tiles per
page, each tile labelled with id + name and an island/group meta line.
"""
import argparse, os, re, shutil, struct, sys, zipfile
from xml.sax.saxutils import escape

W = 'http://schemas.openxmlformats.org/wordprocessingml/2006/main'
EMU_PER_IN = 914400

# --- geometry (US Letter, 1in margins -> 6.5in content width) --------------
CELL_W      = 4680          # twips, 3.25in; two cells = 9360 = 6.5in
CELL_MAR_LR = 144           # 0.10in -> 3.05in of usable width per cell
CELL_MAR_T  = 72
CELL_MAR_B  = 108
IMG_W_IN    = 3.05
TILES_PER_ROW, ROWS_PER_PAGE = 2, 3
INDEX_TAB   = CELL_W - 2 * CELL_MAR_LR   # right tab stop inside an index cell
INDEX_SZ    = 18            # half-points: the whole index sets at 9 pt
SECTION_TITLE = "Supplementary figures detailing concession-level deforestation"

# ---------------------------------------------------------------- typst data
def parse_atlas_data(path):
    src = open(path, encoding='utf-8').read()
    tile_re = re.compile(
        r'\(id:\s*"([^"]*)",\s*name:\s*"([^"]*)",\s*island:\s*"([^"]*)",'
        r'\s*group:\s*"([^"]*)",\s*file:\s*"([^"]*)"\)')
    tiles = [dict(id=a, name=b, island=c, group=d, file=e)
             for a, b, c, d, e in tile_re.findall(src)]

    groups, gblock = [], re.search(r'#let groups = \((.*)\n\)\n', src, re.S)
    if gblock:
        for gname, body in re.findall(
                r'\(name:\s*"([^"]*)",\s*entries:\s*\((.*?)\)\),', gblock.group(1), re.S):
            entries = [dict(id=i, name=n) for i, n in
                       re.findall(r'\(id:\s*"([^"]*)",\s*name:\s*"([^"]*)"\)', body)]
            groups.append(dict(name=gname, entries=entries))
    m = re.search(r'#let ungrouped_count = (\d+)', src)
    ungrouped = int(m.group(1)) if m else 0
    if not tiles:
        sys.exit("no tiles parsed from " + path)
    return tiles, groups, ungrouped

def png_size(path):
    with open(path, 'rb') as fh:
        head = fh.read(33)
    if head[:8] != b'\x89PNG\r\n\x1a\n':
        raise ValueError("not a PNG: " + path)
    return struct.unpack('>II', head[16:24])

# ------------------------------------------------------------- xml fragments
def bookmark_name(cid):
    # Word bookmark names: letters, digits, underscore only; must start alpha.
    return 'atlas_' + re.sub(r'[^A-Za-z0-9]', '_', cid)

def run(text, *, sz=None, bold=False, italic=False, color=None, font=None):
    rpr = ''
    if font:   rpr += f'<w:rFonts w:ascii="{font}" w:hAnsi="{font}"/>'
    if bold:   rpr += '<w:b/>'
    if italic: rpr += '<w:i/>'
    if color:  rpr += f'<w:color w:val="{color}"/>'
    if sz:     rpr += f'<w:sz w:val="{sz}"/><w:szCs w:val="{sz}"/>'
    rpr = f'<w:rPr>{rpr}</w:rPr>' if rpr else ''
    return f'<w:r>{rpr}<w:t xml:space="preserve">{escape(text)}</w:t></w:r>'

def swatch(fill):
    return (f'<w:r><w:rPr><w:sz w:val="15"/><w:szCs w:val="15"/>'
            f'<w:shd w:val="clear" w:color="auto" w:fill="{fill}"/>'
            f'</w:rPr><w:t xml:space="preserve">    </w:t></w:r>')

def para(runs, *, style=None, spacing=None, jc=None, tabs=None, keep_next=False,
         mark_sz=None):
    ppr = ''
    if style:   ppr += f'<w:pStyle w:val="{style}"/>'
    if keep_next: ppr += '<w:keepNext/>'
    if tabs:    ppr += tabs
    if spacing is not None: ppr += spacing
    if jc:      ppr += f'<w:jc w:val="{jc}"/>'
    # The paragraph mark carries the formatting of tab leader dots, so an index
    # line whose mark is left at the 11 pt default draws 11 pt dots under 9 pt
    # text. Setting it explicitly keeps the whole line at one size.
    if mark_sz: ppr += f'<w:rPr><w:sz w:val="{mark_sz}"/><w:szCs w:val="{mark_sz}"/></w:rPr>'
    ppr = f'<w:pPr>{ppr}</w:pPr>' if ppr else ''
    return f'<w:p>{ppr}{runs}</w:p>'

TIGHT = '<w:spacing w:before="0" w:after="0" w:line="240" w:lineRule="auto"/>'
BODY  = '<w:spacing w:before="0" w:after="120" w:line="276" w:lineRule="auto"/>'

def page_break():
    return '<w:p><w:pPr>' + TIGHT + '</w:pPr><w:r><w:br w:type="page"/></w:r></w:p>'

def image_run(rid, docpr_id, w_in, h_in, name):
    cx, cy = int(round(w_in * EMU_PER_IN)), int(round(h_in * EMU_PER_IN))
    return (
      '<w:r><w:drawing>'
      f'<wp:inline distT="0" distB="0" distL="0" distR="0">'
      f'<wp:extent cx="{cx}" cy="{cy}"/><wp:effectExtent l="0" t="0" r="0" b="0"/>'
      f'<wp:docPr id="{docpr_id}" name="{escape(name)}" descr="{escape(name)}"/>'
      '<wp:cNvGraphicFramePr><a:graphicFrameLocks '
      'xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main" noChangeAspect="1"/>'
      '</wp:cNvGraphicFramePr>'
      '<a:graphic xmlns:a="http://schemas.openxmlformats.org/drawingml/2006/main">'
      '<a:graphicData uri="http://schemas.openxmlformats.org/drawingml/2006/picture">'
      '<pic:pic xmlns:pic="http://schemas.openxmlformats.org/drawingml/2006/picture">'
      f'<pic:nvPicPr><pic:cNvPr id="{docpr_id}" name="{escape(name)}"/><pic:cNvPicPr/></pic:nvPicPr>'
      f'<pic:blipFill><a:blip r:embed="{rid}" '
      'xmlns:r="http://schemas.openxmlformats.org/officeDocument/2006/relationships"/>'
      '<a:stretch><a:fillRect/></a:stretch></pic:blipFill>'
      '<pic:spPr><a:xfrm><a:off x="0" y="0"/>'
      f'<a:ext cx="{cx}" cy="{cy}"/></a:xfrm>'
      '<a:prstGeom prst="rect"><a:avLst/></a:prstGeom></pic:spPr>'
      '</pic:pic></a:graphicData></a:graphic></wp:inline></w:drawing></w:r>')

def tc(content, width=CELL_W, mar_t=None, mar_b=None):
    mar_t = CELL_MAR_T if mar_t is None else mar_t
    mar_b = CELL_MAR_B if mar_b is None else mar_b
    return (f'<w:tc><w:tcPr><w:tcW w:w="{width}" w:type="dxa"/>'
            '<w:tcMar>'
            f'<w:top w:w="{mar_t}" w:type="dxa"/><w:left w:w="{CELL_MAR_LR}" w:type="dxa"/>'
            f'<w:bottom w:w="{mar_b}" w:type="dxa"/><w:right w:w="{CELL_MAR_LR}" w:type="dxa"/>'
            '</w:tcMar></w:tcPr>' + content + '</w:tc>')

def borderless_table(rows_xml, ncols=2, mar_t=None, mar_b=None):
    mar_t = CELL_MAR_T if mar_t is None else mar_t
    mar_b = CELL_MAR_B if mar_b is None else mar_b
    grid = ''.join(f'<w:gridCol w:w="{CELL_W}"/>' for _ in range(ncols))
    return ('<w:tbl><w:tblPr><w:tblW w:w="0" w:type="auto"/>'
            '<w:tblBorders>' + ''.join(
                f'<w:{s} w:val="none" w:sz="0" w:space="0" w:color="auto"/>'
                for s in ('top','left','bottom','right','insideH','insideV')) +
            '</w:tblBorders>'
            '<w:tblLayout w:type="fixed"/>'
            '<w:tblCellMar>'
            f'<w:top w:w="{mar_t}" w:type="dxa"/><w:left w:w="{CELL_MAR_LR}" w:type="dxa"/>'
            f'<w:bottom w:w="{mar_b}" w:type="dxa"/><w:right w:w="{CELL_MAR_LR}" w:type="dxa"/>'
            '</w:tblCellMar></w:tblPr>'
            f'<w:tblGrid>{grid}</w:tblGrid>' + rows_xml + '</w:tbl>')

def pageref(bm, sz=None):
    """A PAGEREF field, with the size on every run.

    The field result inherits from the runs that carry it, and the tab leader
    before it is drawn in the tab run's own size -- so a field left at the
    default renders an 11 pt page number and 11 pt dots beside 9 pt text.
    """
    rpr = f'<w:rPr><w:sz w:val="{sz}"/><w:szCs w:val="{sz}"/></w:rPr>' if sz else ''
    return (f'<w:r>{rpr}<w:fldChar w:fldCharType="begin"/></w:r>'
            f'<w:r>{rpr}<w:instrText xml:space="preserve"> PAGEREF {bm} \\h </w:instrText></w:r>'
            f'<w:r>{rpr}<w:fldChar w:fldCharType="separate"/></w:r>'
            + run('–', sz=sz) +
            f'<w:r>{rpr}<w:fldChar w:fldCharType="end"/></w:r>')


# -------------------------------------------------------------- atlas legend
def swatch_sz(fill, sz):
    return (f'<w:r><w:rPr><w:sz w:val="{sz}"/><w:szCs w:val="{sz}"/>'
            f'<w:shd w:val="clear" w:color="auto" w:fill="{fill}"/>'
            f'</w:rPr><w:t xml:space="preserve">    </w:t></w:r>')

def legend_runs(sz):
    """The shared colour/line key, as a run sequence at the given half-point size."""
    return (swatch_sz("009E73", sz) + run("  Forest ", sz=sz)
          + swatch_sz("F0E442", sz) + run("  Non-forest ", sz=sz)
          + swatch_sz("CC79A7", sz) + run("  Pulpwood plantation ", sz=sz)
          + run("─── Earliest ZDC year of downstream mill ", sz=sz)
          + run("– – – License year", sz=sz))


# ------------------------------------------------------------ section bodies
INTRO = ("Based on our analyses of deforestation and pulpwood plantation expansion, we are "
         "able to track changes in land cover since 2000 within each individual pulpwood "
         "concession. The figures in this section visualize these changes for each of the "
         "pulpwood concessions in our database, breaking land cover into (a) areas that are "
         "still forested (green); (b) areas that have been deforested but were not immediately "
         "converted to pulpwood plantations (yellow); and (c) areas that were converted into "
         "pulpwood plantations (pink). In addition, we provide contextual information detailing "
         "the year in which each concession was first licensed, as well as the year in which any "
         "downstream buyers first enacted a zero-deforestation commitment (ZDC).")


def legend_para():
    """The colour/line key, repeated under the bottom row of every figure page.

    This lives in the body rather than in a footer: Word dims header and footer
    content while the body is being edited, which washes out the three swatches
    and makes them disagree with the figures they describe.
    """
    sp = '<w:spacing w:before="140" w:after="0" w:line="240" w:lineRule="auto"/>'
    return para(legend_runs(15), spacing=sp, jc="center", mark_sz=15)

def build_index_rows(entries, bid):
    """Two-column borderless table, filled row-wise, dot leader + PAGEREF."""
    tabs = f'<w:tabs><w:tab w:val="right" w:leader="dot" w:pos="{INDEX_TAB}"/></w:tabs>'
    cells = []
    for e in entries:
        bm = bookmark_name(e['id'])
        body = (run(e['id'] + '  ' + e['name'], sz=INDEX_SZ)
                + f'<w:r><w:rPr><w:sz w:val="{INDEX_SZ}"/><w:szCs w:val="{INDEX_SZ}"/>'
                  '</w:rPr><w:tab/></w:r>'
                + pageref(bm, INDEX_SZ))
        cells.append(tc(para(body, spacing=TIGHT, tabs=tabs, mark_sz=INDEX_SZ),
                        mar_t=0, mar_b=0))
    rows = ''
    for i in range(0, len(cells), 2):
        pair = cells[i:i+2]
        if len(pair) == 1:
            pair.append(tc(para('', spacing=TIGHT, mark_sz=INDEX_SZ), mar_t=0, mar_b=0))
        rows += '<w:tr><w:trPr><w:cantSplit/></w:trPr>' + ''.join(pair) + '</w:tr>'
    return rows

def build_tile_cell(t, rid, docpr_id, img_h_in, bid):
    bm = bookmark_name(t['id'])
    meta = t['island'] or '\u2014'
    head = (f'<w:bookmarkStart w:id="{bid}" w:name="{bm}"/>'
            + run(t['id'] + '   ' + t['name'], sz=15, bold=True)
            + f'<w:bookmarkEnd w:id="{bid}"/>')
    return tc(
        para(head, spacing=TIGHT, keep_next=True)
        + para(run(meta, sz=14, italic=True, color="666666"), spacing=TIGHT, keep_next=True)
        + para(image_run(rid, docpr_id, IMG_W_IN, img_h_in, t['id']), spacing=TIGHT))

# ------------------------------------------------------------------ assembly
def build_atlas_xml(tiles, groups, ungrouped, tiles_dir, start_rid, start_bid,
                    start_docpr, media):
    """Returns (xml, rels_added) and appends (src, dest) pairs to `media`."""
    # No leading page break: every section break in this document is nextPage,
    # so the preceding section already starts the atlas on a fresh page.
    out = [para(run(SECTION_TITLE), style="Heading2")]
    out.append(para(run(INTRO), spacing=BODY))


    rid, bid, docpr = start_rid, start_bid, start_docpr

    # --- 9.1 index of concessions (bookmarks are defined later, at the tiles)
    out.append(para(run("Index of concessions"), style="Heading3"))
    out.append(borderless_table(build_index_rows(tiles, bid), mar_t=0, mar_b=0))

    # --- 9.2 the tiles
    out.append(page_break())
    out.append(para(run("Concession figures"), style="Heading3"))

    per_page = TILES_PER_ROW * ROWS_PER_PAGE
    pages = [tiles[i:i + per_page] for i in range(0, len(tiles), per_page)]
    for pi, chunk in enumerate(pages):
        rows = ''
        for i in range(0, len(chunk), TILES_PER_ROW):
            cells = ''
            for t in chunk[i:i + TILES_PER_ROW]:
                src = os.path.join(tiles_dir, os.path.basename(t['file']))
                if not os.path.exists(src):
                    sys.exit("missing tile image: " + src)
                pw, ph = png_size(src)
                dest = 'media/atlas_%s.png' % re.sub(r'[^A-Za-z0-9]', '_', t['id'])
                media.append((src, dest, 'rId%d' % rid))
                cells += build_tile_cell(t, 'rId%d' % rid, docpr,
                                         IMG_W_IN * ph / pw, bid)
                rid += 1; docpr += 1; bid += 1
            while cells.count('<w:tc>') < TILES_PER_ROW:
                cells += tc(para('', spacing=TIGHT))
            rows += '<w:tr><w:trPr><w:cantSplit/></w:trPr>' + cells + '</w:tr>'
        out.append(borderless_table(rows))
        out.append(legend_para())
        out.append(page_break())
    return ''.join(out), rid, bid, docpr


def strip_cover_sdt(doc):
    """Remove a stray 'Cover Pages' content control, keeping its content.

    A w:sdt wrapper around part of the body makes Zotero fail to iterate its
    fields ("Zotero experienced an error updating your document"). Unwrapping
    it leaves every run, field and paragraph in place.
    """
    i = doc.find('<w:sdt>')
    if i < 0 or 'docPartGallery' not in doc:
        return doc, 0
    depth, j = 0, i
    while j < len(doc):
        nxt_o, nxt_c = doc.find('<w:sdt>', j), doc.find('</w:sdt>', j)
        if nxt_c < 0:
            return doc, 0
        if 0 <= nxt_o < nxt_c:
            depth += 1; j = nxt_o + 7
        else:
            depth -= 1; j = nxt_c + 8
            if depth == 0:
                break
    blk = doc[i:j]
    m = re.search(r'<w:sdtContent>', blk)
    if not m or not blk.endswith('</w:sdtContent></w:sdt>'):
        return doc, 0
    inner = blk[m.end():-len('</w:sdtContent></w:sdt>')]
    return doc[:i] + inner + doc[j:], len(blk) - len(inner)

# ---------------------------------------------------------------------- main
def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('--docx', required=True)
    ap.add_argument('--atlas-data', required=True)
    ap.add_argument('--tiles-dir', required=True)
    ap.add_argument('--out', required=True)
    a = ap.parse_args()

    tiles, groups, ungrouped = parse_atlas_data(a.atlas_data)
    print(f"parsed {len(tiles)} tiles, {len(groups)} groups, {ungrouped} ungrouped")

    zin = zipfile.ZipFile(a.docx)
    names = zin.namelist()
    doc = zin.read('word/document.xml').decode('utf-8')
    rels = zin.read('word/_rels/document.xml.rels').decode('utf-8')

    # insertion point: immediately before the bold "References" paragraph that
    # precedes the Zotero bibliography field.
    mb = re.search(r'ADDIN ZOTERO_BIBL', doc)
    if not mb:
        sys.exit("no ZOTERO_BIBL field found - wrong document?")
    refs = None
    for m in re.finditer(r'<w:p\b[^>]*>(?:(?!</w:p>).)*?'
                         r'<w:t[^>]*>References</w:t>.*?</w:p>', doc, re.S):
        if m.end() <= mb.start():
            refs = m
    if refs is None:
        sys.exit("could not locate the References heading before the bibliography")
    ins_at = refs.start()
    print(f"insertion point at char {ins_at} (References para at {refs.start()}..{refs.end()}, "
          f"ZOTERO_BIBL at {mb.start()})")

    start_rid   = max(int(x) for x in re.findall(r'Id="rId(\d+)"', rels)) + 1
    used_bids   = [int(x) for x in re.findall(r'<w:bookmarkStart w:id="(\d+)"', doc)] or [0]
    start_bid   = max(used_bids) + 1
    used_docpr  = [int(x) for x in re.findall(r'<wp:docPr id="(\d+)"', doc)] or [0]
    start_docpr = max(max(used_docpr) + 1, 500000)

    # The atlas adds no section of its own: it inherits the surrounding page
    # setup and footers, and carries its legend in the body instead.
    media = []
    xml, _, _, _ = build_atlas_xml(tiles, groups, ungrouped, a.tiles_dir,
                                   start_rid, start_bid, start_docpr, media)
    newdoc = doc[:ins_at] + xml + doc[ins_at:]

    newdoc, removed = strip_cover_sdt(newdoc)
    if removed:
        print(f"removed stray 'Cover Pages' content control ({removed} chars of wrapper); "
              "content kept")

    newrels = rels.replace('</Relationships>', ''.join(
        f'<Relationship Id="{rid}" Type="http://schemas.openxmlformats.org/'
        f'officeDocument/2006/relationships/image" Target="{dest}"/>'
        for _, dest, rid in media) + '</Relationships>')

    with zipfile.ZipFile(a.out, 'w', zipfile.ZIP_DEFLATED) as zo:
        for n in names:
            if n == 'word/document.xml':
                zo.writestr(n, newdoc.encode('utf-8'))
            elif n == 'word/_rels/document.xml.rels':
                zo.writestr(n, newrels.encode('utf-8'))
            else:
                zo.writestr(zin.getinfo(n), zin.read(n))
        for src, dest, _ in media:
            zo.write(src, 'word/' + dest)

    print(f"wrote {a.out}: +{len(media)} images, "
          f"document.xml {len(doc)} -> {len(newdoc)} chars")

if __name__ == '__main__':
    main()
