## ---------------------------------------------------------
## Project: Indonesia pulp deforestation
## Purpose: Build SI section 9 concession atlas (tiled PDF appendix)
## Author: Robert Heilmayr and Jason Jon Benedict
## ---------------------------------------------------------
##
## The atlas is a standalone PDF holding one small multiple per concession,
## six to a page, behind a hyperlinked index. It is merged onto the end of the
## Word-exported SI PDF at submission time (see
## scripts/04_figures_and_outputs/merge_si_with_atlas.R).
##
## Tiles are re-rendered rather than reusing the full-size figures in
## outputs/figures/concessions: those are 10 in wide, so tiling them 6-up
## scales them to 0.31 and renders their 9 pt axis labels at ~3 pt. Rendering
## natively at final size keeps type at its stated size. Both renderings descend
## from the same `hti_annual_lc` target, so they cannot diverge in data.
##
## Pipeline inputs (targets in _targets.R; paths relative to
##   data/01_data_replication/ unless noted)
##        1) hti_annual_lc -> 02_out/tables/hti_land_use_change_areas.csv:
##               Annual land cover areas within each concession.
##               Produced by scripts/02_data_preparation/01_data_prep.R
##        2) hti_conv_timing -> 02_out/tables/hti_grps_deforestation_timing.csv:
##               Supplier group and ownership class per concession.
##               Produced by scripts/02_data_preparation/01_data_prep.R
##        3) groups_reclass_hti -> 01_in/tables/ALIGNED_NAMES_GROUP_HTI_reclassed.csv:
##               Concession ownership groups, reclassified by hand.
##        4) hti -> 01_in/klhk/IUPHHK_HTI_TRASE_20230314_proj.shp: Concession
##               boundaries and names (project input).
##        5) atlas_template_file -> typst/concession_atlas.typ (in the repo):
##               Page layout of the atlas.
##        Requires the typst binary (see find_typst()).
##
## Pipeline outputs
##        1) atlas_meta: Per-concession metadata (display name, island,
##               ownership group) for tile labels and the atlas indices.
##        2) concession_tile_pngs -> outputs/figures/concession_tiles/: One
##               small tile per concession.
##        3) atlas_data_typ -> outputs/atlas/atlas_data.typ: Tile list read by
##               the typst template.
##        4) concession_atlas_pdf -> outputs/atlas/concession_atlas.pdf: The
##               standalone atlas (SI section 9).
##        merge_si_pdf() is not a target: it is run at submission time by
##               scripts/04_figures_and_outputs/merge_si_with_atlas.R.

library(tidyverse)
library(stringr)
library(scales)
library(showtext)
library(sysfonts)

ATLAS_TILE_WIDTH_IN <- 3.11
ATLAS_TILE_HEIGHT_IN <- 2.333
ATLAS_TILE_DPI <- 400

#' Locate a usable typst binary
#'
#' Searched in order: TYPST_BIN, PATH, the copies bundled inside Positron and
#' RStudio's Quarto, then common standalone install locations. Typst is used
#' rather than LaTeX because it needs no distribution install, and rather than
#' the R pdf() device because that device cannot emit hyperlink annotations or
#' PDF bookmarks.
find_typst <- function(min_version = "0.11.0") {
  arch <- if (grepl("^aarch64|^arm", R.version$arch)) "aarch64" else "x86_64"
  candidates <- c(
    Sys.getenv("TYPST_BIN", unset = NA),
    unname(Sys.which("typst")),
    file.path(
      c("/Applications", path.expand("~/Applications")),
      c("Positron.app", "RStudio.app"),
      "Contents/Resources/app/quarto/bin/tools",
      arch,
      "typst"
    ),
    file.path(
      c("/Applications", path.expand("~/Applications")),
      "RStudio.app/Contents/Resources/app/quarto/bin/tools/typst"
    ),
    "/usr/local/bin/typst",
    "/opt/homebrew/bin/typst",
    file.path(
      Sys.getenv("LOCALAPPDATA", unset = ""),
      "Programs/Positron/resources/app/quarto/bin/tools/typst.exe"
    ),
    "C:/Program Files/RStudio/resources/app/quarto/bin/tools/typst.exe"
  )
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]

  found <- list()
  for (cand in candidates) {
    if (!file.exists(cand)) next
    ver <- tryCatch(
      system2(cand, "--version", stdout = TRUE, stderr = TRUE),
      error = function(e) NA_character_
    )
    ver <- suppressWarnings(str_match(paste(ver, collapse = " "), "typst\\s+([0-9.]+)")[, 2])
    if (is.na(ver)) next
    if (utils::compareVersion(ver, min_version) < 0) next
    found[[length(found) + 1]] <- list(bin = cand, version = ver)
  }

  if (!length(found)) {
    stop(
      "No typst binary (>= ", min_version, ") found. Install one of:\n",
      "  macOS:   brew install typst\n",
      "  Windows: winget install Typst.Typst\n",
      "  any OS:  https://github.com/typst/typst/releases\n",
      "or set TYPST_BIN to an existing binary. Positron and RStudio each bundle\n",
      "a copy under Contents/Resources/app/quarto/bin/tools/.",
      call. = FALSE
    )
  }

  # Prefer the newest available; 0.15 has --pages, which 0.11 lacks.
  versions <- vapply(found, function(f) f$version, character(1))
  found[[which.max(vapply(versions, function(v) utils::compareVersion(v, "0.0.0"), numeric(1)) +
    seq_along(versions) * 1e-6)]]
}

#' Assemble per-concession metadata for atlas tile labels and indices
#'
#' Island is derived from the KLHK concession shapefile's province code
#' (`kode_prov`), whose first digit is the BPS island-group prefix. That covers
#' all 305 concessions, where `hti_grps_deforestation_timing.csv` -- the obvious
#' source, and the one used previously -- carries an island string for only 292.
#' The two agree on all 292 they share, which this function asserts.
#'
#' Corporate group comes from the aligned-names crosswalk. It is genuinely
#' sparse: only 81 of the 305 concessions have an identified parent group, and
#' the remaining 224 are independent or third-party suppliers with no named
#' affiliation in any available source. (`group_reclassed` in the same file is
#' not a corporate group -- it is a three-level supply-relationship class.)
build_atlas_metadata <- function(hti_annual_lc, hti_conv_timing, groups_reclass_hti, hti) {
  suppliers <- hti_annual_lc %>%
    filter(all == 1) %>%
    distinct(supplier_id, supplier)

  stopifnot(
    nrow(suppliers) == n_distinct(suppliers$supplier_id),
    all(str_detect(suppliers$supplier_id, "^H-[0-9]{4}$"))
  )

  # First digit of the province code is the BPS island group.
  island_of_prefix <- c(
    "1" = "Sumatera",
    "3" = "Jawa",
    "5" = "Balinusa",
    "6" = "Kalimantan",
    "7" = "Sulawesi",
    "8" = "Maluku",
    "9" = "Papua"
  )

  hti_attrs <- hti
  if (inherits(hti_attrs, "sf")) {
    hti_attrs <- sf::st_drop_geometry(hti_attrs)
  }
  islands <- hti_attrs %>%
    as_tibble() %>%
    distinct(supplier_id = ID, kode_prov) %>%
    filter(!is.na(kode_prov)) %>%
    mutate(island = island_of_prefix[substr(as.character(kode_prov), 1, 1)]) %>%
    select(supplier_id, island)

  dup_islands <- islands %>%
    count(supplier_id) %>%
    filter(n > 1)
  if (nrow(dup_islands)) {
    stop(
      "Concessions with more than one province code in the shapefile: ",
      paste(dup_islands$supplier_id, collapse = ", "),
      call. = FALSE
    )
  }

  # Cross-check against the timing table so a data refresh cannot silently
  # change the island assignment.
  check <- hti_conv_timing %>%
    filter(!is.na(island)) %>%
    distinct(supplier_id, island_timing = island) %>%
    inner_join(islands, by = "supplier_id") %>%
    filter(island_timing != island)
  if (nrow(check)) {
    stop(
      "Island derived from the shapefile disagrees with ",
      "hti_grps_deforestation_timing.csv for: ",
      paste(check$supplier_id, collapse = ", "),
      call. = FALSE
    )
  }

  groups <- groups_reclass_hti %>%
    distinct(supplier_id = id, group) %>%
    filter(!is.na(group)) %>%
    mutate(
      group = group %>%
        str_to_title() %>%
        # str_to_title lowercases acronyms
        str_replace_all("\\bRge\\b", "RGE") %>%
        str_replace_all("\\bAdr\\b", "ADR")
    )

  out <- suppliers %>%
    left_join(islands, by = "supplier_id") %>%
    left_join(groups, by = "supplier_id") %>%
    # Tile labels must fit one line, or a wrapped name pushes its figure down and
    # breaks the grid alignment. Dropping the "formerly known as" parenthetical
    # (DH = dahulu, EKS = eks) shortens the 10 longest names and, unlike dropping
    # every parenthetical, keeps the SK license numbers that are the only thing
    # distinguishing three pairs of concessions.
    mutate(
      display_name = str_trim(
        str_replace(supplier, "\\s*\\((?:DH|D/H|EKS)[^()]*\\)\\s*$", "")
      )
    ) %>%
    arrange(supplier_id)

  if (any(is.na(out$island))) {
    warning(
      "No island for: ", paste(out$supplier_id[is.na(out$island)], collapse = ", "),
      call. = FALSE
    )
  }

  out
}

#' Measure a rendered tile's panel left edge, in inches
#'
#' Finds the first pixel column containing one of the three land cover fills.
#' Used to align panels across tiles: the y-axis label block is 5-7 characters
#' wide depending on the concession's magnitude, which shifts the panel and is
#' visible in a two-column grid.
measure_panel_left_in <- function(png_path, dpi) {
  img <- png::readPNG(png_path)
  band <- img[round(dim(img)[1] * 0.5), , 1:3]
  r <- band[, 1]
  g <- band[, 2]
  b <- band[, 3]
  is_fill <-
    (abs(r - 0.000) < 0.15 & abs(g - 0.620) < 0.15 & abs(b - 0.451) < 0.15) |
      (abs(r - 0.941) < 0.12 & abs(g - 0.894) < 0.12 & abs(b - 0.259) < 0.12) |
      (abs(r - 0.800) < 0.12 & abs(g - 0.475) < 0.12 & abs(b - 0.655) < 0.12)
  if (!any(is_fill)) {
    return(NA_real_)
  }
  min(which(is_fill)) / dpi
}

#' Render one tile-sized land cover change figure per concession
#'
#' Differs from render_and_save_all_concessions() in four ways, all forced by the
#' 3.11 in tile width: no in-plot title (typst sets it in Palatino above the
#' tile, where it is selectable text and doubles as the index/bookmark anchor),
#' no legend (one shared legend runs in the page footer), five-year x breaks
#' instead of 22 annual ones, and no dead right margin.
render_and_save_concession_tiles <- function(hti_gav_annual_lc_df,
                                             atlas_meta,
                                             output_dir,
                                             width_in = ATLAS_TILE_WIDTH_IN,
                                             height_in = ATLAS_TILE_HEIGHT_IN,
                                             dpi = ATLAS_TILE_DPI) {
  tryCatch(
    {
      sysfonts::font_add_google(name = "DM Sans", family = "DM Sans")
    },
    error = function(e) NULL
  )
  showtext::showtext_auto()

  # The tryCatch above is the upstream pattern, but a silent fallback would
  # render all 305 tiles in the wrong font and go unnoticed until someone opened
  # the PDF. Fail loudly instead.
  if (!"DM Sans" %in% sysfonts::font_families()) {
    stop(
      "Font 'DM Sans' is not registered, so tiles would render in a fallback ",
      "font. font_add_google() needs network access.",
      call. = FALSE
    )
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  theme_tile <- theme(
    text = element_text(family = "DM Sans", colour = "#3A484F"),
    panel.background = element_rect(colour = NA, fill = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(
      color = "grey70",
      linetype = "dashed",
      linewidth = 0.2
    ),
    plot.title = element_blank(),
    # ylab("")/xlab("") would still reserve the axis-title space -- about 0.19 in
    # on the left and 0.08 in at the foot of a tile this size, all of it blank.
    axis.title = element_blank(),
    axis.line.x = element_line(linewidth = 0.25),
    axis.ticks = element_blank(),
    axis.text.x = element_text(size = 6, color = "grey30"),
    axis.text.y = element_text(size = 6, color = "grey30"),
    legend.position = "none"
  )

  # No " ha" suffix: repeating the unit on every tick costs about 0.13 in of
  # every tile's width, which is panel area. The unit is stated once in the
  # page footer and in the front matter instead.
  label_fun <- scales::label_number(
    scale_cut = scales::cut_short_scale(),
    accuracy = 0.1,
    drop0trailing = TRUE
  )

  build_tile <- function(id_, left_pad_in) {
    filtered_df <- hti_gav_annual_lc_df %>%
      filter(supplier_id == id_) %>%
      mutate(
        class_desc = ordered(
          class_desc,
          levels = c("Forest", "Non-forest", "Cleared for pulp")
        )
      )

    non_zero <- filtered_df %>%
      filter(area_ha > 0) %>%
      pull(class_desc) %>%
      unique() %>%
      sort()

    p <- ggplot(filtered_df, aes(year, area_ha)) +
      geom_area(
        aes(fill = as.factor(class_desc)),
        position = position_stack(reverse = FALSE)
      ) +
      scale_x_continuous(
        expand = c(0, 0),
        breaks = c(2001, 2005, 2010, 2015, 2020),
        limits = c(2001, 2022)
      ) +
      scale_y_continuous(
        breaks = scales::breaks_pretty(n = 3),
        labels = label_fun,
        expand = c(0, 0)
      ) +
      labs(x = NULL, y = NULL) +
      scale_fill_manual(
        values = c(
          "Forest" = "#009E73",
          "Non-forest" = "#F0E442",
          "Cleared for pulp" = "#CC79A7"
        ),
        breaks = non_zero
      ) +
      theme_tile +
      theme(
        # Top margin leaves room for the topmost tick label, which sits at the
        # panel edge and was being clipped at the image boundary.
        plot.margin = unit(c(0.07, 0.04, 0.02, 0.02 + left_pad_in), "in")
      )

    # Context lines, drawn only where the year falls inside the plotted window.
    # Kept identical to the full-size figures so the two agree.
    if (!all(is.na(filtered_df$zdc_year))) {
      p <- p +
        geom_vline(
          aes(xintercept = zdc_year),
          colour = "#000000",
          linewidth = 0.3,
          na.rm = TRUE
        )
    }

    if (
      !all(
        filtered_df$license_year < 2001 | filtered_df$license_year >= 2022,
        na.rm = TRUE
      )
    ) {
      p <- p +
        geom_vline(
          aes(xintercept = license_year),
          colour = "#000000",
          linewidth = 0.3,
          linetype = "dashed",
          na.rm = TRUE
        )
    }

    p
  }

  save_tile <- function(p, path) {
    # showtext's dpi must match ggsave's, or every point size is rescaled.
    showtext::showtext_opts(dpi = dpi)
    on.exit(showtext::showtext_opts(dpi = 96), add = TRUE)
    ggsave(
      plot = p,
      filename = path,
      dpi = dpi,
      width = width_in,
      height = height_in,
      units = "in",
      limitsize = FALSE
    )
    path
  }

  # The width of a tile's y-axis label block depends on which break labels it
  # draws ("0 ha" through "300K ha"), which shifts the panel's left edge and is
  # visible in a two-column grid. Predicting the labels proved unreliable -- the
  # plot drops out-of-range breaks, and "K", "." and space are not digit-width --
  # so render once, measure where each panel actually landed, and re-render with
  # the difference added to the left margin. Two passes cost ~20 s.
  ids <- atlas_meta$supplier_id
  pads <- rep(0, length(ids))

  if (requireNamespace("png", quietly = TRUE)) {
    probe_dir <- file.path(tempdir(), "atlas_probe")
    dir.create(probe_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(probe_dir, recursive = TRUE), add = TRUE)

    offsets <- vapply(
      ids,
      function(id_) {
        path <- file.path(probe_dir, paste0(id_, ".png"))
        save_tile(build_tile(id_, 0), path)
        measure_panel_left_in(path, dpi)
      },
      numeric(1),
      USE.NAMES = FALSE
    )

    if (!all(is.na(offsets))) {
      pads <- max(offsets, na.rm = TRUE) - offsets
      pads[is.na(pads)] <- 0
      message(
        sprintf(
          "Tile panel alignment: offsets spanned %.3f in across %d tiles; padding to align.",
          max(offsets, na.rm = TRUE) - min(offsets, na.rm = TRUE),
          sum(!is.na(offsets))
        )
      )
    }
  } else {
    warning(
      "Package 'png' is not available, so tile panels cannot be aligned; ",
      "y-axis label widths will shift panel edges between tiles.",
      call. = FALSE
    )
  }

  saved_filepaths <- character(nrow(atlas_meta))

  for (i in seq_along(ids)) {
    saved_filepaths[[i]] <- save_tile(
      build_tile(ids[[i]], pads[[i]]),
      file.path(output_dir, paste0(ids[[i]], ".png"))
    )
  }

  saved_filepaths
}

#' Escape a string for embedding in a typst string literal
typst_str <- function(x) {
  x <- str_replace_all(x, fixed("\\"), "\\\\")
  x <- str_replace_all(x, fixed("\""), "\\\"")
  paste0("\"", x, "\"")
}

#' Write the generated typst data file consumed by typst/concession_atlas.typ
#'
#' Layout lives in the git-tracked template; this file carries only data, so
#' iterating on layout costs a two-second recompile rather than a tile re-render.
write_atlas_data_typ <- function(tile_files, atlas_meta, typ_path, project_root = getwd()) {
  tiles <- tibble(
    path = tile_files,
    supplier_id = str_remove(basename(tile_files), "\\.png$")
  )

  meta <- atlas_meta %>%
    inner_join(tiles, by = "supplier_id") %>%
    arrange(supplier_id)

  if (nrow(meta) != nrow(atlas_meta)) {
    stop(
      "Tile files and metadata disagree: ", nrow(atlas_meta), " concessions but ",
      nrow(meta), " matched tiles.",
      call. = FALSE
    )
  }
  stopifnot(all(file.exists(meta$path)))

  # typst resolves paths against --root, so emit root-relative absolute paths.
  root <- normalizePath(project_root, winslash = "/", mustWork = TRUE)
  rel <- normalizePath(meta$path, winslash = "/", mustWork = TRUE) %>%
    str_remove(fixed(root)) %>%
    str_replace("^/?", "/")

  entries <- sprintf(
    "  (id: %s, name: %s, island: %s, group: %s, file: %s),",
    typst_str(meta$supplier_id),
    typst_str(meta$display_name),
    typst_str(coalesce(meta$island, "")),
    typst_str(coalesce(meta$group, "")),
    typst_str(rel)
  )

  # Secondary index by corporate group. Only a minority of concessions carry a
  # named affiliation, so the index lists those and states the remainder as a
  # count rather than padding itself with an "Other" list of the majority.
  grouped <- meta %>%
    filter(!is.na(group)) %>%
    add_count(group, name = "group_n") %>%
    arrange(desc(group_n), group, supplier_id)

  # Largest affiliations first; group_by() would re-sort the keys alphabetically
  # and discard that ordering, so walk the groups explicitly.
  group_order <- grouped %>%
    distinct(group, group_n) %>%
    arrange(desc(group_n), group) %>%
    pull(group)

  group_blocks <- group_order %>%
    map(function(gname) {
      rows <- grouped %>% filter(group == gname)
      c(
        sprintf("  (name: %s, entries: (", typst_str(gname)),
        sprintf(
          "    (id: %s, name: %s),",
          typst_str(rows$supplier_id),
          typst_str(rows$display_name)
        ),
        "  )),"
      )
    }) %>%
    unlist()

  if (!dir.exists(dirname(typ_path))) {
    dir.create(dirname(typ_path), recursive = TRUE, showWarnings = FALSE)
  }

  writeLines(
    c(
      "// Generated by write_atlas_data_typ(). Do not edit by hand.",
      sprintf("// %d concessions, ordered by concession id.", nrow(meta)),
      "#let tiles = (",
      entries,
      ")",
      "",
      sprintf(
        "// %d concessions under %d named corporate groups; %d unaffiliated.",
        nrow(grouped),
        n_distinct(grouped$group),
        nrow(meta) - nrow(grouped)
      ),
      "#let groups = (",
      group_blocks,
      ")",
      "",
      sprintf("#let ungrouped_count = %d", nrow(meta) - nrow(grouped))
    ),
    typ_path
  )

  typ_path
}

#' Compile the atlas PDF with typst
#'
#' `sm_pages` offsets the page counter so folios continue the page numbering of
#' the Word-exported SI rather than restarting at 1. Pipeline builds pass 0; the
#' merge script recompiles with the real count.
compile_concession_atlas <- function(template_file,
                                     data_typ,
                                     pdf_path,
                                     sm_pages = 0L,
                                     project_root = getwd()) {
  typst <- find_typst()
  if (!str_starts(typst$version, "0.15")) {
    warning(
      "Building with typst ", typst$version,
      "; the atlas layout was developed against 0.15.x. Page counts can drift ",
      "by a page between versions.",
      call. = FALSE
    )
  }

  if (!dir.exists(dirname(pdf_path))) {
    dir.create(dirname(pdf_path), recursive = TRUE, showWarnings = FALSE)
  }

  root <- normalizePath(project_root, winslash = "/", mustWork = TRUE)
  stopifnot(file.exists(data_typ))

  args <- c(
    "compile",
    "--root", shQuote(root),
    "--input", shQuote(paste0("sm-pages=", as.integer(sm_pages))),
    shQuote(normalizePath(template_file, mustWork = TRUE)),
    shQuote(pdf_path)
  )

  out <- system2(typst$bin, args, stdout = TRUE, stderr = TRUE)
  status <- attr(out, "status")

  # Unresolved names in a font fallback chain warn and fall through, so warnings
  # on stderr are expected. Only the exit status means failure.
  if (!is.null(status) && status != 0) {
    stop(
      "typst compile failed (status ", status, "):\n",
      paste(out, collapse = "\n"),
      call. = FALSE
    )
  }
  if (length(out)) {
    message(paste(out, collapse = "\n"))
  }

  message("Built ", pdf_path, " with typst ", typst$version)
  pdf_path
}

#' Count pages in a PDF using poppler's pdfinfo
pdf_page_count <- function(pdf_path) {
  info <- system2("pdfinfo", shQuote(normalizePath(pdf_path, mustWork = TRUE)),
    stdout = TRUE, stderr = TRUE
  )
  n <- suppressWarnings(as.integer(str_match(
    paste(info, collapse = "\n"),
    "Pages:\\s+([0-9]+)"
  )[, 2]))
  if (is.na(n)) {
    stop("Could not read a page count from pdfinfo for ", pdf_path, call. = FALSE)
  }
  n
}

#' Count link annotations in a PDF
pdf_link_count <- function(pdf_path) {
  raw <- readBin(pdf_path, "raw", file.size(pdf_path))
  txt <- rawToChar(raw[raw != as.raw(0)])
  sum(
    str_count(txt, fixed("/Subtype /Link")),
    str_count(txt, fixed("/Subtype/Link"))
  )
}

#' Merge the concession atlas onto the end of the exported SI PDF
#'
#' Recompiles the atlas first so its folios continue the SI's page numbering,
#' then concatenates with poppler's pdfunite.
#'
#' Note: pdfunite preserves link annotations but discards the document outline,
#' so the merged file has working internal links and no PDF bookmarks. The
#' standalone atlas keeps its 305 bookmarks. Verified on poppler 26.04.0. If
#' bookmarks are wanted in the merged file, `pip install pypdf` and use
#' PdfWriter().append(), which preserves outlines, or submit the atlas as a
#' separate supplementary file.
merge_si_pdf <- function(si_pdf,
                         out_pdf,
                         template_file = "typst/concession_atlas.typ",
                         data_typ = "outputs/atlas/atlas_data.typ",
                         atlas_pdf = "outputs/atlas/concession_atlas.pdf") {
  pdfunite <- Sys.which("pdfunite")
  if (!nzchar(pdfunite)) {
    stop(
      "pdfunite not found. Install poppler (macOS: brew install poppler; ",
      "Debian/Ubuntu: apt-get install poppler-utils).",
      call. = FALSE
    )
  }

  si_pages <- pdf_page_count(si_pdf)
  message("Exported SI is ", si_pages, " pages; numbering the atlas from ", si_pages + 1, ".")

  compile_concession_atlas(template_file, data_typ, atlas_pdf, sm_pages = si_pages)
  atlas_pages <- pdf_page_count(atlas_pdf)
  atlas_links <- pdf_link_count(atlas_pdf)

  if (!dir.exists(dirname(out_pdf))) {
    dir.create(dirname(out_pdf), recursive = TRUE, showWarnings = FALSE)
  }

  status <- system2(
    pdfunite,
    shQuote(c(normalizePath(si_pdf, mustWork = TRUE), atlas_pdf, out_pdf))
  )
  if (status != 0) {
    stop("pdfunite failed with status ", status, call. = FALSE)
  }

  merged_pages <- pdf_page_count(out_pdf)
  merged_links <- pdf_link_count(out_pdf)

  if (merged_pages != si_pages + atlas_pages) {
    stop(
      "Merged page count is ", merged_pages, ", expected ",
      si_pages + atlas_pages, " (", si_pages, " + ", atlas_pages, ").",
      call. = FALSE
    )
  }
  if (merged_links < atlas_links) {
    stop(
      "Merged file has ", merged_links, " link annotations but the atlas alone ",
      "had ", atlas_links, "; the merge dropped internal links.",
      call. = FALSE
    )
  }

  message(
    "Merged ", merged_pages, " pages (", si_pages, " + ", atlas_pages, "), ",
    merged_links, " link annotations preserved."
  )
  warning(
    "pdfunite discards the PDF outline, so the merged file has no bookmarks. ",
    "The standalone atlas at ", atlas_pdf, " retains them.",
    call. = FALSE
  )

  out_pdf
}
