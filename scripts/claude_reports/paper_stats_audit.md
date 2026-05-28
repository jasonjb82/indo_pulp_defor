# Paper Statistics Audit: 12_paper_stats.R vs. Published Paper

**Script:** `scripts/04_figures_and_outputs/12_paper_stats.R`  
**Paper:** `spf_pulp_defor_r2.docx`  
**Generated:** 2026-05-20  
**Updated:** 2026-05-20 — script executed; all values verified against actual output

This report catalogs every empirical statistic reported in the main text of the paper, maps it to the script code that should produce it, and flags discrepancies or gaps. Statistics are organized in the order they appear in the paper.

**Legend:**
- ✅ Computed in script, value consistent with paper
- ⚠️ Computed in script, but value or methodology inconsistent with paper
- ❌ Not currently computed in `12_paper_stats.R` (missing or commented out)
- 🔲 Paper contains a blank/unfilled placeholder — value expected from script

---

## Abstract

| # | Paper text | Paper value | Script output | Script lines | Status |
|---|-----------|-------------|--------------|-------------|--------|
| A1 | "Between 2001 and 2011, ___ hectares of rainforest were directly converted to pulpwood plantations" | **[BLANK in docx]** | **734,974 ha** (≈ 735,000 ha) | 216–219 | 🔲 Computed correctly; paper placeholder not yet filled. Value not assigned to a named variable. |

---

## Progress Towards Zero-Deforestation

| # | Paper text | Paper value | Script output | Script lines | Status |
|---|-----------|-------------|--------------|-------------|--------|
| P1 | "pulp-driven deforestation fell by 95%" | –95% | **–94.9%** (`conv_2011` = 108,096; `conv_2017` = 5,554) | 223–226 | ✅ |
| P2 | "which expanded by 1.__ million hectares between 2000 and **2011**" | **[BLANK]** | **1,623,439 ha** (2000→2015) | 278–281 | ⚠️ **Date mismatch:** script computes 2000→2015 expansion (`pulp_2015 - pulp_2000`); paper text says "2000 and 2011". Script comment also references "2000 and 2015." The paper date or script date range needs to be reconciled. |
| P3 | "plantations now supply nearly all of Indonesia's **47 million m³** of annual pulpwood demand" | 47,000,000 m³ | **46,555,205 m³** | 284 | ✅ Rounds to 47 million m³ |

---

## "Despite the sector's ambitious goals..." paragraph (Deforestation 2015–2022)

| # | Paper text | Paper value | Script output | Script lines | Status |
|---|-----------|-------------|--------------|-------------|--------|
| D1 | "107,400 hectares of forests were directly converted to pulpwood plantations between 2015 and 2022" | 107,400 ha | **107,353 ha** | 374–385 | ✅ Rounds to 107,400 ha |
| D2 | "Concessions officially claimed by APP and APRIL had little pulp-driven deforestation after 2015 (**3,600 ha**)" | 3,600 ha | **3,615 ha** ("Owned or acknowledged" row) | 374–383 | ✅ Rounds to 3,600 ha |
| D3 | "responsible for **63,900 ha (60%)** of pulp-driven deforestation during this period" | 63,900 ha / 60% | **63,895 ha / 59.5%** ("NGO-linked" row) | 374–383 | ✅ Rounds to 63,900 ha / 60% |
| D4 | "The remaining **37%** of pulp-driven deforestation occurred in concessions controlled by external suppliers" | 37% | **37.1%** ("Indirect supplier" 32.8% + NA 4.3%) | 374–383 | ✅ Rounds to 37% |

> **Note on D1–D4:** The ZDC violations section (lines 305–368) is commented out and replaced by `ownership_defor`, which groups by `group_reclassed` from `ALIGNED_NAMES_GROUP_HTI_reclassed.csv`. All four paper values are readable from the printed table but none are extracted to named scalars, making automated verification difficult.

---

## An Emerging Boom in Deforestation

| # | Paper text | Paper value | Script output | Script lines | Status |
|---|-----------|-------------|--------------|-------------|--------|
| E1 | "annual rate of conversion increased from **5,600 ha/year** to **24,300 ha/year**" | 5,600 and 24,300 | **5,554** and **24,304** | 223–230 | ✅ Round to 5,600 and 24,300 |
| E2 | "(**338% increase**)" | 338% | **337.6%** | 231–232 | ✅ Rounds correctly; apparent arithmetic inconsistency from independently rounded inputs |
| E3 | "pulp-driven conversion of peatlands increased from **1,500 ha/year** to **7,800 ha/year**" | 1,500 and 7,800 | **1,427** and **7,784** | 250–251 | ✅ Round to nearest 100: 1,400 and 7,800; close to paper rounding |
| E4 | "(**446% increase**)" | 446% | **445.7%** | 252–253 | ✅ Rounds correctly |
| E5 | "deforestation rates in 2022 were still **76% lower** than the 2011 peak" | –76% | **–77.5%** | 234–236 | ⚠️ Script gives –77.5%; paper says 76%. User confirmed paper value is stale — paper should be updated to 78%. |
| E6 | "the region has been responsible for **89%** of pulp-driven deforestation since 2017" | 89% | **89.0%** | 258–266 | ✅ |

---

## New Pulp Mills and Capacity Expansion

> **Note on C2–C4:** The capacity/scenario calculations were previously duplicated between `12_paper_stats.R` and `22_pulp_expansion_scenarios.R` with stale inline comments. As of 2026-05-20, `12_paper_stats.R` now loads C3/C4/productivity stats from `scenario_stats.csv` (produced by `22_pulp_expansion_scenarios.R`). Run `22_pulp_expansion_scenarios.R` before `12_paper_stats.R`.

| # | Paper text | Paper value | Script output | Script lines | Status |
|---|-----------|-------------|--------------|-------------|--------|
| C1 | "over **95%** of the industry's production capacity" [Sinar Mas + RGE] | 95% | Not computed | — | ❌ Stated as context in the paper; no calculation in `12_paper_stats.R` |
| C2 | "increase the country's pulp capacity by **78%** (**8.53 million tonnes**)" | 78% / 8.53 Mt | **78.2%** / **8.53 Mt** | capacity section | ✅ Previous inline comment saying "91%" was stale; dynamic calculation matches paper |
| C3 | "increase the country's annual demand for pulpwood by **34 million m³**" | 34,000,000 m³ | **34.27 million m³** (from `scenario_stats.csv`) | loaded from `22_pulp_expansion_scenarios.R` | ✅ Rounds to 34 million m³ |
| C4 | "an additional **1.84 million hectares** of plantations will be needed" | 1,840,000 ha | **1.838 million ha** (from `scenario_stats.csv`) | loaded from `22_pulp_expansion_scenarios.R` | ✅ Previous inline comment saying "1.63 million hectares" was stale |

---

## Barriers Facing Productivity Improvements

| # | Paper text | Paper value | Script output | Script lines | Status |
|---|-----------|-------------|--------------|-------------|--------|
| B1 | "productivity of approximately ___ per year (S3)" | **[BLANK]** | **3.2%/year** (`mai_df$yield_growth = 0.032`, from `key_parameters.csv`) | `08_calc_mai.R` → `key_parameters.csv` | 🔲 Value is confirmed at 3.2%/year (with CI); paper placeholder not yet filled. See note below. |

> **Note on B1:** `key_parameters.csv` contains `yield_growth = 0.032` (3.2%/year). This is the authoritative value used in `22_pulp_expansion_scenarios.R` for scenario projections. The previously hardcoded `yield_growth = 1.059` override has been removed from `12_paper_stats.R` as part of the 2026-05-20 refactor.

---

## "We find that 2.94 million hectares of primary forests..." paragraph

| # | Paper text | Paper value | Script output | Script lines | Status |
|---|-----------|-------------|--------------|-------------|--------|
| F1 | "**2.94 million hectares** of primary forests, **18%** of which are on peat soils" | 2.94 Mha / 18% | **2.964 Mha** / **18.5%** | 290–297 | ⚠️ **Area discrepancy:** script gives 2.964 Mha; paper says 2.94 Mha (gap of ~24,000 ha / 0.8%). Peat share rounds to 18% ✅. Total area gap may reflect a change in input data or filter criteria since the paper was written. |
| F2 | "**46%** of these forests within pulp concessions are located in Kalimantan" | 46% | Not computed | — | ❌ No island-level breakdown of remaining primary forests in `12_paper_stats.R`. |

---

## Summary Table

| Status | Count | Statistics |
|--------|-------|-----------|
| ✅ Computed, consistent with paper | 14 | P1, P3, D1, D2, D3, D4, E1, E2, E3, E4, E6, C2, C3, C4 |
| ⚠️ Computed but inconsistent with paper | 3 | P2 (date mismatch), E5 (76% vs 77.5%), F1 (2.94 vs 2.96 Mha) |
| ❌ Not computed in this script | 2 | C1, F2 |
| 🔲 Blank placeholder in paper | 2 | A1 (735K ha confirmed), B1 (3.2%/yr confirmed) |

---

## Priority Issues to Resolve

### 1. 🔴 HIGH — E5: paper says 76% lower, script gives 77.5% (stale)
User confirmed the 76% figure in the paper is stale. The script computes `(conv_2022 - conv_2011) / conv_2011 = –77.5%`, which rounds to **–78%**, not –76%. **Paper should be updated to "78% lower."**

### 2. 🟠 MEDIUM — F1: 2.94 Mha vs. 2.964 Mha remaining primary forests
The script sums GFC TTM pixels with values in {100, 400, 600} and gets 2,964,002 ha. The paper says "2.94 million hectares." The gap of ~24,000 ha (~0.8%) likely reflects a change in input data or filter criteria. Needs investigation — either the input data changed and the paper number is stale, or the filter has drifted from what produced the original published figure.

### 3. 🟠 MEDIUM — P2: date mismatch in plantation expansion
The paper says "expanded by 1.___ million hectares between 2000 and **2011**." The script computes `pulp_2015 - pulp_2000`. Either the paper text or the script endpoint needs to be corrected. The script's inline comment also references "2000 and 2015," so this appears to be a paper text error.

### 4. 🟡 LOW — A1 and B1: blank placeholders in paper
- A1: 2001–2011 deforestation = 734,974 ha ≈ 735,000 ha. Paper placeholder should be filled.
- B1: productivity growth = 3.2%/year. Paper placeholder should be filled.

### 5. 🟡 LOW — D1–D4: ownership breakdown not extracted as named scalars
All four ZDC statistics are readable from the printed `ownership_defor` table but are not assigned to named variables, making automated verification impossible.

### 6. 🟡 LOW — F2: Kalimantan share of remaining forests not computed (46%)
No island-level breakdown of remaining primary forests within HTI concessions exists in `12_paper_stats.R`.

### 7. 🟡 LOW — Script crashes at line 549 on Windows path
`read_excel(paste0(wdir, '\\01_data\\01_in\\wwi\\RPBBI_2022_compiled.xlsx'))` uses backslashes and fails on macOS. This affects SI-section stats (wood type share, active supplier share) but not any main-text paper statistics. Fix: replace `\\` with `/`.

---

## Statistics From Other Scripts Referenced in Paper

| Paper stat | Value | Source script | Variable |
|-----------|-------|--------------|---------|
| "productivity of approximately ___ per year" | **3.2%/yr** | `03_analysis_modelling/08_calc_mai.R` | `key_parameters.csv: yield_growth` |
| ROC-AUC of RF model | 0.957 | `03_analysis_modelling/21_pulp_expansion_model.R` | Printed to console only |
| Expansion scenarios (1.84M ha total, 150K ha deforestation, etc.) | Various | `03_analysis_modelling/22_pulp_expansion_scenarios.R` | `scenario_stats.csv` (as of 2026-05-20) |
