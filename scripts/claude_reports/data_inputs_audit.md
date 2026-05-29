# Data Inputs Audit & Simplification Strategy

**Scope:** All scripts in `03_analysis_modelling/` and `04_figures_and_outputs/`
**Purpose:** Streamline the data bundle for a journal replication package.

> Note: The brief referenced `05_figures_and_outputs/`, but the matching folder in this repo is `04_figures_and_outputs/`. This report covers that folder.

Scripts audited:

- `03_analysis_modelling/08_calc_mai.R`
- `03_analysis_modelling/17_defor_elasticity.R`
- `03_analysis_modelling/20_pulp_expansion_table.R`
- `03_analysis_modelling/21_pulp_expansion_model.R`
- `03_analysis_modelling/22_pulp_expansion_scenarios.R`
- `03_analysis_modelling/23_rs_accuracy_assessment.R`
- `04_figures_and_outputs/01_summary_figure.R`
- `04_figures_and_outputs/02_deforestation_timing.R`
- `04_figures_and_outputs/03_land_use_change.R`
- `04_figures_and_outputs/12_paper_stats.R`

---

## Task 1: Data Input Audit Table

| Name | Link | Type | Format | Source | Script | Description |
| :--- | :--- | :--- | :--- | :--- | :--- | :--- |
| HTI harvest year totals | `remote/01_data/02_out/tables/hti_harvest_yr.csv` | Non-spatial | csv | Intermediate output (upstream prep) | 08_calc_mai.R | Single CSV; annual harvested area per HTI concession with peat splits and weather covariates |
| Merged wood supply 2015–2022 | `remote/01_data/02_out/tables/ws_merge_clean_2015_2022.csv` | Non-spatial | csv | Intermediate output (consolidated RPBBI) | 08_calc_mai.R, 12_paper_stats.R | Single CSV replacing the four annual RPBBI xlsx files |
| HTI MAI (intermediate) | `remote/01_data/02_out/tables/hti_mai.csv` | Non-spatial | csv | Intermediate output from 08_calc_mai.R | 17_defor_elasticity.R | Concession-level mean annual increment |
| Pulp deforestation panel (10 km) | `remote/01_data/02_out/tables/tbl_long_pulp_clearing_gfc_forest.csv` | Non-spatial | csv | Intermediate output | 17_defor_elasticity.R | Long panel of pulp clearing × forest by pixel-year |
| RISI / Fastmarkets pulp prices | `remote/01_data/01_in/wwi/Fastmarkets_2025_01_14-103617.xlsx` | Non-spatial | xlsx | Fastmarkets RISI | 17_defor_elasticity.R | Daily BHKP price series (Indo / SA / NA-SC); read with `skip=4` |
| WRQ pulpwood prices | `remote/01_data/01_in/wwi/WRQ_pulpwood_prices.xlsx` | Non-spatial | xlsx | Wood Resources Quarterly | 17_defor_elasticity.R | Quarterly Indonesian pulpwood prices (USD/m3) |
| FRED IDR/USD exchange rate | `remote/01_data/01_in/tables/FRED_CCUSSP02IDM650N.csv` | Non-spatial | csv | FRED | 17_defor_elasticity.R | Single CSV, monthly exchange rate |
| FRED Indonesian CPI | `remote/01_data/01_in/tables/FRED_IDNCPIALLAINMEI.csv` | Non-spatial | csv | FRED | 17_defor_elasticity.R | Single CSV, annual CPI |
| Mill transport costs | `remote/01_data/02_out/tables/centroids_mills_cost.csv` | Non-spatial | csv | Intermediate output | 17_defor_elasticity.R | Per-pixel USD/tonne transport cost to mills |
| GAEZ grid share | `remote/01_data/02_out/tables/gaez_grid_share.csv` | Non-spatial | csv | GAEZ (FAO/IIASA), aggregated to grid | 17_defor_elasticity.R | Pixel-level GAEZ suitability class share |
| GAEZ HTI areas | `remote/01_data/02_out/tables/gaez_hti_areas.csv` | Non-spatial | csv | GAEZ, summarized by HTI | 17_defor_elasticity.R | Concession × GAEZ class area |
| 10 km grid admin lookup | `remote/01_data/02_out/tables/grid_10km_adm_prov_kab_kec.csv` | Non-spatial | csv | Intermediate output | 17_defor_elasticity.R | Pixel-to-prov/kab/kec crosswalk |
| Mill capacities | `remote/01_data/01_in/wwi/MILLS_EXPORTERS_20200405.xlsx` | Non-spatial | xlsx | WWI mill registry | 17_defor_elasticity.R, 01_summary_figure.R, 12_paper_stats.R | Mill metadata + pulp capacity (MTPY) |
| Mill production 2015–2024 | `remote/01_data/01_in/wwi/MILL_PRODUCTION_2015_2024.xlsx` | Non-spatial | xlsx | WWI | 17_defor_elasticity.R | Annual mill output series |
| HTI license dates | `remote/01_data/01_in/wwi/HTI_LICENSE_DATES.csv` | Non-spatial | csv | WWI / MoEF | 20_pulp_expansion_table.R | Per-concession license date table |
| HTI sample IDs | `remote/01_data/02_out/samples/samples_hti_id.csv` | Non-spatial | csv | Intermediate output (GEE sampling) | 20_pulp_expansion_table.R | sid-to-HTI lookup |
| HTI concession boundaries | `remote/01_data/01_in/klhk/IUPHHK_HTI_TRASE_20230314_proj.shp` | Spatial | shapefile | KLHK | 20_pulp_expansion_table.R, 12_paper_stats.R | Multi-file shapefile of concession polygons |
| Annual pulp expansion table | `remote/01_data/02_out/tables/pulp_expansion_areas_2001_2022.csv` | Non-spatial | csv | Intermediate output (written by 12_paper_stats.R) | 20_pulp_expansion_table.R | Annual national pulp expansion totals |
| HTI/non-HTI conversion (TreeMap) | `remote/01_data/02_out/tables/idn_pulp_conversion_hti_nonhti_treemap.csv` | Non-spatial | csv | Intermediate output (TreeMap-derived) | 20_pulp_expansion_table.R, 12_paper_stats.R | Annual conversion areas by HTI / non-HTI |
| TreeMap landuse samples | `remote/01_data/02_out/tables/samples_landuse_ttm.csv` | Non-spatial | csv | Intermediate output (concatenated GEE export) | 20_pulp_expansion_table.R | Per-sample annual TreeMap class table |
| GFC-TTM sample classes | `remote/01_data/02_out/tables/samples_gfc_ttm.csv` | Non-spatial | csv | Intermediate output (concatenated GEE export) | 20_pulp_expansion_table.R, 12_paper_stats.R | Per-sample GFC × TreeMap codes |
| Pulp expansion model vars (2017) | `remote/01_data/02_out/tables/pulp_exp_model_var_1km_2017.csv` | Non-spatial | csv | Intermediate (GEE 1 km stack) | 21_pulp_expansion_model.R | 1 km pixel-level predictor panel, 2017 baseline |
| Pulp expansion model vars (2022) | `remote/01_data/02_out/tables/pulp_exp_model_var_1km_2022.csv` | Non-spatial | csv | Intermediate (GEE 1 km stack) | 21_pulp_expansion_model.R, 22_pulp_expansion_scenarios.R | 1 km predictor panel, 2022 baseline |
| Kabupaten boundaries (BIG) | `remote/01_data/01_in/big/idn_kabupaten_big.shp` | Spatial | shapefile | BIG (Indonesia) | 21_pulp_expansion_model.R, 22_pulp_expansion_scenarios.R, 01_summary_figure.R, 12_paper_stats.R | Multi-file shapefile, district polygons |
| Pulp predictions (2022→2027) | `remote/01_data/02_out/tables/pulp_predictions.csv` | Non-spatial | csv | Intermediate output from 21 | 22_pulp_expansion_scenarios.R | Pixel-level predicted probabilities + coords |
| Key MAI parameters | `remote/01_data/04_results/key_parameters.csv` | Non-spatial | csv | Intermediate output from 08 | 22_pulp_expansion_scenarios.R, 12_paper_stats.R | Single-row CSV of MAI / yield-growth scalars |
| Scenario stats | `remote/01_data/04_results/scenario_stats.csv` | Non-spatial | csv | Intermediate output from 22 | 12_paper_stats.R | Single-row CSV of scenario outputs |
| Validation spreadsheet (11 classes) | `remote/01_data/01_in/gaveau/Validation_11classes_land-cover-change-map_v1-2.xlsx` | Non-spatial | xlsx | Gaveau validation campaign | 23_rs_accuracy_assessment.R | Multi-sheet xlsx (Inputs `A6:C17` + Points sheets) |
| Policy timeline | `remote/01_data/01_in/tables/policy_timeline_cats_rev1.csv` | Non-spatial | csv | Authors' compilation | 01_summary_figure.R | Single CSV of dated policy events |
| Annual pulp deforestation (Indonesia, forest) | `remote/01_data/02_out/gee/gaveau/pulp_annual_defor_forest_id.csv` | Non-spatial | csv (GEE export) | GEE / TreeMap | 01_summary_figure.R | Single GEE export CSV; province × year wide table |
| Annual pulp expansion (Indonesia, non-forest) | `remote/01_data/02_out/gee/gaveau/pulp_annual_defor_non-forest_id.csv` | Non-spatial | csv (GEE export) | GEE / TreeMap | 01_summary_figure.R | Single GEE export CSV; province × year wide table |
| Timber-for-pulp (Obidzinski & Dermawan) | `remote/01_data/01_in/obidzinski_dermawan/plot_data.csv` | Non-spatial | csv | Obidzinski & Dermawan digitized | 01_summary_figure.R | Single CSV (digitized plot data) |
| Pulp prices (FRED WPU0911) | `remote/01_data/01_in/tables/WPU0911_FRED.csv` | Non-spatial | csv | FRED | 01_summary_figure.R | Single CSV, monthly PPI |
| Annual pulp production share (MoEF) | `remote/01_data/01_in/tables/annual_pulp_shr_prod.xlsx` | Non-spatial | xlsx | KLHK / MoEF | 01_summary_figure.R | Single xlsx, annual pulp production with MTH / plantation shares |
| HTI deforestation timing | `remote/01_data/02_out/tables/hti_grps_deforestation_timing.csv` | Non-spatial | csv | Intermediate output | 02_deforestation_timing.R, 12_paper_stats.R | Concession-level pulp / non-pulp clearing × ZDC timing |
| HTI annual land-use change | `remote/01_data/02_out/tables/hti_land_use_change_areas.csv` | Non-spatial | csv | Intermediate output | 03_land_use_change.R | Concession-year-class area panel (drives per-HTI plot loop) |
| Cleaned wood supply (aligned) | `remote/01_data/01_in/wwi/PULP_WOOD_SUPPLY_CLEAN_ALL_ALIGNED_2020_2022.csv` | Non-spatial | csv | WWI | 12_paper_stats.R | Single cleaned CSV |
| Annual expansion stats (TreeMap) | `remote/01_data/02_out/tables/id_annual_expansion_stats_ttm.csv` | Non-spatial | csv | Intermediate output | 12_paper_stats.R | Annual national pulp + palm forest / non-forest expansion |
| Kalimantan annual pulp expansion stats | `remote/01_data/02_out/tables/kali_annual_pulp_exp_stats_ttm.csv` | Non-spatial | csv | Intermediate output (replaces Gaveau xlsx Kalimantan tab) | 12_paper_stats.R | Single CSV of Kalimantan-only pulp forest loss |
| Annual pulp area within HTI | `remote/01_data/02_out/gee/pulp_annual_area_hti_only.csv` | Non-spatial | csv (GEE export) | GEE / TreeMap | 12_paper_stats.R | Single GEE export CSV |
| Annual pulp area (Indonesia) | `remote/01_data/02_out/gee/pulp_annual_area_id.csv` | Non-spatial | csv (GEE export) | GEE / TreeMap | 12_paper_stats.R | Single GEE export CSV |
| Group reclass (ownership) | `remote/01_data/01_in/tables/ALIGNED_NAMES_GROUP_HTI_reclassed.csv` | Non-spatial | csv | Authors' compilation | 12_paper_stats.R | Concession-to-conglomerate crosswalk |
| Gaveau annual pulp areas (HTI) | `remote/01_data/02_out/tables/gaveau_annual_pulp_areas.csv` | Non-spatial | csv | Intermediate output | 12_paper_stats.R | Sample-level annual pulp class table |
| Pulp expansion on peat / mineral | `remote/01_data/02_out/gee/gaveau/idn_pulp_annual_expansion_peat_mineral_soils.csv` | Non-spatial | csv (GEE export) | GEE / TreeMap | 12_paper_stats.R | Single CSV split by soil type |

**Audit deltas vs. prior version:**

- **08_calc_mai.R** no longer reads the three annual RPBBI xlsx files (`RPBBI_2015_2019_compiled.xlsx`, `RPBBI_2020_compiled.xlsx`, `RPBBI_2021_compiled.xlsx`); it now consumes the consolidated `ws_merge_clean_2015_2022.csv` directly.
- **12_paper_stats.R** no longer reads the Gaveau `IDN_2001_2022 landcover change of Oil Palm and Pulpwood_05JUNE2023.xlsx` at all — its Kalimantan-tab data has been promoted to a standalone CSV (`kali_annual_pulp_exp_stats_ttm.csv`).
- Net effect: the audit list has lost four `.xlsx` raw inputs (three RPBBI files + Gaveau LCC workbook) since the prior version. The replication bundle's Excel surface area is shrinking on its own.

---

## Task 2: Data Optimization & Simplification Strategy

### 1. Consolidation
- **Wood supply pipeline** is now consolidated upstream (`ws_merge_clean_2015_2022.csv`). Two cleanups remain: (a) confirm `PULP_WOOD_SUPPLY_CLEAN_ALL_ALIGNED_2020_2022.csv` isn't a near-duplicate of the merged file — if it is, drop it; (b) ensure `08` and `12` actually read the same canonical file (they now both point at `ws_merge_clean_2015_2022.csv`, so this looks clean).
- **GAEZ grid + GAEZ HTI** (`gaez_grid_share.csv`, `gaez_hti_areas.csv`) — both feed the same aggregated 4-class scheme in `17_defor_elasticity.R`; precompute the aggregated columns and ship one slim file per scope.
- **Two scalar parameter files** (`key_parameters.csv`, `scenario_stats.csv`) are single-row handoff CSVs. Merge into one `parameters.csv` or have `12_paper_stats.R` recompute.
- **FRED IDR/USD + FRED CPI** are both small annual series — merge to one `fred_macro.csv`.
- The two `pulp_annual_defor_*_id.csv` (forest / non-forest) and `pulp_annual_area_id.csv` are all GEE province-level wide tables — merge to one long-format file.
- **National + Kalimantan TreeMap stats** (`id_annual_expansion_stats_ttm.csv`, `kali_annual_pulp_exp_stats_ttm.csv`) are now two tiny CSVs with the same schema — could be a single long-format file with a `region` column.

### 2. Pruning
- **All GEE exports** carry `system:index`, `.geo`, `constant`, `kab`, `kab_code`, `prov_code`, `type` columns that scripts immediately drop. Strip these before shipping (huge wins on `.geo` which is per-row WKT geometry).
- **`MILLS_EXPORTERS_20200405.xlsx`** is read in three scripts and each time only 2–3 columns (`MILL_ID`, `PULP_CAP_MTPY`) are kept. Ship a slim CSV.
- **`MILL_PRODUCTION_2015_2024.xlsx`** — only `MILL_ID, YEAR, TOTAL_PROD_KG_NET` are used.
- **Fastmarkets pulp prices** — only 3 series columns are kept (`fp_plp_0045 / 0053 / 0056`); drop other series and reformat to long.
- **`IUPHHK_HTI_TRASE_20230314_proj.shp`** — scripts only use `ID` and `namaobj` (plus geometry). Drop other attributes; consider geometry simplification (~50–100 m tolerance) since outputs sit at 1 km / 10 km grid scale.
- **`idn_kabupaten_big.shp`** — only `prov, prov_code, kab, kab_code` are used and maps are at country scale. Simplify geometries and drop other columns.
- **`pulp_exp_model_var_1km_*.csv`** — by far the heaviest tables (1 km × Sumatra+Kalimantan × ~25 covariates + 64 `ya_*` spectral anomaly indices). The `ya_*` indices dominate file size; check variable importance and prune any negligible ones.
- **`pulp_predictions.csv`** — already pruned by 21 to 7 columns; good.

### 3. Aggregation
- **`pulp_exp_model_var_1km_*.csv`**: the model averages predictions to 0.1° (~10 km) for plotting. If the public deliverable is the figures and tables (not RF retraining), ship a **pre-aggregated 10 km panel** — likely a 100× size reduction. If RF reproduction is required, keep 1 km but Parquet-encode (see §4).
- **`tbl_long_pulp_clearing_gfc_forest.csv`** is a 10 km pixel-year panel; already coarse. Confirm it's restricted to Sumatra+Kalimantan (the modelled extent) before shipping.
- **HTI annual land-use change** (`hti_land_use_change_areas.csv`) — already aggregated; fine.
- **Validation spreadsheet** (`Validation_11classes...xlsx`) — only the `Points` sheet and `Inputs!A6:C17` range are used. Ship as two tiny CSVs.
- **Fastmarkets daily prices** — script collapses to annual means; ship the annual series rather than daily.

### 4. Reformatting
- **All remaining `.xlsx` reads**: every Excel file in this audit is treated as a flat table. Convert to `.csv` (or `.csv.gz`). The remaining xlsx surface area is small now — only Fastmarkets, WRQ, MILLS_EXPORTERS, MILL_PRODUCTION, the Validation workbook, and `annual_pulp_shr_prod.xlsx` — so this is a low-effort, high-clarity win.
- **Heavy intermediate panels** (`pulp_exp_model_var_1km_*.csv`, `tbl_long_pulp_clearing_gfc_forest.csv`, `samples_*_ttm.csv`): ship as **Parquet** (`arrow::write_parquet`) — typically 5–10× smaller than CSV with native typing.
- **Shapefiles** (`IUPHHK_HTI_TRASE_*.shp`, `idn_kabupaten_big.shp`): each is a 4-file bundle. Replace with **GeoPackage (`.gpkg`)** — single file, smaller, no `.shx / .dbf / .prj` clutter.
- The `key_parameters.csv` / `scenario_stats.csv` single-row handoffs could just be saved as `.rds` or skipped by sourcing scripts in order.

### 5. General Feedback
- **Path standardization**: `wdir` is set inconsistently — some scripts use `"remote"`, some `"remote/"`. Standardize via `file.path()` or `here::here()` and use forward slashes throughout.
- **Hard-coded folder structure** (`01_data/01_in`, `02_out`, `04_results`): include a README diagram and preserve these exact subfolders so all `paste0(wdir, "/01_data/...")` calls resolve without edits.
- **Intermediate-vs-input ambiguity**: many files in `01_data/02_out/...` are *intermediate outputs* produced by upstream scripts that are **not** in the replication folders (`hti_harvest_yr.csv`, `tbl_long_pulp_clearing_gfc_forest.csv`, `hti_land_use_change_areas.csv`, `samples_landuse_ttm.csv`, `samples_gfc_ttm.csv`, `centroids_mills_cost.csv`, `grid_10km_adm_prov_kab_kec.csv`, `ws_merge_clean_2015_2022.csv`, `id_annual_expansion_stats_ttm.csv`, `kali_annual_pulp_exp_stats_ttm.csv`, etc.). Reviewers need a README distinguishing **(a) raw inputs**, **(b) intermediate artifacts shipped pre-built**, and **(c) outputs the scripts will create**.
- **Circular dependency**: `20_pulp_expansion_table.R` reads `pulp_expansion_areas_2001_2022.csv`, which is *written* by `12_paper_stats.R`. Document the run order, or ship the file as a pre-built intermediate.
- **Font dependencies** (`DM Sans`) and `type="cairo-png"` in `ggsave` will fail silently on systems without the font / cairo. Either embed the font or fall back to `theme_bw()` for the replication package.
- **Per-HTI loop in `03_land_use_change.R`** writes hundreds of PNGs to `lu_traj_plots_check/`. Default to writing a small sample (e.g. 10 largest concessions) and gate the full loop behind a flag.
- **Folder hygiene**: `01_data/01_in/` has provider-named subfolders (`wwi/`, `klhk/`, `big/`, `gaveau/`, `obidzinski_dermawan/`, `tables/`) — fine, but add a `01_data/01_in/README.md` documenting provenance and licensing.

Net effect: the bundle should drop from many xlsx + multi-file shapefiles + 1 km nationwide CSVs to a tidy mix of `.csv(.gz)` / `.parquet` / `.gpkg` files — likely a 5–20× reduction in total footprint and a much shorter file count. The wood-supply and Gaveau-LCC consolidations have already removed four xlsx files from the active surface.
