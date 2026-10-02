## ---------------------------------------------------------
## Project: Indonesia pulp deforestation
## Purpose of script: Refactored functions to create SI Table 2 (Pulp Expansion Table)
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Pipeline inputs (targets in _targets.R; paths relative to
##   data/01_data_replication/)
##        1) hti_file -> 01_in/klhk/IUPHHK_HTI_TRASE_20230314_proj.shp:
##               Pulpwood (HTI) concession boundaries (project input).
##        2) lic_dates_hti_file -> 01_in/wwi/HTI_LICENSE_DATES.csv: Concession
##               license dates (project input).
##        3) samples_gfc_ttm_file -> 02_out/tables/samples_gfc_ttm.csv
##           samples_landuse_ttm_file -> 02_out/tables/samples_landuse_ttm.csv:
##               Sample-point forest loss and land use.
##               Produced by scripts/02_data_preparation/02_clean_ttm_areas.R
##        4) samples_hti_file -> 02_out/samples/samples_hti_id.csv: Concession
##               and island for each sample point.
##               Produced by scripts/02_data_preparation/03_sample_cleanup.R
##        5) hti_nonhti_conv_file -> 02_out/tables/idn_pulp_conversion_hti_nonhti_treemap.csv:
##               Pulpwood conversion inside and outside concessions.
##               Produced by scripts/02_data_preparation/01_data_prep.R
##        6) id_annual_exp_stats -> 02_out/tables/id_annual_expansion_stats_ttm.csv:
##               Annual pulp-driven forest loss, turned into the ann_pulp_tbl
##               target by calc_annual_pulp_expansion().
##               Produced by scripts/02_data_preparation/02_clean_ttm_areas.R
##
## Pipeline outputs
##        1) si_table_2_csv -> outputs/tables/pulp_expansion_areas_all_2001_2022.csv:
##               SI Table 2.
##        Intermediate targets: ann_pulp_tbl, hti_concession_names, hti_dates_clean,
##               samples_df, hti_pulp_conv, hti_pulp_conv_all,
##               hti_pulp_conv_license, hti_pulp_driven_defor, si_table_2_df.
## ---------------------------------------------------------

# =========================================================================
# 1. CLEANING & HELPER FUNCTIONS
# =========================================================================

#' Clean HTI Concession names from spatial boundary object
clean_hti_concession_names <- function(hti) {
  hti %>%
    st_drop_geometry() %>%
    select(supplier_id = ID, supplier = namaobj) %>%
    mutate(supplier_label = paste0(supplier, " (", supplier_id, ")"))
}

#' Clean HTI Concession license dates
clean_hti_license_dates <- function(lic_dates_hti) {
  lic_dates_hti %>%
    mutate(YEAR = year(license_date)) %>%
    select(supplier_id = HTI_ID, license_year = YEAR)
}

#' Extract sample IDs (SIDs) that eventually transition to pulp by 2022
get_treemap_pulp_sids <- function(samples_landuse_ttm) {
  samples_landuse_ttm %>%
    select(sid, timberdeforestation_2022) %>%
    lazy_dt() %>%
    as.data.table() %>%
    dt_pivot_longer(cols = c(-sid), names_to = 'year', values_to = 'class') %>%
    as_tibble() %>%
    filter(class == "3") %>%
    distinct() %>%
    pull(sid)
}

# =========================================================================
# 2. DATA PREPARATION FUNCTIONS
# =========================================================================

#' Prepare joined sample point data with HTI, island, and land use info
#' Prepare joined sample point data with HTI, island, and land use info
prep_samples_df <- function(
  samples_gfc_ttm,
  samples_hti,
  samples_landuse_ttm,
  hti_dates_clean,
  hti_concession_names
) {
  forest_loss_codes <- c(101:122, 401:422, 601:622)
  treemap_pulp_sids <- get_treemap_pulp_sids(samples_landuse_ttm)

  # 1. Pivot AND filter class == 3 immediately to keep RAM tiny
  treemap_annual_conv <- samples_landuse_ttm %>%
    lazy_dt() %>%
    as.data.table() %>%
    dt_pivot_longer(cols = c(-sid), names_to = 'year', values_to = 'class') %>%
    filter(class == 3)

  # 2. Prepare base GFC and HTI sample data
  samples_df <- samples_gfc_ttm %>%
    lazy_dt() %>%
    mutate(start_for = ifelse(gfc_ttm %in% forest_loss_codes, "Y", "N")) %>%
    left_join(samples_hti, by = "sid") %>%
    drop_na(sid) %>%
    mutate(
      island_name = case_when(
        island == 1 ~ "Balinusa",
        island == 2 ~ "Kalimantan",
        island == 3 ~ "Maluku",
        island == 4 ~ "Papua",
        island == 5 ~ "Sulawesi",
        island == 6 ~ "Sumatera",
        TRUE ~ NA_character_
      )
    ) %>%
    select(-island) %>%
    rename(island = island_name, supplier_id = ID) %>%
    as_tibble()

  # 3. Join the lightweight pre-filtered table
  samples_df %>%
    left_join(hti_dates_clean, by = "supplier_id") %>%
    left_join(hti_concession_names, by = "supplier_id") %>%
    left_join(treemap_annual_conv, by = "sid") %>%
    mutate(pulp = ifelse(sid %in% treemap_pulp_sids, "Y", "N"))
}

#' Filter sample points converted from forest to pulp
get_hti_pulp_conversion <- function(samples_df) {
  samples_df %>%
    filter(start_for == "Y" & pulp == "Y") %>%
    as_tibble() %>%
    mutate(year_pulp = str_replace(year, "timberdeforestation_", "")) %>%
    filter(year_pulp != "2000") %>%
    group_by(sid, supplier_id) %>%
    slice_min(year) %>%
    ungroup()
}

# =========================================================================
# 3. AGGREGATION & TABLE BUILDING FUNCTIONS
# =========================================================================

#' Summarize annual HTI pulp expansion area (all years)
calc_hti_pulp_expansion_all <- function(hti_pulp_conv) {
  hti_pulp_conv %>%
    mutate(year = as.double(year_pulp)) %>%
    group_by(year) %>%
    summarize(pulp_expansion_area_ha = n(), .groups = "drop")
}

#' Summarize annual HTI pulp expansion area occurring after permit issue date
calc_hti_pulp_expansion_post_license <- function(hti_pulp_conv) {
  hti_pulp_conv %>%
    filter(year_pulp > license_year) %>%
    mutate(year = as.double(year_pulp)) %>%
    group_by(year) %>%
    summarize(pulp_permit_area_ha = n(), .groups = "drop")
}

#' Summarize annual pulp-driven deforestation within HTI concessions
calc_hti_pulp_driven_defor <- function(hti_nonhti_conv) {
  hti_nonhti_conv %>%
    filter(conv_type == 2 & !is.na(supplier_id)) %>%
    group_by(year) %>%
    summarize(
      Pulp_driven_deforestation_hti_kha = sum(area_ha / 1000),
      .groups = "drop"
    )
}

#' Annual pulp expansion in Indonesia, 2001-2022 (kha)
#'
#' Replaces the precomputed 02_out/tables/pulp_expansion_areas_2001_2022.csv,
#' whose writer is commented out in the old
#' scripts/04_figures_and_outputs/05_paper_stats.R. Same values, computed from
#' the treemap annual expansion stats; the planted-area column of that file is
#' not reproduced because nothing in the pipeline uses it.
calc_annual_pulp_expansion <- function(id_annual_exp_stats) {
  id_annual_exp_stats %>%
    filter(year > 2000) %>%
    transmute(
      Year = year,
      Pulp_driven_deforestation_kha = forest_loss_pulp_ha / 1000,
      Other_pulp_expansion_kha = nonforest_loss_pulp_ha / 1000,
      Aggregate_pulp_expansion_kha = (forest_loss_pulp_ha +
        nonforest_loss_pulp_ha) /
        1000
    ) %>%
    arrange(Year)
}

#' Assemble final SI Table 2 dataset
prep_si_table_2 <- function(
  ann_pulp_tbl,
  hti_pulp_driven_defor,
  hti_pulp_conv_all,
  hti_pulp_conv_license
) {
  ann_pulp_tbl %>%
    select(Year, Pulp_driven_deforestation_kha) %>%
    rename(year = Year) %>%
    left_join(hti_pulp_driven_defor, by = "year") %>%
    left_join(hti_pulp_conv_all, by = "year") %>%
    left_join(hti_pulp_conv_license, by = "year") %>%
    rename(
      Pulp_expansion_hti_kha = pulp_expansion_area_ha,
      Pulp_expansion_hti_after_permit_year_kha = pulp_permit_area_ha
    ) %>%
    mutate(
      Pulp_expansion_hti_kha = Pulp_expansion_hti_kha / 1000,
      Pulp_expansion_hti_after_permit_year_kha = Pulp_expansion_hti_after_permit_year_kha /
        1000
    )
}

#' Save SI Table 2 to CSV file
save_si_table_2 <- function(si_table_df, output_path) {
  dir_path <- dirname(output_path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }

  write_csv(si_table_df, output_path)
  return(output_path)
}
