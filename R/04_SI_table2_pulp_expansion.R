## ---------------------------------------------------------
## Project: Indonesia pulp deforestation
## Purpose of script: Refactored functions to create SI Table 2 (Pulp Expansion Table)
## Author: Robert Heilmayr and Jason Jon Benedict
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
  write_csv(si_table_df, output_path)
  return(output_path)
}
