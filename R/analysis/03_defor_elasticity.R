## ---------------------------------------------------------
## Project: Indonesia pulp deforestation
## Purpose of script: Estimate the deforestation elasticity. Largely the
##   foundation for SI Section 5, but also includes stats on pulp price trends
##   and mill capacity utilisation reported in SI Section 4.2.
## Author: Robert Heilmayr
## Notes: Refactored for the targets pipeline from
##   scripts/03_analysis_modelling/03_defor_elasticity.R. The calculations are
##   unchanged; inputs are passed in and outputs are returned rather than read
##   from and written to hard-coded paths. The standalone script defined
##   glance_custom.fixest() at the top level, reading a global defor_df; here
##   the cluster counts are computed inside run_defor_elasticity() and passed
##   to the table writer instead.
##
## Pipeline inputs (targets in _targets.R; paths relative to
##   data/01_data_replication/)
##        1) defor_long_file -> 02_out/tables/tbl_long_pulp_clearing_gfc_forest.csv:
##               Annual pulp-driven deforestation and other pulp expansion by
##               10 km grid cell, 2001-2022. The estimation panel.
##               Produced by scripts/02_data_preparation/16_create_long_data_10km_gc.R
##        2) grid_admin_file -> 02_out/tables/grid_10km_adm_prov_kab_kec.csv:
##               Province, kabupaten and kecamatan per grid cell (kecamatan is
##               the clustering unit). Spans Sumatra and Kalimantan only.
##               Produced by scripts/02_data_preparation/16_create_long_data_10km_gc.R
##        3) pulp_prices_annual_file -> 02_out/tables/pulp_prices_annual_2001_2024.csv:
##               Annual pulp and pulpwood prices in constant 2015 IDR. Derived
##               from licensed Fastmarkets and WRQ data, which are read only by
##               scripts/02_data_preparation/19_prep_pulp_prices.R (outside the
##               pipeline); the derived series is shareable.
##        4) gaez_hti_file -> 02_out/tables/gaez_hti_areas.csv
##           gaez_grid_file -> 02_out/tables/gaez_grid_share.csv:
##               Agro-ecological zone composition of concessions (ha) and of
##               grid cells (%).
##               Produced by scripts/02_data_preparation/18_gaez_classes_hti_centroids.R
##        5) mai_results$hti_mai: Concession-level DMAI.
##               Produced by run_calc_mai() (R/analysis/02_calc_mai.R)
##        6) cap_df -> 01_in/wwi/MILLS_EXPORTERS_20200405.xlsx
##           mill_prod_file -> 01_in/wwi/MILL_PRODUCTION_2015_2024.xlsx:
##               Mill capacity and annual production (SI Section 4.2; project
##               inputs from a collaborator).
##
## Pipeline outputs
##        1) si_table8_csv -> outputs/tables/si_table8_aez_productivity.csv:
##               SI Table 8, Equation 8 coefficients.
##        2) si_fig3_png -> outputs/figures/SI_f3_elasticity.png: SI Figure 3.
##        3) si_sections4_5_txt -> outputs/text/si_sections4_5_statements.txt:
##               SI Sections 4.2, 5.2 and 5.3 statements with values from this
##               run.
##        4) si_tables9_10_docx -> outputs/tables/si_table9_defor_elasticity.docx
##               and si_table10_defor_elasticity_robustness.docx: SI Tables 9
##               and 10 (needs pandoc).
## ---------------------------------------------------------

#' Estimate the deforestation elasticity
#'
#' @param defor_long_csv,grid_admin_csv,pulp_prices_csv,gaez_hti_csv,gaez_grid_csv
#'   Paths to the input tables described in the header.
#' @param hti_mai Concession-level DMAI (mai_results$hti_mai).
#' @param cap_df Mill capacities (the cap_df target).
#' @param mill_prod_xlsx Path to 01_in/wwi/MILL_PRODUCTION_2015_2024.xlsx.
#' @param results_paths Output paths named in the SI statements (text only).
#' @return A named list: si_table8, defor_plot, models_main, models_robust,
#'   n_clusters (per model, for SI Tables 9 and 10), si_text and diagnostics.
run_defor_elasticity <- function(
  defor_long_csv,
  grid_admin_csv,
  pulp_prices_csv,
  gaez_hti_csv,
  gaez_grid_csv,
  hti_mai,
  cap_df,
  mill_prod_xlsx,
  results_paths = c(
    si_table8 = "outputs/tables/si_table8_aez_productivity.csv",
    tables = "outputs/tables/defor_elast_main.docx and defor_elast_robust.docx",
    si_fig3 = "outputs/figures/SI_f3_elasticity.png"
  )
) {
  # =========================================================================
  # Load data
  # =========================================================================

  defor_df <- read_csv(defor_long_csv, show_col_types = FALSE)

  # Annual pulp and pulpwood prices, in constant 2015 IDR.
  pulp_prices_annual <- read_csv(pulp_prices_csv, show_col_types = FALSE)

  # Data about grid cell composition along GAEZ classes
  grid_gaez <- read_csv(gaez_grid_csv, show_col_types = FALSE)

  # Data about hti composition along GAEZ classes
  hti_gaez <- read_csv(gaez_hti_csv, show_col_types = FALSE) %>%
    select(-total_area_ha, supplier_id = ID)

  # Add administrative labels
  grid_admin <- read_csv(grid_admin_csv, show_col_types = FALSE)

  # mill-level production
  mill_prod <- readxl::read_excel(mill_prod_xlsx)

  # =========================================================================
  # Merge datasets
  # =========================================================================

  defor_df <- defor_df %>%
    left_join(grid_admin, by = "pixel_id")

  # Add total pulp expansion variable
  defor_df <- defor_df %>%
    mutate(pulp_exp_ha = pulp_forest_ha + pulp_non_forest_ha)

  # Add prices to defor_df
  defor_df <- defor_df %>%
    left_join(pulp_prices_annual, by = "year")

  # =========================================================================
  # Estimate cross-sectional variation in productivity
  # =========================================================================

  # Re-assign GAEZ into aggregated classes.
  # NOTE on class 2: the HTI aggregation folds GAEZ classes 2, 3 and 6 into
  # "noLimitations", while the grid aggregation below uses only classes 3 and
  # 6. The 10 km grid spans Sumatra and Kalimantan only, and GAEZ class 2
  # ("Tropics, lowland; sub-humid") occurs in no grid cell, so the shorter
  # grid formula misallocates no grid area and pot_mai is unaffected. It does
  # mean the regression is trained on concessions containing a land class the
  # prediction domain cannot contain.
  hti_gaez <- hti_gaez %>%
    mutate(
      class_noLimitations = class_2 + class_3 + class_6, # tropic lowlands; sub-humid tropic lowlands; humid topic highlands
      class_hydromorphic = class_27 + class_28, # land with ample irrigated soils, dominantly hydromorphic soils
      class_terrain = class_25 + class_26, # very steep terrain, land with severe soil/terrain limitations,
      class_other = class_32 + class_33
    ) %>% # water and developed, to be dropped
    select(
      supplier_id,
      class_noLimitations,
      class_hydromorphic,
      class_terrain,
      class_other
    )
  grid_gaez <- grid_gaez %>%
    mutate(
      noLimitations = (class_3_pct + class_6_pct) / 100,
      hydromorphic = (class_27_pct + class_28_pct) / 100,
      terrain = (class_25_pct + class_26_pct) / 100,
      other = (class_32_pct + class_33_pct) / 100
    ) %>%
    select(pixel_id, noLimitations, hydromorphic, terrain, other)

  # Recalculate removing "other" class (water and developed)
  grid_gaez <- grid_gaez %>%
    mutate(
      no_other_sum = noLimitations + hydromorphic + terrain,
      noLimitations = noLimitations / no_other_sum,
      hydromorphic = hydromorphic / no_other_sum,
      terrain = terrain / no_other_sum
    ) %>%
    select(pixel_id, noLimitations, hydromorphic, terrain)

  hti_gaez <- hti_gaez %>%
    pivot_longer(
      cols = starts_with("class_"),
      names_prefix = "class_",
      names_to = "class",
      values_to = "area_ha"
    )

  hti_gaez <- hti_gaez %>%
    group_by(supplier_id)

  # Proportions in grouped classes (quoted in SI Section 5.2)
  aez_shares <- hti_gaez %>%
    group_by(class) %>%
    summarize(area_ha = sum(area_ha, na.rm = TRUE)) %>%
    mutate(prop_area = area_ha / sum(area_ha, na.rm = TRUE))

  hti_gaez <- hti_gaez %>%
    filter(class != "other") %>%
    mutate(share = area_ha / sum(area_ha, na.rm = TRUE))

  hti_gaez <- hti_gaez %>%
    select(-area_ha) %>%
    pivot_wider(names_from = class, values_from = share) %>%
    left_join(hti_mai, by = "supplier_id") %>%
    drop_na()

  # Model DMAI as a function of GAEZ shares (SI Equation 8).
  # No intercept: the three shares sum to 1 by construction, so a constant
  # would be perfectly collinear with them. Without it, each coefficient is the
  # mean productivity of a concession composed entirely of that AEZ class.
  mod <- lm(
    dmai_winsorized ~ 0 + noLimitations + hydromorphic + terrain,
    data = hti_gaez
  )

  # SI Table 8
  si_table8 <- tibble(
    Coefficient = c("alpha_nl", "alpha_h", "alpha_t"),
    Interpretation = c(
      "Average productivity on lands with few agricultural limitations",
      "Average productivity on saturated soils",
      "Average productivity on terrain with topographic limitations"
    ),
    Value = sprintf(
      "%.2f",
      coef(mod)[c("noLimitations", "hydromorphic", "terrain")]
    ),
    `Standard error` = sprintf(
      "%.2f",
      summary(mod)$coefficients[
        c("noLimitations", "hydromorphic", "terrain"),
        "Std. Error"
      ]
    )
  )

  # Predict potential DMAI for all grid cells
  grid_pot_mai <- grid_gaez %>%
    mutate(
      pot_mai = predict(mod, newdata = grid_gaez),
      pot_mai = ifelse(pot_mai < 0, 0, pot_mai)
    ) %>% # winsorize negative potential production to 0
    select(pixel_id, pot_mai)

  # Join productivity data back to defor_df
  defor_df <- defor_df %>%
    left_join(grid_pot_mai, by = "pixel_id")

  # Calculate potential revenues and net revenues
  defor_df <- defor_df %>%
    mutate(
      pot_revenues = (sa_prices_real * pot_mai),
      pot_revenues_indo = (indo_prices_real * pot_mai),
      pot_revenues_dev = (sa_prices_dev * pot_mai),
      post_2015 = year > 2015
    )

  # =========================================================================
  # Estimate elasticity of deforestation (SI Table 9)
  # =========================================================================

  mod_1 <- fixest::feols(
    pulp_forest_ha ~ pot_revenues | pixel_id + year,
    data = defor_df,
    vcov = ~kec_code
  )

  mod_2 <- fixest::feols(
    pulp_forest_ha ~ post_2015:pot_revenues | pixel_id + year,
    data = defor_df,
    vcov = ~kec_code
  )

  mod_3 <- fixest::feols(
    pulp_non_forest_ha ~ pot_revenues | pixel_id + year,
    data = defor_df,
    vcov = ~kec_code
  )

  mod_4 <- fixest::feols(
    pulp_non_forest_ha ~ post_2015:pot_revenues | pixel_id + year,
    data = defor_df,
    vcov = ~kec_code
  )

  # =========================================================================
  # Robustness (SI Table 10)
  # =========================================================================

  # Re-run base model with same coefficients
  defor_df <- defor_df %>%
    mutate(pot_revenues_r = pot_revenues)
  rmod_0 <- fixest::feols(
    pulp_forest_ha ~ post_2015:pot_revenues_r | pixel_id + year,
    data = defor_df,
    vcov = ~kec_code
  )

  # Add suitability time trend
  rmod_1 <- fixest::feols(
    pulp_forest_ha ~ post_2015:pot_revenues_r + pot_mai * year | pixel_id + year,
    data = defor_df,
    vcov = ~kec_code
  )

  # Lagged rents
  defor_df <- defor_df %>%
    group_by(pixel_id) %>%
    arrange(pixel_id, year) %>%
    mutate(pot_revenues_r = lag(pot_revenues)) %>%
    ungroup() # lag needs the grouping; nothing downstream should inherit it
  rmod_2 <- fixest::feols(
    pulp_forest_ha ~ post_2015:pot_revenues_r | pixel_id + year,
    data = defor_df,
    vcov = ~kec_code
  )

  # Price deviation
  defor_df <- defor_df %>%
    mutate(pot_revenues_r = pot_revenues_dev)
  rmod_3 <- fixest::feols(
    pulp_forest_ha ~ post_2015:pot_revenues_r | pixel_id + year,
    data = defor_df,
    vcov = ~kec_code
  )

  # Use Indonesian pulpwood price series instead of SA
  defor_df <- defor_df %>%
    mutate(pot_revenues_r = pot_revenues_indo)
  rmod_4 <- fixest::feols(
    pulp_forest_ha ~ post_2015:pot_revenues_r | pixel_id + year,
    data = defor_df,
    vcov = ~kec_code
  )

  models_main <- list(
    "Pulp deforestation" = list("(1)" = mod_1, "(2)" = mod_2),
    "Other pulp expansion" = list("(3)" = mod_3, "(4)" = mod_4)
  )
  models_robust <- list(
    "Primary spec." = list("(1)" = rmod_0),
    "Control for suitability time-trend" = list("(2)" = rmod_1),
    "Lagged returns" = list("(3)" = rmod_2),
    "Price shocks" = list("(4)" = rmod_3),
    "Indonesian price series" = list("(5)" = rmod_4)
  )

  # Number of kecamatan clusters behind each model, for the GOF rows of SI
  # Tables 9 and 10. As in the standalone script's glance_custom.fixest(),
  # obs() row indices are looked up in the final defor_df.
  all_models <- c(
    list(mod_1, mod_2, mod_3, mod_4),
    list(rmod_0, rmod_1, rmod_2, rmod_3, rmod_4)
  )
  names(all_models) <- c(
    "mod_1",
    "mod_2",
    "mod_3",
    "mod_4",
    "rmod_0",
    "rmod_1",
    "rmod_2",
    "rmod_3",
    "rmod_4"
  )
  kec_code <- defor_df$kec_code
  n_clusters <- vapply(
    all_models,
    function(m) length(unique(kec_code[fixest::obs(m)])),
    integer(1)
  )

  # =========================================================================
  # Interpretation - SI Figure 3
  # =========================================================================

  defor_cf <- defor_df %>%
    mutate(defor_price_partial = pot_revenues * mod_1$coefficients[1])

  total_pulp_defor <- defor_cf %>%
    group_by(year) %>%
    summarize(
      pot_revenues = mean(pot_revenues, na.rm = TRUE),
      pulp_forest_ha_true = sum(pulp_forest_ha, na.rm = TRUE) / 1000,
      pulp_forest_ha_cf = sum(defor_price_partial, na.rm = TRUE) / 1000
    )

  defor_plot <- plot_si_fig3(total_pulp_defor)

  # =========================================================================
  # Mill capacity utilisation is inelastic (SI Section 4.2)
  # =========================================================================

  mill_prod <- mill_prod %>%
    select(MILL_ID, YEAR, TOTAL_PROD_KG_NET) %>%
    group_by(MILL_ID, YEAR) %>%
    summarize(
      prod_mtpy = sum(TOTAL_PROD_KG_NET) / 1000000000,
      .groups = "drop_last"
    ) %>%
    left_join(cap_df %>% select(MILL_ID, PULP_CAP_MTPY), by = "MILL_ID")

  mill_prod <- mill_prod %>%
    mutate(
      PULP_CAP_MTPY = if_else(
        MILL_ID == "M-0004" & YEAR < 2023,
        2.9,
        PULP_CAP_MTPY
      )
    ) %>% # Adjusting RAPP capacity - pre-dates capacity expansion
    mutate(cap_usage = prod_mtpy / PULP_CAP_MTPY)

  cap_usage_trend <- mill_prod %>%
    filter(!(MILL_ID == "M-0003" & YEAR < 2019), MILL_ID != "M-0007") %>%
    group_by(YEAR) %>%
    summarize(cap = sum(PULP_CAP_MTPY), prod = sum(prod_mtpy)) %>%
    mutate(cap_usage = prod / cap) %>%
    rename(year = YEAR)

  cap_usage_trend <- cap_usage_trend %>%
    left_join(
      pulp_prices_annual %>% select(year, indo_prices_real_idr),
      by = 'year'
    )

  # =========================================================================
  # SI statements
  # =========================================================================

  aez_pct <- function(cls) {
    100 * aez_shares$prop_area[aez_shares$class == cls]
  }
  # SI 5.3 describes the peak and trough "over the past 15 years", i.e. the
  # last 15 years of the panel. Over the full 2001-2022 panel the peak is 2004;
  # from 2005 onward it is 2011.
  recent_window_start <- max(total_pulp_defor$year) - 14
  defor_by_year <- total_pulp_defor %>%
    filter(year >= recent_window_start)
  peak_year <- defor_by_year$year[which.max(defor_by_year$pulp_forest_ha_true)]
  trough_year <- defor_by_year$year[which.min(defor_by_year$pulp_forest_ha_true)]

  si_para <- function(...) c(strwrap(sprintf(...), width = 78), "")

  si_text <- c(
    "SI SECTIONS 4.2 AND 5: DEFORESTATION ELASTICITY",
    strrep("=", 78),
    "Generated by R/analysis/03_defor_elasticity.R (targets pipeline)",
    "",
    "4.2 Projected increases in pulpwood demand",
    strrep("-", 78),
    si_para(
      paste(
        "Between %d and %d, sector-wide capacity utilization rates remained",
        "stable and high (mean = %.0f percent; minimum = %.0f percent; maximum =",
        "%.0f percent; standard deviation = %.0f percent), despite much larger",
        "fluctuations in real pulp prices (mean = %.2f million IDR/tonne;",
        "minimum = %.2f million IDR/tonne; maximum = %.2f million IDR/tonne;",
        "standard deviation = %.2f million IDR/tonne). Prices are expressed in",
        "constant 2015 Indonesian rupiah, converting the reported USD price",
        "series at the annual average market exchange rate and deflating by the",
        "Indonesian consumer price index, so that they reflect the domestic",
        "purchasing power of mill revenue. Relative to their means, prices varied",
        "more than three times as much as capacity utilization (coefficients of",
        "variation of %.2f and %.2f, respectively)."
      ),
      min(cap_usage_trend$year),
      max(cap_usage_trend$year),
      100 * mean(cap_usage_trend$cap_usage),
      100 * min(cap_usage_trend$cap_usage),
      100 * max(cap_usage_trend$cap_usage),
      100 * sd(cap_usage_trend$cap_usage),
      mean(cap_usage_trend$indo_prices_real_idr),
      min(cap_usage_trend$indo_prices_real_idr),
      max(cap_usage_trend$indo_prices_real_idr),
      sd(cap_usage_trend$indo_prices_real_idr),
      sd(cap_usage_trend$indo_prices_real_idr) /
        mean(cap_usage_trend$indo_prices_real_idr),
      sd(cap_usage_trend$cap_usage) / mean(cap_usage_trend$cap_usage)
    ),
    "5.2 Data sources: agro-ecological zone composition",
    strrep("-", 78),
    si_para(
      paste(
        "Areas with few limitations for agricultural production (AFL). This class",
        "combines areas categorized as \"humid tropic lowlands\", \"humid tropic",
        "highlands\" and \"sub-humid tropic lowlands,\" and represents %.1f%% of",
        "concession area."
      ),
      aez_pct("noLimitations")
    ),
    si_para(
      paste(
        "Areas with hydromorphic soils (AH). This class combines areas",
        "categorized as \"land with ample irrigated soils\" and \"dominantly",
        "hydromorphic soils\" and represents %.1f%% of concession area."
      ),
      aez_pct("hydromorphic")
    ),
    si_para(
      paste(
        "Areas with topographic limitations (AT). This class combines areas",
        "categorized as \"very steep terrain\" and \"land with severe soil or",
        "terrain limitations\" and represents %.1f%% of concession area."
      ),
      aez_pct("terrain")
    ),
    si_para(
      paste(
        "Areas that can't be used for pulpwood production due to other land cover",
        "(AO). This class combines areas categorized as \"water\" and \"developed\"",
        "classes, and represents %.2f%% of concession area. We remove this final",
        "class from all analyses."
      ),
      aez_pct("other")
    ),
    si_para(
      "Equation 8 coefficients (SI Table 8) are written to %s.",
      results_paths[["si_table8"]]
    ),
    "5.3 Results",
    strrep("-", 78),
    si_para(
      paste(
        "We find that increases in potential returns to pulpwood production do",
        "lead to a statistically significant increase in pulp-driven deforestation",
        "(Table 9, Column 1). However, this effect is small - every 1,000,000 IDR",
        "increase in potential returns (%.0f%% increase relative to mean) leads to",
        "a %.2f hectare increase in pulp-driven deforestation in a grid cell."
      ),
      100 / mean(defor_df$pot_revenues, na.rm = TRUE),
      mod_1$coefficients[["pot_revenues"]]
    ),
    si_para(
      paste(
        "For example, over the past 15 years the highest level of pulp-driven",
        "deforestation occurred in %d, and the lowest level occurred in %d.",
        "However, producers faced slightly higher real potential returns in %d",
        "than in %d."
      ),
      peak_year,
      trough_year,
      trough_year,
      peak_year
    ),
    si_para(
      "Regression results (SI Tables 9 and 10) are written to %s; SI Figure 3 is written to %s.",
      results_paths[["tables"]],
      results_paths[["si_fig3"]]
    )
  )

  # =========================================================================
  # Console diagnostics (captured, not printed)
  # =========================================================================

  diagnostics <- utils::capture.output({
    cat("AEZ class shares of concession area:\n")
    print(aez_shares)
    cat("\nEquation 8 (SI Table 8):\n")
    print(summary(mod))
    cat("\nPotential returns, mean per grid cell (million IDR):\n")
    cat(sprintf(
      "  2011: %.4f   2017: %.4f   2022: %.4f\n",
      mean(defor_df$pot_revenues[defor_df$year == 2011]),
      mean(defor_df$pot_revenues[defor_df$year == 2017]),
      mean(defor_df$pot_revenues[defor_df$year == 2022])
    ))
    cat("\nObserved vs price-predicted deforestation by year (kha):\n")
    print(total_pulp_defor, n = Inf)
    cat("\nMill capacity utilisation by year:\n")
    print(cap_usage_trend, n = Inf)
    for (nm in names(all_models)) {
      cat("\nModel", nm, "\n")
      print(summary(all_models[[nm]]))
    }
  })

  list(
    si_table8 = si_table8,
    defor_plot = defor_plot,
    models_main = models_main,
    models_robust = models_robust,
    n_clusters = n_clusters,
    si_text = si_text,
    diagnostics = diagnostics
  )
}

#' Build SI Figure 3 (observed vs price-predicted deforestation)
#'
#' Built in its own function because a ggplot keeps the environment it was
#' created in; inside run_defor_elasticity() that would carry the full panel
#' into the stored result.
#' @param total_pulp_defor Annual observed and counterfactual totals (kha)
plot_si_fig3 <- function(total_pulp_defor) {
  ggplot(
    total_pulp_defor %>% filter(year > 2000, year < 2023),
    aes(x = year)
  ) +
    geom_line(aes(y = pulp_forest_ha_true)) +
    geom_line(aes(y = pulp_forest_ha_cf), linetype = 2) +
    labs(x = "Year", y = "Pulp-driven deforestation (thousand ha)") +
    theme_minimal(base_size = 12) +
    annotate(
      "text",
      x = 2020,
      y = 107,
      label = "Deforestation as predicted\nby price variation",
      color = "black"
    ) +
    annotate(
      "text",
      x = 2018,
      y = 20,
      label = "Observed deforestation",
      color = "black"
    )
}

#' Save SI Figure 3
#' @param elast_results Output of run_defor_elasticity()
#' @param output_path Destination .png path
#' @return output_path, as required by format = "file"
save_si_fig3 <- function(elast_results, output_path) {
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  ggsave_without_showtext(
    output_path,
    plot = elast_results$defor_plot,
    width = 7,
    height = 5
  )
  output_path
}

#' Write SI Tables 9 and 10 (deforestation elasticity regressions)
#'
#' Writing .docx requires the pandoc program and the R package of the same
#' name; if either is missing this stops with a message saying so. Any other
#' extension that modelsummary supports (e.g. .html) works too.
#' @param elast_results Output of run_defor_elasticity()
#' @param main_path,robust_path Destination paths for SI Tables 9 and 10
#' @return The paths written, as required by format = "file"
save_defor_elast_tables <- function(elast_results, main_path, robust_path) {
  # modelsummary looks up glance_custom() methods to add GOF rows; register
  # one for this call that reports the precomputed cluster counts, then
  # restore whatever was registered before.
  n_clusters <- elast_results$n_clusters
  all_models <- c(
    unlist(elast_results$models_main, recursive = FALSE),
    unlist(elast_results$models_robust, recursive = FALSE)
  )
  glance_clusters <- function(x, ...) {
    idx <- which(vapply(all_models, identical, logical(1), x))[1]
    data.frame(n_clusters = unname(n_clusters[idx]))
  }
  ns <- asNamespace("modelsummary")
  old_method <- utils::getS3method("glance_custom", "fixest", optional = TRUE)
  registerS3method("glance_custom", "fixest", glance_clusters, envir = ns)
  on.exit(
    if (!is.null(old_method)) {
      registerS3method("glance_custom", "fixest", old_method, envir = ns)
    },
    add = TRUE
  )

  # Suppress modelsummary's automatic significance note: the thresholds are
  # already stated in the table notes (global option in modelsummary >= 2.4.0).
  old_opt <- options(modelsummary_stars_note = FALSE)
  on.exit(options(old_opt), add = TRUE)

  notes <- paste(
    "All models include grid cell and year fixed effects.",
    "Standard errors clustered by district (kecamatan) in parentheses.",
    "* p < 0.1, ** p < 0.05, *** p < 0.01"
  )
  gof_map_defor <- tribble(
    ~raw         , ~clean            , ~fmt ,
    "nobs"       , "N. Observations" ,    0 ,
    "n_clusters" , "N. Clusters"     ,    0
  )
  # SI Table 10 prints the same two labels in lower case
  gof_map_robust <- tribble(
    ~raw         , ~clean            , ~fmt ,
    "nobs"       , "N. observations" ,    0 ,
    "n_clusters" , "N. clusters"     ,    0
  )

  tbl_args <- list(
    elast_results$models_main,
    stars = c('*' = .1, '**' = .05, '***' = .01),
    coef_omit = "^(?!.*revenues)",
    coef_rename = c(
      "pot_revenues" = "Potential revenues",
      "post_2015FALSE:pot_revenues" = "Potential revenues (y<=2015)",
      "post_2015TRUE:pot_revenues" = "Potential revenues (y>2015)"
    ),
    gof_map = gof_map_defor,
    notes = notes,
    shape = "cbind"
  )
  rtbl_args <- list(
    elast_results$models_robust,
    stars = c('*' = .1, '**' = .05, '***' = .01),
    coef_omit = "^(?!.*revenues)",
    coef_rename = c(
      "post_2015FALSE:pot_revenues_r" = "Potential revenues (y<=2015)",
      "post_2015TRUE:pot_revenues_r" = "Potential revenues (y>2015)"
    ),
    gof_map = gof_map_robust,
    notes = notes,
    shape = "cbind"
  )

  write_table <- function(args, path) {
    dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
    tryCatch(
      {
        do.call(modelsummary::msummary, c(args, list(output = path)))
        path
      },
      error = function(e) {
        stop(
          "Could not write ",
          path,
          ": ",
          conditionMessage(e),
          "\n  .docx output needs pandoc on the PATH and the R package 'pandoc'.",
          call. = FALSE
        )
      }
    )
  }

  c(write_table(tbl_args, main_path), write_table(rtbl_args, robust_path))
}
