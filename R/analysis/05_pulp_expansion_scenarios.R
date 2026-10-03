## ---------------------------------------------------------
## Project: Indonesia pulp deforestation
## Purpose of script: Allocate the projected area of new pulpwood plantation
##   across space, and tabulate the resulting expansion by island, starting
##   forest cover and soil type. Produces main text Figure 3 and the headline
##   deforestation and peatland conversion estimates. Supports SI Sections 4.3
##   and 8.4.
## Author: Robert Heilmayr
## Notes: Refactored for the targets pipeline from
##   scripts/03_analysis_modelling/05_pulp_expansion_scenarios.R. The
##   calculations are unchanged; inputs are passed in and outputs are returned
##   rather than read from and written to hard-coded paths. The interactive
##   tmap diagnostic map (pulp_expansion_scenarios.html, not reported in the
##   manuscript) is not carried over, so tmap is not needed. Figure 3 is built
##   in plot_fig3() so the stored plot does not carry the full prediction table.
##
## Pipeline inputs (targets in _targets.R)
##        1) pulp_predictions: Predicted probability of pulpwood plantation
##               expansion for every 1 km point not yet converted as of 2022.
##               Produced by predict_pulp_expansion() (R/analysis/04_pulp_expansion_model.R)
##        2) mai_df: DMAI in 2021 and its annual growth rate with confidence
##               interval. Produced by run_calc_mai() (R/analysis/02_calc_mai.R)
##        3) rs_acc_df: Area-corrected pulpwood plantation extent in 2022.
##               Produced by run_rs_accuracy() (R/analysis/01_rs_accuracy.R)
##        4) cap_df -> 01_in/wwi/MILLS_EXPORTERS_20200405.xlsx: Existing mill
##               capacity, the baseline for the percentage increase.
##        5) ws_2015_2022 -> 02_out/tables/ws_merge_clean_2015_2022.csv: 2022
##               pulpwood deliveries, the denominator for the demand increase.
##               Produced by scripts/02_data_preparation/04_merge_ws_data.R
##        6) kab -> 01_in/big/idn_kabupaten_big.shp: District boundaries,
##               dissolved to provinces for the map.
##        Planned capacity expansions are hard coded below to match SI Table 7.
##        Neighbouring-country outlines come from the rnaturalearth package.
##
## Pipeline outputs
##        1) scenario_stats (from scenario_results$scenario_stats): New wood
##               demand, capacity increase, share of demand met by productivity
##               growth, area demanded, and the deforestation and peatland
##               conversion totals with bounds.
##               Read by paper_stats (calc_paper_stats() in R/05_paper_stats.R)
##        2) scenario_stats_csv -> outputs/tables/scenario_stats.csv
##        3) fig3_png -> outputs/figures/f3_expansion_combined.png: Figure 3.
##        4) si_sections4_8_txt -> outputs/text/si_sections4_8_statements.txt:
##               Main text and SI 4.3 / 8.4 statements with values from this run.
## ---------------------------------------------------------

#' Allocate projected pulpwood expansion and summarise the scenarios
#'
#' @param pulp_predictions Output of predict_pulp_expansion()
#' @param mai_df,rs_acc_df,cap_df,ws_2015_2022,kab Pipeline targets (see
#'   header)
#' @param results_paths Output paths named in the statements (text only)
#' @return A named list: scenario_stats, expansion_table, fig3, si_text and
#'   diagnostics
run_pulp_expansion_scenarios <- function(
  pulp_predictions,
  mai_df,
  rs_acc_df,
  cap_df,
  ws_2015_2022,
  kab,
  results_paths = c(
    fig3 = "outputs/figures/f3_expansion_combined.png",
    stats = "outputs/tables/scenario_stats.csv"
  )
) {
  pred_df <- pulp_predictions

  # Province boundaries for maps
  prov_sf <- kab %>%
    group_by(prov, prov_code) %>%
    summarise(.groups = "drop")

  # =========================================================================
  # Raster template (0.1 degree) for mapping the scenarios
  # =========================================================================
  pred2027_vect <- pred_df %>%
    drop_na(lon, lat) %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    terra::vect()
  rast_template <- terra::rast(pred2027_vect, resolution = 0.1)

  # =========================================================================
  # Calculate new wood demand
  # =========================================================================
  # Hard code planned capacity expansions: Matches SI Table 7
  cap_expansions <- tibble(
    expansions = c("oki", "rapp_1", "rapp_2", "phoenix"),
    cap = c(4200000, 1330000, 1300000, 1700000), # tonnes pulp per year
    conv_factor = c(4.7, 4.7, 2.75, 2.75)
  ) # m3 pulpwood / tonne pulp

  cap_expansions <- cap_expansions %>%
    mutate(wood_demand = cap * conv_factor / 1e6) # m3

  # Implied pulpwood demand assuming 100% capacity utilization
  new_wood_demand <- cap_expansions %>% pull(wood_demand) %>% sum() # Million m3

  # Percent increase in capacity
  baseline_cap_mt <- cap_df %>%
    select(MILL_ID, PULP_CAP_MTPY) %>%
    distinct() %>%
    pull(PULP_CAP_MTPY) %>%
    sum()

  cap_increase <- (cap_expansions %>% pull(cap) %>% sum() / 1000000)
  cap_pct_increase <- cap_increase / baseline_cap_mt

  # Pulpwood actually delivered to the mills in 2022 (million m3), as reported
  # to RPBBI; the alternative route via 2022 pulp production x 4.7 m3/tonne
  # agrees to within 0.1%.
  ws_2022_delivered <- ws_2015_2022 %>%
    filter(YEAR == 2022) %>%
    pull(VOLUME_M3) %>%
    sum(na.rm = TRUE) /
    1e6

  demand_pct_increase <- new_wood_demand / ws_2022_delivered

  # Starting pulpwood area
  prior_plantations <- rs_acc_df %>%
    filter(stat_name == "total_pp_area_2022") %>%
    pull(estimated_area_kha) *
    1000

  # =========================================================================
  # Estimate needed new pulp plantation area for scenarios
  # =========================================================================
  n_years <- 7

  mai_2021 <- mai_df$dmai_2021
  mai_rate <- c(
    "lb" = mai_df$yield_growth - mai_df$yield_growth_ci,
    "central" = mai_df$yield_growth,
    "ub" = mai_df$yield_growth + mai_df$yield_growth_ci
  )
  mai_2028 <- (1 + mai_rate)^n_years * mai_2021
  extra_production <- prior_plantations * (mai_2028 - mai_2021) / 1000000

  additional_area <- (new_wood_demand - extra_production) / mai_2028

  stopifnot(
    "Productivity growth exceeds new demand - additional_area is negative" = all(
      additional_area > 0
    )
  )

  # =========================================================================
  # Pulp expansion scenarios
  # =========================================================================
  # Scenario numbering runs from the most to the least optimistic productivity
  # assumption, so it is inverted relative to area demanded:
  #   scenario 1 <- mai_rate["ub"]      (highest growth, smallest area)
  #   scenario 2 <- mai_rate["central"]
  #   scenario 3 <- mai_rate["lb"]      (lowest growth, largest area)
  exp_area_1 <- additional_area['ub'] * 1000000
  exp_area_2 <- additional_area['central'] * 1000000
  exp_area_3 <- additional_area['lb'] * 1000000

  scenario_growth <- c(
    s1 = mai_rate[["ub"]],
    s2 = mai_rate[["central"]],
    s3 = mai_rate[["lb"]]
  )

  # --- 1. Derive island and land type for each candidate pixel ---
  scenario_df <- pred_df %>%
    mutate(
      island = case_when(
        str_sub(as.character(kab_code), 1, 1) == "1" ~ "Sumatra",
        str_sub(as.character(kab_code), 1, 1) == "6" ~ "Kalimantan"
      ),
      land_type = case_when(
        forest_start == 1 & peat == 1 ~ "Forest on peat",
        forest_start == 1 & peat == 0 ~ "Forest off peat",
        forest_start == 0 & peat == 1 ~ "Non-forest on peat",
        TRUE ~ "Other"
      )
    ) %>%
    # Keep Sumatra and Kalimantan only (drops the 0.7% of points in Kepulauan
    # Riau, none of which approach the selection threshold)
    filter(!is.na(island))

  # --- 2. Helper functions ---
  # Select top-probability pixels up to the target expansion area
  select_scenario <- function(df, exp_area_ha) {
    n_px <- exp_area_ha / 100 # 1 pixel = 1 km2 = 100 ha
    n_px <- round(n_px)
    stopifnot(
      "Expansion area must be positive" = n_px > 0,
      "Expansion area exceeds the available candidate land" = n_px <= nrow(df)
    )
    df %>%
      slice_max(.pred_pulp, n = n_px, with_ties = FALSE) %>%
      mutate(area_ha = 100)
  }

  # Summarise selected pixels by island x land type, with island subtotals
  summarise_scenario <- function(scenario_df) {
    by_type <- scenario_df %>%
      group_by(island, land_type) %>%
      summarise(area_ha = sum(area_ha), .groups = "drop")

    bind_rows(
      by_type,
      by_type %>%
        group_by(island) %>%
        summarise(area_ha = sum(area_ha), .groups = "drop") %>%
        mutate(land_type = "Island total")
    ) %>%
      arrange(island, land_type == "Island total", land_type)
  }

  # --- 3. Run all three scenarios ---
  scenario1_df <- select_scenario(scenario_df, exp_area_1)
  scenario2_df <- select_scenario(scenario_df, exp_area_2)
  scenario3_df <- select_scenario(scenario_df, exp_area_3)

  # --- 4. Build combined table with one column per scenario ---
  expansion_table <- summarise_scenario(scenario1_df) %>%
    rename(scenario_1_ha = area_ha) %>%
    full_join(
      summarise_scenario(scenario2_df) %>% rename(scenario_2_ha = area_ha),
      by = c("island", "land_type")
    ) %>%
    full_join(
      summarise_scenario(scenario3_df) %>% rename(scenario_3_ha = area_ha),
      by = c("island", "land_type")
    ) %>%
    mutate(across(
      c(scenario_1_ha, scenario_2_ha, scenario_3_ha),
      ~ replace_na(.x, 0)
    ))

  # --- Reorganised table: land type as rows, islands as columns ---
  # Renders one cell as "central (low-high)". The bounds come from scenarios 1
  # and 3: scenario 1 is the highest-growth case and so the SMALLEST area.
  fmt_cell <- function(central, low, high) {
    fmt <- function(x) formatC(x, format = "d", big.mark = ",")
    pad <- function(x, w) strrep(" ", pmax(w - nchar(fmt(x)), 0L))
    # Scenario 1's pixels are a strict subset of scenario 3's (same ranking,
    # fewer taken), so the low bound can never exceed the high bound.
    stopifnot("fmt_cell: low bound exceeds high bound" = all(low <= high))
    paste0(
      pad(central, 3L),
      fmt(central),
      " (",
      pad(low, 3L),
      fmt(low),
      "–",
      pad(high, 5L),
      fmt(high),
      ")"
    )
  }
  zero_cell <- paste0(
    strrep(" ", 2L),
    "0 (",
    strrep(" ", 2L),
    "0–",
    strrep(" ", 4L),
    "0)"
  )

  land_type_levels <- c(
    "Forest on peat",
    "Forest off peat",
    "Non-forest on peat",
    "Other"
  )

  reorg_base <- expansion_table %>%
    filter(land_type != "Island total") %>%
    mutate(
      across(c(scenario_1_ha, scenario_2_ha, scenario_3_ha), ~ round(.x / 1000)),
      land_type = factor(land_type, levels = land_type_levels)
    ) %>%
    arrange(land_type)

  # Total column: sum across islands for each land type
  reorg_totals_col <- reorg_base %>%
    group_by(land_type) %>%
    summarise(
      across(c(scenario_1_ha, scenario_2_ha, scenario_3_ha), sum),
      .groups = "drop"
    ) %>%
    mutate(
      Total = fmt_cell(
        central = scenario_2_ha,
        low = scenario_1_ha,
        high = scenario_3_ha
      )
    ) %>%
    select(land_type, Total)

  # Total row: sum across land types for each island + overall total
  # (cross_join() replaces the standalone script's deprecated
  # left_join(by = character()); the result is the same)
  reorg_totals_row <- reorg_base %>%
    group_by(island) %>%
    summarise(
      across(c(scenario_1_ha, scenario_2_ha, scenario_3_ha), sum),
      .groups = "drop"
    ) %>%
    mutate(
      cell = fmt_cell(
        central = scenario_2_ha,
        low = scenario_1_ha,
        high = scenario_3_ha
      ),
      land_type = "Total"
    ) %>%
    select(land_type, island, cell) %>%
    pivot_wider(
      names_from = island,
      values_from = cell,
      values_fill = zero_cell
    ) %>%
    cross_join(
      reorg_base %>%
        summarise(across(c(scenario_1_ha, scenario_2_ha, scenario_3_ha), sum)) %>%
        mutate(
          Total = fmt_cell(
            central = scenario_2_ha,
            low = scenario_1_ha,
            high = scenario_3_ha
          )
        ) %>%
        select(Total)
    )

  expansion_cells <- reorg_base %>%
    mutate(
      cell = fmt_cell(
        central = scenario_2_ha,
        low = scenario_1_ha,
        high = scenario_3_ha
      )
    ) %>%
    select(land_type, island, cell) %>%
    pivot_wider(
      names_from = island,
      values_from = cell,
      values_fill = zero_cell
    ) %>%
    left_join(reorg_totals_col, by = "land_type") %>%
    bind_rows(reorg_totals_row)

  # --- 5. Rasterise scenario 2 expansion pixels for the map ---
  scenario2_rast <- scenario2_df %>%
    drop_na(lon, lat) %>%
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    terra::vect() %>%
    terra::rasterize(rast_template, field = "area_ha", fun = sum)
  names(scenario2_rast) <- "scenario2_expansion"

  # Expansion ha per 0.1 degree cell, as a data frame for geom_tile
  scenario2_plot_df <- as.data.frame(scenario2_rast, xy = TRUE) %>%
    rename(expansion_ha = scenario2_expansion) %>%
    drop_na()

  fig3 <- plot_fig3(scenario2_plot_df, prov_sf, expansion_cells)

  # =========================================================================
  # Scenario statistics
  # =========================================================================
  defor_stats <- expansion_table %>%
    filter(land_type %in% c("Forest on peat", "Forest off peat")) %>%
    summarise(across(c(scenario_1_ha, scenario_2_ha, scenario_3_ha), sum))

  peat_stats <- expansion_table %>%
    filter(land_type %in% c("Forest on peat", "Non-forest on peat")) %>%
    summarise(across(c(scenario_1_ha, scenario_2_ha, scenario_3_ha), sum))

  # additional_area["ub"] corresponds to high MAI growth (= low area demand);
  # additional_area["lb"] to low MAI growth (= high area demand).
  scenario_stats <- tibble(
    new_wood_demand_mm3 = new_wood_demand,
    cap_increase = cap_increase,
    cap_pct_increase = cap_pct_increase,
    ws_2022_delivered_mm3 = ws_2022_delivered,
    demand_pct_increase = demand_pct_increase,
    pct_demand_met_central = extra_production["central"] / new_wood_demand * 100,
    pct_demand_met_low = extra_production["lb"] / new_wood_demand * 100,
    pct_demand_met_high = extra_production["ub"] / new_wood_demand * 100,
    area_demand_central_mha = additional_area["central"],
    area_demand_low_mha = additional_area["ub"],
    area_demand_high_mha = additional_area["lb"],
    mai_growth_central_pct = mai_rate["central"] * 100,
    mai_growth_lb_pct = mai_rate["lb"] * 100,
    mai_growth_ub_pct = mai_rate["ub"] * 100,
    mai_2028_central = mai_2028["central"],
    mai_2028_lb = mai_2028["lb"],
    mai_2028_ub = mai_2028["ub"],
    defor_central_ha = defor_stats$scenario_2_ha,
    defor_low_ha = defor_stats$scenario_1_ha,
    defor_high_ha = defor_stats$scenario_3_ha,
    peat_central_ha = peat_stats$scenario_2_ha,
    peat_low_ha = peat_stats$scenario_1_ha,
    peat_high_ha = peat_stats$scenario_3_ha
  )

  # =========================================================================
  # Statements (main text, SI 4.3 and 8.4)
  # =========================================================================
  si_para <- function(...) c(strwrap(sprintf(...), width = 78), "")

  # The SM quotes the naive area requirement at the historical average
  # delivered MAI, before any further productivity growth.
  naive_area_mha <- new_wood_demand / mai_df$dmai
  fmt_d <- function(x) formatC(x, format = "d", big.mark = ",")

  si_text <- c(
    "EXPANSION SCENARIOS: MAIN TEXT AND SI SECTIONS 4.3 AND 8.4",
    strrep("=", 78),
    "Generated by R/analysis/05_pulp_expansion_scenarios.R (targets pipeline)",
    "",
    "Main text: scale of the planned expansion",
    strrep("-", 78),
    si_para(
      paste(
        "Together, these three projects will increase the country's pulp capacity",
        "by %.0f%% (%.2f million tonnes of pulp per year) and, once fully",
        "operational, will increase the country's pulp sector's annual demand for",
        "pulpwood inputs by %.0f million m3."
      ),
      100 * cap_pct_increase,
      cap_increase,
      new_wood_demand
    ),
    si_para(
      paste(
        "Entering these data into Equation 4, we estimate that, once fully",
        "operational, the new production lines detailed in Table 7 will demand",
        "%.1f million m3 of delivered pulpwood per year. This represents an",
        "increase of %.0f%% over total Indonesian pulpwood consumption in %d."
      ),
      new_wood_demand,
      100 * demand_pct_increase,
      2022
    ),
    "SI 4.3 Required supply base",
    strrep("-", 78),
    si_para(
      paste(
        "Assuming that planned capacity expansions will require a further %.1f",
        "million m3 of delivered pulpwood per year and that, in practice, each",
        "hectare of pulpwood plantation will continue to generate approximately",
        "%.1f m3 of net deliverable pulpwood per year, we estimate that %.2f",
        "million hectares of new Indonesian pulpwood plantations (net planted",
        "area) would be needed."
      ),
      new_wood_demand,
      mai_df$dmai,
      naive_area_mha
    ),
    si_para(
      paste(
        "However, if productivity continued to increase by %.1f (+/-%.1f) percent",
        "per year, average delivered mean annual increment would reach %.1f (95%%",
        "confidence interval: %.1f-%.1f) m3 of wood per hectare per year by %d."
      ),
      100 * mai_rate[["central"]],
      100 * mai_df$yield_growth_ci,
      mai_2028[["central"]],
      mai_2028[["lb"]],
      mai_2028[["ub"]],
      2021 + n_years
    ),
    si_para(
      paste(
        "Given these yield improvements, Indonesia's existing %.2f million",
        "hectares of plantation forests could provide a further %.1f (%.1f-%.1f)",
        "million m3 of pulpwood per year, or %.0f (%.1f-%.1f) percent of",
        "anticipated demand growth."
      ),
      prior_plantations / 1e6,
      extra_production[["central"]],
      extra_production[["lb"]],
      extra_production[["ub"]],
      100 * extra_production[["central"]] / new_wood_demand,
      100 * extra_production[["lb"]] / new_wood_demand,
      100 * extra_production[["ub"]] / new_wood_demand
    ),
    si_para(
      paste(
        "Even under this highly optimistic scenario, %s (%s-%s) hectares of",
        "additional plantations would be needed to meet the pulpwood demand from",
        "new pulp mill production lines."
      ),
      fmt_d(round(additional_area[["central"]] * 1e6)),
      fmt_d(round(additional_area[["ub"]] * 1e6)),
      fmt_d(round(additional_area[["lb"]] * 1e6))
    ),
    "Main text and SI 8.4: where that expansion would fall",
    strrep("-", 78),
    si_para(
      paste(
        "Assuming that this pulp expansion follows similar patterns to the recent",
        "past (2017-2022), we estimate that it will drive %s (%s-%s) hectares of",
        "additional deforestation and %s (%s-%s) hectares of additional peatland",
        "conversion (Figure 3)."
      ),
      fmt_d(defor_stats$scenario_2_ha),
      fmt_d(defor_stats$scenario_1_ha),
      fmt_d(defor_stats$scenario_3_ha),
      fmt_d(peat_stats$scenario_2_ha),
      fmt_d(peat_stats$scenario_1_ha),
      fmt_d(peat_stats$scenario_3_ha)
    ),
    "SUPPORTING VALUES (not reported in the manuscript)",
    strrep("-", 78),
    si_para(
      paste(
        "Denominator for the consumption comparison above: %.1f million m3 of",
        "pulpwood delivered to the mills in 2022, per RPBBI sourcing reports."
      ),
      ws_2022_delivered
    ),
    si_para(
      paste(
        "Scenario bounds follow the productivity assumption, not the area: S1",
        "assumes %.1f%% annual growth and so demands the least new planting, S3",
        "assumes %.1f%% and demands the most."
      ),
      100 * scenario_growth[["s1"]],
      100 * scenario_growth[["s3"]]
    ),
    si_para(
      "Figure 3 is written to %s and the underlying statistics to %s.",
      results_paths[["fig3"]],
      results_paths[["stats"]]
    )
  )

  diagnostics <- utils::capture.output({
    cat("Expansion by island and land type (ha), scenarios 1-3:\n")
    print(expansion_table, n = Inf)
    cat("\nScenario statistics:\n")
    print(tidyr::pivot_longer(scenario_stats, everything()), n = Inf)
  })

  list(
    scenario_stats = scenario_stats,
    expansion_table = expansion_table,
    fig3 = fig3,
    si_text = si_text,
    diagnostics = diagnostics
  )
}

#' Build Figure 3: (A) map of scenario 2 expansion, (B) expansion table
#'
#' Built in its own function because a ggplot keeps the environment it was
#' created in; inside run_pulp_expansion_scenarios() that would carry the full
#' prediction table into the stored result.
#' @param scenario2_plot_df Expansion ha per 0.1 degree cell (x, y,
#'   expansion_ha)
#' @param prov_sf Province boundaries
#' @param expansion_cells Formatted table cells (land type x island + totals)
plot_fig3 <- function(scenario2_plot_df, prov_sf, expansion_cells) {
  expansion_gt <- expansion_cells %>%
    gt::gt(rowname_col = "land_type") %>%
    gt::tab_header(
      title = "Projected pulpwood plantation expansion (thousand ha)"
    ) %>%
    gt::cols_label(
      Kalimantan = "Kalimantan",
      Sumatra = "Sumatra",
      Total = "Total"
    ) %>%
    gt::tab_options(table.width = gt::pct(100)) %>%
    gt::tab_style(
      style = gt::cell_text(align = "center"),
      locations = gt::cells_column_labels()
    ) %>%
    gt::tab_style(
      style = gt::cell_text(font = "Courier New", size = gt::px(13), color = "black"),
      locations = gt::cells_body()
    )

  # Clip to Sumatra + Kalimantan (st_transform ensures geom_sf and geom_tile
  # share the same CRS)
  map_bbox <- sf::st_bbox(c(xmin = 93, xmax = 121, ymin = -8, ymax = 8), crs = 4326)
  prov_wgs84 <- sf::st_transform(prov_sf, 4326)
  prov_clip <- sf::st_crop(prov_wgs84, map_bbox)

  # Neighboring country land (Malaysia, Brunei, PNG, etc.) for context
  neighbors <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf") %>%
    filter(sovereignt != "Indonesia") %>%
    sf::st_transform(4326) %>%
    sf::st_crop(map_bbox)

  pub_map <- ggplot() +
    # Ocean background - drawn first so land sits on top
    annotate(
      "rect",
      xmin = 93,
      xmax = 121,
      ymin = -8,
      ymax = 8,
      fill = "grey99"
    ) +
    # Neighboring countries (lighter grey - context only)
    geom_sf(
      data = neighbors,
      fill = "grey80",
      colour = "grey60",
      linewidth = 0.2
    ) +
    # Indonesia provinces
    geom_sf(data = prov_clip, fill = "grey92", colour = NA) +
    # Expansion hot spots (scenario 2); geom_tile avoids coord_sf alignment issues
    geom_tile(data = scenario2_plot_df, aes(x = x, y = y, fill = expansion_ha)) +
    scale_fill_gradientn(
      colours = c("#F0E442", "#E69F00", "#D55E00"),
      name = "Projected pulpwood\nplantation expansion\n(ha)",
      labels = scales::comma
    ) +
    # Province borders on top
    geom_sf(data = prov_clip, fill = NA, colour = "grey50", linewidth = 0.2) +
    # Island labels
    annotate(
      "text",
      x = 102.5,
      y = -4,
      label = "Sumatra",
      hjust = 1,
      vjust = 1,
      size = 3.5,
      fontface = "italic",
      colour = "grey20"
    ) +
    annotate(
      "text",
      x = 114,
      y = -4,
      label = "Kalimantan",
      hjust = 0.5,
      vjust = 1,
      size = 3.5,
      fontface = "italic",
      colour = "grey20"
    ) +
    coord_sf(xlim = c(93, 121), ylim = c(-8, 8), expand = FALSE, crs = 4326) +
    labs(x = NULL, y = NULL) +
    theme_bw(base_size = 11) +
    theme(
      panel.background = element_rect(fill = "grey97", colour = NA),
      panel.grid = element_line(colour = "white", linewidth = 0.3),
      legend.position = "inside",
      legend.position.inside = c(0.02, 0.05),
      legend.justification = c(0, 0),
      legend.background = element_rect(fill = alpha("white", 0.7), colour = NA),
      axis.text = element_text(size = 8)
    )

  # as_gtable() produces fixed-width columns; convert to proportional npc units
  # so the table stretches to fill whatever width patchwork allocates to it
  gt_grob <- gt::as_gtable(expansion_gt)
  w <- as.numeric(gt_grob$widths)
  gt_grob$widths <- grid::unit(w / sum(w), "npc")

  # The plot keeps this function's environment; drop the full-resolution
  # boundaries it no longer needs (the layers hold the cropped versions)
  rm(prov_sf, prov_wgs84, expansion_cells, expansion_gt)

  (pub_map + theme(plot.margin = margin(4, 4, 2, 4, "pt"))) /
    (patchwork::wrap_elements(full = gt_grob) +
      theme(plot.margin = margin(0, 4, 4, 4, "pt"))) +
    patchwork::plot_annotation(tag_levels = "A") +
    patchwork::plot_layout(heights = c(1.8, 1))
}

#' Save Figure 3
#' @param scenario_results Output of run_pulp_expansion_scenarios()
#' @param output_path Destination .png path
#' @return output_path, as required by format = "file"
save_fig3 <- function(scenario_results, output_path) {
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  ggsave_without_showtext(
    output_path,
    scenario_results$fig3,
    width = 7.5,
    height = 8,
    dpi = 300
  )
  output_path
}
