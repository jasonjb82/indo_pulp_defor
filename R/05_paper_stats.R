## ---------------------------------------------------------
## Project: Indonesia pulp deforestation
## Purpose of script: Calculate statistics for paper
## Notes: Refactored for the targets pipeline.
## ---------------------------------------------------------

#' Calculate all paper statistics and text snippets
#' @param rs_acc_df RS accuracy assessment stats
#' @param id_annual_exp_stats Treemap annual expansion stats
#' @param pw_annual_area_id Pulpwood areas Indonesia
#' @param pulp_ttm_soil_type Expansion on soil type
#' @param ws_2015_2022 Wood supply
#' @param kali_annual_pulp_exp_stats Kalimantan annual pulp expansion
#' @param hti_nonhti_conv Pulpwood conversion HTI/non-HTI
#' @param groups_reclass_hti Reclassed ownership groups
#' @param cap_df Mill capacities
#' @param scenario_stats Wood demand and scenarios
#' @param mai_df Parameters from MAI analysis
calc_paper_stats <- function(
  rs_acc_df,
  id_annual_exp_stats,
  pw_annual_area_id,
  pulp_ttm_soil_type,
  ws_2015_2022,
  kali_annual_pulp_exp_stats,
  hti_nonhti_conv,
  groups_reclass_hti,
  cap_df,
  scenario_stats,
  mai_df
) {
  # Formatting function used later
  fmt_ha <- function(x) {
    formatC(round(x, -2), format = "f", digits = 0, big.mark = ",")
  }

  # Inputs read with $ or filtered to specific rows drop a sentence silently
  # when a column or row is missing, so report anything absent up front.
  inputs_complete <- check_paper_inputs(rs_acc_df, scenario_stats, mai_df)

  # =========================================================================
  # Overarching trends in pulp expansion, deforestation, peat conversion
  # =========================================================================

  ## Introduction: pulp-driven deforestation 2001-2011 and share of primary forest loss
  pulp_defor_row <- rs_acc_df %>% filter(stat_name == "defor_2001_2011")
  # Named so it cannot be masked by the forest_loss_pulp_ha column of
  # id_annual_exp_stats inside summarize()
  pulp_defor_2001_2011_ha <- pulp_defor_row %>%
    pull(estimated_area_kha) *
    1000

  pulp_def_share_2001_2011 <- id_annual_exp_stats %>%
    filter(year < 2012) %>%
    summarize(
      total_forest_loss_ha = sum(forest_loss_ha),
      shr_pulp_forest_loss = (pulp_defor_2001_2011_ha / total_forest_loss_ha) *
        100
    )

  text_intro_pulp_defor_2001_2011 <- sprintf(
    paste0(
      "\n[Introduction]\n",
      "Between 2001 and 2011, \033[1m%s\033[0m (95%% CI: \033[1m%s\033[0m–\033[1m%s\033[0m) hectares of primary rainforest were directly\n",
      "converted to pulpwood plantations (see supplementary materials (SM) Section 1), representing \033[1m%.0f\033[0m%% of\n",
      "Indonesian primary forest loss.\n\n"
    ),
    formatC(
      round(pulp_defor_row$estimated_area_kha * 1000, -3),
      format = "f",
      digits = 0,
      big.mark = ","
    ),
    formatC(
      round(pulp_defor_row$ci95_lower_kha * 1000, -3),
      format = "f",
      digits = 0,
      big.mark = ","
    ),
    formatC(
      round(pulp_defor_row$ci95_upper_kha * 1000, -3),
      format = "f",
      digits = 0,
      big.mark = ","
    ),
    pulp_def_share_2001_2011$shr_pulp_forest_loss
  )

  # Table of total pulp areas each year
  annual_pulp_areas <- pw_annual_area_id %>%
    select(constant, starts_with("pulp_")) %>%
    pivot_longer(
      cols = -c(constant),
      names_to = 'year',
      values_to = 'area_ha'
    ) %>%
    mutate(year = as.double(str_replace(year, "pulp_", ""))) %>%
    group_by(year) %>%
    summarize(area_ha = sum(area_ha) - 5000) %>% # GEE calculations adjustment
    mutate(
      annual_pulp_area = area_ha - lag(area_ha, default = first(area_ha))
    ) %>%
    left_join(id_annual_exp_stats, by = "year") %>%
    select(
      year,
      annual_pulp_expansion_area_ha = annual_pulp_area,
      forest_loss_ha,
      forest_loss_pulp_ha,
      nonforest_loss_pulp_ha,
      annual_pulp_area_ha = area_ha
    )

  annual_conv <- annual_pulp_areas %>%
    group_by(year) %>%
    summarize(area_ha = sum(forest_loss_pulp_ha))

  # Introduction / An emerging boom: pulp-driven deforestation declined by 95% (2011-2017)
  conv_2011 <- annual_conv %>% filter(year == 2011) %>% pull(area_ha)
  conv_2017 <- annual_conv %>% filter(year == 2017) %>% pull(area_ha)
  early_change <- (conv_2017 - conv_2011) / conv_2011

  text_intro_decline_2011_2017 <- sprintf(
    paste0(
      "\n[Introduction]\n",
      "we describe how these four elements interacted over a period of time (2011-2017)\n",
      "when pulp-driven deforestation fell by \033[1m%.0f\033[0m%%\n\n"
    ),
    abs(early_change) * 100
  )

  text_boom_decline_2011_2017 <- sprintf(
    paste0(
      "\n[An emerging boom in deforestation?]\n",
      "Although pulp-driven deforestation declined by \033[1m%.0f\033[0m%% between 2011 and 2017,\n",
      "recent trends and policy debates highlight the fragility of this progress.\n\n"
    ),
    abs(early_change) * 100
  )

  # An emerging boom: increase in pulp-driven deforestation and peat conversion (2017-2022)
  conv_2022 <- annual_conv %>% filter(year == 2022) %>% pull(area_ha)
  late_change <- (conv_2022 - conv_2017) / conv_2017

  # Conversion of peat between 2017 and 2022
  annual_pulp_conv <- pulp_ttm_soil_type %>%
    select(
      -`system:index`,
      -constant,
      -kab,
      -kab_code,
      -prov_code,
      -.geo,
      -type
    ) %>%
    pivot_longer(cols = -c(prov), names_to = 'year', values_to = 'area_ha') %>%
    mutate(
      class = str_extract(year, "[^_]+"),
      year = as.numeric(gsub("[^0-9]", "", year))
    ) %>%
    ungroup() %>%
    group_by(year, class) %>%
    summarize(area_ha = sum(area_ha), .groups = "keep")

  pulp_conv_2017 <- annual_pulp_conv %>%
    filter(class == "peat" & year == 2017) %>%
    pull(area_ha)
  pulp_conv_2022 <- annual_pulp_conv %>%
    filter(class == "peat" & year == 2022) %>%
    pull(area_ha)
  overall_pulp_change <- (pulp_conv_2022 - pulp_conv_2017) / pulp_conv_2017

  text_boom_rates_2017_2022 <- sprintf(
    paste0(
      "\n[An emerging boom in deforestation?]\n",
      "Between 2017 and 2022, the annual rate of conversion of primary forests to pulpwood\n",
      "plantations increased from \033[1m%s\033[0m ha/year to \033[1m%s\033[0m ha/year (\033[1m%.0f\033[0m%% increase), while\n",
      "pulp-driven conversion of peatlands increased from \033[1m%s\033[0m ha/year to \033[1m%s\033[0m ha/year\n",
      "(\033[1m%.0f\033[0m%% increase).\n\n"
    ),
    formatC(round(conv_2017, -2), format = "f", digits = 0, big.mark = ","),
    formatC(round(conv_2022, -2), format = "f", digits = 0, big.mark = ","),
    late_change * 100,
    formatC(
      round(pulp_conv_2017, -2),
      format = "f",
      digits = 0,
      big.mark = ","
    ),
    formatC(
      round(pulp_conv_2022, -2),
      format = "f",
      digits = 0,
      big.mark = ","
    ),
    overall_pulp_change * 100
  )

  pulp_exp_row <- rs_acc_df %>% filter(stat_name == "pulp_expansion_2001_2011")
  text_productivity_expansion_2001_2011 <- sprintf(
    paste0(
      "\n[Improved productivity]\n",
      "Many of these forests were cleared to make room for industrial acacia and eucalyptus\n",
      "plantations, which expanded by \033[1m%.2f\033[0m (\033[1m%.2f\033[0m–\033[1m%.2f\033[0m) million hectares between 2001 and 2011.\n\n"
    ),
    pulp_exp_row$estimated_area_kha / 1e3,
    pulp_exp_row$ci95_lower_kha / 1e3,
    pulp_exp_row$ci95_upper_kha / 1e3
  )

  # Deforestation rates in 2022 vs 2011 peak
  overall_change <- (conv_2022 - conv_2011) / conv_2011
  text_boom_vs_2011_peak <- sprintf(
    paste0(
      "\n[An emerging boom in deforestation?]\n",
      "While pulp-driven deforestation rates in 2022 were still \033[1m%.0f\033[0m%% lower than the 2011 peak.\n\n"
    ),
    abs(overall_change) * 100
  )

  # Deforestation for pulp vs palm in 2022
  defor_2022 <- id_annual_exp_stats %>%
    filter(year == 2022) %>%
    summarize(
      pulp_ha = sum(forest_loss_pulp_ha),
      palm_ha = sum(forest_loss_palm_ha)
    )

  text_boom_pulp_vs_palm_2022 <- sprintf(
    paste0(
      "\n[An emerging boom in deforestation? - validation]\n",
      "more of Indonesia’s forests were converted to new\n",
      "pulpwood plantations than to industrial oil palm plantations in 2022:\n",
      "\033[1m%s\033[0m\n\n"
    ),
    ifelse(defor_2022$pulp_ha > defor_2022$palm_ha, "TRUE", "FALSE")
  )

  # Improved productivity: pulp plantations supply
  current_wood_demand <- ws_2015_2022 %>%
    filter(YEAR == 2022) %>%
    pull(VOLUME_M3) %>%
    sum()
  text_productivity_supply_2022 <- sprintf(
    paste0(
      "\n[Improved productivity]\n",
      "As a result of this combination of pulpwood plantation expansion and intensification,\n",
      "plantations now supply nearly all of Indonesia's \033[1m%.0f\033[0m million m3 of annual pulpwood demand.\n\n"
    ),
    current_wood_demand / 1e6
  )

  # Kalimantan deforestation share since 2017
  kali_pulp_driven_defor <- kali_annual_pulp_exp_stats %>%
    left_join(annual_conv, by = "year") %>%
    filter(year >= 2017) %>%
    summarize(shr_kali_pulp_defor = sum(forest_loss_ha) / sum(area_ha) * 100)

  text_mills_kalimantan_share <- sprintf(
    paste0(
      "\n[New pulp mills and capacity expansion]\n",
      "The expansion of pulp processing infrastructure into Kalimantan is particularly\n",
      "important since the region has been responsible for \033[1m%.0f\033[0m%% of pulp-driven\n",
      "deforestation since 2017.\n\n"
    ),
    kali_pulp_driven_defor$shr_kali_pulp_defor
  )

  # =========================================================================
  # Description of ZDC violations
  # =========================================================================

  ownership_defor <- hti_nonhti_conv %>%
    left_join(groups_reclass_hti, by = c("supplier_id" = "id")) %>%
    filter(conv_type == 2 & year >= 2015) %>%
    group_by(group_reclassed) %>%
    summarize(area_ha = sum(area_ha), .groups = "drop") %>%
    mutate(share = (area_ha / sum(area_ha)) * 100)

  total_defor_2015_2022 <- sum(ownership_defor$area_ha)
  app_april_ha <- ownership_defor %>%
    filter(group_reclassed == "Owned or acknowledged") %>%
    pull(area_ha)
  linked_ha <- ownership_defor %>%
    filter(group_reclassed == "NGO-linked") %>%
    pull(area_ha)
  linked_pct <- ownership_defor %>%
    filter(group_reclassed == "NGO-linked") %>%
    pull(share)
  external_pct <- ownership_defor %>%
    filter(group_reclassed == "Indirect supplier" | is.na(group_reclassed)) %>%
    pull(share) %>%
    sum()

  text_commitments_ownership <- sprintf(
    paste0(
      "\n[Private commitments to end deforestation]\n",
      "Despite the sector's ambitious goals, we find that \033[1m%s\033[0m hectares of forests were\n",
      "directly converted to pulpwood plantations between 2015 and 2022. Concessions\n",
      "officially claimed by APP and APRIL had little pulp-driven deforestation after 2015\n",
      "(\033[1m%s\033[0m ha), providing evidence of progress towards their environmental commitments.\n",
      "However, APP and APRIL's parent conglomerates, the Sinar Mas Group and\n",
      "the Royal Golden Eagle Group (RGE), have suspected corporate control over\n",
      "concessions that were responsible for \033[1m%s\033[0m ha (\033[1m%.0f\033[0m%%) of pulp-driven deforestation\n",
      "during this period. The remaining \033[1m%.0f\033[0m%% of pulp-driven deforestation occurred in\n",
      "concessions controlled by external suppliers or outside of concessions, underscoring\n",
      "the important role that external suppliers play in the implementation of\n",
      "zero-deforestation commitments.\n\n"
    ),
    formatC(
      round(total_defor_2015_2022, -2),
      format = "f",
      digits = 0,
      big.mark = ","
    ),
    formatC(round(app_april_ha, -2), format = "f", digits = 0, big.mark = ","),
    formatC(round(linked_ha, -2), format = "f", digits = 0, big.mark = ","),
    linked_pct,
    external_pct
  )

  # =========================================================================
  # Capacity expansions
  # =========================================================================

  sinar_rge_cap_share <- (cap_df %>%
    filter(MILL_ID != "M-0005") %>%
    pull(PULP_CAP_MTPY) %>%
    sum()) /
    sum(cap_df$PULP_CAP_MTPY) *
    100

  text_mills_capacity_share <- sprintf(
    paste0(
      "\n[New pulp mills and capacity expansion]\n",
      "As of 2026, Sinar Mas and RGE – which together control over \033[1m%.0f\033[0m%% of the\n",
      "industry’s production capacity – are both working towards major expansions of\n",
      "existing pulp mills in Sumatra, while a company linked to RGE has begun operating\n",
      "a new semi-chemical pulp mill in Kalimantan.\n\n"
    ),
    sinar_rge_cap_share
  )

  area_demand_historical <- scenario_stats$new_wood_demand_mm3 / mai_df$dmai
  text_mills_capacity_demand <- sprintf(
    paste0(
      "\n[New pulp mills and capacity expansion]\n",
      "Together, these three projects will increase the country's pulp capacity by \033[1m%.0f\033[0m%%\n",
      "(\033[1m%.2f\033[0m million tonnes of pulp per year) and, once fully operational, will increase\n",
      "the pulp sector's annual demand for pulpwood inputs by \033[1m%.0f\033[0m million m3 (SM Section 4).\n",
      "At historical levels of plantation productivity, an additional \033[1m%.2f\033[0m million\n",
      "hectares of plantations will be needed to meet this anticipated boom in pulpwood demand.\n\n"
    ),
    scenario_stats$cap_pct_increase * 100,
    scenario_stats$cap_increase,
    scenario_stats$new_wood_demand_mm3,
    area_demand_historical
  )

  # Description of growth trend
  mai_ci <- scenario_stats$mai_growth_central_pct -
    scenario_stats$mai_growth_lb_pct
  mai_lb <- scenario_stats$mai_growth_lb_pct
  mai_ub <- scenario_stats$mai_growth_ub_pct
  hardiyanto_pct <- mai_df$hardiyanto_cagr * 100
  hardiyanto_in_ci <- hardiyanto_pct >= mai_lb & hardiyanto_pct <= mai_ub

  text_barriers_productivity_growth <- sprintf(
    paste0(
      "\n[Barriers facing productivity improvements]\n",
      "We find that, between 2015 and 2021, pulpwood plantations achieved increases in\n",
      "productivity of approximately \033[1m%.1f\033[0m (± \033[1m%.1f\033[0m) percent per year (SM Section 3),\n",
      "which is consistent with estimates based on pre-harvest inventory data from\n",
      "operational plantations.\n\n",
      "Validation - Hardiyanto CAGR (%.1f%%) falls within our CI [%.1f%%–%.1f%%]: \033[1m%s\033[0m\n\n"
    ),
    scenario_stats$mai_growth_central_pct,
    mai_ci,
    hardiyanto_pct,
    mai_lb,
    mai_ub,
    ifelse(hardiyanto_in_ci, "TRUE", "FALSE")
  )

  text_barriers_scenarios <- sprintf(
    paste0(
      "\n[Barriers facing productivity improvements]\n",
      "If companies are able to sustain these recent rates of productivity improvement, we\n",
      "estimate that the increased production on existing plantations would meet only\n",
      "\033[1m%.0f\033[0m (\033[1m%.0f\033[0m–\033[1m%.0f\033[0m) percent of the anticipated growth in pulpwood\n",
      "demand (SM Section 4). Even under these optimistic assumptions, a further\n",
      "\033[1m%s\033[0m (\033[1m%s\033[0m–\033[1m%s\033[0m) hectares of pulpwood plantations would be needed. Assuming\n",
      "that this pulp expansion follows similar patterns to the recent past (2017–2022),\n",
      "we estimate that it will drive \033[1m%s\033[0m (\033[1m%s\033[0m–\033[1m%s\033[0m) hectares of additional\n",
      "deforestation and \033[1m%s\033[0m (\033[1m%s\033[0m–\033[1m%s\033[0m) hectares of additional peatland conversion\n",
      "(Figure 3).\n\n"
    ),
    scenario_stats$pct_demand_met_central,
    scenario_stats$pct_demand_met_low,
    scenario_stats$pct_demand_met_high,
    fmt_ha(scenario_stats$area_demand_central_mha * 1e6),
    fmt_ha(scenario_stats$area_demand_low_mha * 1e6),
    fmt_ha(scenario_stats$area_demand_high_mha * 1e6),
    fmt_ha(scenario_stats$defor_central_ha),
    fmt_ha(scenario_stats$defor_low_ha),
    fmt_ha(scenario_stats$defor_high_ha),
    fmt_ha(scenario_stats$peat_central_ha),
    fmt_ha(scenario_stats$peat_low_ha),
    fmt_ha(scenario_stats$peat_high_ha)
  )

  # The abstract repeats the headline scenario estimates. An incomplete
  # scenario_stats (such as an outdated copy) can still carry these columns but
  # from a different model run, so leave the sentence out rather than print it.
  text_abstract_scenarios <- if (!inputs_complete) character(0) else sprintf(
    paste0(
      "\n[Abstract]\n",
      "Given barriers to further productivity gains and wavering domestic policy support for\n",
      "conservation, we estimate ongoing pulp mill capacity expansion could drive another\n",
      "\033[1m%s\033[0m (\033[1m%s\033[0m–\033[1m%s\033[0m) hectares of deforestation and \033[1m%s\033[0m (\033[1m%s\033[0m–\033[1m%s\033[0m) hectares\n",
      "of peatland conversion.\n\n"
    ),
    fmt_ha(scenario_stats$defor_central_ha),
    fmt_ha(scenario_stats$defor_low_ha),
    fmt_ha(scenario_stats$defor_high_ha),
    fmt_ha(scenario_stats$peat_central_ha),
    fmt_ha(scenario_stats$peat_low_ha),
    fmt_ha(scenario_stats$peat_high_ha)
  )

  new_demand_pct_increase <- scenario_stats$new_wood_demand_mm3 /
    (current_wood_demand / 1e6) *
    100
  text_sm4_new_demand <- sprintf(
    paste0(
      "\n[SM Section 4]\n",
      "Entering these data into Equation 4, we estimate that, once fully operational, the new\n",
      "production lines detailed in Table 7 will demand \033[1m%.1f\033[0m million m3 of delivered\n",
      "pulpwood per year. This represents an increase of \033[1m%.0f\033[0m%% over total Indonesian\n",
      "pulpwood consumption in 2022.\n\n"
    ),
    scenario_stats$new_wood_demand_mm3,
    new_demand_pct_increase
  )

  prior_plantations_mha <- rs_acc_df %>%
    filter(stat_name == "total_pp_area_2022") %>%
    pull(estimated_area_kha) /
    1e3
  extra_prod_central <- scenario_stats$pct_demand_met_central *
    scenario_stats$new_wood_demand_mm3 /
    100
  extra_prod_low <- scenario_stats$pct_demand_met_low *
    scenario_stats$new_wood_demand_mm3 /
    100
  extra_prod_high <- scenario_stats$pct_demand_met_high *
    scenario_stats$new_wood_demand_mm3 /
    100

  text_sm4_area_needed <- sprintf(
    paste0(
      "\n[SM Section 4]\n",
      "Assuming that planned capacity expansions will require a further \033[1m%.1f\033[0m million m3 of\n",
      "delivered pulpwood per year and that, in practice, each hectare of pulpwood plantation\n",
      "will continue to generate approximately \033[1m%.1f\033[0m m3 of net deliverable pulpwood per year,\n",
      "we estimate that \033[1m%.2f\033[0m million hectares of new Indonesian pulpwood plantations (net\n",
      "planted area) would be needed. However, if productivity continued to increase by\n",
      "\033[1m%.1f\033[0m (±\033[1m%.1f\033[0m) percent per year for a further five years, average delivered mean\n",
      "annual increment would reach \033[1m%.1f\033[0m (95%% confidence interval: \033[1m%.1f\033[0m–\033[1m%.1f\033[0m) m3 of wood\n",
      "per hectare per year by 2028. Given these yield improvements, Indonesia's existing\n",
      "\033[1m%.2f\033[0m million hectares of plantation forests could provide a further\n",
      "\033[1m%.1f\033[0m (\033[1m%.1f\033[0m–\033[1m%.1f\033[0m) million m3 of pulpwood per year, or\n",
      "\033[1m%.0f\033[0m (\033[1m%.1f\033[0m–\033[1m%.1f\033[0m) percent of anticipated demand growth. Even under\n",
      "this highly optimistic scenario, \033[1m%s\033[0m (\033[1m%s\033[0m–\033[1m%s\033[0m) hectares of additional\n",
      "plantations would be needed to meet the pulpwood demand from new pulp mill production lines.\n\n"
    ),
    scenario_stats$new_wood_demand_mm3,
    mai_df$dmai,
    area_demand_historical,
    scenario_stats$mai_growth_central_pct,
    mai_ci,
    scenario_stats$mai_2028_central,
    scenario_stats$mai_2028_lb,
    scenario_stats$mai_2028_ub,
    prior_plantations_mha,
    extra_prod_central,
    extra_prod_low,
    extra_prod_high,
    scenario_stats$pct_demand_met_central,
    scenario_stats$pct_demand_met_low,
    scenario_stats$pct_demand_met_high,
    fmt_ha(scenario_stats$area_demand_central_mha * 1e6),
    fmt_ha(scenario_stats$area_demand_low_mha * 1e6),
    fmt_ha(scenario_stats$area_demand_high_mha * 1e6)
  )

  # =========================================================================
  # Package everything into a single return list
  # =========================================================================

  stats_list <- list(
    # Raw numeric variables
    pulp_defor_2001_2011_kha = pulp_defor_row$estimated_area_kha,
    shr_pulp_forest_loss_2001_2011 = pulp_def_share_2001_2011$shr_pulp_forest_loss,
    early_change_pct = abs(early_change) * 100,
    late_change_pct = late_change * 100,
    peat_conv_increase_pct = overall_pulp_change * 100,
    pulp_exp_2001_2011_mha = pulp_exp_row$estimated_area_kha / 1e3,
    overall_change_pct = abs(overall_change) * 100,
    defor_2022_pulp_vs_palm = ifelse(
      defor_2022$pulp_ha > defor_2022$palm_ha,
      TRUE,
      FALSE
    ),
    current_wood_demand_mm3 = current_wood_demand / 1e6,
    kali_pulp_driven_defor_pct = kali_pulp_driven_defor$shr_kali_pulp_defor,
    zdc_total_defor_2015_2022_ha = total_defor_2015_2022,
    zdc_app_april_ha = app_april_ha,
    zdc_linked_ha = linked_ha,
    zdc_linked_pct = linked_pct,
    zdc_external_pct = external_pct,
    sinar_rge_cap_share = sinar_rge_cap_share,
    area_demand_historical = area_demand_historical,
    hardiyanto_in_ci = hardiyanto_in_ci,
    new_demand_pct_increase = new_demand_pct_increase,
    prior_plantations_mha = prior_plantations_mha,
    extra_prod_central = extra_prod_central,
    extra_prod_low = extra_prod_low,
    extra_prod_high = extra_prod_high,

    # Formatted text blocks, in manuscript order (sections of the main text,
    # then the SM)
    text_abstract_scenarios = text_abstract_scenarios,
    text_intro_pulp_defor_2001_2011 = text_intro_pulp_defor_2001_2011,
    text_intro_decline_2011_2017 = text_intro_decline_2011_2017,
    text_productivity_expansion_2001_2011 = text_productivity_expansion_2001_2011,
    text_productivity_supply_2022 = text_productivity_supply_2022,
    text_commitments_ownership = text_commitments_ownership,
    text_boom_decline_2011_2017 = text_boom_decline_2011_2017,
    text_boom_rates_2017_2022 = text_boom_rates_2017_2022,
    text_boom_pulp_vs_palm_2022 = text_boom_pulp_vs_palm_2022,
    text_boom_vs_2011_peak = text_boom_vs_2011_peak,
    text_mills_capacity_share = text_mills_capacity_share,
    text_mills_kalimantan_share = text_mills_kalimantan_share,
    text_mills_capacity_demand = text_mills_capacity_demand,
    text_barriers_productivity_growth = text_barriers_productivity_growth,
    text_barriers_scenarios = text_barriers_scenarios,
    text_sm4_new_demand = text_sm4_new_demand,
    text_sm4_area_needed = text_sm4_area_needed
  )

  check_paper_sentences(stats_list)

  return(stats_list)
}

#' Warn when inputs to calc_paper_stats() lack columns or rows it relies on
#'
#' Warns rather than stops while the Zenodo scenario_stats.csv predates the
#' columns used below; switch to stop() once scenario_stats is computed in the
#' pipeline.
#' @param rs_acc_df,scenario_stats,mai_df Inputs to calc_paper_stats()
#' @return TRUE if nothing is missing, invisibly
check_paper_inputs <- function(rs_acc_df, scenario_stats, mai_df) {
  required_cols <- list(
    scenario_stats = c(
      "new_wood_demand_mm3",
      "cap_increase",
      "cap_pct_increase",
      paste0("pct_demand_met_", c("central", "low", "high")),
      paste0("area_demand_", c("central", "low", "high"), "_mha"),
      paste0("mai_growth_", c("central", "lb", "ub"), "_pct"),
      paste0("mai_2028_", c("central", "lb", "ub")),
      paste0("defor_", c("central", "low", "high"), "_ha"),
      paste0("peat_", c("central", "low", "high"), "_ha")
    ),
    mai_df = c("dmai", "hardiyanto_cagr")
  )
  inputs <- list(scenario_stats = scenario_stats, mai_df = mai_df)

  problems <- character(0)
  for (nm in names(required_cols)) {
    missing_cols <- setdiff(required_cols[[nm]], names(inputs[[nm]]))
    if (length(missing_cols) > 0) {
      problems <- c(
        problems,
        paste0(nm, " is missing columns: ", paste(missing_cols, collapse = ", "))
      )
    }
  }

  required_stats <- c(
    "defor_2001_2011",
    "pulp_expansion_2001_2011",
    "total_pp_area_2022"
  )
  missing_stats <- setdiff(required_stats, rs_acc_df$stat_name)
  if (length(missing_stats) > 0) {
    problems <- c(
      problems,
      paste0(
        "rs_acc_df is missing stat_name rows: ",
        paste(missing_stats, collapse = ", ")
      )
    )
  }

  if (length(problems) > 0) {
    warning(
      "calc_paper_stats() inputs incomplete; affected sentences will be ",
      "missing from the output:\n  ",
      paste(problems, collapse = "\n  "),
      call. = FALSE
    )
  }
  invisible(length(problems) == 0)
}

#' Warn when any paper sentence was not produced as exactly one string
#'
#' Catches sentences dropped because an input value was empty, and sentences
#' repeated because an input value had more than one element.
#' @param stats_list The named list built in calc_paper_stats()
#' @return TRUE if every sentence is a single string, invisibly
check_paper_sentences <- function(stats_list) {
  text_keys <- grep("^text_", names(stats_list), value = TRUE)
  n_strings <- lengths(stats_list[text_keys])
  bad <- n_strings != 1

  if (any(bad)) {
    warning(
      "Paper sentences not produced as a single string ",
      "(count in brackets): ",
      paste0(text_keys[bad], " [", n_strings[bad], "]", collapse = ", "),
      call. = FALSE
    )
  }
  invisible(!any(bad))
}

#' Save calculated paper text snippets to a text document
#' @param stats_list The named list output from calc_paper_stats()
#' @param file_path Output destination file path (e.g., .txt)
save_paper_stats <- function(stats_list, file_path) {
  # Ensure the destination directory exists
  dir.create(dirname(file_path), recursive = TRUE, showWarnings = FALSE)

  # Extract all text blocks starting with "text_"
  text_keys <- grep("^text_", names(stats_list), value = TRUE)
  text_snippets <- unlist(stats_list[text_keys])

  # Strip ANSI bold terminal escape codes (\033[1m) for clean reading
  clean_snippets <- gsub("\033\\[[0-9;]*m", "", text_snippets)

  # Write directly out as a formatted plain-text document
  writeLines(clean_snippets, file_path)

  # Return file path as required by format = "file"
  return(file_path)
}
