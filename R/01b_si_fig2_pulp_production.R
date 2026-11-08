## ---------------------------------------------------------
## Project: Indonesia pulp deforestation
## Purpose: SM Figure 2 -- pulp production by wood type.
##
## Previously panel B of the main-text Figure 1 composite; moved to the SM
## when the editors restricted the main text to Figures 1 and 2. Standalone,
## so it carries no panel tag. Styling comes from R/00_fig_shared.R and the
## shared x limits are retained, so the plotted content is unchanged.
##
## Pipeline inputs (targets in _targets.R; paths relative to
##   data/01_data_replication/)
##        1) timber_for_pulp_file -> 01_in/obidzinski_dermawan/plot_data.csv
##           pulp_production_file -> 01_in/tables/annual_pulp_shr_prod.xlsx:
##               Wood supply by source and pulp production (project inputs).
##
## Pipeline outputs
##        1) si_fig2_files -> outputs/figures/
##               si_f2_pulp_production_by_wood_type.png and .svg.
##        2) si_fig2_data -> outputs/figure_data/
##               si_f2_pulp_production_by_wood_type.csv.
##        Intermediate targets: pulp_prod_ratio_merged, si_fig2_plot.
## ---------------------------------------------------------

# =========================================================================
# 1. DATA PREPARATION
# =========================================================================

#' Prepare wood supply transition data for Panel B
prep_wood_supply_data <- function(timber_for_pulp, pulp_production) {
  # O-D ratios
  timber_for_pulp_od <- timber_for_pulp %>%
    pivot_wider(
      names_from = label,
      values_from = c(year_digitized, timber_m3)
    ) %>%
    mutate(timber_m3_mth = timber_m3_total - timber_m3_plantation) %>%
    select(year, timber_m3_plantation, timber_m3_mth) %>%
    pivot_longer(
      cols = c(-year),
      names_to = 'woodtype',
      values_to = 'annual_prod_mtpy'
    ) %>%
    mutate(
      woodtype = ifelse(
        woodtype == "timber_m3_plantation",
        "Plantation",
        "Mixed Tropical Hardwoods"
      )
    ) %>%
    group_by(year) %>%
    mutate(ratio = annual_prod_mtpy / sum(annual_prod_mtpy)) %>%
    ungroup() %>%
    select(year, woodtype, ratio)

  # Pulp production with ratios
  pulp_prod_modified <- pulp_production %>%
    select(year, annual_prod_mtpy, total_pulp_mth, total_pulp_plantation) %>%
    pivot_longer(
      cols = c(-year, -annual_prod_mtpy),
      names_to = 'woodtype',
      values_to = 'ratio'
    ) %>%
    mutate(
      prod_woodtype = ratio * annual_prod_mtpy,
      woodtype = ifelse(
        woodtype == "total_pulp_plantation",
        "Plantation",
        "Mixed Tropical Hardwoods"
      ),
      ratio = ifelse(is.na(ratio), 0, ratio)
    )

  # Merge OD and KLHK data
  timber_for_pulp_od %>%
    full_join(pulp_prod_modified, by = c("year", "woodtype")) %>%
    filter(year > 2000) %>%
    mutate(
      ratio = ifelse(!is.na(ratio.x), ratio.x, ratio.y),
      annual_prod_mtpy = ratio * annual_prod_mtpy
    ) %>%
    select(year, woodtype, annual_prod_mtpy, ratio)
}

# =========================================================================
# 2. PLOT
# =========================================================================

#' Render SM Figure 2: wood supply transition
plot_si_fig2 <- function(pulp_prod_ratio_merged) {
  colorBlind8 <- get_colorblind_palette()
  theme_plot <- get_fig1_theme()

  ggplot(pulp_prod_ratio_merged) +
    geom_bar(
      stat = "identity",
      position = "stack",
      aes(x = year, y = annual_prod_mtpy, fill = as.factor(woodtype))
    ) +
    # Breaks stop at 2022: the wood-supply series ends there, so a 2023 tick
    # labelled an empty slot.
    scale_x_continuous(
      breaks = seq(from = 2001, to = 2022, by = 1),
      limits = FIG_X_LIMITS_TO_2022,
      expand = c(0, 0)
    ) +
    xlab("") +
    scale_y_continuous(
      name = "Pulp production (Million tonnes)\n",
      limits = c(0, 10),
      breaks = seq(0, 19, by = 1),
      expand = c(0, 0)
    ) +
    theme_plot +
    labs(fill = "\n") +
    scale_fill_manual(values = c(colorBlind8[4], colorBlind8[2])) +
    guides(fill = guide_legend(title.position = "top", nrow = 1)) +
    ggtitle("")
}


# =========================================================================
# 3. SAVE
# =========================================================================

#' Save SM Figure 2 to disk
#'
#' Mirrors the save path the composite used: the PNG goes through showtext so
#' DM Sans renders, the SVG has showtext switched off so svglite writes real
#' <text> elements rather than glyph outlines.
#'
#' @param plot_obj The ggplot for this figure.
#' @param output_png,output_svg Destination paths.
#' @return Both paths, as required by format = "file".
save_si_fig2 <- function(plot_obj, output_png, output_svg) {
  dir.create(dirname(output_png), recursive = TRUE, showWarnings = FALSE)

  tryCatch(
    sysfonts::font_add_google(name = "DM Sans", family = "DM Sans"),
    error = function(e) NULL
  )
  if (!"DM Sans" %in% sysfonts::font_families()) {
    stop(
      "Font 'DM Sans' is not registered, so SM Figure 2 would render in a ",
      "fallback font. font_add_google() needs network access.",
      call. = FALSE
    )
  }

  showtext::showtext_auto()
  showtext::showtext_opts(dpi = 400)
  on.exit(showtext::showtext_opts(dpi = 96), add = TRUE)
  ggsave(plot_obj, file = output_png, dpi = 400, width = 12, height = 5)

  showtext::showtext_auto(FALSE)
  on.exit(showtext::showtext_auto(), add = TRUE)
  ggsave(plot_obj, file = output_svg, dpi = 400, width = 12, height = 5)
  c(output_png, output_svg)
}
