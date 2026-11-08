## ---------------------------------------------------------
## Project: Indonesia pulp deforestation
## Purpose: Figure 1 (main text) -- pulp-driven deforestation by island.
##
## Previously panel A of a three-panel composite. The editors restricted the
## main text to Figures 1 and 2, so this is now a standalone figure and
## carries no panel tag. Styling comes from R/00_fig_shared.R, so the plotted
## content is identical to the panel it replaces.
##
## Pipeline inputs (targets in _targets.R; paths relative to
##   data/01_data_replication/)
##        1) kab_file -> 01_in/big/idn_kabupaten_big.shp: District boundaries,
##               used to assign provinces to islands (project input).
##        2) pulp_for_id_file -> 02_out/gee/gaveau/pulp_annual_defor_forest_id.csv
##           pulp_nonfor_id_file -> 02_out/gee/gaveau/pulp_annual_defor_non-forest_id.csv:
##               Annual pulp expansion on forest and non-forest land by
##               district. Google Earth Engine exports, produced by
##               scripts/01_data_extraction/other_extraction/
##               extract_fnf_pulp_expansion_ttm_kab.js
##
## Pipeline outputs
##        1) fig1_files -> outputs/figures/f1_pulp_driven_deforestation.png
##               and .svg: main text Figure 1.
##        2) fig1_data -> outputs/figure_data/
##               fig1_deforestation_by_island.csv: data behind the figure.
##        Intermediate targets: islands_df, id_pulp_conv_for,
##               id_pulp_conv_nonfor, defor_by_island, fig1_plot.
## ---------------------------------------------------------

# =========================================================================
# 1. DATA PREPARATION
# =========================================================================

#' Extract island mapping from kabupaten shapefile
prep_island_mapping <- function(kab) {
  kab %>%
    st_drop_geometry() %>%
    mutate(island = str_sub(prov_code, 1, 1)) %>%
    mutate(
      island = case_when(
        island == 1 ~ "Sumatra",
        island == 6 ~ "Kalimantan",
        island == 9 ~ "Papua"
      )
    ) %>%
    distinct(prov_code, island) %>%
    drop_na(island)
}

#' Clean pulp conversion dataset (forest or non-forest)
clean_pulp_conversion <- function(pulp_df, islands, conv_type_label) {
  pulp_df %>%
    left_join(islands, by = "prov_code") %>%
    select(-prov, -kab, -kab_code, -prov_code, -type) %>%
    dt_pivot_longer(
      cols = -c(island),
      names_to = 'year',
      values_to = 'area_ha'
    ) %>%
    as_tibble() %>%
    filter(area_ha != "0") %>%
    mutate(
      year = as.double(str_replace(year, "deforestation_", ""))
    ) %>%
    group_by(island, year) %>%
    summarize(area_ha = sum(area_ha), .groups = "drop") %>%
    mutate(conv_type = conv_type_label)
}

#' Combine pulp-driven deforestation by island and year for Panel A
prep_defor_by_island <- function(id_pulp_conv_for, id_pulp_conv_nonfor) {
  id_pulp_conv_for %>%
    bind_rows(id_pulp_conv_nonfor) %>%
    filter(year < 2023 & conv_type == "forest") %>%
    group_by(year, island) %>%
    summarize(area_ha = sum(area_ha), .groups = "drop")
}


# =========================================================================
# 2. PLOT
# =========================================================================

#' Render Figure 1: pulp-driven deforestation by island
plot_fig1 <- function(defor_by_island) {
  colorBlind8 <- get_colorblind_palette()
  theme_plot <- get_fig1_theme()
  island_order <- c("Sumatra", "Kalimantan", "Papua")

  ggplot(data = defor_by_island, aes(x = year)) +
    geom_bar(
      stat = "identity",
      position = "stack",
      aes(y = area_ha / 1000, fill = factor(island, levels = rev(island_order)))
    ) +
    ylab("Pulp-driven deforestation (kha)\n") +
    xlab("") +
    scale_fill_manual(
      values = c(colorBlind8[7], colorBlind8[3], colorBlind8[5]),
      breaks = island_order,
      labels = island_order
    ) +
    scale_x_continuous(
      breaks = seq(from = 2001, to = 2022, by = 1),
      limits = FIG_X_LIMITS_TO_2022,
      expand = c(0, 0)
    ) +
    scale_y_continuous(limits = c(0, 150), expand = c(0, 0)) +
    guides(
      fill = guide_legend(nrow = 1, reverse = FALSE),
      keyheight = 10
    ) +
    theme_plot
}


# =========================================================================
# 3. SAVE
# =========================================================================

#' Save Figure 1 to disk
#'
#' Mirrors the save path the composite used: the PNG goes through showtext so
#' DM Sans renders, the SVG has showtext switched off so svglite writes real
#' <text> elements rather than glyph outlines.
#'
#' @param plot_obj The ggplot for this figure.
#' @param output_png,output_svg Destination paths.
#' @return Both paths, as required by format = "file".
save_fig1 <- function(plot_obj, output_png, output_svg) {
  dir.create(dirname(output_png), recursive = TRUE, showWarnings = FALSE)

  tryCatch(
    sysfonts::font_add_google(name = "DM Sans", family = "DM Sans"),
    error = function(e) NULL
  )
  if (!"DM Sans" %in% sysfonts::font_families()) {
    stop(
      "Font 'DM Sans' is not registered, so Figure 1 would render in a ",
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
