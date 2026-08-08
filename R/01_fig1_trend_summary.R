## ---------------------------------------------------------
## Project: Indonesia pulp deforestation
## Purpose of script: Refactored functions to generate Figure 1 (Summary Figure)
## Author: Robert Heilmayr and Jason Jon Benedict
## ---------------------------------------------------------

# =========================================================================
# 1. HELPER FUNCTIONS: THEMES & PALETTES
# =========================================================================

#' Get the standard 8-color colorblind friendly palette
get_colorblind_palette <- function() {
  c(
    "#999999",
    "#E69F00",
    "#56B4E9",
    "#009E73",
    "#F0E442",
    "#0072B2",
    "#D55E00",
    "#CC79A7"
  )
}

#' Get standard figure theme
get_fig1_theme <- function() {
  sysfonts::font_add_google(name = "DM Sans", family = "DM Sans")
  showtext::showtext_auto()
  showtext::showtext_opts(dpi = 400)

  theme(
    text = element_text(family = "DM Sans", colour = "#3A484F"),
    panel.background = element_rect(colour = NA, fill = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(
      color = "grey70",
      linetype = "dashed",
      linewidth = 0.35
    ),
    plot.title = element_text(hjust = 0.5),
    axis.line.x = element_line(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x = element_text(
      size = 8,
      color = "grey30",
      angle = 0,
      face = "bold"
    ),
    axis.text.y = element_text(size = 9, color = "grey30"),
    axis.title.x = element_text(size = 10, color = "grey30"),
    axis.title.y = element_text(size = 10, color = "grey30"),
    strip.text.x = element_text(size = 12, face = "bold", color = "grey30"),
    strip.background = element_rect(color = NA, fill = NA),
    legend.key.height = unit(12, "pt"),
    legend.key.width = unit(12, "pt"),
    legend.text = element_text(size = 9, colour = "grey30"),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.direction = "horizontal",
    plot.margin = unit(c(0.1, 1.5, 0.1, 0.5), "cm")
  )
}

# =========================================================================
# 2. DATA PREPARATION FUNCTIONS
# =========================================================================

#' Extract island mapping from kabupaten shapefile
prep_island_mapping <- function(kab) {
  kab %>%
    st_drop_geometry() %>%
    mutate(island = str_sub(prov_code, 1, 1)) %>%
    mutate(
      island = case_when(
        island == 1 ~ "Sumatera",
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

#' Clean PPI price dataset
clean_pulp_prices <- function(pulp_prices) {
  pulp_prices %>%
    select(DATE, PPI = WPU0911) %>%
    mutate(DATE = as.Date(DATE, format = "%m/%d/%Y")) %>%
    mutate(year = year(DATE), PPI = as.double(PPI)) %>%
    select(year, PPI)
}

#' Combine deforestation and price data for Panel A
prep_defor_price_comb <- function(
  id_pulp_conv_for,
  id_pulp_conv_nonfor,
  pulp_prices_clean
) {
  id_pulp_conv_for %>%
    bind_rows(id_pulp_conv_nonfor) %>%
    left_join(pulp_prices_clean, by = "year") %>%
    filter(year < 2023 & conv_type == "forest") %>%
    group_by(year, island) %>%
    summarize(area_ha = sum(area_ha), PPI = max(PPI), .groups = "drop")
}

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

#' Prepare policy timeline data for Panel C
prep_timeline_data <- function(policy_tl) {
  policy_tl_clean <- policy_tl %>%
    mutate(year_col = as.Date(year_proper, format = "%d/%m/%Y"))

  df <- policy_tl_clean[with(policy_tl_clean, order(year)), ]

  type_levels <- c(
    "Indonesian government",
    "Companies",
    "International governments"
  )
  df$type <- factor(df$type, levels = type_levels, ordered = TRUE)

  positions <- c(0.5)
  directions <- unique(df$direction)

  line_pos <- data.frame(
    "year" = unique(df$year),
    "position" = rep(positions, length.out = length(unique(df$year))),
    "direction" = rep(directions, length.out = length(unique(df$year)))
  )

  df <- merge(x = df, y = line_pos, by = "year", all = TRUE)
  df <- df[with(df, order(year, type)), ]
  df$year_count <- ave(df$year == df$year, df$year, FUN = cumsum)
  df$text_position <- df$type_cat

  df %>%
    mutate(
      direction = as.factor(direction.x),
      text_position_mod = case_when(
        event == "Omnibus Law for Job Creation" ~ 0.1,
        event == "PT Phoenix mill proposed" ~ 1.6,
        event == "REDD+ agreement with Norway" ~ 3.5,
        event == "Indonesia withdraws from Norway REDD+" ~ 3.5,
        event == "Norway REDD+ restart" ~ 3.5,
        TRUE ~ text_position
      ),
      text_position = ifelse(row_cat == 32, 3.5, text_position),
      text_position = ifelse(row_cat == 11, 1, text_position),
      text_position = ifelse(row_cat == 21, 1.5, text_position),
      text_position = ifelse(row_cat == 22, 2, text_position),
      text_position = ifelse(row_cat == 23, 2.5, text_position)
    )
}

# =========================================================================
# 3. PANEL PLOTTING FUNCTIONS
# =========================================================================

#' Render Panel A: Deforestation and PPI
plot_panel_a <- function(defor_price_comb) {
  colorBlind8 <- get_colorblind_palette()
  theme_plot <- get_fig1_theme()
  island_order <- c("Sumatera", "Kalimantan", "Papua")
  pa_scale_factor <- 0.5

  ggplot(data = defor_price_comb, aes(x = year)) +
    geom_bar(
      stat = "identity",
      position = "stack",
      aes(y = area_ha / 1000, fill = factor(island, levels = rev(island_order)))
    ) +
    geom_line(aes(y = PPI * pa_scale_factor, color = "Producer Price Index")) +
    geom_point(aes(y = PPI * pa_scale_factor, color = "Producer Price Index")) +
    ylab("Pulp-drive deforestation (Kha)\n") +
    xlab("") +
    scale_fill_manual(
      values = c(colorBlind8[7], colorBlind8[3], colorBlind8[5]),
      breaks = island_order,
      labels = island_order
    ) +
    scale_color_manual(values = c("black")) +
    scale_x_continuous(
      breaks = seq(from = 2001, to = 2022, by = 1),
      expand = c(0, 1)
    ) +
    scale_y_continuous(
      sec.axis = sec_axis(
        ~ . * 1,
        labels = number_format(scale = 1 / pa_scale_factor),
        name = "Producer Price Index\n"
      ),
      limits = c(0, 150),
      expand = c(0, 0)
    ) +
    guides(
      fill = guide_legend(nrow = 1, reverse = FALSE),
      color = guide_legend(nrow = 1, reverse = TRUE),
      keyheight = 10
    ) +
    theme_plot
}

#' Render Panel B: Wood Supply Transition
plot_panel_b <- function(pulp_prod_ratio_merged) {
  colorBlind8 <- get_colorblind_palette()
  theme_plot <- get_fig1_theme()

  ggplot(pulp_prod_ratio_merged) +
    geom_bar(
      stat = "identity",
      position = "stack",
      aes(x = year, y = annual_prod_mtpy, fill = as.factor(woodtype))
    ) +
    scale_x_continuous(breaks = seq(from = 2001, to = 2023, by = 1)) +
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

#' Render Panel C: Policy Timeline
plot_panel_c <- function(tl_df) {
  colorBlind8 <- get_colorblind_palette()
  type_levels <- c(
    "Indonesian government",
    "Companies",
    "International governments"
  )
  type_colors <- c(colorBlind8[4], colorBlind8[6], colorBlind8[8])
  type_fill <- c(colorBlind8[4], colorBlind8[6], colorBlind8[8])
  type_shape <- c(16)

  ggplot(
    tl_df,
    aes(x = year, y = 0, col = type, label = type, shape = direction)
  ) +
    geom_segment(
      data = subset(tl_df, row_cat == 11),
      aes(
        y = text_position,
        yend = 1,
        x = min(year),
        xend = max(year),
        group = 1
      ),
      alpha = 1,
      linewidth = 1.75,
      linetype = 'solid',
      color = c(colorBlind8[4])
    ) +
    geom_segment(
      data = subset(tl_df, row_cat == 21),
      aes(
        y = text_position,
        yend = 1.5,
        x = min(year),
        xend = max(year),
        group = 1
      ),
      alpha = 1,
      linewidth = 1.75,
      linetype = 'solid',
      color = c(colorBlind8[6])
    ) +
    geom_segment(
      data = subset(tl_df, row_cat == 22),
      aes(
        y = text_position,
        yend = 2,
        x = min(year),
        xend = max(year),
        group = 1
      ),
      alpha = 1,
      linewidth = 1.75,
      linetype = 'solid',
      color = c(colorBlind8[6])
    ) +
    geom_segment(
      data = subset(tl_df, row_cat == 23),
      aes(
        y = text_position,
        yend = 2.5,
        x = min(year),
        xend = max(year),
        group = 1
      ),
      alpha = 1,
      linewidth = 1.75,
      linetype = 'solid',
      color = c(colorBlind8[6])
    ) +
    geom_segment(
      data = subset(tl_df, row_cat == 31),
      aes(
        y = text_position,
        yend = 3,
        x = min(year),
        xend = max(year),
        group = 1
      ),
      alpha = 1,
      linewidth = 1.75,
      linetype = 'solid',
      color = c(colorBlind8[8])
    ) +
    geom_segment(
      data = subset(tl_df, row_cat == 32),
      aes(
        y = text_position,
        yend = 3.5,
        x = min(year),
        xend = max(year),
        group = 1
      ),
      alpha = 1,
      linewidth = 1.75,
      linetype = 'solid',
      color = c(colorBlind8[8])
    ) +
    ylab("\n") +
    scale_color_manual(
      values = type_colors,
      labels = type_levels,
      drop = FALSE,
      guide = guide_legend(reverse = TRUE),
      name = "",
      na.translate = FALSE
    ) +
    scale_fill_manual(
      values = type_fill,
      labels = type_levels,
      drop = FALSE,
      guide = "legend",
      name = "",
      na.translate = FALSE
    ) +
    scale_shape_manual(
      values = type_shape,
      labels = type_levels,
      drop = TRUE,
      guide = "none",
      name = "",
      na.translate = FALSE
    ) +
    theme_classic() +
    scale_x_continuous(expand = c(0, 0.5), breaks = seq(2001, 2023, by = 1)) +
    scale_y_discrete(expand = c(0, 0.2)) +
    geom_point(aes(y = text_position), size = 4.5, alpha = 0.75) +
    geom_point(
      data = tl_df[tl_df$direction.x == 0, ],
      aes(y = text_position),
      size = 4.5,
      alpha = 1,
      na.rm = TRUE
    ) +
    ggrepel::geom_text_repel(
      aes(
        y = text_position_mod + 0.05,
        x = year,
        label = stringr::str_wrap(event, 25),
        na.rm = TRUE
      ),
      size = 2.75,
      hjust = 0,
      vjust = -1.25,
      family = "DM Sans",
      fontface = "bold",
      show.legend = FALSE,
      min.segment.length = 2.5
    ) +
    theme(
      text = element_text(family = "DM Sans"),
      panel.grid.major.x = element_line(colour = "grey95", linewidth = 6),
      axis.line.y = element_blank(),
      axis.text.y = element_blank(),
      axis.title.x = element_blank(),
      axis.ticks.y = element_blank(),
      axis.text.x = element_text(
        vjust = 5,
        color = "grey30",
        angle = 0,
        face = "bold"
      ),
      axis.ticks.x = element_blank(),
      axis.line.x = element_blank(),
      legend.title = element_blank(),
      legend.position = "bottom"
    )
}

# =========================================================================
# 4. COMPOSITE PLOT & SAVE FUNCTIONS
# =========================================================================

#' Combine Panels A, B, and C into a composite figure
create_fig1_summary <- function(panel_a, panel_b, panel_c) {
  comb_plot <- panel_a / panel_b / panel_c
  comb_plot +
    plot_annotation(tag_levels = "A") &
    theme(plot.tag = element_text(face = 'bold', size = 12))
}

#' Save composite plot to disk
save_fig1 <- function(comb_plot, output_png, output_svg) {
  ggsave(comb_plot, file = output_png, dpi = 400, width = 12, height = 15)
  ggsave(comb_plot, file = output_svg, dpi = 400, width = 12, height = 15)
  return(c(output_png, output_svg))
}
