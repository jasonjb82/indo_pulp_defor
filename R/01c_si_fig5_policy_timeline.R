## ---------------------------------------------------------
## Project: Indonesia pulp deforestation
## Purpose: SM Figure 5 -- policy timeline.
##
## Previously panel C of the main-text Figure 1 composite; moved to the SM
## when the editors restricted the main text to Figures 1 and 2. Standalone,
## so it carries no panel tag. Styling comes from R/00_fig_shared.R and the
## shared x limits are retained, so the plotted content is unchanged.
##
## Pipeline inputs (targets in _targets.R; paths relative to
##   data/01_data_replication/)
##        1) policy_tl_file -> 01_in/tables/policy_timeline_cats_rev1.csv:
##               Policy timeline (compiled by hand).
##
## Pipeline outputs
##        1) si_fig5_files -> outputs/figures/si_f5_policy_timeline.png
##               and .svg.
##        2) si_fig5_data -> outputs/figure_data/si_f5_policy_timeline.csv.
##        Intermediate targets: tl_df, si_fig5_plot.
## ---------------------------------------------------------

# =========================================================================
# 1. DATA PREPARATION
# =========================================================================

# to its own dot to read as belonging to it.
TIMELINE_WRAP_WIDTH <- c(
  "EU VPA negotiations begin" = 19,
  "APRIL and APP joins FSC" = 14,
  "Proposed end to SVLK" = 13,
  "Omnibus Law for Job Creation" = 16,
  "Indonesia withdraws from REDD+ agreement" = 21,
  "Norway REDD+ restart" = 13,
  "APP & APRIL propose mill expansions" = 20,
  "PT Phoenix mill proposed" = 13,
  "Introduction of new Forest Business Licenses" = 19
)

#' Wrap a timeline label, honouring any hand-set width
#'
#' str_wrap() takes a single width, so the widths are applied one label at a
#' time rather than vectorised.
wrap_timeline_label <- function(event, default = 25) {
  width <- unname(TIMELINE_WRAP_WIDTH[event])
  width[is.na(width)] <- default
  vapply(
    seq_along(event),
    function(i) {
      if (is.na(event[i])) NA_character_ else stringr::str_wrap(event[i], width[i])
    },
    character(1)
  )
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
    ) %>%
    # Label placement is assigned by hand. The default is that a label sits
    # just above its own dot and is left aligned at it, which is how the
    # submitted figure reads. The events listed below are the exceptions:
    # `label_below` drops a label under its line to clear the neighbour that
    # shares its row, and `label_hjust` right aligns one so it runs leftwards
    # from its dot -- needed both for the last events on the axis and wherever
    # a left aligned label would stretch away towards the following dot and
    # read as belonging to that one instead. Automatic placement (ggrepel, or
    # packing labels into lanes by estimated text width) was tried and slid
    # labels away from the events they describe.
    dplyr::mutate(
      label_wrapped = wrap_timeline_label(event),
      label_below = event %in% c(
        "Omnibus Law for Job Creation", # shares the 2020 dot with "Proposed end to SVLK"
        "VPA enters into force", # 2014, under the tail of "VPA ratified"
        "Norway REDD+ restart", # 2022, under "Indonesia withdraws from..."
        "PT Phoenix mill proposed" # 2023, under "APP & APRIL propose..."
      ),
      label_hjust = ifelse(
        event %in% c(
          "Omnibus Law for Job Creation", # else it reaches across to the 2022 dot
          "Indonesia withdraws from REDD+ agreement", # else it reaches the 2022 dot
          "APP & APRIL propose mill expansions", # else it reaches the 2023 dot
          "Norway REDD+ restart", # last two years of the axis
          "PT Phoenix mill proposed",
          "FSC signs remedy agreements with APP and APRIL"
        ),
        1,
        0
      ),
      # Right aligned labels stop just past their dot rather than on it.
      label_x = year + ifelse(label_hjust == 1, 0.25, 0)
    )
}

# =========================================================================
# 2. PLOT
# =========================================================================

#' Render SM Figure 5: policy timeline
plot_si_fig5 <- function(tl_df) {
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
    # Breaks stop at 2023, the last event year, but the axis runs past it so
    # that labels on the final events stay inside the panel.
    scale_x_continuous(
      limits = FIG1_X_LIMITS,
      expand = c(0, 0),
      breaks = seq(2001, 2023, by = 1)
    ) +
    # Labels on the final events run past the last gridline into the shared
    # right-hand plot margin, so drawing must not be clipped to the panel.
    coord_cartesian(clip = "off") +
    # Extra room below the lowest line: "Omnibus Law for Job Creation" is the
    # one label drawn under the bottom row and its second line needs the space.
    scale_y_continuous(expand = ggplot2::expansion(add = c(0.42, 0.22))) +
    geom_point(aes(y = text_position), size = 4.5, alpha = 0.75) +
    geom_point(
      data = tl_df[tl_df$direction.x == 0, ],
      aes(y = text_position),
      size = 4.5,
      alpha = 1,
      na.rm = TRUE
    ) +
    # Labels are drawn where prep_timeline_data() placed them: beside their own
    # dot, offset a fixed distance from their line, with the few hand-listed
    # exceptions dropped below it.
    geom_text(
      aes(
        y = text_position + ifelse(label_below, -0.11, 0.11),
        x = label_x,
        label = label_wrapped,
        vjust = ifelse(label_below, 1, 0),
        hjust = label_hjust
      ),
      size = 2.5,
      family = "DM Sans",
      fontface = "bold",
      show.legend = FALSE,
      na.rm = TRUE
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
      legend.position = "bottom",
      # Same margin as get_fig1_theme() gives panels A and B, so all three
      # panel regions line up and a year sits at one x position throughout.
      plot.margin = unit(c(0.1, 1.5, 0.1, 0.5), "cm")
    )
}

# =========================================================================
# 3. SAVE
# =========================================================================

#' Save SM Figure 5 to disk
#'
#' Mirrors the save path the composite used: the PNG goes through showtext so
#' DM Sans renders, the SVG has showtext switched off so svglite writes real
#' <text> elements rather than glyph outlines.
#'
#' @param plot_obj The ggplot for this figure.
#' @param output_png,output_svg Destination paths.
#' @return Both paths, as required by format = "file".
save_si_fig5 <- function(plot_obj, output_png, output_svg) {
  dir.create(dirname(output_png), recursive = TRUE, showWarnings = FALSE)

  tryCatch(
    sysfonts::font_add_google(name = "DM Sans", family = "DM Sans"),
    error = function(e) NULL
  )
  if (!"DM Sans" %in% sysfonts::font_families()) {
    stop(
      "Font 'DM Sans' is not registered, so SM Figure 5 would render in a ",
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
