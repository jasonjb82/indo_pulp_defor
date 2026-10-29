## ---------------------------------------------------------
## Project: Indonesia pulp deforestation
## Purpose: Generate plot of deforestation type, timing and remaining forest areas
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Pipeline inputs (targets in _targets.R; paths relative to
##   data/01_data_replication/)
##        1) hti_conv_timing_file -> 02_out/tables/hti_grps_deforestation_timing.csv:
##               Land cover change by concession, with supplier group,
##               ownership class and timing of conversion.
##               Produced by scripts/02_data_preparation/01_data_prep.R
##
## Pipeline outputs
##        1) fig2_png, fig2_svg -> outputs/figures/
##               f2_supplier_groups_defor_class_plot.png and .svg: Figure 2.
##        2) fig2_data -> outputs/figure_data/
##               fig2_deforestation_by_ownership_and_class.csv: data behind
##               Figure 2 (one row per stacked bar segment).
## ---------------------------------------------------------

library(tidyverse)
library(scales)
library(patchwork)
library(janitor)
library(showtext)
library(sysfonts)

# The DM Sans font is registered in save_fig2() rather than here, so that
# loading the pipeline (tar_source()) does not need network access

# Ordering of the figure's two categorical axes. These are file-level because
# both the plot and the exported figure data depend on them: the plot uses them
# as factor levels, and the export uses them to keep its rows to the categories
# the figure actually draws, in the order it draws them.
FIG2_OWNERSHIP_ORDER <- c(
  "Acknowledged ownership",
  "Suspected corporate control based on civil society investigations",
  "Third-party suppliers",
  "Not yet supplying to mills"
)

FIG2_DEFOR_ORDER <- c(
  "Deforestation for pulp after 2015",
  "Deforestation for pulp during 2001-2015",
  "Deforestation not for pulp",
  "Remaining forest"
)

#' Prepare frequency table from hti_conv_timing dataset
#'
#' The affiliation categories are no longer derived here. They arrive in
#' `affiliation_class`, computed once in scripts/02_data_preparation/
#' 01_data_prep.R, so the deposited table and this figure cannot disagree.
#' Deriving them here previously meant a concession could be labelled
#' "Third-party suppliers" in the data and drawn in a different bar, and a
#' Marubeni-only clause silently overrode the suspected-corporate-control
#' category for one concession.
prep_hti_defor_timing <- function(hti_conv_timing_df) {
  if (!"affiliation_class" %in% names(hti_conv_timing_df)) {
    stop(
      "hti_grps_deforestation_timing.csv has no affiliation_class column.\n",
      "It is produced by scripts/02_data_preparation/01_data_prep.R; data ",
      "built before that step needs regenerating.",
      call. = FALSE
    )
  }
  hti_conv_timing_df %>%
    filter(all == 1) %>%
    filter(conv_type == 2 | is.na(conv_type) | is.na(supplier_group)) %>%
    check_fig2_affiliation_classes() %>%
    # One row per segment the figure draws. The earlier version also grouped by
    # linked_group and carried a within-group `freq`, but the figure maps only
    # affiliation class (y), class (fill) and area_ha (x), so linked_group split
    # each segment into sub-rectangles of one colour that were never
    # distinguishable, and `freq` was never plotted at all.
    group_by(ownership_class = affiliation_class, class) %>%
    summarize(area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop") %>%
    arrange(
      factor(ownership_class, levels = FIG2_OWNERSHIP_ORDER),
      factor(class, levels = FIG2_DEFOR_ORDER)
    )
}

#' Build Figure 2 broken bar plot using patchwork
plot_fig2_defor_timing <- function(freq_tab_df) {
  check_fig2_ownership_classes(freq_tab_df)
  ownership_order <- FIG2_OWNERSHIP_ORDER
  defor_order <- FIG2_DEFOR_ORDER

  theme_plot <- theme(
    text = element_text(family = "DM Sans", colour = "#3A484F"),
    panel.background = element_rect(colour = NA, fill = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(
      color = "grey70",
      linetype = "dashed",
      linewidth = 0.35
    ),
    plot.title = element_text(hjust = 0, size = 11, face = "bold"),
    axis.line.x = element_line(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.spacing = unit(1, "lines"),
    axis.text.x = element_text(size = 9, color = "grey30"),
    axis.text.y = element_text(size = 9, color = "grey30"),
    axis.title.x = element_text(size = 10, color = "grey30"),
    axis.title.y = element_text(size = 10, color = "grey30"),
    strip.text.x = element_text(size = 12, face = "bold", color = "grey30"),
    strip.background = element_rect(color = NA, fill = NA),
    legend.key.height = unit(12, "pt"),
    legend.key.width = unit(12, "pt"),
    legend.text = element_text(size = 9, colour = "grey30"),
    legend.title = element_blank()
  )

  base_plot <- freq_tab_df %>%
    as_tibble() %>%
    mutate(label_order = factor(ownership_class, rev(ownership_order))) %>%
    ggplot() +
    aes(
      y = label_order,
      x = area_ha,
      fill = factor(class, levels = defor_order)
    ) +
    geom_bar(
      stat = "identity",
      position = position_stack(reverse = TRUE)
    ) +
    theme_plot +
    ylab("Association with RGE or Sinar Mas") +
    xlab("Area (ha)") +
    # Wrap wider than the default so the long "Suspected corporate control..."
    # label resolves into fewer, longer lines; ggplot then allocates more
    # horizontal room to the tick labels.
    scale_y_discrete(labels = function(x) str_wrap(x, width = 26)) +
    guides(fill = guide_legend(nrow = 2)) +
    scale_fill_manual(
      values = c("#CC79A7", "#0072B2", "#F0E442", "#009E73"),
      name = "Group",
      breaks = defor_order,
      labels = defor_order
    )

  # Left panel (0 to 1M ha)
  p_left <- base_plot +
    coord_cartesian(xlim = c(0, 1000000)) +
    scale_x_continuous(
      labels = scales::label_number(
        scale_cut = c("k" = 1e3, "M" = 1e6)
      ),
      breaks = c(0, 500000, 1000000),
      expand = c(0, 0)
    ) +
    theme(
      # Moderate right margin (0.3 cm) to keep gap small without text overlap
      plot.margin = margin(t = 0.5, r = 0.3, b = 0.5, l = 0.5, unit = "cm")
    )

  # Right panel (2.5M+ ha)
  # Upper bound sits just above the longest bar (~2.62M ha) rather than at a
  # round 3M, which left roughly three quarters of this panel empty.
  p_right <- base_plot +
    coord_cartesian(xlim = c(2500000, 2700000)) +
    scale_x_continuous(
      # accuracy = 0.1 is needed here: the default rounds this panel's single
      # break of 2,500,000 to "2M", which misreads as the bar starting half a
      # million hectares lower than it does. The left panel's breaks are whole
      # units, so it keeps the default.
      labels = scales::label_number(
        accuracy = 0.1,
        scale_cut = c("k" = 1e3, "M" = 1e6)
      ),
      breaks = c(2500000),
      expand = c(0, 0)
    ) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      axis.ticks.y = element_blank(),
      axis.line.y = element_blank(),
      # Moderate left margin (0.3 cm)
      plot.margin = margin(t = 0.5, r = 0.5, b = 0.5, l = 0.3, unit = "cm")
    )

  # Combine using patchwork
  p_combined <- (p_left | p_right) +
    plot_layout(widths = c(3, 0.6), guides = "collect") &
    theme(
      legend.position = "bottom",
      text = element_text(family = "DM Sans")
    )

  return(p_combined)
}

#' Check the incoming affiliation_class column before it is aggregated
#'
#' 01_data_prep.R guarantees affiliation_class is one of the four categories
#' and never NA, but this table arrives from a deposited file that may have
#' been built by an older version of that script. Checking here, on the rows
#' rather than on the aggregate, catches both an unexpected label and a
#' concession that would otherwise be dropped -- the aggregate-level check
#' cannot see NA rows, because grouping has already discarded them.
check_fig2_affiliation_classes <- function(df) {
  n_missing <- sum(is.na(df$affiliation_class))
  if (n_missing) {
    stop(
      "Figure 2: ", n_missing, " row(s) have a missing affiliation_class, ",
      "covering ",
      format(sum(df$area_ha[is.na(df$affiliation_class)], na.rm = TRUE),
             big.mark = ","),
      " ha. 01_data_prep.R should leave none; the data needs rebuilding.",
      call. = FALSE
    )
  }
  unexpected <- setdiff(unique(df$affiliation_class), FIG2_OWNERSHIP_ORDER)
  if (length(unexpected)) {
    stop(
      "Figure 2: affiliation_class values not in FIG2_OWNERSHIP_ORDER:\n",
      paste0("  - ", gsub("\n", " ", unexpected), collapse = "\n"),
      "\nExpected:\n",
      paste0("  - ", gsub("\n", " ", FIG2_OWNERSHIP_ORDER), collapse = "\n"),
      call. = FALSE
    )
  }
  invisible(df)
}

#' Check that the aggregated table's categories are the ones the figure expects
#'
#' The plot turns ownership_class into a factor with FIG2_OWNERSHIP_ORDER as
#' its levels, so a category spelled any other way silently becomes NA and the
#' figure draws it as a row labelled "NA". That is how a renamed input label
#' ("Suspected ownership based on..." vs "Suspected corporate control based
#' on...") once got as far as a rendered figure. Fail loudly instead.
check_fig2_ownership_classes <- function(freq_tab_df) {
  unexpected <- setdiff(
    unique(freq_tab_df$ownership_class),
    FIG2_OWNERSHIP_ORDER
  )
  if (length(unexpected)) {
    stop(
      "Figure 2: ownership_class values not in FIG2_OWNERSHIP_ORDER:\n",
      paste0("  - ", gsub("\n", " ", unexpected), collapse = "\n"),
      "\nExpected:\n",
      paste0("  - ", gsub("\n", " ", FIG2_OWNERSHIP_ORDER), collapse = "\n"),
      "\nThe input table's labels and FIG2_OWNERSHIP_ORDER have diverged; ",
      "reconcile them rather than letting the category drop out.",
      call. = FALSE
    )
  }
  invisible(freq_tab_df)
}

#' Save Figure 2 to PNG or SVG
save_fig2 <- function(freq_tab_df, output_path) {
  dir_path <- dirname(output_path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }

  # Register DM Sans here (needs network access) and fail loudly if that is
  # not possible, rather than silently drawing in a fallback font
  tryCatch(
    sysfonts::font_add_google(name = "DM Sans", family = "DM Sans"),
    error = function(e) NULL
  )
  if (!"DM Sans" %in% sysfonts::font_families()) {
    stop(
      "Font 'DM Sans' is not registered, so Figure 2 would render in a ",
      "fallback font. font_add_google() needs network access.",
      call. = FALSE
    )
  }

  p <- plot_fig2_defor_timing(freq_tab_df)

  # Set showtext's resolution here rather than relying on a global value: the
  # concession figures and atlas tiles reset it to 96 after drawing, which
  # would otherwise shrink the text if they ran first
  showtext::showtext_auto()
  showtext::showtext_opts(dpi = 400)
  on.exit(showtext::showtext_opts(dpi = 96), add = TRUE)

  # showtext draws every glyph as an outline, which leaves an SVG with no
  # editable text. Switch it off for vector output so svglite writes real
  # <text> elements carrying the font family name (as in Robert's script).
  if (grepl("\\.svg$", output_path)) {
    showtext::showtext_auto(FALSE)
    on.exit(showtext::showtext_auto(), add = TRUE)
  }

  ggplot2::ggsave(
    filename = output_path,
    plot = p,
    width = 9,
    height = 4,
    dpi = 400,
    units = "in"
  )

  return(output_path)
}
