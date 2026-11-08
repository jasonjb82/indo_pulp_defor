## ---------------------------------------------------------
## Project: Indonesia pulp deforestation
## Purpose: Shared styling for the trend figures.
##
## The main-text Figure 1 and SM Figures 2 and 5 were previously three panels
## of one composite. The editors restricted the main text to Figure 1 and
## Figure 2, so each panel is now produced as a standalone figure by its own
## script. The palette, theme and x-axis limits live here because all three
## still share them -- keeping the limits identical preserves the appearance
## each panel had in the composite.
## ---------------------------------------------------------

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

# X-axis limits.
#
# These were one shared constant while the three figures were panels of a
# composite and had to align. Now that each is standalone they do not, and
# the two bar charts carry data only to 2022 -- so they end at 2022 rather
# than reserving blank space for a 2023 the data never reaches. The padding
# is symmetric: 1.0 before the first break, 1.0 after the last.
FIG_X_LIMITS_TO_2022 <- c(2000, 2023)

# The policy timeline genuinely runs to 2023 and needs room beyond the last
# gridline for labels on the final events, which overhang into the plot
# margin (see the clip = "off" note in R/01c_si_fig5_policy_timeline.R).
FIG1_X_LIMITS <- c(2000, 2023.7)

# Panel C labels wrap at 25 characters by default. These are the ones where a
# hand-set width is what keeps a label clear of its neighbour, or close enough
