## ---------------------------------------------------------
## Project: Indonesia pulp deforestation
## Purpose: Create plots of annual land cover change within HTI concessions
## Author: Robert Heilmayr and Jason Jon Benedict
## ---------------------------------------------------------

library(tidyverse)
library(stringr)
library(janitor)
library(lubridate)
library(scales)
library(showtext)
library(sysfonts)

#' Render and save land cover change plots for all HTI concessions
render_and_save_all_concessions <- function(hti_gav_annual_lc_df, output_dir) {
  # Safely add Google font and initialize showtext for worker sessions
  tryCatch(
    {
      sysfonts::font_add_google(name = "DM Sans", family = "DM Sans")
    },
    error = function(e) NULL
  )
  showtext::showtext_auto()

  # Ensure destination folder exists
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  # Plot theme definition
  theme_plot <- theme(
    text = element_text(family = "DM Sans", colour = "#3A484F"),
    panel.background = element_rect(colour = NA, fill = NA),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_line(
      color = "grey70",
      linetype = "dashed",
      linewidth = 0.35
    ),
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.line.x = element_line(),
    axis.ticks.x = element_blank(),
    axis.ticks.y = element_blank(),
    panel.spacing = unit(2, "lines"),
    axis.text.x = element_text(
      size = 9,
      color = "grey30",
      angle = 45,
      hjust = 1
    ),
    axis.text.y = element_text(size = 9, color = "grey30"),
    axis.title.x = element_text(size = 10, color = "grey30"),
    axis.title.y = element_text(size = 10, color = "grey30"),
    strip.text.x = element_text(size = 12, face = "bold", color = "grey30"),
    strip.background = element_rect(color = NA, fill = NA),
    legend.key = element_rect(linewidth = 12, fill = "white", colour = NA),
    legend.key.height = unit(10, "pt"),
    legend.key.width = unit(10, "pt"),
    legend.text = element_text(size = 8, colour = "grey30"),
    legend.title = element_blank(),
    legend.position = "bottom",
    legend.box = "horizontal",
    plot.margin = unit(c(0.5, 1.5, 0.5, 0.5), "cm")
  )

  # Get list of concessions from supplier_label
  concessions <- hti_gav_annual_lc_df %>%
    filter(all == 1) %>%
    distinct(supplier_label) %>%
    pull(supplier_label)

  saved_filepaths <- c()

  for (concession_ in concessions) {
    filtered_df <- hti_gav_annual_lc_df %>%
      filter(supplier_label == concession_) %>%
      mutate(
        class_desc = ordered(
          class_desc,
          levels = c("Forest", "Non-forest", "Cleared for pulp")
        )
      )

    # Get actual classes present in plot
    non_zero <- filtered_df %>%
      filter(area_ha > 0) %>%
      pull(class_desc) %>%
      unique() %>%
      sort()

    p <- ggplot(filtered_df, aes(year, area_ha)) +
      geom_area(
        aes(fill = as.factor(class_desc)),
        position = position_stack(reverse = FALSE)
      ) +
      scale_x_continuous(
        expand = c(0, 0),
        breaks = seq(2001, 2022, by = 1),
        limits = c(2001, 2022)
      ) +
      scale_y_continuous(
        labels = scales::label_number(
          scale_cut = scales::cut_short_scale(),
          accuracy = 0.1,
          drop0trailing = TRUE,
          suffix = " ha"
        ),
        expand = c(0, 0)
      ) +
      ylab("") +
      xlab("") +
      ggtitle(paste0(str_sub(concession_, end = -10))) +
      scale_fill_manual(
        values = c(
          "Forest" = "#009E73",
          "Non-forest" = "#F0E442",
          "Cleared for pulp" = "#CC79A7"
        ),
        breaks = non_zero
      ) +
      guides(
        fill = guide_legend(nrow = 1),
        color = guide_legend(nrow = 1),
        shape = guide_legend(nrow = 2),
        keyheight = 10
      ) +
      theme_plot

    # 1. Add ZDC line if available
    if (!all(is.na(filtered_df$zdc_year))) {
      p <- p +
        geom_vline(
          aes(
            xintercept = zdc_year,
            color = "Earliest ZDC year\nof downstream mill"
          ),
          linewidth = 0.5,
          na.rm = TRUE
        )
    }

    if (
      !all(
        filtered_df$license_year < 2001 | filtered_df$license_year >= 2022,
        na.rm = TRUE
      )
    ) {
      p <- p +
        geom_vline(
          aes(xintercept = license_year, color = "License\nyear"),
          linewidth = 0.5,
          linetype = "dashed",
          na.rm = TRUE
        )
    }

    # 3. Add the color scale and guides
    p <- p +
      scale_color_manual(
        values = c(
          "License\nyear" = "#000000",
          "Earliest ZDC year\nof downstream mill" = "#000000"
        )
      )
    file_path <- file.path(
      output_dir,
      paste0(gsub(" ", "_", concession_), "_TreeMap_AnnualChanges.png")
    )

    showtext::showtext_opts(dpi = 400)
    ggsave(
      plot = p,
      filename = file_path,
      dpi = 400,
      width = 10,
      height = 6,
      units = "in",
      limitsize = FALSE
    )
    showtext::showtext_opts(dpi = 96)

    saved_filepaths <- c(saved_filepaths, file_path)
  }

  return(saved_filepaths)
}
