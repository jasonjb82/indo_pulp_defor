## ---------------------------------------------------------
## Project: Indonesia pulp deforestation
## Purpose: Generate plot of deforestation type, timing and remaining forest areas
## Author: Robert Heilmayr and Jason Jon Benedict
## ---------------------------------------------------------

library(tidyverse)
library(scales)
library(patchwork)
library(janitor)
library(showtext)
library(sysfonts)

# Enable DM Sans font rendering for ggplot & ggsave
sysfonts::font_add_google("DM Sans", "DM Sans")
showtext::showtext_auto()
showtext::showtext_opts(dpi = 400)

#' Prepare frequency table from hti_conv_timing dataset
prep_hti_defor_timing <- function(hti_conv_timing_df) {
  freq_tab <- hti_conv_timing_df %>%
    filter(all == 1) %>%
    filter(conv_type == 2 | is.na(conv_type) | is.na(supplier_group)) %>%
    mutate(
      supplier_group = case_when(
        linked_group == "APP" &
          ownership_class == "NGO-linked" ~ "SINAR MAS (NGO-LINKED)",
        linked_group == "APRIL" &
          ownership_class ==
            "NGO-linked" ~ "ROYAL GOLDEN EAGLE / TANOTO (NGO-LINKED)",
        TRUE ~ supplier_group
      )
    ) %>%
    mutate(
      ownership_class = case_when(
        (ownership_class == "Third-party suppliers" | is.na(ownership_class)) &
          april == 0 &
          app == 0 &
          marubeni == 0 ~ "Not yet\nsupplying\nto mills",
        april == 0 & app == 0 & marubeni == 1 ~ "Not yet\nsupplying\nto mills",
        TRUE ~ ownership_class
      )
    ) %>%
    group_by(ownership_class, linked_group, class) %>%
    summarize(area_ha = sum(area_ha, na.rm = TRUE), .groups = "drop_last") %>%
    mutate(freq = area_ha / sum(area_ha)) %>%
    drop_na(ownership_class) %>%
    ungroup()

  return(freq_tab)
}

#' Build Figure 2 broken bar plot using patchwork
plot_fig2_defor_timing <- function(freq_tab_df) {
  ownership_order <- c(
    "Acknowledged ownership",
    "Suspected ownership based on civil society investigations",
    "Third-party suppliers",
    "Not yet\nsupplying\nto mills"
  )

  defor_order <- c(
    "Deforestation for pulp after 2015",
    "Deforestation for pulp during 2001-2015",
    "Deforestation not for pulp",
    "Remaining forest"
  )

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
    geom_bar(stat = "identity", position = position_stack(reverse = TRUE)) +
    theme_plot +
    ylab("Association with RGE or Sinar Mas") +
    xlab("Area (ha)") +
    scale_y_discrete(labels = function(x) str_wrap(x, width = 18)) +
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
  p_right <- base_plot +
    coord_cartesian(xlim = c(2500000, 3000000)) +
    scale_x_continuous(
      labels = scales::label_number(
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
    plot_layout(widths = c(3, 1), guides = "collect") &
    theme(
      legend.position = "bottom",
      text = element_text(family = "DM Sans")
    )

  return(p_combined)
}

#' Save Figure 2 to PNG
save_fig2 <- function(freq_tab_df, output_path) {
  dir_path <- dirname(output_path)
  if (!dir.exists(dir_path)) {
    dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
  }

  p <- plot_fig2_defor_timing(freq_tab_df)

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