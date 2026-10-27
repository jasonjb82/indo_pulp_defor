## ---------------------------------------------------------
##
## Project: Indonesia pulp deforestation
##
## Purpose of script: Generate plot of deforestation type, timing and remaining forest areas
##
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Date Created: 2023-05-08
##
## ---------------------------------------------------------
##
## Notes:
##
## ---------------------------------------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------------------------------------

### Load packages
library(stringr)
library(naniar)
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(sf)
library(scales)
library(dtplyr)
library(tidyfast)
library(showtext)
library(svglite) # device used by ggsave() for .svg output
library(khroma) # palettes for color blindness
library(ggbreak)

font_add_google(name = "DM Sans", family = "DM Sans")
showtext_auto()
showtext_opts(dpi = 400)

## set working directory -------------------------------------

wdir <- "remote"
data_dir <- "/01_data/"

## read data -------------------------------------------------
'%ni%' <- Negate('%in%') # filter out function

# read data on hti conversion timing
hti_conv_timing <- read_csv(paste0(
  wdir,
  data_dir,
  "02_out/tables/hti_grps_deforestation_timing.csv"
))

# plot -------------------------------------------------------

## set up theme
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
  axis.text.x.top = element_blank(),
  axis.line.x.top = element_blank(),
  axis.ticks.x.top = element_blank(),
  strip.text.x = element_text(size = 12, face = "bold", color = "grey30"),
  strip.background = element_rect(color = NA, fill = NA),
  legend.key.height = unit(12, "pt"),
  legend.key.width = unit(12, "pt"),
  legend.text = element_text(size = 9, colour = "grey30"),
  legend.title = element_blank(),
  legend.position = "bottom",
  legend.box = "horizontal",
  plot.margin = unit(c(0.5, 1.5, 0.5, 0.5), "cm")
)

options(crayon.enabled = FALSE)

# set order of group ownership type
ownership_order <- c(
  "Acknowledged ownership",
  "Suspected corporate control based on civil society investigations",
  "Third-party suppliers",
  "Not yet\nsupplying\nto mills"
)

defor_order <- c(
  "Deforestation for pulp after 2015",
  "Deforestation for pulp during 2001-2015",
  "Deforestation not for pulp",
  "Remaining forest"
)

# calculate areas
freq_tab <- hti_conv_timing %>%
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
  summarize(area_ha = sum(area_ha)) %>%
  mutate(freq = area_ha / sum(area_ha)) %>%
  drop_na(ownership_class) %>%
  ungroup()


# create plot
defor_plot <- freq_tab %>%
  as_tibble() %>%
  mutate(label_order = factor(ownership_class, rev(ownership_order))) %>%
  ggplot() +
  aes(
    y = label_order,
    x = area_ha,
    fill = factor(class, levels = defor_order)
  ) +
  # Stroke each segment in its own fill colour. Without it, SVG renderers
  # anti-alias the two sides of every boundary between stacked segments
  # independently and a hairline of background shows through; the PNG device
  # hides this but the vector export does not.
  geom_bar(
    stat = "identity",
    position = position_stack(reverse = TRUE),
    aes(colour = after_scale(fill)),
    linewidth = 0.3
  ) +
  theme_plot +
  ylab("Association with RGE or Sinar Mas") +
  xlab("") +
  # Wrap wider so the long "Suspected corporate control..." label resolves into
  # fewer, longer lines; ggplot then allocates more room to the tick labels.
  scale_y_discrete(labels = function(x) str_wrap(x, width = 26)) +
  scale_x_continuous(
    # accuracy = 0.1 is needed for the 2,500,000 break: the default rounds it
    # to "2M", which misreads as the bar starting half a million hectares
    # lower than it does.
    labels = scales::label_number(
      accuracy = 0.1,
      scale_cut = c("k" = 1e3, "M" = 1e6),
      suffix = " ha"
    ),
    expand = c(0, 0),
    breaks = c(0, 500000, 1000000, 1500000, 2000000, 2500000)
  ) +
  guides(fill = guide_legend(nrow = 2)) +
  scale_fill_manual(
    values = c("#CC79A7", "#0072B2", "#F0E442", "#009E73"),
    name = "Group",
    breaks = defor_order,
    labels = defor_order
  ) +
  theme(axis.title.y = element_text(angle = 90))


defor_broken_plot <- defor_plot +
  scale_x_break(c(1000000, 2500000), space = 0.5)

# showtext draws every glyph as an outline, which leaves the SVG with no editable
# text and inflates the file. Switch it off for the vector export so svglite
# writes real <text> elements carrying the font family name, then switch it back
# on for any raster output that follows.
showtext_auto(FALSE)

# export plot to svg file
ggsave(
  defor_broken_plot,
  file = paste0(
    wdir,
    data_dir,
    "04_results/figures/f2_supplier_groups_defor_class_plot.svg"
  ),
  dpi = 400,
  w = 9,
  h = 4,
  limitsize = FALSE,
  units = "in"
)
showtext_auto()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# export underlying data for Figure 2 --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Plot-ready values behind Figure 2, supplied to the journal alongside the
# vector figure so the figure can be rebuilt without re-running the analysis.
# Comment this block out once the files have been provided.
fig_data_dir <- paste0(wdir, data_dir, "04_results/figure_data")
dir.create(fig_data_dir, showWarnings = FALSE, recursive = TRUE)

# Deforestation area by supplier ownership class and deforestation timing
# class: one row per stacked bar segment.
write_csv(
  as_tibble(freq_tab),
  file.path(fig_data_dir, "fig2_deforestation_by_ownership_and_class.csv")
)
