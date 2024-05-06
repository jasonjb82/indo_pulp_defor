## ---------------------------------------------------------
## 
## Project: Indonesia pulp deforestation
##
## Purpose of script: Generate plot of deforestation timing
##
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Date Created: 2023-05-08
## 
## ---------------------------------------------------------
##
## Notes: Input datasets
##        1) HTI concessions (boundaries) and concession start year from KLHK
##        2) Gaveau landuse change - pulp deforestation (2000 - 2022) from TreeMap
##        4) GFC Hansen deforestation year (2001 - 2022) - earthenginepartners.appspot.com/science-2013-global-forest
##        5) Peat (MoA Indonesia, 2019) & Margono forest mask (TreeMap version)
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
library(d3.format)
library(tidyfast)
library(patchwork)
library(showtext)
library(khroma) # palettes for color blindness

## set working directory -------------------------------------

wdir <- "remote"

## read data -------------------------------------------------
'%ni%' <- Negate('%in%') # filter out function

# load color palette
source("scripts\\001_misc\\001_color_palettes.R")

# read data on hti conversion timing
hti_conv_timing <- read_csv(paste0(wdir,"\\01_data\\02_out\\tables\\hti_grps_deforestation_timing.csv"))

# plot -------------------------------------------------------

## set up theme
theme_plot <- theme(text = element_text(family = "DM Sans",colour="#3A484F"),
                     panel.background = element_rect(colour=NA,fill=NA),
                     panel.grid.minor = element_blank(),
                     panel.grid.major.x= element_line(color="grey70",linetype="dashed",size=0.35),
                     plot.title = element_text(hjust = 0,size=11,face="bold"),
                     axis.line.x = element_line(),
                     axis.ticks.x = element_blank(),
                     axis.ticks.y = element_blank(),
                     panel.spacing = unit(2, "lines"),
                     axis.text.x = element_text(size = 9, color = "grey30"),
                     axis.text.y = element_text(size = 9, color = "grey30"),
                     axis.title.x = element_text(size = 10, color = "grey30"),
                     axis.title.y = element_text(size = 10, color = "grey30"),
                     strip.text.x = element_text(size = 12, face = "bold",color="grey30"),
                     strip.background = element_rect(color=NA, fill=NA),
                     legend.key.height = unit(12, "pt"),
                     legend.key.width = unit(12, "pt"),
                     legend.text = element_text(size = 9,colour="grey30"),
                     legend.title = element_blank(),
                     legend.position="bottom",
                     legend.box="horizontal",
                     plot.margin=unit(c(0.5,1.5,0.5,0.5),"cm"))

options(crayon.enabled = FALSE)

## Deforestation timing plot

order <- c("SINAR MAS",
           "SINAR MAS (NGO-LINKED)",
           "ROYAL GOLDEN EAGLE / TANOTO",
           "ROYAL GOLDEN EAGLE / TANOTO (NGO-LINKED)",
           "SUMITOMO",
           "KORINDO",
           "DJARUM",
           "MARUBENI",
           "GOVERNMENT",
           "ADR",
           "OTHER")

order <- c("Acknowledged ownership",
           "Suspected ownership based on civil society investigations",
           "Third-party suppliers")

plot_order_deft_pulp <- c(
  "Deforestation for pulp after 2015",
  "Deforestation for pulp from 2001-2015",
  "Deforestation not for pulp",
  "Remaining forest"
  )

## Generate frequency table by group
group_var <- "ownership_class" # Generally either island, supplier_group or supplier_label
mill_var <- "all" # Generally either april,app,marubeni or all (all concessions)

freq_tab <- hti_conv_timing %>%
  filter(!!sym(mill_var) == 1) %>%
  filter(conv_type == 2 | is.na(conv_type) | is.na(supplier_group)) %>%
  mutate(supplier_group = 
           case_when(
             linked_group == "APP" & ownership_class == "NGO-linked" ~ "SINAR MAS (NGO-LINKED)",
             linked_group == "APRIL" & ownership_class == "NGO-linked" ~ "ROYAL GOLDEN EAGLE / TANOTO (NGO-LINKED)",
             TRUE ~ supplier_group)) %>%
  #filter(island == "kalimantan") %>% # filter to island if required
  group_by(.data[[group_var]],linked_group,class) %>% 
  summarize(area_ha = sum(area_ha)) %>% 
  mutate(freq = area_ha / sum(area_ha)) %>%
  drop_na(ownership_class) %>%
  ungroup()

freq_tab

## plot frequencies
defor_plot <- freq_tab %>% 
  as_tibble() %>%
  mutate(label_order = factor(!!sym(group_var),rev(order))) %>%
  ggplot() +
  aes(y = label_order, x = area_ha, fill = factor(class,levels=plot_order_deft_pulp)) +
  geom_bar(stat = "identity",position = position_stack(reverse = TRUE)) +
  theme_plot +
  ylab("Association with\nRGE or Sinar Mas\n") + 
  xlab("") +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_x_continuous(labels = d3_format(".3~s",suffix = " ha"),expand = c(0,0)) +
  guides(fill = guide_legend(nrow = 2)) +
  scale_fill_manual(values = cols,name ="Group",
                    breaks=plot_order_deft_pulp,labels=plot_order_deft_pulp) +
  theme(axis.title.y = element_text(angle = 90))

defor_plot

ggsave(defor_plot,file=paste0(wdir,"\\01_data\\02_out\\plots\\001_figures\\supplier_groups_defor_class_plot_rev5.png"), dpi=400, w=8, h=4,limitsize = FALSE)

