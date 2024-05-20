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
library(d3.format) # to install: devtools::install_github("dreamRs/d3.format")
library(tidyfast)
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

# set order of group ownership type
ownership_order <- c("Acknowledged ownership",
           "Suspected ownership based on civil society investigations",
           "Third-party suppliers")

defor_order <- c(
  "Deforestation for pulp after 2015",
  "Deforestation for pulp from 2001-2015",
  "Deforestation not for pulp",
  "Remaining forest"
  )

# calculate areas
freq_tab <- hti_conv_timing %>%
  filter(all == 1) %>%
  filter(conv_type == 2 | is.na(conv_type) | is.na(supplier_group)) %>%
  mutate(supplier_group = 
           case_when(
             linked_group == "APP" & ownership_class == "NGO-linked" ~ "SINAR MAS (NGO-LINKED)",
             linked_group == "APRIL" & ownership_class == "NGO-linked" ~ "ROYAL GOLDEN EAGLE / TANOTO (NGO-LINKED)",
             TRUE ~ supplier_group)) %>%
  group_by(ownership_class,linked_group,class) %>% 
  summarize(area_ha = sum(area_ha)) %>% 
  mutate(freq = area_ha / sum(area_ha)) %>%
  drop_na(ownership_class) %>%
  ungroup()

freq_tab

# create plot
defor_plot <- freq_tab %>% 
  as_tibble() %>%
  mutate(label_order = factor(ownership_class,rev(ownership_order))) %>%
  ggplot() +
  aes(y = label_order, x = area_ha, fill = factor(class,levels=defor_order)) +
  geom_bar(stat = "identity",position = position_stack(reverse = TRUE)) +
  theme_plot +
  ylab("Association with\nRGE or Sinar Mas\n") + 
  xlab("") +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_x_continuous(labels = d3_format(".3~s",suffix = " ha"),expand = c(0,0)) +
  guides(fill = guide_legend(nrow = 2)) +
  scale_fill_manual(values = cols,name ="Group",
                    breaks=ownership_order,labels=ownership_order) +
  theme(axis.title.y = element_text(angle = 90))

defor_plot

# export plot to png file
ggsave(defor_plot,file=paste0(wdir,"\\01_data\\02_out\\plots\\001_figures\\supplier_groups_defor_class_plot_rev5.png"), dpi=400, w=8, h=4,limitsize = FALSE)

