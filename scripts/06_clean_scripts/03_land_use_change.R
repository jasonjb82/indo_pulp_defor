## ---------------------------------------------------------
## 
## Project: Indonesia pulp deforestation
##
## Purpose of script: Create plots of annual land cover change within HTI concessions
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
library(tidyverse)
library(tidylog)
library(janitor)
library(lubridate)
library(scales)
library(dtplyr)
library(d3.format) # to install: devtools::install_github("dreamRs/d3.format")
library(tidyfast)
library(concordance)
library(extrafont)
library(showtext)
library(khroma) # palettes for color blindness

'%ni%' <- Negate('%in%') # filter out function

## set working directory -------------------------------------

wdir <- "remote"

## read data -------------------------------------------------

# load color palette
source("scripts\\001_misc\\001_color_palettes.R")

# concession annual land use changes
hti_gav_annual_lc <- read_csv(paste0(wdir,"\\01_data\\02_out\\tables\\hti_land_use_change_areas.csv"))

## Plotting -------------------------------------------------

# set up theme
theme_plot <- theme(text = element_text(family = "DM Sans",colour="#3A484F"),
                    panel.background = element_rect(colour=NA,fill=NA),
                    panel.grid.minor = element_blank(),
                    panel.grid.major.y = element_line(color="grey70",linetype="dashed",linewidth=0.35),
                    plot.title = element_text(hjust = 0.5, face="bold"),
                    axis.line.x = element_line(),
                    axis.ticks.x = element_blank(),
                    axis.ticks.y = element_blank(),
                    panel.spacing = unit(2, "lines"),
                    axis.text.x = element_text(size = 9, color = "grey30",angle = 45,hjust=1),
                    axis.text.y = element_text(size = 9, color = "grey30"),
                    axis.title.x = element_text(size = 10, color = "grey30"),
                    axis.title.y = element_text(size = 10, color = "grey30"),
                    strip.text.x = element_text(size = 12, face = "bold",color="grey30"),
                    strip.background = element_rect(color=NA, fill=NA),
                    legend.key = element_rect(linewidth = 12, fill = "white", colour = NA),
                    legend.key.height = unit(10, "pt"),
                    legend.key.width = unit(10, "pt"),
                    legend.text = element_text(size = 8,colour="grey30"),
                    legend.title = element_blank(),
                    legend.position="bottom",
                    legend.box="horizontal",
                    plot.margin=unit(c(0.5,1.5,0.5,0.5),"cm"))

# get list of concessions
concessions <- hti_gav_annual_lc %>%
  filter(all == 1) %>%
  distinct(supplier_label) %>%
  pull(supplier_label) %>%
  print()

hti_plots = list()

# create plots by loop and export as png files
for(concession_ in concessions) {
  
  filtered_df <- hti_gav_annual_lc %>% filter(supplier_label == concession_) %>%
    mutate(class_desc = ordered(class_desc, levels = c("Forest", "Non-forest", "Cleared for pulp")))
  
  # get actual classes in plot
  non_zero <- filtered_df %>% filter(area_ha > 0) %>% pull(class_desc) %>% unique() %>% sort()
  
  hti_plots[[concession_]] <- ggplot(filtered_df,aes(year,area_ha))+
    geom_area(aes(fill= as.factor(class_desc)), position = position_stack(reverse = F)) +
    scale_x_continuous(expand=c(0,0),breaks=seq(2001,2022,by=1),limits = c(2001,2022)) +
    scale_y_continuous(labels = d3_format(".2~s",suffix = "ha"),expand = c(0,0)) +
    ylab("") +
    xlab("") +
    ggtitle(paste0(str_sub(concession_, end=-10))) +
    scale_fill_manual(values=c("Forest"="#009E73","Non-forest"="#F0E442","Cleared for pulp"="#CC79A7"),
                      breaks = non_zero)+
    guides(fill = guide_legend(nrow = 1),color = guide_legend(nrow=1),shape = guide_legend(nrow=2),keyheight = 10) +
    theme_plot
  
  if (!all(is.na(filtered_df$zdc_year))) {
    hti_plots[[concession_]] <- hti_plots[[concession_]] + 
      geom_vline(aes(xintercept = zdc_year, color = "Earliest ZDC year\nof downstream mill"),size=0.5,na.rm=T) +
      scale_color_manual(values = c("License\nyear"="#000000","Earliest ZDC year\nof downstream mill"= "#000000")) +
      guides(fill = guide_legend(nrow = 1,order=1),color = guide_legend(nrow=1,order=2),shape = guide_legend(nrow=2,order=3),keyheight = 10)
  }
  
  
  if (!all(filtered_df$license_year < 2001 | filtered_df$license_year >= 2022)) {
    hti_plots[[concession_]] <- hti_plots[[concession_]] + 
      geom_vline(aes(xintercept = license_year, color = "License\nyear"),size=0.5,linetype="dashed",na.rm=T) +
      scale_color_manual(values = c("License\nyear"="#000000","Earliest ZDC year\nof downstream mill"= "#000000")) +
      guides(fill = guide_legend(nrow = 1,order=1),color = guide_legend(nrow=1,order=2),shape = guide_legend(nrow=2,order=3),keyheight = 10)
  }
  
  print(hti_plots[[concession_]])
  ggsave(hti_plots[[concession_]], file=paste0(wdir,"\\01_data\\02_out\\plots\\001_figures\\lu_traj_plots_all\\",gsub(" ","_",concession_),"_TreeMap_AnnualChanges.png"), dpi=400, w=10, h=6,device="png")

}
