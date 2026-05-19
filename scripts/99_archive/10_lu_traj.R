## ---------------------------------------------------------
##
## Purpose of script: Clean Mapbiomas classes in sample points extracted using gee
##
## Author: Jason Benedict
##
## Date Created: 2021-02-08
## 
## ---------------------------------------------------------
##
## Notes:
##   
##
## ---------------------------------------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------------------------------------

## load packages
library(tidyverse)
library(readxl)
library(tidylog)
library(data.table)
library(janitor)
library(lubridate)
library(sf)
library(zoo)
library(ggsankey) # devtools::install_github("davidsjoberg/ggsankey")

## set working directory -------------------------------------
wdir <- "remote"
# setwd(wdir)

## Clean data ----------------

## data lookup table
lu_table <- read_csv(paste0(wdir,"\\01_data\\02_out\\gee\\data_lookup_table.csv"))

# TreeMap cleared area classes
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\gaveau\\"),pattern = "*gaveau_classes.csv",full.names= TRUE)

samples_ttm <- filenames %>% map_dfr(read_csv) %>% janitor::clean_names() 

lu_df <- sample_class_df %>% 
  mutate(year = year %>% as.integer()) %>% 
  rename(lu = class_code)

ttm_classes <- lu_table %>%
  filter(dataset == "gaveau landuse") %>%
  select(class_code=class,class_desc)


df_hti_test <- samples_hti %>%
  left_join(samples_ttm,by="sid") %>%
  as_tibble() %>%
  filter(ID == "H-0363")

df <- df_hti_test %>%
  #samples_ttm %>%
  filter(timberdeforestation_2022 != 0) %>%
  make_long(timberdeforestation_2000,timberdeforestation_2001,
            timberdeforestation_2002,timberdeforestation_2003,timberdeforestation_2004,timberdeforestation_2005,
            timberdeforestation_2006,timberdeforestation_2007,timberdeforestation_2008,timberdeforestation_2009,
            timberdeforestation_2010,timberdeforestation_2011,timberdeforestation_2012,timberdeforestation_2013,
            timberdeforestation_2014,timberdeforestation_2015,timberdeforestation_2016,timberdeforestation_2017,
            timberdeforestation_2018,timberdeforestation_2019,timberdeforestation_2019,timberdeforestation_2020,timberdeforestation_2021,timberdeforestation_2022) %>%
  mutate(x = str_replace(x,"timberdeforestation_", ""),next_x = str_replace(next_x,"timberdeforestation_",""),x = as.double(x),next_x = as.double(next_x)) 


df_reclassed <- df %>%
  left_join(ttm_classes,by=c("node"="class_code")) %>%
  left_join(ttm_classes,by=c("next_node"="class_code")) %>%
  select(x,next_x,node=class_desc.x,next_node=class_desc.y) %>%
  mutate(node = factor(node,levels=c("forest","non-forest","itp - industrial timber plantation"))) 


lu_traj <- ggplot(df_reclassed, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  geom_sankey(flow.alpha = .75,node.color = "gray20",width=0.0001) +
  scale_fill_manual(values=c("seagreen4","yellowgreen","orange3"),
                    labels= c("Forest","Non-forest","Pulp"))+ 
  theme_sankey(base_size = 12) +
  scale_x_continuous(expand=c(0,0),breaks=seq(2000,2022,by=2)) +
  labs(x = NULL) +
  theme(text = element_text(family = "DM Sans",colour="#3A484F"),
        #legend.position = "none",
        plot.title = element_text(hjust = .5,size=9),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(size = 9, color = "grey30")) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.75))) 

lu_traj


lu_alluvial_traj <- ggplot(df_reclassed, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  geom_alluvial(flow.alpha = .6) +
  geom_alluvial_text(size = 0, color = NA) +
  scale_fill_manual(values=c("seagreen4","yellowgreen","orange3"),
                    labels= c("Forest","Non-forest","Pulp"))+ 
  theme_alluvial(base_size = 18) +
  labs(x = NULL) +
  theme(text = element_text(family = "DM Sans",colour="#3A484F"),
        #legend.position = "none",
        plot.title = element_text(hjust = .5,size=9),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(size = 9, color = "grey30")) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.75)))

lu_alluvial_traj

## export to png
ggsave(lu_alluvial_traj,file=paste0("D:\\treemap_pulp_conversion_traj_plot.png"), dpi=300, w=18, h=9,type="cairo-png",limitsize = FALSE)

