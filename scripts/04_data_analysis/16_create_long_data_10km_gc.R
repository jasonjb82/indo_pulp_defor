## ---------------------------------------------------------
## 
## Project: Indonesia pulp deforestation
##
## Purpose of script: Create long table to use for regressions (deforestation & transport costs)
##
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Date Created: 2025-03-18
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
library(sf)
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

# deforestation and forest cover (hansen)
gfw_df <- read_csv(paste0(wdir,"\\01_data\\02_out\\gee\\explore_deforestation_Indonesia.csv"))

# grid centroids covering kalimantan and sumatera
grid_10km_sf <- read_sf(paste0(wdir,"\\01_data\\01_in\\ucsb\\kalisuma_10km_grid_centroids_proj.shp"))

# pulp non-forest conversion
pulp_nonfor_df <- read_csv(paste0(wdir,"\\01_data\\02_out\\gee\\pulp_annual_defor_non-forest_kalisuma.csv"))

# pulp forest conversion
pulp_for_df <- read_csv(paste0(wdir,"\\01_data\\02_out\\gee\\pulp_annual_defor_forest_kalisuma.csv"))

## clean data ------------------------------------------------

# convert to areas and subset to selected attributes
gfw_areas_long <- gfw_df %>%
  select(pixel_id,year,annual_forest_loss_pixel_count,treecover_annual) %>%
  mutate(annual_forest_loss_ha = annual_forest_loss_pixel_count * 900/10000,
         annual_treecover_ha = treecover_annual * 900/10000) %>%
  select(pixel_id,year,annual_forest_loss_ha,annual_treecover_ha) %>%
  print()

# pulp non-forest conversion
pulp_nonfor_long <- pulp_nonfor_df %>%
  left_join(grid_10km_sf,by="pixel_id") %>%
  select(-`system:index`,-constant,-geometry,-.geo) %>%
  pivot_longer(cols = -c(pulau,kode_pulau,pixel_id),
               names_to = 'year',
               values_to = 'area_ha') %>%
  filter(area_ha != "0") %>%
  mutate(year = str_replace(year,"deforestation_", ""),year = as.double(year)) %>%
  group_by(pixel_id,year) %>%
  summarize(area_ha = sum(area_ha)) %>%
  print()

# pulp forest conversion
pulp_for_long <- pulp_for_df %>%
  left_join(grid_10km_sf,by="pixel_id") %>%
  select(-`system:index`,-constant,-geometry,-.geo) %>%
  pivot_longer(cols = -c(pulau,kode_pulau,pixel_id),
               names_to = 'year',
               values_to = 'area_ha') %>%
  filter(area_ha != "0") %>%
  mutate(year = str_replace(year,"deforestation_", ""),year = as.double(year)) %>%
  group_by(pixel_id,year) %>%
  summarize(area_ha = sum(area_ha)) %>%
  print()

# merge data
pulp_fnf_long <- pulp_for_long %>%
  mutate(type = "pulp_forest_ha") %>%
  bind_rows(pulp_nonfor_long) %>%
  mutate(type = ifelse(is.na(type),"pulp_non_forest_ha",type)) %>%
  pivot_wider(names_from=type,values_from=area_ha,values_fill = 0) %>%
  arrange(pixel_id,-year) %>%
  print()

# data check
total_annual_areas <- pulp_for_long %>%
  mutate(type = "pulp_forest_ha") %>%
  bind_rows(pulp_nonfor_long) %>%
  mutate(type = ifelse(is.na(type),"pulp_non_forest_ha",type)) %>%
  left_join(grid_10km_sf,by="pixel_id") %>%
  filter(area_ha != "0") %>%
  group_by(year,pulau,type) %>%
  summarize(area_ha = sum(area_ha)) %>%
  print()

# order for plotting
type_order <- c("pulp_non_forest_ha","pulp_forest_ha")

# plot to visually check areas correspond to TTM data
total_annual_areas %>%
filter(pulau == "KALIMANTAN") %>%
ggplot() +
  aes(x=year,y=area_ha,fill=factor(type,levels = type_order)) +
  geom_col()

# create full pixel table with all years
pixel_id_tbl <- grid_10km_sf %>%
  st_drop_geometry() %>%
  select(pixel_id) %>%
  group_by(pixel_id) %>%
  summarise(start = min(2001),
            end = max(2022)) %>%
  mutate(year = Map(seq, start, end)) %>%
  unnest(cols =year) %>%
  select(pixel_id,year) %>%
  print()

# create full table
tbl_long <- pixel_id_tbl %>%
  left_join(pulp_fnf_long,by=c("pixel_id","year")) %>%
  left_join(gfw_areas_long,by=c("pixel_id","year")) %>%
  mutate(across(everything(), ~ ifelse(is.na(.), 0, .))) %>%
  print()

# export data to csv
write_csv(tbl_long,paste0(wdir,""))
  


                        