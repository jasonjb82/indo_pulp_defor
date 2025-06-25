## ---------------------------------------------------------
## 
## Project: Indonesia pulp deforestation
##
## Purpose of script: Get GAEZ classes for HTI and grid centroids
##
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Date Created: 2025-06-06
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
library(dlookr)
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

# GAEZ classes in HTIs
hti_gaez_df <- read_csv(paste0(wdir,"\\01_data\\02_out\\gee\\gaez_classes_by_conc.csv"))

# GAEZ classes in grids
grid_gaez_df <- read_csv(paste0(wdir,"\\01_data\\02_out\\gee\\gaez_classes_by_grid_10km.csv"))

# GAEZ classes
gaez_class <- tibble::tribble(
  ~class_desc,~class_code,
"Tropics, lowland; semi-arid","class_1",
"Tropics, lowland; sub-humid","class_2",
"Tropics, lowland; humid","class_3",
"Tropics, highland; semi-arid","class_4",
"Tropics, highland; sub-humid","class_5",
"Tropics, highland; humid","class_6",
"Sub-tropics, warm; semi-arid","class_7",
"Sub-tropics, warm; sub-humid","class_8",
"Sub-tropics, warm; humid" ,"class_9",
"Sub-tropics, moderately cool; semi-arid","class_10",
"Sub-tropics, moderately cool; sub-humid","class_11",
"Sub-tropics, moderately cool; humid","class_12",
"Sub-tropics, cool; semi-arid","class_13",
"Sub-tropics, cool; sub-humid","class_14",
"Sub-tropics, cool; humid","class_15",
"Temperate, moderate; dry","class_16",
"Temperate, moderate; moist","class_17",
"Temperate, moderate; wet","class_18",
"Temperate, cool; dry","class_19",
"Temperate, cool; moist","class_20",
"Temperate, cool; wet","class_21",
"Cold, no permafrost; dry","class_22",
"Cold, no permafrost; moist","class_23",
"Cold, no permafrost; wet","class_24",
"Dominantly very steep terrain","class_25",
"Land with severe soil/terrain limitations","class_26",
"Land with ample irrigated soils" ,"class_27",
"Dominantly hydromorphic soils","class_28",
"Desert/Arid climate","class_29",
"Boreal/Cold climate","class_30",
"Arctic/Very cold climate","class_31",
"Dominantly built-up land","class_32",
"Dominantly water","class_33") %>%
  select(class_code,class_desc)

# clean data -------------------------------------------------------------------

hti_gaez_wide_df <- hti_gaez_df %>%
  pivot_longer(-ID,names_to="class",values_to="area") %>%
  drop_na(area) %>%
  mutate(area_ha = area*0.0001) %>%
  select(-area) %>%
  pivot_wider(
    names_from = class,
    values_from = area_ha,
    names_prefix = "class_",
    values_fill = 0
  ) %>%
  rowwise() %>%
  mutate(total_area_ha = sum(c_across(starts_with("class_")), na.rm = TRUE)) %>%
  ungroup()

grid_gaez_wide_df <- grid_gaez_df %>%
  pivot_longer(-pixel_id,names_to="class",values_to="area") %>%
  drop_na(area) %>%
  mutate(area_ha = area*0.0001) %>%
  select(-area) %>%
  pivot_wider(
    names_from = class,
    values_from = area_ha,
    names_prefix = "class_",
    values_fill = 0
  ) %>%
  rowwise() %>%
  mutate(total_area_ha = sum(c_across(starts_with("class_")), na.rm = TRUE)) %>%
  ungroup()

grid_gaez_wide_pct <- grid_gaez_wide_df %>%
  mutate(across(
    starts_with("class_") & !ends_with("_share") & !ends_with("_pct") & !matches("total_area_ha"),
    ~ round((.x / total_area_ha) * 100, 1),
    .names = "{.col}_pct"
  )) %>%
  select(pixel_id, ends_with("_pct"))


# export files to csv ----------------------------------------------------------

# GAZ areas in HTI
write_csv(hti_gaez_wide_df,paste0(wdir,("\\01_data\\02_out\\tables\\gaez_hti_areas.csv")))

# GAEZ areas in 10km grids
write_csv(grid_gaez_wide_pct,paste0(wdir,("\\01_data\\02_out\\tables\\gaez_grid_share.csv")))

# GAEZ classes
write_csv(gaez_class,paste0(wdir,("\\01_data\\02_out\\tables\\gaez_class_codes_desc.csv")))

