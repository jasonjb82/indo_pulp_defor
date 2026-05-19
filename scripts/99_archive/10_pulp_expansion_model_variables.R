## ---------------------------------------------------------
## 
## Project: Indonesia pulp deforestation
##
## Purpose of script: Clean and prepare variables for pulp expansion modelling
##
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Date Created: 2026-02-17
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

'%ni%' <- Negate('%in%') # filter out function

## set working directory -------------------------------------

wdir <- "remote"

## read data -------------------------------------------------

# grid centroids covering kalimantan and sumatera
grid_10km_sf <- read_sf(paste0(wdir,"\\01_data\\01_in\\ucsb\\kalisuma_10km_grid_centroids_proj.shp"))

# landuse variables
lu_df <- read_csv(paste0(wdir,"\\01_data\\02_out\\gee\\pulp_expansion_modelling\\kalisuma_for_pulp_peat_hti_op_pa.csv")) %>%
   replace(is.na(.), 0)

# climate, topo and soil
bio_df <- read_csv(paste0(wdir,"\\01_data\\02_out\\gee\\pulp_expansion_modelling\\kalisuma_topo_climate_soil.csv"))

# alpha earth embeddings
aee_df <- read_csv(paste0(wdir,"\\01_data\\02_out\\gee\\pulp_expansion_modelling\\kalisuma_AEE_emb_2017_2022.csv")) %>%
  clean_names()

# river distance
river_df <- read_csv(paste0(wdir,"\\01_data\\02_out\\gee\\pulp_expansion_modelling\\kalisuma_riverdist.csv"))

# closest mill cost
#mill_cost_df <- read_csv(paste0(wdir,"\\01_data\\02_out\\tables\\centroids_mills_cost.csv")) 
mill_dist_2017_df <- read_csv(paste0(wdir,"\\01_data\\02_out\\tables\\centroids_mills_dist_2017.csv")) 
mill_dist_2022_df <- read_csv(paste0(wdir,"\\01_data\\02_out\\tables\\centroids_mills_dist_2022.csv"))

## extract additional data ------------------------------------

# kawasan hutan (2022)
kh_2022 <- read_sf(paste0(wdir,"\\01_data\\01_in\\klhk\\kh\\PNUNJUKKWSHUTAN_AR_250K_2021_06.shp")) %>% st_make_valid()

# ensure CRS match
# transform the points to match the KH projection.
if (st_crs(grid_10km_sf) != st_crs(kh_2022)) {
  kh_2022 <- st_transform(kh_2022, st_crs(grid_10km_sf))
}

# perform spatial join
grid_joined <- st_join(grid_10km_sf, kh_2022, join = st_intersects)

# clean up data
kh_df <- grid_joined %>%
  select(pixel_id, kh=fngskws) %>%
  mutate(kh = as.character(kh)) %>%
  st_drop_geometry() 

mill_dist_2017_clean <- mill_dist_2017_df %>%
  mutate(mill_dist_km_2017=dist_land_kalimantan_km + dist_land_sumatera_km + dist_sea_km) %>%
  select(pixel_id=id,mill_dist_km_2017) 

mill_dist_2022_clean <- mill_dist_2022_df %>%
  mutate(mill_dist_km_2022=dist_land_kalimantan_km + dist_land_sumatera_km + dist_sea_km) %>%
  select(pixel_id=id,mill_dist_km_2022) 

mill_dist_df <- mill_dist_2017_clean %>%
  left_join(mill_dist_2022_clean,by=c("pixel_id")) %>%
  print()

## clean data -------------------------------------------------

full_df <- lu_df %>%
  left_join(bio_df,by= "pixel_id") %>%
  left_join(aee_df,by="pixel_id") %>%
  left_join(kh_df,by="pixel_id") %>%
  left_join(river_df,by="pixel_id") %>%
  left_join(mill_dist_df,by="pixel_id")

# separate according to time period ----------------------------

# variables for 2017
var_2017 <- full_df %>%
  select(-matches("2022")) 

# variables for 2022
var_2022 <- full_df %>%
  select(-matches("2016|2017")) 


## write to csv --------------------------------------------------

write_csv(var_2017,paste0(wdir,"\\01_data\\02_out\\tables\\pulp_exp_model_var_2017.csv"))
write_csv(var_2022,paste0(wdir,"\\01_data\\02_out\\tables\\pulp_exp_model_var_2022.csv"))
