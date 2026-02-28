## ---------------------------------------------------------
## 
## Project: Indonesia pulp deforestation
##
## Purpose of script: Clean 1km extracted variables and 
##                    separate variable files for 2017 and 2022
##                    for pulp expansion modelling
##
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Date Created: 2026-02-25
## 
## ---------------------------------------------------------
##
## Notes: 
##
## ---------------------------------------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------------------------------------

# load packages
library(tidyverse)
library(fs)
library(vroom)
library(sf)
library(skimr)

## set working directory -------------------------------------

wdir <- "remote"

## set data path ---------------------------------------------
data_path <- paste0(wdir,"\\01_data\\02_out\\gee\\pulp_expansion_modelling\\")

## read other relevant datasets ------------------------------
grid_1km_df <- read_csv(paste0(wdir,"\\01_data\\02_out\\samples\\kalisuma_1km_grid_pts.csv"))
grid_1km_sf <- read_sf(paste0(wdir,"\\01_data\\02_out\\samples\\kalisuma_1km_grid.shp"))

# kawasan hutan (2022)
kh_2022 <- read_sf(paste0(wdir,"\\01_data\\01_in\\klhk\\kh\\PNUNJUKKWSHUTAN_AR_250K_2021_06.shp")) %>% st_make_valid()

# merge function --------------------------------------------
merge_gee_batch <- function(folder_path, file_prefix) {
  message(paste("Merging batch:", file_prefix))
  
  # List files that start with the prefix (e.g., "mega_lon")
  files <- dir_ls(folder_path, regexp = paste0("/", file_prefix, ".*\\.csv$"))
  
  if (length(files) == 0) return(NULL)
  
  # use vroom for lightning-fast reading
  # id = "source_file" tracks which chunk the data came from
  batch_data <- vroom(files, id = "source_file", show_col_types = FALSE) %>%
    # remove any duplicate pixel_ids from overlapping GEE tiles
    distinct(pixel_id, .keep_all = TRUE)
  
  return(batch_data)
}

# merge datasets -------------------------------------------
climate_data  <- merge_gee_batch(data_path, "kalisuma_topo") %>% select(-source_file)
landuse_data  <- merge_gee_batch(data_path, "kalisuma_landuse") %>% select(-source_file) %>%
  mutate(across(everything(), ~replace(., . == -9999, 0))) # value can only be 1/0 - presence/absence 
river_dist_data  <- merge_gee_batch(data_path, "kalisuma_riverdist") %>% select(-source_file)
mill_dist_data  <- merge_gee_batch(data_path, "mill_dist") %>% select(-source_file)
aee_embed_data  <- merge_gee_batch(data_path, "kalisuma_aee_embed") %>% select(-source_file)

# ensure CRS match
# transform the points to match the KH projection.
if (st_crs(grid_1km_sf) != st_crs(kh_2022)) {
  kh_2022 <- st_transform(kh_2022, st_crs(grid_1km_sf))
}

# perform spatial join
grid_joined <- st_join(grid_1km_sf, kh_2022, join = st_intersects)

# clean up data
kh_df <- grid_joined %>%
  select(pixel_id, kh=fngskws) %>%
  mutate(kh = as.character(kh)) %>%
  st_drop_geometry() 

# clean and final merge ------------------------------------
final_combined <- grid_1km_df %>%
  select(pixel_id,lon,lat,kab_code,prov_code) %>%
  left_join(landuse_data, by = "pixel_id") %>%
  left_join(climate_data, by="pixel_id") %>%
  left_join(kh_df,by="pixel_id") %>%
  left_join(aee_embed_data,by="pixel_id") %>%
  left_join(river_dist_data, by = "pixel_id") %>%
  left_join(mill_dist_data, by = "pixel_id") 

# split data into the 2 required files ---------------------

# variables for 2017
var_2017 <- final_combined %>%
  select(-matches("2022")) 

# variables for 2022
var_2022 <- final_combined %>%
  select(-matches("2016|2017")) 

# export to csv files --------------------------------------
write_csv(var_2017,paste0(wdir,"\\01_data\\02_out\\tables\\pulp_exp_model_var_1km_2017.csv"))
write_csv(var_2022,paste0(wdir,"\\01_data\\02_out\\tables\\pulp_exp_model_var_1km_2022.csv"))
