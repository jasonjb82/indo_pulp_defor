## ---------------------------------------------------------
## 
## Project: Indonesia pulp deforestation
##
## Purpose of script: Create 1km grid points for pulp expansion modelling
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

### Load packages
library(sf)
library(dplyr)
library(tidyverse)

## set working directory -------------------------------------

wdir <- "remote"

## read data -------------------------------------------------

# choose projection: Cylindrical Equal Area
indonesian_crs <- "+proj=cea +lon_0=115.0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

# islands
island <- read_sf(paste0(wdir,"\\01_data\\01_in\\klhk\\KLHK_Kelompok_Pulau_20210308.shp")) %>%
  st_transform(., indonesian_crs)

aoi <- read_sf(paste0(wdir,"/01_data/01_in/big/idn_kabupaten_big.shp")) %>%
  mutate(island = str_sub(prov_code, 1, 1)) %>%
  mutate(
    island = case_when(
      island == 1 ~ "SUMATRA", island == 2 ~ "RIAU ARCHIPELAGO",
      island == 3 ~ "JAVA", island == 5 ~ "BALI AND NUSA TENGGARA",
      island == 6 ~ "KALIMANTAN", island == 7 ~ "SULAWESI",
      island == 8 ~ "MALUKU", island == 9 ~ "PAPUA"
    )
  ) %>%
  filter(island %in% c("KALIMANTAN","SUMATRA","RIAU ARCHIPELAGO")) 

# project to a metric crs 
aoi_metric <- st_transform(aoi, indonesian_crs)

# generate the 1km x 1km grid
grid_points <- st_make_grid(aoi_metric, 
                            cellsize = c(1000, 1000), 
                            what = "centers")

# clip to the island shapes
grid_clipped <- grid_points[aoi_metric,]

# convert sfc to sf and add pixel_id
grid_sf <- st_sf(grid_clipped) %>%
  st_transform(crs = 4326) %>%
  mutate(pixel_id = row_number(),
    lon = st_coordinates(.)[,1],
    lat = st_coordinates(.)[,2]
  )

# adding kabupaten and province code to grids
grid_sf_proj <- st_transform(grid_sf, indonesian_crs)

grid_kab_sf <- st_sf(geometry = grid_sf_proj) %>%
  st_join(aoi_metric["kab_code"], join = st_intersects, left = FALSE, largest = TRUE)

grid_adm_df <- grid_kab_sf %>%
  st_drop_geometry() %>%
  left_join(aoi,by="kab_code") %>%
  select(-geometry) %>%
  print()

# export files
write_csv(grid_adm_df,paste0(wdir,"\\01_data\\02_out\\samples\\kalisuma_1km_grid_pts.csv"))
write_sf(grid_adm_df,paste0(wdir,"\\01_data\\02_out\\samples\\kalisuma_1km_grid.shp"))

