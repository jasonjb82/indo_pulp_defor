## ---------------------------------------------------------
## 
## Project: Indonesia pulp deforestation
##
## Purpose of script: Clean od-matrix output files
##
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Date Created: 2025-05-01
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

# port to port distances
suma_cm_df <- read_csv(paste0(wdir,"\\01_data\\02_out\\tables\\sumatera_centroids_mills_odmatrix.csv"))

# grid centroids covering kalimantan and sumatera
grid_10km_sf <- read_sf(paste0(wdir,"\\01_data\\01_in\\ucsb\\kalisuma_10km_grid_centroids_proj.shp"))

## clean data ------------------------------------------------

# get proper columns
suma_cm_clean_df <- suma_cm_df %>%
  mutate(Name = str_replace(Name," - "," = ")) %>%
  separate(Name,into=c("orig","dest"),sep= " = ") %>%
  mutate(dist_km = Total_Length/1000) %>%
  select(orig,dest,dist_km) %>%
  print()

# get grid cells in Sumatra
suma_grid_10km_sf <- grid_10km_sf %>%
  filter(pulau == "SUMATERA") %>%
  st_drop_geometry()


## check data ------------------------------------------------

suma_links <- suma_cm_clean_df %>%
  select(orig) %>%
  mutate(orig = as.integer(orig),linked=1) %>%
  distinct() 

suma_links_check <- suma_links %>%
  full_join(suma_grid_10km_sf,by=c("orig"="pixel_id")) %>%
  mutate(linked = ifelse(is.na(linked),0,1)) %>%
  print()

write_sf(suma_links_check,"D:/suma_links.shp")
