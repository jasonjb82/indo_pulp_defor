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

# centroids to mills (sumatera)
suma_cm_df <- read_csv(paste0(wdir,"\\01_data\\02_out\\tables\\sumatera_centroids_mills_odmatrix.csv"))

# centroids to sea transport locations (kalimantan)
kali_cp_df <- read_csv(paste0(wdir,"\\01_data\\02_out\\tables\\kalimantan_centroids_kali_pts_odmatrix.csv"))

# sumatera ports to mills
suma_pm_df <- read_csv(paste0(wdir,"\\01_data\\02_out\\tables\\sumatera_suma_pts_mills_odmatrix.csv"))

# kalimantan - sumatera ports
kali_suma_pts_df <- read_csv(paste0(wdir,"\\01_data\\02_out\\tables\\idn_kali_suma_ports_od.csv"))

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

kali_cp_clean_df <- kali_cp_df %>%
  mutate(Name = str_replace(Name," - "," = ")) %>%
  separate(Name,into=c("orig","dest"),sep= " = ") %>%
  mutate(dist_km = Total_Length/1000) %>%
  select(orig,dest,dist_km) %>%
  print()

kali_suma_pts_clean_df <- kali_suma_pts_df %>%
  mutate(dist_km = distance) %>%
  select(orig=port_start,dest=port_end,dist_km) %>%
  print()

suma_pm_clean_df <- suma_pm_df %>%
  mutate(Name = str_replace(Name," - "," = ")) %>%
  separate(Name,into=c("orig","dest"),sep= " = ") %>%
  mutate(dist_km = Total_Length/1000) %>%
  select(orig,dest,dist_km) %>%
  print()

# get grid cells in Sumatra
suma_grid_10km_sf <- grid_10km_sf %>%
  filter(pulau == "SUMATERA") %>%
  st_drop_geometry()

kali_grid_10km_sf <- grid_10km_sf %>%
  filter(pulau == "KALIMANTAN") %>%
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

kali_links <- kali_cp_clean_df %>%
  select(orig) %>%
  mutate(orig = as.integer(orig),linked=1) %>%
  distinct() 

kali_links_check <- kali_links %>%
  full_join(kali_grid_10km_sf,by=c("orig"="pixel_id")) %>%
  mutate(linked = ifelse(is.na(linked),0,1)) %>%
  print()

# summarize numbers
suma_links_check %>%
  summarise(
    n_linked = sum(linked == 1),
    n_unlinked = sum(linked == 0)
  ) %>%
  mutate(shr_unlinked = n_unlinked/(n_unlinked+n_linked)*100) %>%
  print()

kali_links_check %>%
  summarise(
    n_linked = sum(linked == 1),
    n_unlinked = sum(linked == 0)
  ) %>%
  mutate(shr_unlinked = n_unlinked/(n_unlinked+n_linked)*100) %>%
  print()

# write_sf(suma_links_check,"D:/suma_links_r2.shp")
# write_sf(kali_links_check,"D:/kali_links_r7.shp")

## merge data -------------------------------------------------

kali_mill_dist <- kali_cp_clean_df %>%
  group_by(orig) %>%
  filter(dist_km == min(dist_km)) %>%
  left_join(kali_suma_pts_clean_df,by="dest") %>%
  left_join(suma_pm_clean_df,by=c("orig.y"="orig")) %>%
  mutate(dist_land_km = dist_km.x+dist_km,dist_sea_km=dist_km.y) %>%
  select(id=orig.x,mill=dest.y,dist_land_km,dist_sea_km) %>%
  print()

suma_mill_dist <- suma_cm_clean_df %>%
  select(id=orig,mill=dest,dist_land_km=dist_km) %>%
  print()

cm_dist_df <- kali_mill_dist %>%
  bind_rows(suma_mill_dist) %>%
  print()

## combine with cost numbers ----------------------------------