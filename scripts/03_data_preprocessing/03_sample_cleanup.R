## ---------------------------------------------------------
##
## Purpose of script: Get table of samples with HTI IDs
##
## Author: Jason Benedict
##
## Date Created: 2021-03-29
## 
## ---------------------------------------------------------
##
## Notes: Input data
##
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
library(scales)
library(aws.s3)
library(showtext)
library(khroma) # palettes for color blindness
library(nngeo)


## credentials ----------------------------------------------

aws.signature::use_credentials()
bucket <- "trase-storage"
Sys.setenv("AWS_DEFAULT_REGION" = "eu-west-1")

## set working directory -------------------------------------

wdir <- "remote"

## read data -------------------------------------------------

# sample file
samples <- read_sf(paste0(wdir,"\\01_data\\02_out\\samples\\samples_hti.shp"))

# hti concessions
hti <- read_sf(paste0(wdir,"\\01_data\\01_in\\klhk\\IUPHHK_HTI_TRASE_20230314_proj.shp"))

# islands
island <- read_sf(paste0(wdir,"\\01_data\\01_in\\klhk\\KLHK_Kelompok_Pulau_20210308.shp"))

## create island mapping
island_tab <- tibble("island_code" = c(1, 2, 3, 4, 5, 6), "island" = c("BALINUSA", "KALIMANTAN", "MALUKU", "PAPUA", "SULAWESI", "SUMATERA"))

## intersect to get hti IDs ----------------------------------

hti_poly <- hti %>%
  select(ID) %>%
  st_transform(st_crs(samples)) %>% 
  st_cast("POLYGON")

# join hti
system.time({
  samples_hti = st_join(samples,hti_poly)
})

## intersect to get island -----------------------------------

island_poly <- island %>%
  select(island=pulau) %>%
  st_transform(st_crs(samples)) %>% 
  st_cast("POLYGON")

# join island
system.time({
  samples_hti_island = st_join(samples_hti,island_poly)
})

## clean up data ---------------------------------------------

samples_hti_table <- samples_hti %>%
  st_drop_geometry()

samples_hti_isl_table <- samples_hti_island %>%
  st_drop_geometry()

# check if na's exist
samples_hti_match_na <- samples_hti_table %>%
  filter(is.na(ID))

# check if na's exist
samples_hti_island_match_na <- samples_hti_isl_table %>%
  filter(is.na(ID)) %>%
  print()

## export to csv ---------------------------------------------

# full table
samples_hti_table <- samples_hti_island %>%
  st_drop_geometry() %>%
  left_join(island_tab,by="island") %>%
  select(sid,ID,island_code) %>%
  rename(island=island_code)

write_excel_csv(samples_hti_table,paste0(wdir,"\\01_data\\02_out\\samples\\samples_hti_id.csv"))

# shapefiles by island
samples_hti_island %>%
  select(-ID) %>%
  group_by(island) %>%
  group_walk(~ write_sf(.x, paste0(wdir,"\\01_data\\02_out\\samples\\shapefiles\\",.y$island,"_samples_hti.shp")))
