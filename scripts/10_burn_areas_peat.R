## ---------------------------------------------------------
##
## Purpose of script: Calculate burned areas on peat within concessions
##
## Author: Jason Benedict
##
## Date Created: 2021-04-22
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
library(d3.format)

## credentials ----------------------------------------------

aws.signature::use_credentials()
bucket <- "trase-storage"
Sys.setenv("AWS_DEFAULT_REGION" = "eu-west-1")

## set working directory -------------------------------------

wdir <- "remote"

## read data -------------------------------------------------

# increase memory size
memory.limit(size=65000)


# hti concessions
hti <- read_sf(paste0(wdir,"\\01_data\\01_in\\klhk\\IUPHHK_HT_proj.shp"))

# burn areas
ba <- read_sf(paste0(wdir,"\\01_data\\01_in\\klhk\\burn_areas\\ba_2015_2019.shp"))

# peat
peat <- read_sf(paste0(wdir,"\\01_data\\01_in\\klhk\\peat\\Indonesian_Peat_Map_BBSDLP_MoAgri_2011.shp"))


## clean data ------------------------------------------------

# clean concession names
hti_concession_names <- hti %>%
  st_drop_geometry() %>%
  select(supplier_id=ID,supplier=NAMOBJ) %>%
  mutate(supplier_label = paste0(supplier," (",supplier_id,")"))

# choose projection: Cylindrical Equal Area
indonesian_crs <- "+proj=cea +lon_0=115.0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

idn_sinu_crs = "proj:+proj=sinu +lon_0=0 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs"

# reproject
hti_proj <- hti %>%
  st_transform(crs=idn_sinu_crs)

ba_proj <- ba %>%
  st_transform(crs=idn_sinu_crs)

peat_proj <- peat %>%
  st_transform(crs=idn_sinu_crs)

## intersect and tabulate -----------------------------------

# overlap concessions and burned areas
hti_ba_overlap = sf::st_intersection(hti_proj,ba_proj) %>%
  select(supplier_id=ID,yr_2015,yr_2016,yr_2017,yr_2018,yr_2019)

# overlap concessions and peat
hti_peat <- sf::st_intersection(hti_proj,peat_proj) %>%
  select(supplier_id=ID,peat=SOURCE) %>%
  mutate(area = st_area(.)) %>%
  st_drop_geometry() %>%
  mutate(area_ha = as.numeric(area)/10000) %>%
  group_by(supplier_id) %>%
  summarize(area_ha = sum(area_ha)) 

# overlap burned areas within concessions and peat
hti_ba_peat <- sf::st_intersection(hti_ba_overlap,peat_proj) %>%
  mutate(area = st_area(.)) %>%
  st_drop_geometry() %>%
  pivot_longer(cols = starts_with("yr_"),
               names_to = 'year',
               values_to = 'burn') %>%
  select(-year) %>%
  filter(burn > 0) %>%
  group_by(supplier_id,year=burn) %>%
  summarise(burn_area_on_peat_ha = as.numeric(sum(area))/10000) %>%
  left_join(hti_concession_names,by="supplier_id") %>%
  left_join(select(hti_peat,supplier_id,peat_area_ha=area_ha),by="supplier_id") %>%
  mutate(burn_area_not_peat_ha = peat_area_ha - burn_area_on_peat_ha) %>%
  relocate(burn_area_not_peat_ha,.after="burn_area_on_peat_ha") %>%
  select(-peat_area_ha) %>%
  arrange(desc(supplier_label)) %>%
  ungroup() %>%
  select(concession=supplier_label,year,burn_area_not_peat_ha,burn_area_on_peat_ha)

# write to csv
write_csv(hti_ba_peat,paste0(wdir,"\\01_data\\02_out\\tables\\concessions_burned_area_peat.csv"))
            