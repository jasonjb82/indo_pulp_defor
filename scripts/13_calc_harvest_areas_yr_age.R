## ---------------------------------------------------------
## 
## Project: Indonesia pulp deforestation
##
## Purpose of script: Clean ITP harvest data to get year and age of harvest - merge with wood supply
##
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Date Created: 2022-02-10
## 
## ---------------------------------------------------------
##
## Notes: Input datasets
##        1) HTI concessions (boundaries) and concession start year - from KLHK
##        2) Gaveau harvested areas
##
##
## ---------------------------------------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------------------------------------

### Load packages
library(stringr)
library(data.table)
library(naniar)
library(visdat)
library(tidyverse)
library(readxl)
library(tidylog)
library(data.table)
library(janitor)
library(lubridate)
library(sf)
library(scales)
library(aws.s3)
library(dtplyr)
library(testthat)
library(d3.format)
library(tidyfast)
library(patchwork)

## credentials ----------------------------------------------

aws.signature::use_credentials()
bucket <- "trase-storage"
Sys.setenv("AWS_DEFAULT_REGION" = "eu-west-1")

## set working directory -------------------------------------

wdir <- "remote"

## read data -------------------------------------------------

# itp harvest years
itp_hv <- read_sf(paste0(wdir,"\\01_data\\01_in\\gaveau\\IDN_ITPHarvesting_V20220208\\IDN_ITPHarvesting_V20220208.shp"))

# hti concessions
hti <- read_sf(paste0(wdir,"\\01_data\\01_in\\klhk\\IUPHHK_HT_proj.shp"))

# wood supply
ws <- read_delim(get_object(object="indonesia/wood_pulp/production/out/PULP_WOOD_SUPPLY_CLEAN_ALL_ALIGNED_2015_2019.csv", bucket), delim = ",")

# wood supply (2020)
ws_2020 <- read_excel(paste0(wdir,"\\01_data\\01_in\\wwi\\RPBBI_2020_compiled.xlsx")) %>%
  select(YEAR,SUPPLIER_ID,VOLUME_M3) %>%
  group_by(YEAR,SUPPLIER_ID) %>%
  summarize(VOLUME_M3 = sum(VOLUME_M3))

## clean up data ---------------------------------------------

itp_hv_proj <- st_transform(itp_hv, crs = st_crs(hti)) 

# intersect to get associated HTI
hti_itp_hv <- pi <- st_intersection(hti,itp_hv_proj) %>% mutate(area_m2 = st_area(.))

# create table and clean up
hti_itp_hv_df <- hti_itp_hv %>%
  st_drop_geometry() %>%
  select(SUPPLIER_ID=ID,year,Class,Harvest1,Harvest2,Harvest3,Ket,yearint,area_m2) %>%
  mutate(area_ha = as.double(area_m2*0.0001)) %>%
  select(-area_m2) %>%
  #pivot_longer(c(-SUPPLIER_ID,-yearint,-Ket,-year,-Class,-area_ha), names_to="var", values_to="vals") 
  filter(is.na(Ket)) %>%
  mutate(hv_age1 = Harvest1 - yearint,hv_age2 = Harvest2 - Harvest1, hv_age3 = Harvest3 - Harvest2) %>%
  mutate(hv_age1 = ifelse(hv_age1 <0,0,hv_age1),hv_age2 = ifelse(hv_age2 <0,0,hv_age2),hv_age3 = ifelse(hv_age3 <0,0,hv_age3)) %>%
  mutate(hv_age3 = ifelse(hv_age3 > 2000, Harvest3 - Harvest1,hv_age3),hv_age2 = ifelse(hv_age2 > 2000, Harvest2 - yearint,hv_age2))

# first year of harvest
hti_hv1_areas <- hti_itp_hv_df %>%
  select(SUPPLIER_ID,HARVEST_YEAR=Harvest1,AGE=hv_age1,AREA_HA=area_ha) %>%
  filter(HARVEST_YEAR > 0)

# second year of harvest
hti_hv2_areas <- hti_itp_hv_df %>%
  select(SUPPLIER_ID,HARVEST_YEAR=Harvest2,AGE=hv_age2,AREA_HA=area_ha) %>%
  filter(HARVEST_YEAR > 0)

# third year of harvest
hti_hv3_areas <- hti_itp_hv_df %>%
  select(SUPPLIER_ID,HARVEST_YEAR=Harvest3,AGE=hv_age3,AREA_HA=area_ha) %>%
  filter(HARVEST_YEAR > 0)

# merge harvest areas by year and age
hti_hv_areas_yr <- hti_hv1_areas %>%
  bind_rows(hti_hv2_areas) %>%
  bind_rows(hti_hv3_areas) %>%
  group_by(SUPPLIER_ID,HARVEST_YEAR,AGE) %>%
  summarize(AREA_HA = sum(AREA_HA)) %>%
  pivot_wider(names_from = AGE, values_from = AREA_HA,values_fill = 0)
               
# wood supply by supplier
ws_all <- ws %>%
  bind_rows(ws_2020) %>%
  group_by(SUPPLIER_ID,YEAR) %>%
  summarize(VOLUME_M3 = sum(VOLUME_M3))

# join with wood supply
hti_harvest_areas_yr_ws <- hti_hv_areas_yr %>%
  left_join(select(ws_all,SUPPLIER_ID,HARVEST_YEAR=YEAR,VOLUME_M3),by=c("SUPPLIER_ID","HARVEST_YEAR")) %>%
  drop_na(VOLUME_M3) 

## export to csv ---------------------------------------------
write_csv(hti_harvest_areas_yr_ws,paste0(wdir,"\\01_data\\02_out\\tables\\hti_ws_wood_harvest_yr_age.csv"))
