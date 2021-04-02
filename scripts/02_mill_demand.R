## ---------------------------------------------------------
##
## Purpose of script: Calculate area of planted pulp needed to satisfy a mill's demand
##
## Author: Jason Benedict
##
## Date Created: 2021-03-29
## 
## ---------------------------------------------------------
##
## Notes: Input data
##   - Total pulpwood planted area in active concessions (David Gaveau's dataset)  
##   - Mill pulp production (WWI)
##   - Flow of pulpwood from concession to mill (RPBBI)
##   
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

## credentials ----------------------------------------------

aws.signature::use_credentials()
bucket <- "trase-storage"
Sys.setenv("AWS_DEFAULT_REGION" = "eu-west-1")

## set working directory -------------------------------------

wdir <- "remote"

## read data and clean ---------------------------------------

# pulp planted area
pulp_area_hti <- read_csv(paste0(wdir, '\\01_data\\01_in\\gee\\pulp_by_year_hti.csv'))

# pulp mill production
pulp_mill_prod <- read_excel(paste0(wdir, '\\01_data\\01_in\\wwi\\PULP_MILL_PRODUCTION.xlsx'))

# wood supply
pulpwood_supply <- read_delim(get_object(object="indonesia/wood_pulp/production/out/PULP_WOOD_SUPPLY_CLEAN_ALL_ALIGNED_2015_2019.csv", bucket),delim=",") 


## clean data ------------------------------------------------

# pulp area in hti by year
pulp_area_clean_hti <- pulp_area_hti %>% 
  select(supplier_id=ID,starts_with("id_")) %>%
  pivot_longer(cols = starts_with("id_"),
               names_to = 'year',
               names_prefix = 'id_',
               values_to = 'pulp_area_ha') %>%
  mutate(year = as.double(year))

# mill production by year
mill_prod <- pulp_mill_prod %>%
  group_by(year=YEAR,mill_id=MILL_ID,mill_name=MILL_NAME) %>%
  summarize(pulp_tons = sum(TOTAL_PROD_KG_NET/1000))

# flow of wood supply to mill
ws_flow <- pulpwood_supply %>%
  select(year=YEAR,supplier_id=SUPPLIER_ID,mill_id=EXPORTER_ID,volume_m3=VOLUME_M3) %>%
  group_by(year,supplier_id,mill_id) %>%
  summarize(volume_m3 = sum(volume_m3))


