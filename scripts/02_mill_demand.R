## ---------------------------------------------------------
##
## Purpose of script: Calculate Area of planted pulp needed to satisfy a mill's demand
##
## Author: Jason Benedict
##
## Date Created: 2021-03-29
## 
## ---------------------------------------------------------
##
## Notes: Input data
##   - Eventual landuse of concession areas (exported from GEE)
##   - First year started supplying pulpwood (wood supply data from RPBBI)  
##   - Year first licensed (available from Brian - compiled from KLHK report)
##   - Forest remaining at start of appearing in RPBBI (GEE)
##   - Annual deforestation (exported from GEE)
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
library(myutil)
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


## clean data ------------------------------------------------

# pulp area in hti by year
pulp_area_clean_hti <- pulp_area_hti %>% 
  select(supplier_id=ID,starts_with("id_")) %>%
  pivot_longer(cols = starts_with("id_"),
               names_to = 'year',
               names_prefix = 'id_',
               values_to = 'pulp_area_ha') 
