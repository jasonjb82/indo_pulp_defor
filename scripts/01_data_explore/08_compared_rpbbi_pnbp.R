## ---------------------------------------------------------
## 
## Project: Indonesia pulp deforestation
##
## Purpose of script: 
##
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Date Created: 2024-03-05
## 
## ---------------------------------------------------------
##
## Notes: Input datasets
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

# hti concessions
hti <- read_sf(paste0(wdir,"\\01_data\\01_in\\klhk\\IUPHHK_HT_proj.shp"))

# wood supply
ws <- read_delim(get_object(object="indonesia/wood_pulp/production/out/PULP_WOOD_SUPPLY_CLEAN_ALL_ALIGNED_2015_2022.csv", bucket), delim = ",") %>%
  select(YEAR,SUPPLIER_ID,SUPPLIER_GROUP,SUPPLIER,VOLUME_M3)

# psdh
psdh <- read_csv(paste0(wdir,"\\01_data\\01_in\\klhk\\psdh\\02_out\\PSDH_HTI_ID_COMBINED.csv"))

## clean up data ---------------------------------------------

ws_clean <- ws %>%
  mutate(HTI_ID = str_replace(SUPPLIER_ID,"ID-WOOD-CONCESSION-","H-")) %>%
  group_by(YEAR,HTI_ID,SUPPLIER_GROUP,SUPPLIER) %>%
  summarize(RPBBI_VOLUME_M3 = sum(VOLUME_M3))

psdh_clean <- psdh %>%
  select(HTI_ID,VOLUME_M3,YEAR) %>%
  filter(YEAR > 2016) %>% 
  group_by(YEAR,HTI_ID) %>%
  summarize(PNBP_VOLUME_M3 = sum(VOLUME_M3)) 

## merge tables ----------------------------------------------

merge_df <- ws_clean %>%
  left_join(psdh_clean,by=c("YEAR","HTI_ID")) %>%
  select(YEAR,HTI_ID,SUPPLIER_GROUP,SUPPLIER,PNBP_VOLUME_M3,RPBBI_VOLUME_M3) %>%
  filter(!is.na(PNBP_VOLUME_M3)) %>%
  mutate(DIFF_M3 = PNBP_VOLUME_M3 - RPBBI_VOLUME_M3,RATIO = RPBBI_VOLUME_M3/PNBP_VOLUME_M3) %>%
  arrange(-desc(HTI_ID),YEAR) %>%
  print()


## write to csv -----------------------------------------------

write_csv(merge_df,paste0(wdir,"\\01_data\\02_out\\tables\\hti_rpbbi_pnbp_volumes.csv"))
