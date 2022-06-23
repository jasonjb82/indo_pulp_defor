## ---------------------------------------------------------
## 
## Project: Indonesia pulp deforestation
##
## Purpose of script: Merge wood supply data (2015-2019 and 2020 and 2021)
##
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Date Created: 2022-02-10
## 
## ---------------------------------------------------------
##
## Notes: Input datasets
##        1) HTI concessions (boundaries) and concession start year - from KLHK
##        2) Wood supply data - RPBBI (cleaned and checked by UCSB & WWI)
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
ws <- read_delim(get_object(object="indonesia/wood_pulp/production/out/PULP_WOOD_SUPPLY_CLEAN_ALL_ALIGNED_2015_2019.csv", bucket), delim = ",")

# supplier groups
groups <- read_csv(paste0(wdir,"\\01_data\\01_in\\wwi\\ALIGNED_NAMES_GROUP_HTI.csv"))

# mills
mills <- s3read_using(read_excel, object = "indonesia/wood_pulp/logistics/out/mills/MILLS_EXPORTERS_20200405.xlsx", bucket = bucket)


## aggregate data -------------------------------------------

# wood supply (2020)
ws_2020 <- read_excel(paste0(wdir,"\\01_data\\01_in\\wwi\\RPBBI_2020_compiled.xlsx")) %>%
  select(YEAR,SUPPLIER_ID,EXPORTER_ID,VOLUME_M3) %>%
  group_by(YEAR,EXPORTER_ID,SUPPLIER_ID) %>%
  summarize(VOLUME_M3 = sum(VOLUME_M3))

# wood supply (2021)
ws_2021 <- read_excel(paste0(wdir,"\\01_data\\01_in\\wwi\\RPBBI_2021_compiled.xlsx")) %>%
  select(YEAR,SUPPLIER_ID,EXPORTER_ID,VOLUME_M3) %>%
  group_by(YEAR,EXPORTER_ID,SUPPLIER_ID) %>%
  summarize(VOLUME_M3 = sum(VOLUME_M3))

# unqiue mills
mills_tbl <- mills %>%
  select(MILL_ID,MILL_GROUP,MILL_NAME) %>%
  distinct()

# unique wood suppliers
ws_tbl <- ws %>%
  select(SUPPLIER_ID,SUPPLIER_GROUP,SUPPLIER) %>%
  distinct()

# wood supply by supplier
ws_all <- ws %>%
  bind_rows(ws_2020) %>%
  bind_rows(ws_2021) %>%
  group_by(YEAR,SUPPLIER_ID,EXPORTER_ID) %>%
  summarize(VOLUME_M3 = sum(VOLUME_M3)) %>%
  left_join(select(groups,SUPPLIER_GROUP=group,SUPPLIER_ID=id,SUPPLIER_NAME=company_clean),by="SUPPLIER_ID") %>%
  left_join(select(ws_tbl,SUPPLIER_ID,SUPPLIER),by="SUPPLIER_ID") %>%
  left_join(select(mills_unique,EXPORTER_ID=MILL_ID,EXPORTER_GROUP=MILL_GROUP,MILL_NAME),by="EXPORTER_ID") %>%
  select(YEAR,SUPPLIER_ID,SUPPLIER_GROUP,SUPPLIER,SUPPLIER_NAME,EXPORTER_ID,EXPORTER_GROUP,MILL_NAME,VOLUME_M3) %>%
  mutate(SUPPLIER_NAME = ifelse(is.na(SUPPLIER_NAME),SUPPLIER,SUPPLIER_NAME)) %>%
  select(-SUPPLIER)


## export to csv ---------------------------------------------
write_csv(ws_all,paste0(wdir,"\\01_data\\02_out\\tables\\ws_merge_clean_2015_2021.csv"))
