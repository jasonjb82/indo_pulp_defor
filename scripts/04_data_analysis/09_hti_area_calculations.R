## ---------------------------------------------------------
## 
## Project: Indonesia pulp deforestation
##
## Purpose of script: Calculate total areas of concession, 
###                   peat (in concession and on planted area) and burned area (in concession and on planted areas)
##
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Date Created: 2021-06-07
## 
## ---------------------------------------------------------
##
## Notes: Input datasets
##        1) HTI concessions (boundaries) and concession start year - from KLHK
##        2) Gaveau landuse change - commodity deforestation (2000 - 2020) (IOPP,ITP and smallholders)
##        3) Burned areas (2015 - 2020)
##        4) Peat areas
##
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

## hti license dates
lic_dates_hti <- readr::read_csv(paste0(wdir,"\\01_data\\01_in\\wwi\\HTI_LICENSE_DATES.csv"),
                                 col_types = cols(license_date = col_date("%m/%d/%Y")))

## supplier groups
groups <- read_csv(paste0(wdir,"\\01_data\\01_in\\wwi\\ALIGNED_NAMES_GROUP_HTI.csv"))

## HTI and sample IDs
samples_hti <- read_csv(paste0(wdir,"\\01_data\\02_out\\samples\\samples_hti_id.csv"))

# hti concessions
hti <- read_sf(paste0(wdir,"\\01_data\\01_in\\klhk\\IUPHHK_HT_proj.shp"))

## Gaveau data
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\gaveau\\"),
                 pattern = "*.csv",
                 full.names= TRUE)

samples_gaveau_landuse <- filenames %>%
  map_dfr(read_csv, .id = "gaveau") %>%
  janitor::clean_names() %>%
  select(-system_index,-geo)

## Peat areas
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\gfc_peat\\"),
                 pattern = "*.csv",
                 full.names= TRUE)

samples_peat_areas <- filenames %>%
  map_dfr(read_csv, .id = "peat_areas") %>%
  janitor::clean_names() %>%
  select(-lossyear,-primary,-forcover)

## Burned areas
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\burn_areas\\"),
                 pattern = "*.csv",
                 full.names= TRUE)

samples_burn_areas <- filenames %>%
  map_dfr(read_csv, .id = "burn_areas") %>%
  janitor::clean_names() %>%
  select(-system_index,-geo)


############################################################################
# Clean / prep data --------------------------------------------------------
############################################################################

## clean hti concession names
hti_concession_names <- hti %>%
  st_drop_geometry() %>%
  select(supplier_id=ID,supplier=NAMOBJ) %>%
  mutate(supplier_label = paste0(supplier," (",supplier_id,")"))

## clean license dates
hti_dates_clean <- lic_dates_hti %>%
  mutate(YEAR = year(license_date)) %>%
  select(supplier_id=HTI_ID,license_year=YEAR)

# clean supplier groups
supplier_groups <- groups %>%
  select(supplier_id = id,supplier_group=group) %>%
  mutate(supplier_group = ifelse(is.na(supplier_group),"OTHER",supplier_group))

## identify pixels that were cleared for pulp at some point in the time series
# gaveau
gaveau_pulp <- samples_gaveau_landuse %>%
  lazy_dt() %>%
  left_join(samples_hti,by="sid") %>%
  as_tibble()

# TRUE/FALSE if sample is ever on pulp clearing 
# gaveau
gaveau_pulp$ever_pulp <- (rowSums(gaveau_pulp[,startsWith(names(gaveau_pulp),"id_")]==4) >= 1) # Gaveau class 4 is industrial pulp clearing

# select columns
# gaveau
gaveau_pulp <- gaveau_pulp %>% 
  select(sid,ever_pulp)

## first year gaveau assigns as pulp for each sid
gaveau_pulp_styr <- samples_gaveau_landuse %>%
  lazy_dt() %>%
  left_join(samples_hti,by="sid") %>%
  select(sid,supplier_id=ID,gaveau,starts_with("id_")) %>%
  as.data.table() %>%
  dt_pivot_longer(cols = c(-supplier_id,-gaveau,-sid),
                  names_to = 'year',
                  values_to = 'class') %>%
  as_tibble() %>%
  filter(class == 4) %>%
  mutate(year = str_replace(year,"id_", ""),year = as.double(year)) %>% 
  group_by(sid) %>% 
  slice(which.min(year)+1) 

## gaveau pulp detection year
gaveau_pulp_yr <- samples_gaveau_landuse %>%
  lazy_dt() %>%
  left_join(samples_hti,by="sid") %>%
  select(sid,supplier_id=ID,gaveau,starts_with("id_")) %>%
  as.data.table() %>%
  dt_pivot_longer(cols = c(-sid,-supplier_id,-gaveau),
                  names_to = 'year',
                  values_to = 'class') %>%
  as_tibble() %>%
  filter(class == 4) %>%
  mutate(year = str_replace(year,"id_", ""),year = as.double(year)+1) # add lag of 1 year

## Join to gaveau, concession, mill supplied, first year pulp
samples_df <- samples_hti %>%
  rename(supplier_id = ID) %>% 
  left_join(hti_dates_clean,by="supplier_id") %>%
  left_join(hti_concession_names,by="supplier_id") %>% 
  left_join(gaveau_pulp, by = "sid") %>% # 
  left_join(gaveau_pulp_yr,by="sid") %>% 
  left_join(samples_peat_areas,by="sid") %>%
  select(sid, supplier_id=supplier_id.x,ever_pulp, license_year, pulp_year=year,supplier_label,peat) 

## total concession area
hti <- samples_hti %>%
  #filter(ever_pulp == "TRUE") %>%
  group_by(supplier_id=ID) %>%
  summarize(concession_area_ha = n()) %>%
  left_join(hti_concession_names,by="supplier_id") %>% 
  select(supplier_id,supplier,supplier_label,concession_area_ha) %>%
  print()

## total pulp in concession
hti_pulp <- samples_df %>%
  filter(ever_pulp == "TRUE") %>%
  group_by(pulp_year,supplier_id,supplier_label) %>%
  summarize(pulp_area_ha = n()) %>%
  print()

## total peat area on planted pulp area
hti_pulp_on_peat <- samples_df %>%
  filter(!is.na(peat) & ever_pulp == "TRUE") %>%
  group_by(pulp_year,supplier_id,supplier_label) %>%
  summarize(pulp_on_peat_area_ha = n()) %>%
  print()

### burn areas on pulp planted area
hti_burn_areas <- samples_burn_areas %>%
  lazy_dt() %>%
  as.data.table() %>%
  dt_pivot_longer(cols = c(-sid,-burn_areas),
                  names_to = 'year',
                  values_to = 'burn') %>%
  as_tibble() %>%
  mutate(burn_year = str_replace(year,"x",""), burn_year = as.double(burn_year)) %>%
  left_join(samples_hti,by="sid") %>%
  rename(supplier_id=ID) %>%
  left_join(select(gaveau_pulp_styr,sid,pulp_year=year),by="sid") %>%
  left_join(gaveau_pulp, by = "sid") %>% 
  left_join(hti_concession_names, by = "supplier_id") %>% 
  select(burn_year,pulp_year,burn,ever_pulp,supplier_id,supplier,supplier_label) %>%
  filter(ever_pulp == "TRUE" & burn == 1 & pulp_year <= burn_year) %>%
  group_by(burn_year,supplier_id,supplier_label) %>%
  summarize(burn_plantation_area_ha = n())

## Merge tables ------------------------
hti_merge <- hti %>%
  right_join(select(hti_pulp,supplier_id,pulp_area_ha),by="supplier_id") %>%
  left_join(select(hti_pulp_on_peat,supplier_id,pulp_on_peat_area_ha),by=c("supplier_id","pulp_year")) %>%
  left_join(select(hti_burn_areas,supplier_id,burn_plantation_area_ha),by=c("supplier_id","pulp_year"="burn_year")) %>%
  select(year=pulp_year,supplier_id,supplier_label,concession_area_ha,pulp_area_ha,pulp_on_peat_area_ha,burn_plantation_area_ha) %>%
  filter(year < 2021) %>%
  mutate_at(4:7, ~replace_na(.,0))
  print()


## Write to csv -----------------------
write_csv(hti_merge,paste0(wdir,"\\01_data\\02_out\\tables\\hti_concessions_area_calcs.csv"))
  
