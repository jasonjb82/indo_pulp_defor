## ---------------------------------------------------------
##
## Purpose of script: Investigating temporal trends of deforestation with pulp plantations in Indonesia
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
library(khroma) # palettes for color blindness
library(d3.format)


## credentials ----------------------------------------------

aws.signature::use_credentials()
bucket <- "trase-storage"
Sys.setenv("AWS_DEFAULT_REGION" = "eu-west-1")

## set working directory -------------------------------------

wdir <- "remote"

## read data and clean ---------------------------------------

# results
results <- read_delim(get_object(object="SUBNATIONAL/INDONESIA/WOOD_PULP/V3.0.1/INDONESIA_WOOD_PULP_V3_0_1.csv", "trase-results"), delim = ";", quote = "'", skip = 1) %>%
  mutate(supplier_id = str_replace(TRASE_ID_2, "ID-CONCESSION-", "H-"),year=TIME)

# wood supply
ws_hti <- read_delim(get_object(object="indonesia/wood_pulp/production/out/PULP_WOOD_SUPPLY_CLEAN_ALL_ALIGNED_2015_2019.csv", bucket), delim = ",")

# annual deforestation
annual_defor_hti <- read_csv(paste0(wdir, '\\01_data\\01_in\\gee\\annual_defor_hti.csv'))

# landuse in 2019
lu_2019_hti <- read_csv(paste0(wdir, '\\01_data\\01_in\\gee\\lu_classes_2019_by_hti.csv'))

# hti license dates
lic_dates_hti <- readr::read_csv(paste0(wdir,"\\01_data\\01_in\\wwi\\HTI_LICENSE_DATES.csv"),
                                 col_types = cols(license_date = col_date("%m/%d/%Y")))

# forest in 2000 (hansen + margono masked)
fc_2000_hti <- read_csv(paste0(wdir, '\\01_data\\01_in\\gee\\fc_hti.csv')) 

# clean data ------------------------------------------

# license dates
lic_dates_clean_hti <- lic_dates_hti %>%
  mutate(license_year = year(license_date)) %>%
  select(supplier_id=HTI_ID,license_year) %>%
  drop_na(supplier_id) 

# first year appearing in rpbbi
hti_in_rpbbi <- ws_hti %>%
  select(supplier_id=SUPPLIER_ID,year=YEAR) %>%
  distinct() %>%
  group_by(supplier_id) %>%
  filter(year == min(year)) %>%
  rename(first_year_rpbbi=year)

# landuse in 2019
lu_2019_clean_hti <- lu_2019_hti %>%
  pivot_longer(cols = c(`1`,`2`,`3`,`4`,`5`),names_to="lu_2019",values_to="lu_class_2019_m2") %>%
  mutate(lu_class_2019 = ifelse(lu_2019 == 1 | lu_2019 == 2,"OTHERS",
                                ifelse(lu_2019 == 3,"INDUSTRIAL OIL PALM PLANTATION",
                                ifelse(lu_2019 == 4,"INDUSTRIAL PULPWOOD PLANTATION","SMALLHOLDER")))) %>%
  select(supplier_id=hti,lu_class_2019,lu_class_2019_m2) %>%
  group_by(supplier_id,lu_class_2019) %>%
  summarize(lu_class_2019_ha = sum(lu_class_2019_m2/10000)) %>%
  drop_na(lu_class_2019_ha)

# annual deforestation
ann_defor_hti <- annual_defor_hti %>% 
  pivot_longer(cols = starts_with("def_"),
               names_to = 'year',
               names_prefix = 'def_',
               values_to = 'annual_defor_ha') %>% 
  mutate(year = as.numeric(year) + 2000) %>%
  select(supplier_id=ID,year,annual_defor_ha) %>%
  arrange(supplier_id,year)

# remaining fc in each year
rem_fc_hti <- fc_2000_hti %>%
  select(supplier_id=ID,fc_2000=sum) %>%
  left_join(ann_defor_hti,by="supplier_id") %>%
  group_by(supplier_id) %>%
  mutate(cum_defor = cumsum(annual_defor_ha)) %>%
  mutate(rem_fc_ha = fc_2000 - cum_defor) %>%
  select(supplier_id,year,rem_fc_ha)


# merge into long ---------------------------------

hti_defor_long <- results %>%
  select(supplier_id,supplier_grp=`SUPPLIER GROUP`,supplier=SUPPLIER) %>%
  distinct() %>%
  left_join(lic_dates_clean_hti,by="supplier_id") %>%
  drop_na() %>%
  left_join(lu_2019_clean_hti,by="supplier_id") %>%
  left_join(ann_defor_hti,by="supplier_id") %>%
  left_join(rem_fc_hti,by=c("supplier_id","year")) %>%
  left_join(hti_in_rpbbi,by=c("supplier_id")) %>%
  relocate(year,.after="supplier")


# export to csv -----------------------------------
write_excel_csv(hti_long,paste0(wdir,"\\01_data\\02_out\\hti_defor_long.csv"))

