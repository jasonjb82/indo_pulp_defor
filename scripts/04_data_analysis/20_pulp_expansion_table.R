## ---------------------------------------------------------
## 
## Project: Indonesia pulp deforestation
##
## Purpose of script: Data preparation
##
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Date Created: 2023-05-08
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
library(data.table)
library(naniar)
library(visdat)
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(sf)
library(scales)
library(dtplyr)
library(d3.format)
library(tidyfast)
library(showtext)
library(khroma) # palettes for color blindness

'%ni%' <- Negate('%in%') # filter out function

## set working directory -------------------------------------

wdir <- "remote"

## read data -------------------------------------------------

# license dates of concessions
lic_dates_hti <- readr::read_csv(paste0(wdir,"\\01_data\\01_in\\wwi\\HTI_LICENSE_DATES.csv"),col_types = cols(license_date = col_date("%m/%d/%Y")))

# sample IDs and HTI
samples_hti <- read_csv(paste0(wdir,"\\01_data\\02_out\\samples\\samples_hti_id.csv"))

# hti concessions
hti <- read_sf(paste0(wdir,"\\01_data\\01_in\\klhk\\IUPHHK_HTI_TRASE_20230314_proj.shp"))

# annual pulp table
ann_pulp_tbl <- read_csv(paste0(wdir,"\\01_data\\02_out\\tables\\pulp_expansion_areas_2001_2022.csv"))

# pulpwood conversion from forest and non-forest within and outside hti concessions
hti_nonhti_conv <- read_csv(paste0(wdir,"\\01_data\\02_out\\tables\\idn_pulp_conversion_hti_nonhti_gaveau.csv"))

## read point sample extracted datasets ##

# TreeMap cleared area classes
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\gaveau\\"),pattern = "*gaveau_classes.csv",full.names= TRUE)

samples_treemap_landuse <- filenames %>% map_dfr(read_csv) %>% janitor::clean_names() 

# GFC deforestation (modified by TreeMap)
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\gfc_ttm\\"), pattern = "*.csv", full.names= TRUE)

samples_gfc_ttm <- filenames %>% map_dfr(read_csv) %>%janitor::clean_names() 

## clean HTI concession names
## Note: 3 non-HTI active supplier concessions included - PT OKI PULP & PAPER MILLS & PT WANA SUBUR SAWIT INDAH are
## IPK concessions [wood utilization permit] and PT MUTAIARA SABUK KHATULISTIWA is classed as Hutan Alam [Natural Forest]  

# HTI concession names
hti_concession_names <- hti %>%
  st_drop_geometry() %>%
  select(supplier_id=ID,supplier=namaobj) %>%
  mutate(supplier_label = paste0(supplier," (",supplier_id,")"))

## clean license dates
hti_dates_clean <- lic_dates_hti %>%
  mutate(YEAR = year(license_date)) %>%
  select(supplier_id=HTI_ID,license_year=YEAR)

# Identify samples that eventually become pulp
treemap_pulp_sids <- samples_treemap_landuse %>%
  select(sid,timberdeforestation_2022) %>%
  lazy_dt() %>%
  as.data.table() %>%
  dt_pivot_longer(cols = c(-sid), names_to = 'year', values_to = 'class') %>%
  as_tibble() %>%
  filter(class == "3") %>%
  distinct() %>%
  pull(sid)

# TreeMap pulp conversion
treemap_annual_conv <- samples_treemap_landuse %>%
  lazy_dt() %>%
  as.data.table() %>%
  dt_pivot_longer(cols = c(-sid),names_to = 'year',values_to = 'class')

# list of codes for forest class
forest_loss_codes <- c(101:122,401:422,601:622)

samples_df <- samples_gfc_ttm %>%
  lazy_dt() %>%
  mutate(start_for = ifelse(gfc_ttm %in% forest_loss_codes,"Y","N")) %>% 
  left_join(samples_hti, by = "sid") %>%
  drop_na(sid) %>%
  mutate(island_name = case_when(
    island == 1 ~ "Balinusa",
    island == 2 ~ "Kalimantan",
    island == 3 ~ "Maluku",
    island == 4 ~ "Papua",
    island == 5 ~ "Sulawesi",
    island == 6 ~ "Sumatera",
    TRUE ~ NA
  )) %>%
  select(-island) %>%
  rename(island = island_name) %>%
  as_tibble()

# add HTI concession license dates and names
samples_df <- samples_df %>% 
  as_tibble() %>%
  add_column(rand = runif(nrow(.))) %>%
  lazy_dt() %>%
  rename(supplier_id = ID) %>% 
  left_join(hti_dates_clean,by="supplier_id") %>%
  left_join(hti_concession_names,by="supplier_id") %>%
  left_join(treemap_annual_conv,by="sid") %>%
  filter(class == 3) %>%
  #left_join(samples_gfc_ttm,by="sid") %>%
  mutate(pulp = ifelse(sid %in% treemap_pulp_sids,"Y","N"))

# Other deforestation using TreeMap's modified GFC layer
# conversion from forest (incl. peat forests)
# 101-1xx = Mineral soil loss. The last two digit represent the year of loss.
# 401-4xx = Mangrove forest loss. The last two digit represent the year of loss.
# 601-6xx = Peat swamp forest loss. The last two digit represent the year of loss.

hti_pulp_conv <- samples_df %>%
  filter(start_for == "Y" & pulp == "Y") %>% # starting as forest and ending as pulp
  as_tibble() %>%
  mutate(year_pulp = str_replace(year,"timberdeforestation_","")) %>%
  filter(year_pulp != 2000) %>%
  group_by(sid,supplier_id) %>%
  slice_min(year)

# Expansion between 2001-2022
hti_pulp_conv_all <- hti_pulp_conv %>%
  ungroup() %>%
  mutate(year = as.double(year_pulp)) %>%
  group_by(year) %>%
  summarize(pulp_expansion_area_ha = n()) %>%
  print()

# Expansion between 2001-2022 after license year
hti_pulp_conv_license <- hti_pulp_conv %>%
  ungroup() %>%
  filter(year_pulp > license_year) %>%
  mutate(year = as.double(year_pulp)) %>%
  group_by(year) %>%
  summarize(pulp_permit_area_ha = n()) %>%
  print()

# Annual conversion to pulp from forest in hhti
hti_pulp_driven_defor <- hti_nonhti_conv %>%
  filter(conv_type == 2 & !is.na(supplier_id)) %>%
  group_by(year) %>%
  summarize(Pulp_driven_deforestation_hti_kha = sum(area_ha/1000)) %>%
  print()

# export table to csv
merged_table <- ann_pulp_tbl %>%
  select(Year,Pulp_driven_deforestation_kha) %>%
  rename(year=Year) %>%
  left_join(hti_pulp_driven_defor,by="year") %>%
  left_join(hti_pulp_conv_all,by="year") %>%
  left_join(hti_pulp_conv_license,by="year") %>%
  rename(Pulp_expansion_hti_kha=pulp_expansion_area_ha,Pulp_expansion_hti_after_permit_year_kha=pulp_permit_area_ha) %>%
  mutate(Pulp_expansion_hti_kha=Pulp_expansion_hti_kha/1000,Pulp_expansion_hti_after_permit_year_kha=Pulp_expansion_hti_after_permit_year_kha/1000) %>%
  print()
  
write_csv(merged_table,paste0(wdir,"\\01_data\\02_out\\tables\\pulp_expansion_areas_all_2001_2022.csv"))
