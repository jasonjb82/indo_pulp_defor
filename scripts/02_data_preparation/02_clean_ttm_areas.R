## ---------------------------------------------------------
## 
## Project: Indonesia pulp deforestation
##
## Purpose of script: Create tables of TTM annual expansion statistics and 1km sampled
## landuse and deforestation areas
##
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Date Created: 2026-05-28
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
library(tidyverse)
library(readxl)
library(janitor)
library(lubridate)
library(sf)
library(scales)
library(dtplyr)
library(tidyfast)

'%ni%' <- Negate('%in%') # filter out function

## set working directory -------------------------------------

wdir <- "remote"

## read data -------------------------------------------------

# TTM landuse classes
filenames <- dir(path = paste0(wdir,"/01_data/02_out/gee/gaveau/"),pattern = "*gaveau_classes.csv",full.names= TRUE)

samples_landuse_ttm <- filenames %>% map_dfr(read_csv) %>% janitor::clean_names() 

# TTM GFC deforestation
filenames <- dir(path = paste0(wdir,"/01_data/02_out/gee/gfc_ttm/"), pattern = "*.csv", full.names= TRUE)

samples_gfc_ttm <- filenames %>% map_dfr(read_csv) %>%janitor::clean_names() 

# pulp expansion
id_annual_pulp_stats <- read_excel(paste0(wdir,"/01_data/01_in/gaveau/IDN_2001_2022 landcover change of Oil Palm and Pulpwood_05JUNE2023.xlsx"),sheet="PULPWOOD EXPANSION",skip=90) %>% clean_names() %>%
  select(year,forest_loss_ha=total_forest_loss,forest_loss_pulp_ha=area_of_forest_converted_to_pulpwood_pw_each_year_ha,nonforest_loss_pulp_ha=non_forest_to_pulpwood) %>%
  mutate(year=year+2000) %>%
  drop_na(year)

# palm expansion
id_annual_palm_stats <- read_excel(paste0(wdir,"/01_data/01_in/gaveau/IDN_2001_2022 landcover change of Oil Palm and Pulpwood_05JUNE2023.xlsx"),sheet="OIL PALM EXPANSION",skip=172) %>% clean_names() %>%
  select(year,forest_loss_palm_ha=area_of_forest_converted_to_oil_palm_pw_each_year_ha) %>%
  mutate(year=year+2000) %>%
  drop_na(year)

# kalimantan pulp expansion
kali_annual_pulp_exp_stats <- read_excel(paste0(wdir,"/01_data/01_in/gaveau/IDN_2001_2022 landcover change of Oil Palm and Pulpwood_05JUNE2023.xlsx"),sheet="PULPWOOD EXPANSION",skip=5,n_max =22 ) %>% 
  clean_names() %>%
  select(year,forest_loss_ha=area_of_forest_converted_to_pulpwood_pw_each_year_ha) %>%
  mutate(year=as.double(year)+2000)

# merge data ---------------------------------------------------

id_annual_exp_stats <- id_annual_pulp_stats %>%
  left_join(id_annual_palm_stats,by=c("year"))

## write data --------------------------------------------------

# landuse expansion (1km samples)
write_csv(samples_landuse_ttm,paste0(wdir,"/01_data/02_out/tables/samples_landuse_ttm.csv"))
# gfc deforestation (1km samples)
write_csv(samples_gfc_ttm,paste0(wdir,"/01_data/02_out/tables/samples_gfc_ttm.csv"))
# kalimantan expansion area statistics
write_csv(kali_annual_pulp_exp_stats,paste0(wdir,"/01_data/02_out/tables/kali_annual_pulp_exp_stats_ttm.csv"))
# annual expansion area statistics
write_csv(id_annual_exp_stats,paste0(wdir,"/01_data/02_out/tables/id_annual_expansion_stats_ttm.csv"))
