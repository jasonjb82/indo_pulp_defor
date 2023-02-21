## ---------------------------------------------------------
## 
## Project: Indonesia pulp deforestation
##
## Purpose of script: Compare JRC TMF, Margono and Hansen F/NF areas
##
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Date Created: 2023-02-21
## 
## ---------------------------------------------------------
##
## Notes: Input datasets
##        1) HTI concessions (boundaries) and concession start year - from KLHK
##        2) Hansen and Margono forest cover
##        3) JRC TMF (1990 - 2021)
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
library(openxlsx)

## credentials ----------------------------------------------

aws.signature::use_credentials()
bucket <- "trase-storage"
Sys.setenv("AWS_DEFAULT_REGION" = "eu-west-1")

## set working directory -------------------------------------

wdir <- "remote"

## read data -------------------------------------------------

## load color palette
source("scripts\\001_misc\\001_color_palettes.R")

## data lookup table
lu_table <- read_csv(paste0(wdir,"\\01_data\\02_out\\gee\\data_lookup_table.csv"))

## HTI and sample IDs
samples_hti <- read_csv(paste0(wdir,"\\01_data\\02_out\\samples\\samples_hti_id.csv"))

# hti concessions
hti <- read_sf(paste0(wdir,"\\01_data\\01_in\\klhk\\IUPHHK_HT_proj.shp"))

# kabupaten
kab <- read_sf(paste0(wdir,"\\01_data\\01_in\\big\\idn_kabupaten_big.shp"))
prov_slim <- kab %>% select(prov,prov_code) %>% st_drop_geometry() %>% distinct() %>%
  mutate(prov_code = ifelse(prov == "PAPUA",92,prov_code))

## JRC
## Annual changes 
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\jrc\\annual_changes\\"),
                 pattern = "*2021.csv",
                 full.names= TRUE)

samples_jrc_tmf <- filenames %>%
  map_dfr(read_csv, .id = "jrc_tmf_ac") %>%
  janitor::clean_names() %>%
  select(-system_index,-geo)

## Hansen and Margono
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\gfc_peat\\"),
                 pattern = "*.csv",
                 full.names= TRUE)

samples_gfc_margono <- filenames %>%
  map_dfr(read_csv, .id = "gfc_margono") %>%
  janitor::clean_names() %>%
  select(-peat,-lossyear)

## clean data -----------------------------------------------

samples_gfc_jrc_margono_2000 <- samples_jrc_tmf %>%
  select(sid,dec2000) %>%
  left_join(samples_gfc_margono,by="sid") %>%
  select(-gfc_margono) %>%
  mutate(jrc_tmf_2000 = ifelse(dec2000==1,1,0),
         gfc_90 = ifelse(forcover >=90,1,0),
         margono=primary) %>%
  select(-forcover,-primary) %>%
  mutate(jrc_margono = 
           case_when(jrc_tmf_2000 == 0 & margono == 0 ~ "Both non-forest",
                     jrc_tmf_2000 == 1 & margono == 1 ~ "Both forest",
                     jrc_tmf_2000 == 1 & margono == 0 ~ "JRC TMF forest, Margono non-forest",
                     jrc_tmf_2000 == 0 & margono == 1 ~ "JRC TMF non-forest, Margono forest")) %>%
  mutate(jrc_hansen = 
           case_when(jrc_tmf_2000 == 0 & gfc_90 == 0 ~ "Both non-forest",
                     jrc_tmf_2000 == 1 & gfc_90 == 1 ~ "Both forest",
                     jrc_tmf_2000 == 1 & gfc_90 == 0 ~ "JRC TMF forest, GFC non-forest",
                     jrc_tmf_2000 == 0 & gfc_90 == 1 ~ "JRC TMF non-forest, GFC forest")) %>%
  mutate(hansen_margono = 
           case_when(margono == 0 & gfc_90 == 0 ~ "Both non-forest",
                     margono == 1 & gfc_90 == 1 ~ "Both forest",
                     margono == 1 & gfc_90 == 0 ~ "Margono forest, GFC non-forest",
                     margono == 0 & gfc_90 == 1 ~ "Margono non-forest, GFC forest"))
  
  
## calculate statistics ----------------------------------

jrc_hansen <- samples_gfc_jrc_margono_2000 %>%
  select(jrc_hansen) %>%
  group_by(jrc_hansen) %>%
  summarize(area_ha = n()) %>%
  print()

jrc_margono <- samples_gfc_jrc_margono_2000 %>%
  select(jrc_margono) %>%
  group_by(jrc_margono) %>%
  summarize(area_ha = n()) %>%
  print()

hansen_margono <- samples_gfc_jrc_margono_2000 %>%
  select(hansen_margono) %>%
  group_by(hansen_margono) %>%
  summarize(area_ha = n()) %>%
  print()

# check hti areas
hti_areas <- samples_hti %>%
  group_by(ID) %>%
  summarize(area_ha = n()) %>%
  print()

## export to excel ---------------------------------------

# create a named list of your dataframes. The list names will be the worksheet names.

xl_lst <- list('jrc_hansen' = jrc_hansen, 'jrc_margono' = jrc_margono, 'gfc_margono' = hansen_margono)
write.xlsx(xl_lst, paste0(wdir,"\\01_data\\02_out\\tables\\datasets_forest_agreement_areas.xlsx"))
