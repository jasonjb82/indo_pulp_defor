## ---------------------------------------------------------
## 
## Project: Indonesia pulp deforestation
##
## Purpose of script: 
##
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Date Created: 2023-05-08
## 
## ---------------------------------------------------------
##
## Notes: Input datasets
##        1) HTI concessions (boundaries) and concession start year from KLHK
##        2) Gaveau landuse change - pulp deforestation (2000 - 2022) from TreeMap
##        3) JRC deforestation (1990 - 2022) - Vancutsem et.al (2021) - https://www.science.org/doi/10.1126/sciadv.abe1603
##        4) GFC Hansen deforestation year (2001 - 2022) - earthenginepartners.appspot.com/science-2013-global-forest
##        5) Peat (MoA Indonesia, 2019) & Margono forest mask (TreeMap version)
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
library(rcartocolor)
library(showtext)
library(khroma) # palettes for color blindness

## credentials ----------------------------------------------

aws.signature::use_credentials()
bucket <- "trase-storage"
Sys.setenv("AWS_DEFAULT_REGION" = "eu-west-1")

## set working directory -------------------------------------

wdir <- "remote"

## read data -------------------------------------------------
'%ni%' <- Negate('%in%') # filter out function

## load color palette
source("scripts\\001_misc\\001_color_palettes.R")

# expansion on soil type (Gaveau)
pulp_ttm_soil_type <- read_csv(paste0(wdir,"\\01_data\\02_out\\gee\\gaveau\\idn_pulp_annual_expansion_peat_mineral_soils.csv"))

# expansion of pulp on peat
pulp_peat_moa <- read_csv(paste0(wdir,"\\01_data\\02_out\\gee\\gfc_peat\\idn_pulp_annual_expansion_peatland_moa.csv"))

## clean data ------------------------------------------------

# TreeMap peat forest vs mineral soil pulp expansion

pulp_ttm_st_long <- pulp_ttm_soil_type %>%
  select(-`system:index`,-constant,-kab,-kab_code,-prov_code,-.geo,-type) %>%
  pivot_longer(cols = -c(prov),
                  names_to = 'year',
                  values_to = 'area_ha') %>%
  mutate(class = str_extract(year, "[^_]+"),
         year = as.numeric(gsub("[^0-9]", "", year))) %>%
  ungroup() %>%
  group_by(year,class) %>%
  summarize(area_ha = sum(area_ha))


pulp_peat_long <- pulp_peat_moa %>%
  select(-`system:index`,-constant,-kab,-kab_code,-prov_code,-.geo,-type) %>%
  pivot_longer(cols = -c(prov),
               names_to = 'year',
               values_to = 'area_ha') %>%
  mutate(class = str_extract(year, "[^_]+"),
         year = as.numeric(gsub("[^0-9]", "", year))) %>%
  ungroup() %>%
  group_by(year,class) %>%
  summarize(area_ha = sum(area_ha))

# peat within HTI
samples_gfc_ttm_peat <- samples_gfc_ttm %>% left_join(samples_gfc_margono_peat,by="sid")

samples_peat <- samples_gfc_ttm_peat %>%
  select(gfc_ttm,peat) %>%
  mutate(ttm_peat = ifelse(gfc_ttm >= 600 & gfc_ttm < 699, "TTM peat","TTM not peat"),
         ucsb_peat = ifelse(is.na(peat),"UCSB not peat","UCSB peat")) %>%
  select(ttm_peat,ucsb_peat) %>%
  #mutate(class = paste0(ttm_peat," - ",ucsb_peat)) %>%
  group_by(ttm_peat,ucsb_peat) %>%
  summarize(n_samples = n()) 

samples_ucsb_peat <- samples_gfc_ttm_peat %>%
  filter(peat == 1 & primary == 100) %>%
  group_by(gfc_ttm) %>%
  summarize(n = n()) %>%
  print()

defor_on_peat_ttm <- samples_gfc_ttm %>%
  mutate(class = case_when(
    gfc_ttm > 600 & gfc_ttm < 699 ~ "Peat",
    gfc_ttm > 100 & gfc_ttm < 199 ~ "Mineral",
    TRUE ~ NA)) %>%
  mutate(gfc_ttm = as.character(gfc_ttm),
         gfc_ttm = str_sub(gfc_ttm,2, -1),
         year = as.double(gfc_ttm) + 2000) %>%
  filter(year > 2000 & !is.na(class)) %>%
  group_by(year,class) %>%
  summarize(area_ha = n()) %>%
  print()

defor_on_peat <- samples_df %>%
  mutate(class = case_when(
    gfc_ttm >= 600 & gfc_ttm < 699 ~ "Peat",
    gfc_ttm >= 100 & gfc_ttm < 199 ~ "Mineral",
    TRUE ~ "Other")) %>%
  mutate(gfc_ttm = as.character(gfc_ttm),
         gfc_ttm = str_sub(gfc_ttm,2, -1),
         year = as.double(gfc_ttm) + 2000) %>%
  filter(year > 2000 & !is.na(class) & pulp == "Y") %>%
  group_by(year,class) %>%
  summarize(area_ha = n()) %>%
  print()


## plotting -------------------------------------------------

defor_on_peat %>%
  as_tibble() %>%
  filter(class != "Other") %>%
  ggplot() +
  aes(y = area_ha, x = year, fill=as.factor(class),color=as.factor(class)) +
  geom_col() +
  xlab("\nYear") +
  ylab("Area (ha)")

pulp_ttm_st_long  %>%
  ggplot() +
  aes(y = area_ha, x = year, fill=as.factor(class),color=as.factor(class)) +
  geom_col() +
  xlab("\nYear") +
  ylab("Area (ha)")

pulp_peat_long  %>%
  ggplot() +
  aes(y = area_ha, x = year, fill=as.factor(class),color=as.factor(class)) +
  geom_col() +
  xlab("\nYear") +
  ylab("Area (ha)")


# export data ------------------------------------------------

write_csv(pulp_ttm_st_long,paste0(wdir,"\\01_data\\02_out\\tables\\pulp_expansion_peat_mineral_soils_treemap.csv"))
write_csv(pulp_peat_long,paste0(wdir,"\\01_data\\02_out\\tables\\pulp_expansion_peat_moa.csv"))
