## ---------------------------------------------------------
## 
## Project: Indonesia pulp deforestation
##
## Purpose of script: Reclassifying HTI groups 
##
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Date Created: 2023-12-05
## 
## ---------------------------------------------------------
##
## Notes: Input datasets
##
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
library(concordance)
library(rcartocolor)
library(vistime)
library(khroma) # palettes for color blindness

## credentials ----------------------------------------------

aws.signature::use_credentials()
bucket <- "trase-storage"
Sys.setenv("AWS_DEFAULT_REGION" = "eu-west-1")

## set working directory -------------------------------------
wdir <- "remote"

## read data -------------------------------------------------

## supplier groups
groups_ori <- read_csv(paste0(wdir,"\\01_data\\01_in\\wwi\\ALIGNED_NAMES_GROUP_HTI.csv"))

## updated groups
groups_mod <- read_csv(paste0(wdir,"\\01_data\\01_in\\wwi\\supplier_defor_list_shadows.csv"))

## clean data and export ------------------------------------

groups_merge <- groups_ori %>%
  right_join(groups_mod,by=c("id"="supplier_id")) %>%
  mutate(group_reclassed = 
           case_when(
             supplier_group == "APP" | supplier_group == "RGE" ~ "NGO-linked",
             group == "SINAR MAS" | group == "ROYAL GOLDEN EAGLE / TANOTO" ~ "Owned or acknowledged",
             supplier_group == "OTHER" | supplier_group != "APP" | supplier_group != "RGE" | is.na(supplier_group) ~ "Indirect supplier"
           )) %>%
  select(id,original_supplier_group=supplier_group,modified_supplier_group=group,company,group_reclassed)

write_csv(groups_merge,paste0(wdir,"\\01_data\\02_out\\tables\\pulp_supplier_groups_reclassed.csv"))
            