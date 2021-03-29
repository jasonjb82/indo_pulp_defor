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

## credentials ----------------------------------------------

aws.signature::use_credentials()
bucket <- "trase-storage"
Sys.setenv("AWS_DEFAULT_REGION" = "eu-west-1")

## set working directory -------------------------------------

dropbox_dir <- dropbox_folder()
wdir <- paste0(dropbox_dir,"Trase_Indo_Pulp")

## read data and clean ---------------------------------------

# results file
results <- read_delim(get_object(object="SUBNATIONAL/INDONESIA/WOOD_PULP/V3.0.1/INDONESIA_WOOD_PULP_V3_0_1.csv", "trase-results"), delim = ";", quote = "'", skip = 1) %>%
  mutate(SUPPLIER_ID = str_replace(TRASE_ID_2, "ID-CONCESSION-", "H-"),YEAR=TIME)


