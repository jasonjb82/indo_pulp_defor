## ---------------------------------------------------------
## 
## Project: Indonesia pulp deforestation
##
## Purpose of script: Create transport cost matrix for regression analysis
##
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Date Created: 2025-03-18
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
library(sf)
library(tidyverse)
library(tidylog)
library(janitor)
library(lubridate)
library(scales)
library(dtplyr)
library(d3.format) # to install: devtools::install_github("dreamRs/d3.format")
library(tidyfast)
library(concordance)
library(extrafont)
library(showtext)
library(khroma) # palettes for color blindness

'%ni%' <- Negate('%in%') # filter out function

## set working directory -------------------------------------

wdir <- "remote"

## read data -------------------------------------------------

# port to port distances
pp_df <- read_csv(paste0(wdir,"\\01_data\\01_in\\ucsb\\idn_port_port_od.csv"))

# ports
ports_df <- read_csv(paste0(wdir,"\\01_data\\01_in\\ucsb\\PORT_DICTIONARY_20231213.csv"))

# islands
islands_df <- read_sf(paste0(wdir,"\\01_data\\01_in\\klhk\\KLHK_Kelompok_Pulau_20210308.shp"))

## clean data ------------------------------------------------

ports_clean_df <- ports_df %>%
  select(ID,PORT,TYPE,LATITUDE,LONGITUDE) %>%
  distinct() %>%
  filter(TYPE == "DOMESTIC LOADING") %>%
  st_as_sf(coords=c(5,4)) %>%
  st_set_crs(st_crs(islands_df)) %>%
  st_intersection(islands_df) %>%
  st_drop_geometry() %>%
  select(-kode_pulau,-objectid,-eco_regn,-kawasan) %>%
  filter(pulau %in% c("SUMATERA","KALIMANTAN")) %>%
  print()

write_csv(ports_clean_df,paste0(wdir,"\\01_data\\01_in\\ucsb\\ports_kalisuma.csv"))
