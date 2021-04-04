## ---------------------------------------------------------
##
## Purpose of script: Get table of samples with HTI IDs
##
## Author: Jason Benedict
##
## Date Created: 2021-03-29
## 
## ---------------------------------------------------------
##
## Notes: Input data
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

## read data -------------------------------------------------

# sample file
samples <- read_sf(paste0(wdir,"\\01_data\\02_out\\samples\\samples_hti.shp"))

# hti concessions
hti <- read_sf(paste0(wdir,"\\01_data\\01_in\\klhk\\IUPHHK_HT_proj.shp"))

## intersect to get hti IDs ----------------------------------

hti_poly <- hti %>%
  select(ID) %>%
  st_transform(st_crs(samples)) %>% 
  st_cast("POLYGON")

# equal area proj
system.time({
  samples_hti = st_join(samples,hti_poly)
})

## clean up data ---------------------------------------------

samples_hti_table <- samples_hti %>%
  st_drop_geometry()

# check if na's exist
samples_hti_match_na <- samples_hti_table %>%
  filter(is.na(ID))

## export to csv ---------------------------------------------
write_excel_csv(samples_hti_table,paste0(wdir,"\\01_data\\02_out\\tables\\samples_hti_id.csv"))
