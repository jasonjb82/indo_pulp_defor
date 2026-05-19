## ---------------------------------------------------------
## 
## Project: Indonesia pulp deforestation
##
## Purpose of script: Generate IDs for new HTI dataset
##
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Date Created: 2021-06-07
## 
## ---------------------------------------------------------
##
## Notes: Input datasets
##        1) HTI concessions (boundaries) and concession start year - from KLHK
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
library(rgdal)
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
library(tidyfast)
library(patchwork)

## credentials ----------------------------------------------

aws.signature::use_credentials()
bucket <- "trase-storage"
Sys.setenv("AWS_DEFAULT_REGION" = "eu-west-1")

## set working directory -------------------------------------

wdir <- "remote"

## read data -------------------------------------------------

# hti concessions (old)
hti_old <- read_sf(paste0(wdir,"\\01_data\\01_in\\klhk\\IUPHHK_HT_proj.shp")) 

# hti concessions (new)
hti <- read_sf(paste0(wdir,"\\01_data\\01_in\\klhk\\boundaries\\IUPHHK_HTI_20220320.shp"))

## clean data -----------------------------------------------

# reproject to same CRS
crs <- st_crs(hti_old) 
hti_proj <- st_transform(hti,crs)

hti_clean <- hti_proj %>%
  left_join(select(as.data.frame(hti_old),namobj=NAMOBJ,ID),by="namobj") %>%
  mutate(TGL_SK_PROPER = as.Date(as.POSIXct(tgl_sk/1000, origin="1970-01-01",tz = "Asia/Jakarta"))) %>%
  select(FID=FID_1,NAMOBJ=namobj,METADATA=metadata,KODE_PROV=kode_prov,NO_SK=no_sk,TGL_SK=TGL_SK_PROPER,ID) %>%
  arrange(-desc(ID)) %>%
  st_transform(crs = 4326)

hti_no_id <- hti_clean %>%
  filter(is.na(ID)) %>%
  st_drop_geometry()

## export shapefile ----------------------------------------
st_write(hti_clean, paste0(wdir,"\\01_data\\01_in\\klhk\\boundaries\\IUPHHK_HTI_UPDATED_20220320.shp"),delete_layer = TRUE)


