## ---------------------------------------------------------
## 
## Project: Indonesia pulp deforestation
##
## Purpose of script: 
##
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Date Created: 2021-06-07
## 
## ---------------------------------------------------------
##
## Notes: 
##
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

## credentials ----------------------------------------------

aws.signature::use_credentials()
bucket <- "trase-storage"
Sys.setenv("AWS_DEFAULT_REGION" = "eu-west-1")

## set working directory -------------------------------------

wdir <- "remote"

## read data -------------------------------------------------

bol_2021 <- s3read_using(read_excel, object = "indonesia/trade/bol/in/2021/INDONESIA_EXPORT_MUL_HS_JAN21_DEC21.xlsx", bucket = bucket)

## filter and export ------------------------------------------

pulp_2021 <- bol_2021 %>%
  filter(stringr::str_detect(HS_CODE, '47032900|47020000'))

write_csv(pulp_2021,paste0(wdir,"/01_data/01_in/trade/INDONESIA_PULP_EXPORT_RAW_2021.csv"))

