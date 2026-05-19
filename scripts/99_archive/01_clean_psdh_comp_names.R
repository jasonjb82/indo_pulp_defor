## ---------------------------
##
## Purpose of script: Get clean PDSH company names to align with HTI concession names
##
## Author: Jason Benedict
##
## Date Created: 2020-05-18
## 
## ---------------------------
##
## Notes:
##   
##
## ---------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------

## load packages

library(tidyverse)
library(readxl)
library(tidylog)
library(data.table)
library(janitor)
library(lubridate)
library(myutil)
library(sf)
library(aws.s3)

## credentials ---------------

aws.signature::use_credentials()
bucket <- "trase-storage"
Sys.setenv("AWS_DEFAULT_REGION" = "eu-west-1")

## read data -----------------

# psdh data
psdh_main <- read_delim(get_object(object="indonesia/wood_pulp/production/in/psdh/hti_psdh_main.csv", bucket), delim = ",")
psdh_add <- read_delim(get_object(object="indonesia/wood_pulp/production/in/psdh/hti_psdh_add.csv", bucket), delim = ",")
psdh_align <- read_delim(get_object(object="indonesia/wood_pulp/companies/align_names/ALIGNED_NAMES_PSDH.csv", bucket,"check_region" = TRUE), delim = ",")

## merge data ----------------

psdh_comb <- psdh_main %>%
  select(nama_perusahaan,kabupaten) %>%
  distinct() %>%
  bind_rows(psdh_add) %>%
  select(nama_perusahaan,kabupaten) %>%
  distinct() %>%
  mutate(company_clean = nama_perusahaan %>%
         str_replace_all(
           c(
             "PT." = "",
             "IUPHHK-HTI AN." = "",
             "IUPHHK-HTI" = "",
             "IUPHHK-HT" = "",
             "IUPHHK-" = "",
             "IUPHHK - HTI" = "",
             "CV."= ""))) %>%
  mutate(company_clean = str_squish(company_clean)) %>%
  left_join(select(psdh_align,id,nama_perusahaan,kabupaten),by=c("nama_perusahaan","kabupaten"))


## write data -------------
aws.s3::s3write_using(psdh_merge, write_delim,",", 
                      object = "indonesia/wood_pulp/companies/align_names/PSDH_HTI_ID_TO_ALIGN.csv",
                      bucket = bucket) 
