## -----------------------------------------------------------
##
## Purpose of script: Cleaning up HTI concession data scraped from SI-PNBP
##
## Author: Jason Benedict
##
## Date Created: 2020-04-14
## 
## ----------------------------------------------------------

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

# import and clean up data ----

# get list of csv files
file.names <- list.files(path= paste0(wdir,"\\01_data\\01_in\\klhk\\psdh\\01_in\\HTI\\"),pattern = "*.csv",full.names=TRUE)

# table 1
hti_df <- purrr::map_df(file.names, ~read.csv(.x, stringsAsFactors = FALSE)) %>% 
  as_tibble(.name_repair = make_clean_names) %>%
  filter(!is.na(jumlah_satuan)) %>%
  select_if(~!all(is.na(.))) %>%
  mutate(date = str_extract(id, "(?<=Tanggal).*")) %>%
  mutate(date = str_squish(date)) %>%
  mutate(date = dmy(date)) %>%
  select(jenis,satuan,jumlah_satuan,tarif_rp_satuan,jumlah_rp,keterangan,nama_perusahaan,alamat_perusahaan,provinsi,kabupaten,tebangan,lhp,url,date)

# table 2
hti_add_df <- purrr::map_df(file.names, ~read.csv(.x, stringsAsFactors = FALSE)) %>%
  as_tibble(.name_repair = make_clean_names) %>%
  filter(is.na(jumlah_satuan)) %>%
  filter(keterangan != "Keterangan") %>%
  select_if(~!all(is.na(.))) %>%
  select(-no,-id,-jenis,-satuan) %>%
  rename(psdh_tarif_rp = psdh_rp) %>%
  rename(psdh_jumlah_rp = psdh_rp_2) %>%
  rename(dr_tarif_us = dr_us) %>%
  rename(dr_jumlah_us = dr_us_2) %>%
  mutate(date = str_extract(lhp, "(?<=Tanggal).*")) %>%
  mutate(date = str_squish(date)) %>%
  mutate(date = dmy(date)) %>%
  mutate(volume_m3 = ifelse(!is.na(volume_sm) & volume_sm > 0, volume_sm,volume_m3))

# export data to csv
write_csv(hti_df,paste0(wdir,"\\01_data\\01_in\\klhk\\psdh\\02_out\\HTI\\hti_psdh_main.csv"))
write_csv(hti_add_df,paste0(wdir,"\\01_data\\01_in\\klhk\\psdh\\02_out\\HTI\\hti_psdh_add.csv"))
