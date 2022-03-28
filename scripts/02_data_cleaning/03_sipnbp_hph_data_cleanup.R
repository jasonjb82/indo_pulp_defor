## -----------------------------------------------------------
##
## Purpose of script: Cleaning up HPH (HA) concession data scraped from SI-PNBP
##
## Author: Jason Benedict
##
## Date Created: 2022-03-22
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
file.names <- list.files(path= paste0(wdir,"\\01_data\\01_in\\klhk\\psdh\\01_in\\HPH\\"),pattern = "*.xlsx",full.names=TRUE)

# clean up table
hph_df <- purrr::map_df(file.names, ~read_excel(.x)) %>% 
  as_tibble(.name_repair = make_clean_names) %>%
  select_if(~!all(is.na(.))) %>%
  mutate(date = str_extract(lhp, "(?<=Tanggal).*")) %>%
  mutate(date = str_squish(date)) %>%
  separate(date, c("day", "month","year"), " ") %>%
  mutate(
      month_en = case_when(
      month == "Januari"  ~ "January",
      month == "Februari"  ~ "February",
      month == "Maret"  ~ "March",
      month == "April"  ~ "April",
      month == "Mei"  ~ "May",
      month == "Juni"  ~ "June",
      month == "Juli"  ~ "July",
      month == "Agustus"  ~ "August",
      month == "September"  ~ "September",
      month == "Oktober"  ~ "October",
      month == "Nopember"  ~ "November",
      month == "Desember"  ~ "December",
      TRUE ~ month)
  ) %>%
  mutate(date_clean = paste0(day," ",month_en," ",year)) %>%
  mutate(date = dmy(date_clean), year = year(date)) %>%
  select(nama_perusahaan, alamat_perusahaan,provinsi,kabupaten,lhp,kelompok_jenis,jumlah_batang,volume_m3,
         psdh_rp_tarif_perm3,psdh_rp_jumlah,dr_usd_tarif_perm3,dr_usd_jumlah,keterangan,url,date,year)


# export data to csv
write_csv(hph_df,paste0(wdir,"\\01_data\\01_in\\klhk\\psdh\\02_out\\HPH\\HPH_CLEAN_MERGE.csv"))
