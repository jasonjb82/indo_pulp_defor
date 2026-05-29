## ---------------------------------------------------------
## 
## Project: Indonesia pulp deforestation
##
## Purpose of script: Merge pulpwood supply data (2015-2022)
##
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Date Created: 2022-02-10
## 
## ---------------------------------------------------------
##
## Notes: Input datasets
##        1) Pulp mills - Compiled for Trase (by UCSB & WWI)
##        2) Wood supply data - RPBBI (cleaned and checked by UCSB & WWI)
##
## ---------------------------------------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------------------------------------

### Load packages
library(stringr)
library(tidyverse)
library(readxl)
library(tidylog)
library(janitor)
library(lubridate)
library(scales)

## set working directory -------------------------------------

wdir <- "remote"

## read data -------------------------------------------------

# wood supply
ws <-read_excel(paste0(wdir, '/01_data/01_in/wwi/RPBBI_2015_2019_compiled.xlsx')) %>%
  select(YEAR,SUPPLIER_ID,EXPORTER_ID,VOLUME_M3)

# supplier groups
groups <- read_csv(paste0(wdir,"/01_data/01_in/wwi/ALIGNED_NAMES_GROUP_HTI.csv"))

# mills
mills <- read_excel(paste0(wdir, "/01_data/01_in/wwi/MILLS_EXPORTERS_20200405.xlsx"))

## aggregate data -------------------------------------------

ws <-read_excel(paste0(wdir, '/01_data/01_in/wwi/RPBBI_2015_2019_compiled.xlsx')) %>%
  select(YEAR,SUPPLIER_ID,EXPORTER_ID,TYPE,VOLUME_M3)

# wood supply (2020)
ws_2020 <- read_excel(paste0(wdir,"/01_data/01_in/wwi/RPBBI_2020_compiled.xlsx")) %>%
  select(YEAR,SUPPLIER_ID,EXPORTER_ID,TYPE,VOLUME_M3)

# wood supply (2021)
ws_2021 <- read_excel(paste0(wdir,"/01_data/01_in/wwi/RPBBI_2021_compiled.xlsx")) %>%
  select(YEAR,SUPPLIER_ID,EXPORTER_ID,TYPE,VOLUME_M3) 

# wood supply (2021)
ws_2022 <- read_excel(paste0(wdir,"/01_data/01_in/wwi/RPBBI_2022_compiled.xlsx")) %>%
  select(YEAR,SUPPLIER_ID,EXPORTER_ID,TYPE,VOLUME_M3) 

# pulp mills
mills_tbl <- mills %>%
  select(MILL_ID,MILL_GROUP,MILL_NAME) %>%
  distinct()

# wood supply by supplier
ws_all <- ws %>%
  bind_rows(ws_2020) %>%
  bind_rows(ws_2021) %>%
  bind_rows(ws_2022) %>%
  group_by(YEAR,SUPPLIER_ID,EXPORTER_ID,TYPE) %>%
  summarize(VOLUME_M3 = sum(VOLUME_M3)) %>%
  left_join(select(groups,SUPPLIER_GROUP=group,SUPPLIER_ID=id,SUPPLIER_NAME=company_clean),by="SUPPLIER_ID") %>%
  left_join(select(mills_tbl,EXPORTER_ID=MILL_ID,EXPORTER_GROUP=MILL_GROUP,MILL_NAME),by="EXPORTER_ID") %>%
  select(YEAR,SUPPLIER_ID,EXPORTER_ID,TYPE,VOLUME_M3) 

## export to csv ---------------------------------------------
write_csv(ws_all,paste0(wdir,"/01_data/02_out/tables/ws_merge_clean_2015_2022.csv"))
