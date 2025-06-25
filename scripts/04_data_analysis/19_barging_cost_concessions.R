## ---------------------------------------------------------
## 
## Project: Indonesia pulp deforestation
##
## Purpose of script: Applying contractor rates to OD calculations for specific concessions
##
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Date Created: 2025-06-02
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
library(dlookr)
library(tidyverse)
library(tidylog)
library(janitor)
library(tidyfast)
library(fuzzyjoin)
library(aws.s3)
library(readxl)

'%ni%' <- Negate('%in%') # filter out function

## credentials ----------------------------------------------

aws.signature::use_credentials()
bucket <- "trase-storage"
Sys.setenv("AWS_DEFAULT_REGION" = "eu-west-1")

## set working directory -------------------------------------

wdir <- "remote"

## read data -------------------------------------------------

# centroids to mills
centroid_all_mills_df <- read_csv(paste0(wdir,"\\01_data\\02_out\\tables\\centroids_mills_all_od_output.csv"))

# contractor table values
contr_tbl <- read_csv(paste0(wdir,"\\01_data\\02_out\\tables\\sm_contractor_rates_simplified.csv"))

# grid points for report concessions
grid_conc <- read_csv(paste0(wdir,"\\01_data\\01_in\\ucsb\\transport_cost\\concession_grid_pts.csv"))

# mills
mills <- s3read_using(read_excel, object = "indonesia/wood_pulp/logistics/out/mills/MILLS_EXPORTERS_20200405.xlsx", bucket = bucket)

## clean data and create outputs -----------------------------

barging_tbl <- contr_tbl %>%
  filter(type == "barging")

trucking_tbl <- contr_tbl %>%
  filter(type == "trucking")

barging_costs <- fuzzy_left_join(
  centroid_mills_df, barging_tbl,
  by = c("dist_sea_km" = "dist_1", "dist_sea_km" = "dist_2"),
  match_fun = list(`>=`, `<=`)
) %>%
  select(id,mill,cost_barging=value) %>%
  print()

trucking_costs_sumatra <- fuzzy_left_join(
  centroid_mills_df, trucking_tbl,
  by = c("dist_land_sumatera_km" = "dist_1", "dist_land_sumatera_km" = "dist_2"),
  match_fun = list(`>=`, `<=`)
) %>%
  select(id,mill,cost_trucking_suma=value)

trucking_costs_kalimantan <- fuzzy_left_join(
  centroid_mills_df, trucking_tbl,
  by = c("dist_land_kalimantan_km" = "dist_1", "dist_land_kalimantan_km" = "dist_2"),
  match_fun = list(`>=`, `<=`)
) %>%
  select(id,mill,cost_trucking_kali=value)

cost_tbl_ext <- barging_costs %>%
  left_join(trucking_costs_sumatra,by="id") %>%
  left_join(trucking_costs_kalimantan,by="id") %>%
  select(id,mill=mill.x,cost_trucking_suma,cost_trucking_kali,cost_barging) %>%
  replace(is.na(.), 0) %>%
  mutate(cost_trucking_suma_usd_perton = cost_trucking_suma/9162.27,
         cost_trucking_kali_usd_perton = cost_trucking_kali/9164.27,
         cost_barging_usd_perton = cost_barging/9164.2) %>%
  mutate(total_cost = cost_trucking_suma+cost_trucking_kali+cost_barging) %>%
  select(id,mill,cost_trucking_suma_usd_perton,cost_trucking_kali_usd_perton,cost_barging_usd_perton) %>%
  print()

# subset to 3 concessions
conc_costs <- grid_conc %>%
  left_join(cost_tbl_ext,by=c("pixel_id"="id")) %>%
  left_join(select(mills,MILL_ID,MILL_NAME),by=c("mill"="MILL_ID")) %>%
  group_by(conc,MILL_NAME) %>%
  slice_min(cost_barging_usd_perton, n = 1, with_ties = FALSE) %>%
  select(conc,mill_name=MILL_NAME,cost_barging_usd_perton) %>%
  filter(mill_name == "INDAH KIAT" | mill_name == "APRIL") %>%
  print()
