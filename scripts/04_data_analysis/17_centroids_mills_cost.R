## ---------------------------------------------------------
## 
## Project: Indonesia pulp deforestation
##
## Purpose of script: Applying contractor rates to OD calculations
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

'%ni%' <- Negate('%in%') # filter out function

## set working directory -------------------------------------

wdir <- "remote"

## read data -------------------------------------------------

# centroids to mills
centroid_mills_df <- read_csv(paste0(wdir,"\\01_data\\02_out\\tables\\centroids_mills_od_output.csv"))

# contractor table values
contr_tbl <- read_csv(paste0(wdir,"\\01_data\\02_out\\tables\\sm_contractor_rates_simplified.csv"))

## merge tables ----------------------------------------------

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

cost_tbl <- barging_costs %>%
  left_join(trucking_costs_sumatra,by="id") %>%
  left_join(trucking_costs_kalimantan,by="id") %>%
  select(id,mill=mill.x,cost_trucking_suma,cost_trucking_kali,cost_barging) %>%
  replace(is.na(.), 0) %>%
  mutate(total_cost = cost_trucking_suma+cost_trucking_kali+cost_barging) %>%
  mutate(cost_usd_perton = total_cost/9164.27) %>%
  select(id,mill,cost_usd_perton) %>%
  print()

# add annual columns
cost_annual_tbl <- cost_tbl %>%
  expand_grid(year = 2000:2020)

## export to csv --------------------------------------

write_csv(cost_tbl,paste0(wdir,"\\01_data\\02_out\\tables\\centroids_mills_cost.csv"))
