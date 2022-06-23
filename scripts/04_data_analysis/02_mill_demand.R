## ---------------------------------------------------------
##
## Purpose of script: Calculate area of planted pulp needed to satisfy a mill's demand
##
## Author: Jason Benedict and Robert Heilmayr
##
## Date Created: 2021-03-29
## 
## ---------------------------------------------------------
##
## Notes: Input data
##   - Total pulpwood planted area in active concessions (David Gaveau's dataset)  
##   - Mill pulp production (WWI)
##   - Flow of pulpwood from concession to mill (RPBBI)
##   
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

## credentials ----------------------------------------------

aws.signature::use_credentials()
bucket <- "trase-storage"
Sys.setenv("AWS_DEFAULT_REGION" = "eu-west-1")

## set working directory -------------------------------------

wdir <- "remote"

## read data and clean ---------------------------------------

# pulp planted area
pulp_area_hti <- read_csv(paste0(wdir, '\\01_data\\02_out\\gee\\pulp_by_year_hti.csv'))

# pulp mill production
pulp_mill_prod <- read_excel(paste0(wdir, '\\01_data\\01_in\\wwi\\PULP_MILL_PRODUCTION.xlsx'))

# wood supply
pulpwood_supply <- read_delim(get_object(object="indonesia/wood_pulp/production/out/PULP_WOOD_SUPPLY_CLEAN_ALL_ALIGNED_2015_2019.csv", bucket),delim=",") 

# 2020 wood supply
pw_supply_2020 <- read_excel(paste0(wdir, '\\01_data\\01_in\\wwi\\RPBBI_2020_compiled.xlsx')) %>%
  select(YEAR,SUPPLIER_ID,EXPORTER_ID,VOLUME_M3)

## clean data ------------------------------------------------

# pulp area in hti by year
pulp_area_clean_hti <- pulp_area_hti %>% 
  select(supplier_id=ID,name = NAMOBJ,starts_with("id_")) %>% 
  pivot_longer(cols = starts_with("id_"),
               names_to = 'year',
               names_prefix = 'id_',
               values_to = 'pulp_area_ha') %>%
  mutate(year = as.double(year))

# mill production by year
mill_prod <- pulp_mill_prod %>%
  group_by(year=YEAR,mill_id=MILL_ID,mill_name=MILL_NAME) %>%
  summarize(pulp_tons = sum(TOTAL_PROD_KG_NET/1000))

# flow of wood supply to mill
ws_flow <- pulpwood_supply %>%
  bind_rows(pw_supply_2020) %>%
  select(year=YEAR,supplier_id=SUPPLIER_ID,mill_id=EXPORTER_ID,volume_m3=VOLUME_M3) %>%
  group_by(year,supplier_id,mill_id) %>%
  summarize(volume_m3 = sum(volume_m3))

## Calculate mill area demand ------------------------------------------------

# Effective yield for concession c in year t
conc_prod <- ws_flow %>% 
  group_by(supplier_id, year) %>% 
  summarise(volume_m3 = sum(volume_m3))

active_hti <- conc_prod %>%
  group_by(supplier_id) %>% 
  summarise(volume_m3 = sum(volume_m3)) %>% 
  filter(volume_m3 > 0) %>% 
  pull(supplier_id)

conc_area <- pulp_area_clean_hti %>% 
  mutate(prod_year = year + 4) %>% 
  select(supplier_id, name, prod_year, pulp_area_ha) %>% 
  filter(prod_year >= 2015,
         prod_year <=2020)


conc_yield = conc_area %>% 
  left_join(conc_prod, by = c("supplier_id", "prod_year" = "year")) %>% 
  mutate(volume_m3 = replace_na(volume_m3, 0),
         pulp_area_ha = na_if(pulp_area_ha, 0),
         active_hti = supplier_id %in% active_hti) %>% 
  filter(active_hti==TRUE) %>% 
  rename(year = prod_year) %>% 
  drop_na()

mean_yield <- conc_yield %>% 
  group_by(supplier_id) %>% 
  summarise(mean_yield = sum(volume_m3) / sum(pulp_area_ha))


# Estimate MAI for APRIL
april_id <- 'M-0004'
april_suppliers <- ws_flow %>% 
  filter(mill_id == april_id) %>% 
  pull(supplier_id) %>% 
  unique()
april_mean_yield <- conc_yield %>% 
  filter(supplier_id %in% april_suppliers) %>% 
  summarise(mean_yield = sum(volume_m3) / sum(pulp_area_ha))


# Gut check - What is aggregate MAI looking across all concessions?
total_area <- conc_yield %>% 
  pull(pulp_area_ha) %>% 
  sum()

total_prod <- conc_yield %>% 
  pull(volume_m3) %>% 
  sum()

ave_mai <- total_prod / total_area


# How does MAI change through time?
annual_yield <- conc_yield %>% 
  group_by(year) %>% 
  summarise(mean_yield = sum(volume_m3) / sum(pulp_area_ha))

annual_yield %>% 
  ggplot(aes(x = year, y = mean_yield)) + 
  geom_line() +
  geom_point() +
  theme_minimal()


# How does MAI change through time for the largest concessions?
top_concessions <- conc_yield %>% 
  filter(year==2019) %>% 
  arrange(desc(pulp_area_ha)) %>% 
  slice(1:10) %>% 
  pull(supplier_id)

top_conc_yield <- conc_yield %>%
  filter(supplier_id %in% top_concessions) %>% 
  mutate(mean_yield = volume_m3 / pulp_area_ha)

top_conc_yield %>% 
  ggplot(aes(x = year, y = mean_yield, color = name)) + 
  geom_line() +
  geom_point() +
  theme_minimal()

# Apply mean concession yield to flows
assumed_yield <- conc_yield %>% 
  select(supplier_id, year) %>% 
  left_join(mean_yield, by = "supplier_id")

area_flow <- ws_flow %>% 
  left_join(assumed_yield, by = c("supplier_id", "year")) %>% 
  mutate(area_demand = volume_m3 / mean_yield)

area_demand <- area_flow %>% 
  group_by(mill_id, year) %>% 
  summarise(area_demand_ha = sum(area_demand, na.rm = TRUE))

# Apply area demand to pulp production
mill_demand_tot <- area_flow %>% 
  group_by(mill_id, year) %>% 
  summarise(volume_m3 = sum(volume_m3))

mill_demand_incl <- area_flow %>%
  drop_na() %>% 
  group_by(mill_id, year) %>% 
  summarise(volume_m3_incl = sum(volume_m3))

mill_demand <- mill_demand_tot %>% 
  left_join(mill_demand_incl, by = c("mill_id", "year")) %>% 
  mutate(shr_incl = volume_m3_incl / volume_m3)
  
mill_demand <- mill_demand %>% 
  left_join(mill_prod, by = c("mill_id", "year")) %>% 
  mutate(pulp_tons_incl = pulp_tons * shr_incl) %>%   
  left_join(area_demand, by = c("mill_id", "year"))


annual_mill_area_intensity <- mill_demand %>% 
  mutate(pulp_area_intensity = (pulp_tons_incl / area_demand_ha))


mill_area_intensity <- mill_demand %>% 
  group_by(mill_id) %>% 
  summarise(pulp_area_intensity = sum(area_demand_ha) / sum(pulp_tons_incl))

area_intensity <- sum(mill_demand$area_demand_ha) / sum(mill_demand$pulp_tons_incl)

oki_prod_2019 <- mill_prod %>% 
  filter(mill_name=="OKI", 
         year == 2019) %>% 
  pull(pulp_tons)
oki_land_demand = area_intensity * oki_prod_2019


# Gut check: Pulp wood to pulp conversion rate
mill_demand <- mill_demand %>% 
  mutate(conv_rate = volume_m3 / pulp_tons)
mill_demand %>% select(conv_rate) %>% summary()

conv_rate <- sum(mill_demand$pulp_tons) / sum(mill_demand$volume_m3)


