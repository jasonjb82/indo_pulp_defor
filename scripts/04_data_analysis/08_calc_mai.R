## ---------------------------------------------------------
## 
## Project: Indonesia pulp deforestation
##
## Purpose of script: Calculate MAI for HTI concessions
##
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Date Created: 2022-02-13
## 
## ---------------------------------------------------------
##
## Notes: Input datasets
##        1) 
##
## ---------------------------------------------------------


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Load packages
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(tidyverse)
options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

# library(stringr)
# library(data.table)
# library(naniar)
# library(visdat)
# library(readxl)
# library(tidylog)
# library(data.table)
# library(janitor)
# library(lubridate)
# library(sf)
# library(scales)
# library(aws.s3)
# library(dtplyr)
# library(testthat)
# library(d3.format)
# library(tidyfast)
# library(patchwork)

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## load data -------------------------------------
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
wdir <- "remote"

harvest_csv <- paste0(wdir, "/01_data/02_out/tables/hti_ws_wood_harvest_yr_age.csv")
harvest_df <- read.csv2(harvest_csv, sep = ",")

pw_supply_2022 <- read_excel(paste0(wdir, '\\01_data\\01_in\\wwi\\RPBBI_2022_compiled.xlsx')) %>%
  select(YEAR,SUPPLIER_ID,EXPORTER_ID,VOLUME_M3)

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## clean data -------------------------------------
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
harvest_df <- harvest_df %>% 
  select(order(colnames(.)))

wood_df <- harvest_df %>% 
  select(HARVEST_YEAR, SUPPLIER_ID, VOLUME_M3) %>% 
  mutate(VOLUME_M3 = as.numeric(VOLUME_M3))

harvest_df <- harvest_df %>% 
  select(-VOLUME_M3)

harvest_df <- harvest_df %>% 
  mutate_at(vars(starts_with("X")), ~as.numeric(.))

harvest_df <- harvest_df %>% 
  pivot_longer(cols = starts_with("X"), 
               names_to = "years_since_clear",
               names_prefix = "X",
               values_to = "area")

harvest_df <- harvest_df %>% 
  mutate(years_since_clear = as.integer(years_since_clear)) %>% 
  arrange(SUPPLIER_ID, HARVEST_YEAR, years_since_clear) %>% 
  mutate(last_clear = HARVEST_YEAR - years_since_clear)


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## explore rotation lengths -------------------------------------
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
harvest_year_summary <- harvest_df %>% 
  # filter(HARVEST_YEAR==2020) %>%
  group_by(years_since_clear) %>% 
  summarise(area_sum = sum(area))

harvest_year_summary %>% 
  ggplot(aes(x = years_since_clear, y = area_sum)) +
  geom_line() +
  theme_bw(base_size = 16) +
  xlab("Years since last clearing when harvested") +
  # ylab("Total area harvested in 2019 (ha)")
  ylab("Total area harvested over 2015-2020 (ha)")
  
## NOTE: based on rotation length plot, set max length for rotation
max_rotation <- 7


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Calculate MAI -------------------------------------
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Filter to acacia?

## Adjust long rotations
harvest_df <- harvest_df %>% 
  mutate(grow_y = ifelse((last_clear<2010) & (years_since_clear>=max_rotation), max_rotation, years_since_clear),
         grow_ha_y = area * grow_y)

harvest_df <- harvest_df %>% 
  group_by(SUPPLIER_ID, HARVEST_YEAR) %>% 
  summarise(grow_ha_y = sum(grow_ha_y),
            area = sum(area)) %>% 
  left_join(wood_df, by = c("SUPPLIER_ID", "HARVEST_YEAR")) %>% 
  ungroup()

## Calculate concession by year MAI
harvest_df <- harvest_df %>% 
  mutate(mai = VOLUME_M3 / grow_ha_y)


## Calculate MAI for entire sector
sector_mai <- sum(harvest_df$VOLUME_M3) / sum(harvest_df$grow_ha_y) # m3 / ha / y 

## Calculate annual MAI across sector
year_mai <- harvest_df %>% 
  group_by(HARVEST_YEAR) %>% 
  summarise(grow_ha_y = sum(grow_ha_y),
            area = sum(area),
            VOLUME_M3 = sum(VOLUME_M3)) %>% 
  mutate(year_mai = VOLUME_M3 / grow_ha_y)


## MAI plots
harvest_df %>% 
  filter(mai < 60) %>% 
  ggplot(aes(x = mai)) +
  geom_histogram(bins = 20) +
  geom_vline(xintercept = sector_mai, linetype = "longdash") +
  theme_bw() +
  xlab("Mean annual increment (m3 / ha / y") +
  ylab("Frequency")

year_mai %>% 
  ggplot(aes(x = HARVEST_YEAR, y = area)) +
  geom_line() +
  theme_bw(base_size = 16) + 
  xlab("Harvest year") +
  ylab("Total area harvested (ha)") +
  ylim(c(0, 380000))

year_mai %>% 
  ggplot(aes(x = HARVEST_YEAR, y = VOLUME_M3)) +
  geom_line() +
  theme_bw(base_size = 16) + 
  xlab("Harvest year") +
  ylab("Total volume produced (m3)") +
  ylim(c(0, 45000000))

year_mai %>% 
  ggplot(aes(x = HARVEST_YEAR, y = year_mai)) +
  geom_line() +
  theme_bw(base_size = 15) + 
  xlab("Harvest year") +
  ylab("Mean annual increment (m3/ha/y)") +
  ylim(c(0, 32)) +
  geom_smooth(method = "lm")

# No significant improvement in MAI over the 6 years observed
mod <- lm(year_mai ~ HARVEST_YEAR, data = year_mai)
summary(mod)



##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Calculate MAI -------------------------------------
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Summary of proposed expansions: remote/01_data/01_in/new_capacity/planned_expansions.xlsx"
oki_exp_mt <- 4.2
rapp_exp_mt <- 2
phoenix_exp_mt <- 1.7
total_exp_mt <- oki_exp_mt + rapp_exp_mt + phoenix_exp_mt
baseline_cap_mt <- 9.3 ## TODO: Check this with Brian. Doesn't match (mills$PULP_CAP_2019_MTPY %>% sum())

# Line 102: Together, these three projects would increase the country’s pulp capacity by 91% and, once fully operational, would lead to a concomitant XX m3 increase in the country’s annual demand for pulpwood. 
total_exp_mt
cap_change <- (total_exp_mt / baseline_cap_mt) %>% print()

# Estimate of land demand from capacity expansions
current_wood_demand <- pw_supply_2022 %>% pull(VOLUME_M3) %>%  sum()
new_wood_demand <- (current_wood_demand * cap_change) %>% print()

# new_wood_demand <- 30600000 # m3 / y - taken from Brian's calculations in paper draft. Was for original expansion estimates without PT phoenix
(area_demand <- new_wood_demand / sector_mai) # ha

harvest_df %>% write_csv(paste0(wdir, "/01_data/02_out/tables/hti_mai.csv"))
