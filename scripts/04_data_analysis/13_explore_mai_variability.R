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
library(janitor)
options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

# library(stringr)
# library(data.table)
# library(naniar)
# library(visdat)
# library(readxl)
# library(tidylog)
# library(data.table)
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

mai_csv <- paste0(wdir, "/01_data/02_out/tables/hti_mai.csv")
mai_df <- read.csv2(harvest_csv, sep = ",") %>% 
  clean_names() %>% 
  as_tibble() %>% 
  rename(year = harvest_year) %>% 
  mutate(mai = as.numeric(mai),
         grow_ha_y = as.numeric(grow_ha_y),
         volume_m3 = as.numeric(volume_m3))

hti_areas_csv <- paste0(wdir, "/01_data/02_out/tables/hti_concessions_area_calcs.csv")
hti_areas <- read.csv2(hti_areas_csv, sep = ",") %>% 
  clean_names() %>% 
  as_tibble() %>% 
  mutate(burn_plantation_area_ha = replace_na(burn_plantation_area_ha, 0))


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Data exploration -------------------------------------
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hti_areas <- hti_areas %>% 
  mutate(peat_shr = pulp_on_peat_area_ha / pulp_area_ha)

burn_areas <- hti_areas %>% 
  group_by(supplier_id) %>% 
  summarise(total_burn_area = sum(burn_plantation_area_ha))

hti_burn_2015 <- hti_areas %>% 
  filter(year == 2015,
         burn_plantation_area_ha>100) %>% 
  pull(supplier_id)


hti_burn_2019 <- hti_areas %>% 
  filter(year == 2019,
         burn_plantation_area_ha>100) %>% 
  pull(supplier_id)


hti_burn_2017 <- hti_areas %>% 
  filter(year == 2016,
         burn_plantation_area_ha>100) %>% 
  pull(supplier_id)

mai_df <- mai_df %>% 
  left_join(hti_areas %>% select(supplier_id, year, peat_shr, pulp_area_ha), by = c("supplier_id", "year")) %>% 
  left_join(burn_areas, by = "supplier_id") %>% 
  mutate(burn_shr = total_burn_area / pulp_area_ha)

mai_df <- mai_df %>% 
  mutate(low_peat = peat_shr < 0.01,
         high_peat = peat_shr == 1,
         high_burn = burn_shr > 0.1) 


mai_df %>% 
  group_by(high_peat) %>% 
  summarize()


peat_mai <- mai_df %>% 
  mutate(peat_shr_grp = cut(peat_shr, breaks = c(-1, 0.01, 0.5,0.99, 2))) %>% 
  group_by(peat_shr_grp, year) %>% 
  summarise(grow_ha_y = sum(grow_ha_y),
            volume_m3 = sum(volume_m3)) %>% 
  mutate(mai = volume_m3 / grow_ha_y) %>% 
  print()


burn_mai <- mai_df %>% 
  mutate(burn_2015 = supplier_id %in% hti_burn_2015) %>% 
  mutate(burn_2019 = supplier_id %in% hti_burn_2019) %>% 
  mutate(burn_2017 = supplier_id %in% hti_burn_2017) %>% 
  group_by(burn_2017, year) %>% 
  summarise(grow_ha_y = sum(grow_ha_y),
            volume_m3 = sum(volume_m3)) %>% 
  mutate(mai = volume_m3 / grow_ha_y) %>% 
  print()





# these are still really preliminary results, but just as a heads up @Brian Orland and @Jason - i started merging some of our delivered mai estimates with burn area / peat area data that jason pulled. I started by looking whether there's clear evidence that the pulp plantations on peatlands are producing less. Strangely enough, I'm seeing the lowest mai on HTI with no plantings on peat (17 m3/ha/y), but the highest mai is on HTI where 100% of plantations are on peat (27 m3/ha/y). In addition, I'm not seeing any clear evidence that the mai is dropping off on those HTI that are entirely planted on peatlands. Will keep probing to ensure these are real results, but may make it hard to use these data to argue that mai is likely to decline on peatlands...

