## ---------------------------------------------------------
## 
## Project: Indonesia pulp deforestation
##
## Purpose of script: Clean ITP harvest data to get year and age of harvest - merge with wood supply
##
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Date Created: 2022-02-10
## 
## ---------------------------------------------------------
##
## Notes: Input datasets
##        1) HTI concessions (boundaries) and concession start year - from KLHK
##        2) Gaveau harvested areas (2010 - 2021)
##        3) Wood supply data - RPBBI (cleaned and checked by UCSB & WWI)
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

# itp harvest years
itp_hv <- read_sf(paste0(wdir,"\\01_data\\01_in\\gaveau\\IDN_ITPHarvesting_V20220208\\IDN_ITPHarvesting_V20220208.shp"))

# hti concessions
hti <- read_sf(paste0(wdir,"\\01_data\\01_in\\klhk\\IUPHHK_HT_proj.shp"))

# wood supply
ws <- read_delim(get_object(object="indonesia/wood_pulp/production/out/PULP_WOOD_SUPPLY_CLEAN_ALL_ALIGNED_2015_2019.csv", bucket), delim = ",")

# supplier groups
groups <- read_csv(paste0(wdir,"\\01_data\\01_in\\wwi\\ALIGNED_NAMES_GROUP_HTI.csv"))
  
# psdh
psdh <- read_csv(paste0(wdir,"\\01_data\\01_in\\klhk\\psdh\\02_out\\PSDH_HTI_ID_COMBINED.csv"))

## clean up data ---------------------------------------------
itp_hv <- st_make_valid(itp_hv) 
itp_hv <- itp_hv %>% 
  mutate(block_id = paste0("B", as.character(str_pad(OBJECTID, 6, pad = "0"))))
itp_hv_proj <- st_transform(itp_hv, crs = st_crs(hti)) %>% st_make_valid() 

# intersect to get associated HTI concession
hti_itp_hv <- st_intersection(hti,itp_hv_proj) %>% mutate(area_m2 = st_area(.))

# create table and clean up
hti_itp_hv_df <- hti_itp_hv %>%
  st_drop_geometry() %>%
  select(block_id, supplier_id=ID, estab_year=yearint, Class,Harvest1,Harvest2,Harvest3,Ket,area_m2) %>%
  mutate(area_ha = as.double(area_m2*0.0001)) %>%
  select(-area_m2)
# %>%
  #pivot_longer(c(-SUPPLIER_ID,-yearint,-Ket,-year,-Class,-area_ha), names_to="var", values_to="vals") 
  # filter(is.na(Ket)) %>% ## JASON - Why are we filtering these?
  # mutate(hv_age1 = Harvest1 - year,hv_age2 = Harvest2 - Harvest1, hv_age3 = Harvest3 - Harvest2) %>%
  # mutate(hv_age1 = ifelse(hv_age1 <0,0,hv_age1),hv_age2 = ifelse(hv_age2 <0,0,hv_age2),hv_age3 = ifelse(hv_age3 <0,0,hv_age3))
  # mutate(hv_age3 = ifelse(hv_age3 > 2000, Harvest3 - Harvest1,hv_age3),hv_age2 = ifelse(hv_age2 > 2000, Harvest2 - yearint,hv_age2))

## NOTE: there are some concessions for which Harvest1 ==0 and Harvest2 > 0, same for harvests 2 and 3
# hti_itp_hv_df %>% filter(Harvest1==0, Harvest2>0)
# hti_itp_hv_df %>% filter(Harvest2==0, Harvest3>0)
## Current solution: Shift these up; But 
## TODO: double check with David that this is ok
hti_itp_hv_df <- hti_itp_hv_df %>% 
  mutate(error_harvest1 = (Harvest1==0) & (Harvest2>0),
         Harvest1 = ifelse(error_harvest1, Harvest2, Harvest1),
         Harvest2 = ifelse(error_harvest1, 0, Harvest2),
         error_harvest2 = (Harvest2==0) & (Harvest3>0),
         Harvest2 = ifelse(error_harvest2, Harvest3, Harvest2),
         Harvest3 = ifelse(error_harvest2, 0, Harvest3)) %>% 
  select(-c(error_harvest1, error_harvest2))

## NOTE: There are some pulp plantations established after their first observed harvest.
# hti_itp_hv_df %>% filter(yearint>Harvest1, Harvest1!=0)
## Current solution: Assume the establishment year is wrong - use maximum rotation length by reassigning establishment year back to 2000
## TODO: double check with David that this is ok
hti_itp_hv_df <- hti_itp_hv_df %>% 
  mutate(error_estab = (estab_year>Harvest1) & (Harvest1!=0),
         estab_year = ifelse(error_estab, 2000, estab_year)) %>% 
  select(-error_estab)

hti_itp_hv_df <- hti_itp_hv_df %>% 
  mutate(Harvest1 = replace(Harvest1, Harvest1==0, NA),
         Harvest2 = replace(Harvest2, Harvest2==0, NA),
         Harvest3 = replace(Harvest3, Harvest3==0, NA),
         hv_age1 = Harvest1 - estab_year,
         hv_age2 = Harvest2 - Harvest1, 
         hv_age3 = Harvest3 - Harvest2)

hti_itp_hv_df_long <- hti_itp_hv_df %>% 
  pivot_longer(cols = starts_with("Harvest"), 
               names_to = "rotation", names_prefix = "Harvest", values_to = "harvest_year") %>% 
  select(block_id, supplier_id, estab_year, rotation, harvest_year, area_ha) %>% 
  drop_na()
  
rot_length <- hti_itp_hv_df %>% 
  pivot_longer(cols = starts_with("hv_age"), 
               names_to = "rotation", names_prefix = "hv_age", values_to = "rotation_length") %>% 
  select(block_id, rotation, rotation_length) %>% 
  drop_na()

fire_flag <- hti_itp_hv_df %>% 
  mutate(fire_flag = !is.na(Ket)) %>% 
  select(block_id, fire_flag)

harvest_df <- hti_itp_hv_df_long %>% 
  left_join(rot_length, by = c("block_id", "rotation")) %>% 
  left_join(fire_flag, by = "block_id")


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## explore rotation lengths -------------------------------------
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
observed_harvests <- harvest_df %>% 
  filter(rotation>1) %>% 
  group_by(rotation_length) %>% 
  summarize(area_sum = sum(area_ha)) %>% 
  print()

observed_harvests %>% 
  ggplot(aes(x = rotation_length, y = area_sum)) +
  geom_line() + 
  theme_bw()

# Proportion of harvests occurring in specific periods
total_harvests <- observed_harvests %>% pull(area_sum) %>% sum()
prop_4_6 <- ((observed_harvests %>% filter(rotation_length >= 4, rotation_length <= 6) %>% pull(area_sum) %>% sum()) / total_harvests) %>% print()
prop_5 <- ((observed_harvests %>% filter(rotation_length == 5) %>% pull(area_sum) %>% sum()) / total_harvests) %>% print()
prop_6 <- ((observed_harvests %>% filter(rotation_length <= 6) %>% pull(area_sum) %>% sum()) / total_harvests) %>% print()
prop_7 <- ((observed_harvests %>% filter(rotation_length <= 7) %>% pull(area_sum) %>% sum()) / total_harvests) %>% print()


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Summarize ha-y harvested in each concession in each year -------------------------------------
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
max_rotation <- 7

concession_harvests <- harvest_df %>% 
  mutate(impute_flag = (rotation==1) & (rotation_length > 7),
         rotation_length = ifelse(impute_flag, max_rotation, rotation_length),
         ha_y = area_ha * rotation_length) %>% 
  group_by(supplier_id, harvest_year) %>% 
  summarise(ha_y = sum(ha_y),
            impute_flag = max(impute_flag),
            fire_flag = max(fire_flag))


# # first year of harvest
# hti_hv1_areas <- hti_itp_hv_df %>%
#   select(SUPPLIER_ID,HARVEST_YEAR=Harvest1,AGE=hv_age1,AREA_HA=area_ha) %>%
#   filter(HARVEST_YEAR > 0)
# 
# # second year of harvest
# hti_hv2_areas <- hti_itp_hv_df %>%
#   select(SUPPLIER_ID,HARVEST_YEAR=Harvest2,AGE=hv_age2,AREA_HA=area_ha) %>%
#   filter(HARVEST_YEAR > 0)
# 
# # third year of harvest
# hti_hv3_areas <- hti_itp_hv_df %>%
#   select(SUPPLIER_ID,HARVEST_YEAR=Harvest3,AGE=hv_age3,AREA_HA=area_ha) %>%
#   filter(HARVEST_YEAR > 0)
# 
# # merge harvest areas by year and age
# hti_hv_areas_yr <- hti_hv1_areas %>%
#   bind_rows(hti_hv2_areas) %>%
#   bind_rows(hti_hv3_areas) %>%
#   group_by(SUPPLIER_ID,HARVEST_YEAR,AGE) %>%
#   summarize(AREA_HA = sum(AREA_HA)) %>%
#   pivot_wider(names_from = AGE, values_from = AREA_HA,values_fill = 0)
#                
# # wood supply by supplier
# ws_all <- ws %>%
#   bind_rows(ws_2020) %>%
#   group_by(SUPPLIER_ID,YEAR) %>%
#   summarize(VOLUME_M3 = sum(VOLUME_M3)) %>%
#   left_join(select(groups,group,id),by=c("SUPPLIER_ID"="id"))
# 
# # join with wood supply
# hti_harvest_areas_yr_ws <- hti_hv_areas_yr %>%
#   left_join(select(ws_all,GROUP=group,SUPPLIER_ID,HARVEST_YEAR=YEAR,VOLUME_M3),by=c("SUPPLIER_ID","HARVEST_YEAR")) %>%
#   drop_na(VOLUME_M3) 
# 
# 
# ## getting wood species composition -----------------------------
# 
# wood_type_hti <- psdh %>%
#   select(HTI_ID,COMPANY_CLEAN,VOLUME_M3,YEAR,DESCRIPTION,TYPE) %>%
#   mutate(TYPE = ifelse(TYPE == "ACASIA" | TYPE == "EKALIPTUS", TYPE,"OTHERS")) %>%
#   mutate(TYPE = case_when(
#     TYPE == "ACASIA"  ~ "ACACIA",
#     TYPE == "EKALIPTUS" ~ "EUCALYPTUS",
#     TRUE ~ "OTHERS")) %>%
#   filter(YEAR < 2020 & YEAR > 2016) %>% # data for 2020 still incomplete
#   group_by(HTI_ID,COMPANY_CLEAN,TYPE) %>%
#   summarize(VOLUME_M3 = sum(VOLUME_M3)) %>%
#   group_by(HTI_ID,COMPANY_CLEAN) %>%
#   mutate(PC_VOL = prop.table(VOLUME_M3)*100) %>%
#   arrange(HTI_ID) %>%
#   select(-VOLUME_M3) %>%
#   pivot_wider(names_from=TYPE,values_from=c(PC_VOL)) 

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Export to csv -------------------------------------
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
write_csv(concession_harvests,paste0(wdir,"\\01_data\\02_out\\tables\\hti_harvest_yr.csv"))


# write_csv(hti_harvest_areas_yr_ws,paste0(wdir,"\\01_data\\02_out\\tables\\hti_ws_wood_harvest_yr_age.csv"))
# write_csv(wood_type_hti,paste0(wdir,"\\01_data\\02_out\\tables\\hti_wood_types_psdh.csv"))
