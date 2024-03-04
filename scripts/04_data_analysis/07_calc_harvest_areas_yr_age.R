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
         Harvest3 = ifelse(error_harvest2, 0, Harvest3))

## NOTE: There are some pulp plantations established after their first observed harvest.
# hti_itp_hv_df %>% filter(yearint>Harvest1, Harvest1!=0)
## Current solution: Assume the establishment year is wrong - Shift up all harvests
## TODO: double check with David that this is ok
hti_itp_hv_df <- hti_itp_hv_df %>% 
  mutate(error_estab = (estab_year>=Harvest1) & (Harvest1!=0),
         estab_year = ifelse(error_estab, Harvest1, estab_year),
         Harvest1 = ifelse(error_estab, Harvest2, Harvest1),
         Harvest2 = ifelse(error_estab, Harvest3, Harvest2),
         Harvest3 = ifelse(error_estab, 0, Harvest3))

# Summarize frequency of edits made in above two steps
hti_itp_hv_df %>% 
  group_by(error_harvest1, error_harvest2, error_estab) %>% 
  summarize(area_n = sum(area_ha)) %>% 
  ungroup() %>% 
  mutate(freq = prop.table(area_n))

hti_itp_hv_df <- hti_itp_hv_df %>% 
  select(-c(error_harvest1, error_harvest2, error_estab))


# Calculate harvest rotation lengths
hti_itp_hv_df <- hti_itp_hv_df %>% 
  mutate(Harvest1 = replace(Harvest1, Harvest1==0, NA),
         Harvest2 = replace(Harvest2, Harvest2==0, NA),
         Harvest3 = replace(Harvest3, Harvest3==0, NA),
         hv_age1 = Harvest1 - estab_year,
         hv_age2 = Harvest2 - Harvest1, 
         hv_age3 = Harvest3 - Harvest2)


# Summarize harvests
hti_itp_hv_df %>% 
  group_by(Harvest1) %>% 
  summarize(area_ha = sum(area_ha)) %>% 
  mutate(freq = prop.table(area_ha))

hti_itp_hv_df %>% 
  group_by(Harvest2) %>% 
  summarize(area_ha = sum(area_ha)) %>% 
  mutate(freq = prop.table(area_ha))

hti_itp_hv_df %>% 
  group_by(Harvest3) %>% 
  summarize(area_ha = sum(area_ha)) %>% 
  mutate(freq = prop.table(area_ha))

hti_itp_hv_df <- hti_itp_hv_df %>% 
  filter(!is.na(Harvest1)) # Can ignore rows where there are no recorded harvests

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## fix burns -------------------------------------
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hti_itp_hv_df <- hti_itp_hv_df %>% 
  mutate(burn_year = str_extract_all(Ket, "[0-9]{4}")) %>% 
  unnest_wider(burn_year, names_sep = "_") %>% 
  mutate(burn_year_1 = as.numeric(burn_year_1),
         burn_year_2 = as.numeric(burn_year_2),
         burn_year_3 = as.numeric(burn_year_3),
         burn_flag = str_detect(Ket, "Burn") | str_detect(Ket, "Burm"),
         burn_flag = replace_na(burn_flag, FALSE))
  

burned_rows <- hti_itp_hv_df %>% 
  filter(burn_flag,
         !is.na(burn_year_1))   # If burn didn't have assigned year, can't fix records. Also, seems like these rows might just be flagging burn year in the Harvest columns? Confirm with david

unburned_rows <- hti_itp_hv_df %>% 
  filter(!burn_flag)

# Identify and remove invalid harvests that occur at the same time as a fire
id_invalid_harvests <- function(rot_end, b1, b2, b3){
  burn_list <- c(b1, b1 + 1, b2, b2 + 1, b3, b3 + 1)
  burned_harv <- (rot_end %in% burn_list)
  burned_harv <- ifelse(is.na(rot_end), NA, burned_harv)
  return(burned_harv)
}

burned_rows <- burned_rows %>% 
  mutate(burned_harv1 = pmap_lgl(.l = list(Harvest1, burn_year_1, burn_year_2, burn_year_3), 
                                 .f = id_invalid_harvests),
         burned_harv2 = pmap_lgl(.l = list(Harvest2, burn_year_1, burn_year_2, burn_year_3), 
                                 .f = id_invalid_harvests),
         burned_harv3 =  pmap_lgl(.l = list(Harvest3, burn_year_1, burn_year_2, burn_year_3), 
                                  .f = id_invalid_harvests),
         Harvest1 = ifelse(burned_harv1, NA, Harvest1),
         Harvest2 = ifelse(burned_harv2, NA, Harvest2),
         Harvest3 = ifelse(burned_harv3, NA, Harvest3))


# Shift harvests to fix harvests that were removed due to burns
burned_rows <- burned_rows %>% 
  mutate(error_harvest1 = (is.na(Harvest1) & !is.na(Harvest2)),
         Harvest1 = ifelse(error_harvest1, Harvest2, Harvest1),
         Harvest2 = ifelse(error_harvest1, Harvest3, Harvest2),
         Harvest3 = ifelse(error_harvest1, NA, Harvest3),
         error_harvest2 = (is.na(Harvest2) & !is.na(Harvest3)),
         Harvest2 = ifelse(error_harvest2, Harvest3, Harvest2),
         Harvest3 = ifelse(error_harvest2, NA, Harvest3),
         hv_age1 = Harvest1 - estab_year,
         hv_age2 = Harvest2 - Harvest1,
         hv_age3 = Harvest3 - Harvest2)

# Adjust harvests that were interrupted by a fire
id_interrupted_harvests <- function(rot_end, rot_length, b1, b2, b3){
  rot_start <- rot_end - rot_length
  burn_list <- c(b1, b2, b3)
  burn_interruptions <- ((burn_list > rot_start) & (burn_list < rot_end))
  new_rot_start <- max(burn_list[burn_interruptions], na.rm = TRUE)
  new_rot_start <- ifelse(is.na(rot_end), NA, new_rot_start)
  new_rot_start <- ifelse(is.finite(new_rot_start), new_rot_start, rot_start)
  return(new_rot_start)
}

burned_rows <- burned_rows %>% 
  mutate(h1_start = pmap_dbl(.l = list(Harvest1, hv_age1, burn_year_1, burn_year_2, burn_year_3), 
                             .f = id_interrupted_harvests),
         hv_age1 = Harvest1 - h1_start,
         h2_start = pmap_dbl(.l = list(Harvest2, hv_age2, burn_year_1, burn_year_2, burn_year_3), 
                             .f = id_interrupted_harvests),
         hv_age2 = Harvest2 - h2_start,
         h3_start = pmap_dbl(.l = list(Harvest3, hv_age3, burn_year_1, burn_year_2, burn_year_3), 
                             .f = id_interrupted_harvests),
         hv_age3 = Harvest3 - h3_start)


# Clean up and re-bind the two dataframes
burned_rows <- burned_rows %>% 
  select(-c(burned_harv1, burned_harv2, burned_harv3, error_harvest1, error_harvest2, h1_start, h2_start, h3_start))
hti_itp_hv_df <- rbind(burned_rows, unburned_rows)



##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## convert to long -------------------------------------
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Convert data to long format
hti_itp_hv_df_long <- hti_itp_hv_df %>% 
  pivot_longer(cols = starts_with("Harvest"), 
               names_to = "rotation", names_prefix = "Harvest", values_to = "harvest_year") %>% 
  select(block_id, supplier_id, estab_year, rotation, harvest_year, area_ha, burn_flag) %>% 
  drop_na()
  
rot_length <- hti_itp_hv_df %>% 
  pivot_longer(cols = starts_with("hv_age"), 
               names_to = "rotation", names_prefix = "hv_age", values_to = "rotation_length") %>% 
  select(block_id, rotation, rotation_length) %>% 
  drop_na()


harvest_df <- hti_itp_hv_df_long %>% 
  left_join(rot_length, by = c("block_id", "rotation")) ## WHY AM I GETTING DUPLICATES HERE?


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
# max_rotation <- 5

concession_harvests <- harvest_df %>% 
  filter(harvest_year >= 2015) %>% 
  mutate(impute_flag = (rotation==1) & (harvest_year - rotation_length < 2009),
         rotation_length = ifelse(impute_flag, harvest_year - 2009, rotation_length),
         ha_y = area_ha * rotation_length) %>% 
  group_by(supplier_id, harvest_year) %>% 
  summarise(impute_prop = weighted.mean(impute_flag, ha_y),
            burn_prop = weighted.mean(burn_flag, ha_y),
            ha_y = sum(ha_y)) %>% 
  mutate(burn_flag = burn_prop > 0,
         impute_flag = impute_prop > 0)

# concession_harvests %>% 
#   group_by(impute_flag, fire_flag) %>% 
#   summarize(area_sum = sum(ha_y)) %>% 
#   ungroup() %>% 
#   mutate(freq = prop.table(area_sum))
# 
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
