#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## 
## Project: Indonesia pulp deforestation
##
## Purpose of script: Clean ITP harvest data to get year and age of harvest - merge with wood supply
##
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Date Created: 2022-02-10
## 
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
##
## Notes: Input datasets
##        1) HTI concessions (boundaries) and concession start year - from KLHK
##        2) Gaveau harvested areas (2010 - 2021)
##        3) Wood supply data - RPBBI (cleaned and checked by UCSB & WWI)
##
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Imports and setup environment ------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## credentials
aws.signature::use_credentials()
bucket <- "trase-storage"
Sys.setenv("AWS_DEFAULT_REGION" = "eu-west-1")

## set working directory
wdir <- "remote"


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Load data ------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# itp harvest years
itp_hv <- read_sf(paste0(wdir,"\\01_data\\01_in\\gaveau\\IDN_ITPHarvesting_V20220208\\IDN_ITPHarvesting_V20220208.shp"))

# itp harvest years corrections
itp_hv_updates <- read_sf(paste0(wdir, "/01_data/03_qc/long_rotations_Checked_20240503/long_rotations_Checked_20240503.shp"))

# gut check - still lots of very long rotations in the revised data...
# itp_hv_updates <- itp_hv_updates %>% 
#   st_transform(itp_hv, crs = st_crs(hti)) %>% 
#   st_make_valid() %>% 
#   mutate(area_ha = st_area(.) * 0.0001)
itp_hv_updates %>%   filter(`Pre2010Hrv` == 0, `1Harvst` %in% c(2019, 2020, 2021, 2022), is.na(Ket))  %>% arrange(desc(area_ha)) %>% select(FCODE, block_d, area_ha, estab_year = yearint, `Pre2010Hrv`, `1Harvst`, `2Harvst`, `3Harvst`, Ket)


# hti concessions
hti <- read_sf(paste0(wdir,"\\01_data\\01_in\\klhk\\IUPHHK_HT_proj.shp"))

# # wood supply
# ws <- read_delim(get_object(object="indonesia/wood_pulp/production/out/PULP_WOOD_SUPPLY_CLEAN_ALL_ALIGNED_2015_2019.csv", bucket), delim = ",")
# 
# # supplier groups
# groups <- read_csv(paste0(wdir,"\\01_data\\01_in\\wwi\\ALIGNED_NAMES_GROUP_HTI.csv"))
#   
# # psdh
# psdh <- read_csv(paste0(wdir,"\\01_data\\01_in\\klhk\\psdh\\02_out\\PSDH_HTI_ID_COMBINED.csv"))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Cleaning David's harvest data ------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# prep original harvest dataset
itp_hv <- st_make_valid(itp_hv) 
itp_hv <- itp_hv %>% 
  mutate(block_id = paste0("B", as.character(str_pad(OBJECTID, 6, pad = "0"))))
itp_hv_proj <- st_transform(itp_hv, crs = st_crs(hti)) %>% st_make_valid() 

# clean up and improve consistency across variable names / order
itp_hv_updates <- itp_hv_updates %>% 
  select(block_id = block_d, year = yearint, Ket, Class, 
         Harvest1 = `1Harvst`, Harvest2 = `2Harvst`, Harvest3 = `3Harvst`, Harvest0 = Pre2010Hrv, 
         Delete = Delete)
itp_hv_proj <- itp_hv_proj %>% 
  select(OBJECTID, block_id, year = yearint, Ket, Class, Harvest1, Harvest2, Harvest3, geometry)

# Remove old data for block_ids that were reviewed / revised by Husna
revised_ids <- itp_hv_updates %>% 
  st_drop_geometry() %>% 
  pull(block_id) %>% 
  unique()
itp_hv <- itp_hv %>% 
  filter(!(block_id %in% revised_ids))

# revised data has some duplicates - remove
itp_hv_updates <- itp_hv_updates %>% 
  distinct()

# some conflicting second harvest dates - correct and remove remaining duplicates
itp_hv_updates %>% 
  group_by(block_id, geometry) %>% 
  tally() %>% 
  filter(n>1)
itp_hv_updates <- itp_hv_updates %>% 
  mutate(Harvest2 = ifelse(block_id == "B032115", 2020, 
                           ifelse(block_id == "B083053", 2018, Harvest2))) %>% 
  distinct()

# some impossible harvests in Husna's updates - drop Harvest1 that pre-dates Harvest2 by a year
itp_hv_updates <- itp_hv_updates %>% 
  mutate(harv_flag = (Harvest1>Harvest2 & Harvest2>0),
         Harvest1 = ifelse(harv_flag, Harvest2, Harvest1),
         Harvest2 = ifelse(harv_flag, Harvest3, Harvest2),
         Harvest3 = ifelse(harv_flag, 0, Harvest3)) %>% 
  select(-harv_flag)


# remove entries flagged for deletion by Husna
itp_hv_updates <- itp_hv_updates %>% 
  filter(is.na(Delete)) %>% 
  select(-Delete)

# append data back to original file
itp_hv_updates <- itp_hv_updates %>% 
  mutate(OBJECTID = 0)
itp_hv_proj <- itp_hv_proj %>% 
  mutate(Harvest0 = "0")
itp_hv_proj <- itp_hv_proj %>% 
  rbind(itp_hv_updates)
itp_hv_proj <- itp_hv_proj %>% 
  mutate(OBJECTID = row_number())

# Test that there are no duplicated entries
n_duplicates <- (itp_hv_proj %>% duplicated()) %>% sum()
test_that("Test of duplicate entries", expect_equal(n_duplicates, 0))

# intersect to get associated HTI concession
hti_itp_hv <- st_intersection(hti, itp_hv_proj) %>% 
  mutate(area_m2 = st_area(.))

# Recreate block_id so that they're still unique after HTI and Husna's splits
hti_itp_hv <- hti_itp_hv %>% 
  mutate(OBJECTID = row_number())

hti_itp_hv <- hti_itp_hv %>% 
  mutate(block_id = paste0("B", as.character(str_pad(OBJECTID, 6, pad = "0"))))

# create table and clean up
hti_itp_hv_df <- hti_itp_hv %>%
  st_drop_geometry() %>%
  select(block_id, supplier_id=ID, estab_year=year, Class, Harvest0, Harvest1, 
         Harvest2, Harvest3, Ket, area_m2) %>%
  mutate(area_ha = as.double(area_m2*0.0001),
         estab_year = as.integer(estab_year),
         Harvest0 = as.integer(Harvest0),
         Harvest1 = as.integer(Harvest1),
         Harvest2 = as.integer(Harvest2),
         Harvest3 = as.integer(Harvest3)) %>%
  select(-area_m2)
# %>%
#pivot_longer(c(-SUPPLIER_ID,-yearint,-Ket,-year,-Class,-area_ha), names_to="var", values_to="vals") 
# filter(is.na(Ket)) %>% ## JASON - Why are we filtering these?
# mutate(hv_age1 = Harvest1 - year,hv_age2 = Harvest2 - Harvest1, hv_age3 = Harvest3 - Harvest2) %>%
# mutate(hv_age1 = ifelse(hv_age1 <0,0,hv_age1),hv_age2 = ifelse(hv_age2 <0,0,hv_age2),hv_age3 = ifelse(hv_age3 <0,0,hv_age3))
# mutate(hv_age3 = ifelse(hv_age3 > 2000, Harvest3 - Harvest1,hv_age3),hv_age2 = ifelse(hv_age2 > 2000, Harvest2 - yearint,hv_age2))




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Update Harvest timings -------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Calculate harvest rotation lengths
hti_itp_hv_df <- hti_itp_hv_df %>% 
  mutate(Harvest1 = replace(Harvest1, Harvest1==0, NA),
         Harvest2 = replace(Harvest2, Harvest2==0, NA),
         Harvest3 = replace(Harvest3, Harvest3==0, NA),
         Harvest0 = replace(Harvest0, Harvest0==0, NA))


## Add Husna's pre-2010 rotation into the mix
hti_itp_hv_df <- hti_itp_hv_df %>% 
  mutate(early_harvest_flag = (!is.na(Harvest0)),
         Harvest4 = ifelse(early_harvest_flag, Harvest3, NA),
         Harvest3 = ifelse(early_harvest_flag, Harvest2, Harvest3),
         Harvest2 = ifelse(early_harvest_flag, Harvest1, Harvest2),
         Harvest1 = ifelse(early_harvest_flag, Harvest0, Harvest1)) %>% 
  select(block_id, supplier_id, Class, estab_year, Harvest1, Harvest2, Harvest3, Harvest4, Ket, area_ha)


## Correcting harvest sequences when Harvest1 ==0 and Harvest2 > 0, same for harvests 2 and 3
hti_itp_hv_df <- hti_itp_hv_df %>% 
  mutate(error_harvest1 = (is.na(Harvest1) & !is.na(Harvest2)),
         Harvest1 = ifelse(error_harvest1, Harvest2, Harvest1),
         Harvest2 = ifelse(error_harvest1, Harvest3, Harvest2),
         Harvest3 = ifelse(error_harvest1, Harvest4, Harvest3),
         Harvest4 = ifelse(error_harvest1, NA, Harvest4),
         error_harvest2 = (is.na(Harvest2) & !is.na(Harvest3)),
         Harvest2 = ifelse(error_harvest2, Harvest3, Harvest2),
         Harvest3 = ifelse(error_harvest2, Harvest4, Harvest3),
         Harvest4 = ifelse(error_harvest2, NA, Harvest4),
         error_harvest3 = (is.na(Harvest3) & !is.na(Harvest4)),
         Harvest3 = ifelse(error_harvest3, Harvest4, Harvest3),
         Harvest4 = ifelse(error_harvest3, NA, Harvest4))


## NOTE: There are some pulp plantations established after their first observed harvest.
# test <- hti_itp_hv_df %>% filter(estab_year>Harvest1, !is.na(Harvest1))
## Current solution: Assume the harvest is invalid is wrong. Remove pre-establishment harvests
hti_itp_hv_df <- hti_itp_hv_df %>% 
  mutate(error_estab = (estab_year>=Harvest1) %>% replace_na(FALSE),
         Harvest1 = ifelse(error_estab, Harvest2, Harvest1),
         Harvest2 = ifelse(error_estab, Harvest3, Harvest2),
         Harvest3 = ifelse(error_estab, Harvest4, Harvest3),
         Harvest4 = ifelse(error_estab, NA, Harvest4))

# Repeat to capture a few remaining records
hti_itp_hv_df <- hti_itp_hv_df %>% 
  mutate(error_estab2 = (estab_year>=Harvest1) %>% replace_na(FALSE),
         Harvest1 = ifelse(error_estab2, Harvest2, Harvest1),
         Harvest2 = ifelse(error_estab2, Harvest3, Harvest2),
         Harvest3 = ifelse(error_estab2, Harvest4, Harvest3),
         Harvest4 = ifelse(error_estab2, NA, Harvest4))


# Confirm that all harvest sequences are valid
harv_errors <- hti_itp_hv_df %>% 
  filter(Harvest1 > Harvest2 |
           Harvest2 > Harvest3 |
           Harvest3 > Harvest4 |
           (is.na(Harvest1) & !is.na(Harvest2)) |
           (is.na(Harvest2) & !is.na(Harvest3)) |
           (is.na(Harvest3) & !is.na(Harvest4)) |
           estab_year >= Harvest1)
test_that("No harvest sequence errors",
          expect_equal(dim(harv_errors)[1], 0))

# Summarize frequency of edits made in above steps
hti_itp_hv_df %>% 
  group_by(error_harvest1, error_harvest2, error_harvest3, error_estab, error_estab2) %>% 
  summarize(area_n = sum(area_ha)) %>% 
  ungroup() %>% 
  mutate(freq = prop.table(area_n))

hti_itp_hv_df <- hti_itp_hv_df %>% 
  select(-c(error_harvest1, error_harvest2, error_harvest3, error_estab, error_estab2))


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

hti_itp_hv_df %>% 
  group_by(Harvest4) %>% 
  summarize(area_ha = sum(area_ha)) %>% 
  mutate(freq = prop.table(area_ha))

# Can drop rows where there are no recorded harvests after 2015
hti_itp_hv_df <- hti_itp_hv_df %>% 
  filter(!is.na(Harvest1))

# Assign start year for each harvest
hti_itp_hv_df <- hti_itp_hv_df %>%
  mutate(hv_start1 = estab_year,
         hv_start2 = Harvest1,
         hv_start3 = Harvest2,
         hv_start4 = Harvest3)

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


# TODO: If burn didn't have assigned year, can't fix records. Also, seems like these rows might just be flagging burn year in the Harvest columns? Confirm with david/husna
# hti_itp_hv_df %>% filter(burn_flag, is.na(burn_year_1)) %>% pull(area_ha) %>% sum()
burned_rows <- hti_itp_hv_df %>%
  filter(burn_flag,
         !is.na(burn_year_1))

unburned_rows <- hti_itp_hv_df %>%
  filter(!burn_flag | (burn_flag & is.na(burn_year_1)))

# Identify failed harvests that occur at the same time as a fire
id_invalid_harvests <- function(rot_end, b1, b2, b3){
  burn_list <- c(b1, b1 + 1, b2, b2 + 1, b3, b3 + 1)
  burned_harv <- (rot_end %in% burn_list)
  burned_harv <- ifelse(is.na(rot_end), NA, burned_harv)
  burned_harv <- replace_na(burned_harv, FALSE)
  return(burned_harv)
}

burned_rows <- burned_rows %>%
  mutate(burned_harv1 = pmap_lgl(.l = list(Harvest1, burn_year_1, burn_year_2, burn_year_3),
                                 .f = id_invalid_harvests),
         burned_harv2 = pmap_lgl(.l = list(Harvest2, burn_year_1, burn_year_2, burn_year_3),
                                 .f = id_invalid_harvests),
         burned_harv3 =  pmap_lgl(.l = list(Harvest3, burn_year_1, burn_year_2, burn_year_3),
                                  .f = id_invalid_harvests),
         burned_harv4 =  pmap_lgl(.l = list(Harvest4, burn_year_1, burn_year_2, burn_year_3),
                                  .f = id_invalid_harvests))
# ,
# Harvest1 = ifelse(burned_harv1, NA, Harvest1),
# Harvest2 = ifelse(burned_harv2, NA, Harvest2),
# Harvest3 = ifelse(burned_harv3, NA, Harvest3),
# Harvest4 = ifelse(burned_harv4, NA, Harvest4))
#
#
# # Shift harvests to fix harvests that were removed due to burns
# burned_rows <- burned_rows %>%
#   mutate(error_harvest1 = (is.na(Harvest1) & !is.na(Harvest2)),
#          Harvest1 = ifelse(error_harvest1, Harvest2, Harvest1),
#          Harvest2 = ifelse(error_harvest1, Harvest3, Harvest2),
#          Harvest3 = ifelse(error_harvest1, Harvest4, Harvest3),
#          Harvest4 = ifelse(error_harvest1, NA, Harvest4),
#          error_harvest2 = (is.na(Harvest2) & !is.na(Harvest3)),
#          Harvest2 = ifelse(error_harvest2, Harvest3, Harvest2),
#          Harvest3 = ifelse(error_harvest1, Harvest4, Harvest3),
#          Harvest4 = ifelse(error_harvest1, NA, Harvest4),
#          error_harvest3 = (is.na(Harvest3) & !is.na(Harvest4)),
#          Harvest3 = ifelse(error_harvest1, Harvest4, Harvest3),
#          Harvest4 = ifelse(error_harvest1, NA, Harvest4),
#          hv_age1 = Harvest1 - estab_year,
#          hv_age2 = Harvest2 - Harvest1,
#          hv_age3 = Harvest3 - Harvest2,
#          hv_age4 = Harvest4 - Harvest3)

# Adjust harvest start year for harvests that were interrupted by a fire
id_interrupted_harvests <- function(rot_end, rot_start, b1, b2, b3){
  burn_list <- c(b1, b2, b3)
  burn_interruptions <- ((burn_list > rot_start) & (burn_list < rot_end))
  new_rot_start <- max(burn_list[burn_interruptions], na.rm = TRUE)
  new_rot_start <- ifelse(is.na(rot_end), NA, new_rot_start)
  new_rot_start <- ifelse(is.finite(new_rot_start), new_rot_start, rot_start)
  return(new_rot_start)
}

burned_rows <- burned_rows %>%
  mutate(hv_start1 = pmap_dbl(.l = list(Harvest1, hv_start1, burn_year_1, burn_year_2, burn_year_3),
                              .f = id_interrupted_harvests),
         hv_start2 = pmap_dbl(.l = list(Harvest2, hv_start2, burn_year_1, burn_year_2, burn_year_3),
                              .f = id_interrupted_harvests),
         hv_start3 = pmap_dbl(.l = list(Harvest3, hv_start3, burn_year_1, burn_year_2, burn_year_3),
                              .f = id_interrupted_harvests),
         hv_start4 = pmap_dbl(.l = list(Harvest4, hv_start4, burn_year_1, burn_year_2, burn_year_3),
                              .f = id_interrupted_harvests))


# Re-bind the two dataframes
unburned_rows <- unburned_rows %>% 
  mutate(burned_harv1 = FALSE,
         burned_harv2 = FALSE,
         burned_harv3 = FALSE,
         burned_harv4 = FALSE)
hti_itp_hv_df <- rbind(burned_rows, unburned_rows)



##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## convert to long -------------------------------------
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Calculate harvest length
hti_itp_hv_df <- hti_itp_hv_df %>% 
  mutate(hv_age1 = Harvest1 - hv_start1,
         hv_age2 = Harvest2 - hv_start2,
         hv_age3 = Harvest3 - hv_start3,
         hv_age4 = Harvest4 - hv_start4)

# Convert data to long format
hti_itp_hv_df_long <- hti_itp_hv_df %>% 
  pivot_longer(cols = starts_with("Harvest"), 
               names_to = "rotation", names_prefix = "Harvest", values_to = "harvest_year") %>% 
  select(block_id, supplier_id, rotation, harvest_year, area_ha, burn_flag)
# select(block_id, supplier_id, estab_year, rotation, harvest_year, area_ha, burn_flag)

# hti_itp_hv_df_long <- hti_itp_hv_df %>% 
#   pivot_longer(cols = c(starts_with("Harvest"), starts_with("hv_age")), 
#                names_to = "rotation", names_prefix = c("Harvest", "hv_age"), values_to = c("harvest_year", "rotation_length")) %>% 
#   select(block_id, supplier_id, estab_year, rotation, harvest_year, area_ha, burn_flag) %>% 
#   drop_na()

rot_length <- hti_itp_hv_df %>% 
  pivot_longer(cols = starts_with("hv_age"), 
               names_to = "rotation", names_prefix = "hv_age", values_to = "rotation_length") %>% 
  select(block_id, rotation, rotation_length)

failed_rotations <- hti_itp_hv_df %>% 
  pivot_longer(cols = starts_with("burned_harv"), 
               names_to = "rotation", names_prefix = "burned_harv", values_to = "burned_harv") %>% 
  select(block_id, rotation, burned_harv)


# Merge back long datasets
harvest_df <- hti_itp_hv_df_long %>% 
  left_join(rot_length, by = c("block_id", "rotation")) %>%
  left_join(failed_rotations, by = c("block_id", "rotation"))


# Dropping non-existent rotations, rotations that failed due to fire, and pre-2015 rotations
harvest_df <- harvest_df %>%
  filter(burned_harv == FALSE) %>% 
  select(-burned_harv) %>% 
  drop_na() %>% 
  filter(harvest_year>=2015)


test <- harvest_df %>% 
  filter(harvest_year>=2015) %>% 
  group_by(rotation_length) %>% 
  summarize(area_harvest = sum(area_ha)) %>% 
  print(n = 30)

# ## NEED TO EXPORT OVERLY LONG ROTATIONS FOR DAVID TO DIG INTO POTENTIAL ERRORS
# harvest_df %>% 
#   filter(harvest_year>=2015) %>% 
#   pull(area_ha) %>% 
#   sum()
# 
# long_rotations <- harvest_df %>% 
#   filter(harvest_year>=2015) %>% 
#   filter(rotation_length>7)
# 
# long_rotations_sp <- hti_itp_hv %>% 
#   right_join(long_rotations, by = "block_id")
# 
# long_rotations_sp$rotation_length %>% summary()
# long_rotations_sp$area_ha %>% sum()
# 
# st_write(long_rotations_sp %>% select(-c("FID_", "OBJECTID")), paste0(wdir,"/01_data/03_qc/long_rotations.shp"), append=FALSE)


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## calculate ha-years in each rotation -------------------------------------
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
harvest_df <- harvest_df %>% 
  mutate(ha_y = area_ha * rotation_length)

# Create winsorized version for too-long rotations
harvest_df <- harvest_df %>% 
  mutate(impute_flag = rotation_length > 8,
         rotation_length_w = ifelse(impute_flag, 8, rotation_length),
         ha_y_w = area_ha * rotation_length_w)

# # 82% of the areas harvested within this period had experienced a plantation establishment or prior harvesting event since 2010
# harvest_df %>% 
#   group_by(impute_flag) %>% 
#   summarise(area = sum(area_ha)) %>% 
#   mutate(freq = prop.table(area)) %>% 
#   print()


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## explore rotation lengths -------------------------------------
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
observed_harvests <- harvest_df %>% 
  # filter(rotation > 1) %>% 
  group_by(rotation_length) %>% 
  summarize(area_sum = sum(area_ha)) %>% 
  print(n = 25)

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
  group_by(supplier_id, harvest_year) %>% 
  summarise(impute_prop = weighted.mean(impute_flag, ha_y),
            burn_prop = weighted.mean(burn_flag, ha_y),
            ha_y = sum(ha_y),
            ha_y_w = sum(ha_y_w))

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
