## ---------------------------------------------------------
## 
## Project: Data preparation (HTI conversion timing & Land Use changes)
##
## Purpose of script: 
##
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Date Created: 2023-05-08
## 
## ---------------------------------------------------------
##
## Notes: Input datasets
##        1) HTI concessions (boundaries) and concession start year from KLHK
##        2) TreeMap landuse change - pulp deforestation (2000 - 2022)
##        4) GFC Hansen deforestation year (2001 - 2022) - earthenginepartners.appspot.com/science-2013-global-forest
##        5) Peat (MoA Indonesia, 2019) & Margono forest mask (TreeMap version)
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
library(data.table)
library(janitor)
library(lubridate)
library(sf)
library(scales)
library(dtplyr)
library(testthat)
library(d3.format)
library(tidyfast)
library(patchwork)
library(rcartocolor)
library(showtext)
library(khroma) # palettes for color blindness

## set working directory -------------------------------------

wdir <- "remote"

## read data -------------------------------------------------

# pulp conversion from non-forest (indonesia wide)
pulp_nonfor_id <- read_csv(paste0(wdir,"\\01_data\\02_out\\gee\\gaveau\\pulp_annual_defor_non-forest_id.csv")) %>%
  select(-`system:index`,-constant,-.geo)
'%ni%' <- Negate('%in%') # filter out function
## data lookup table
lu_table <- read_csv(paste0(wdir,"\\01_data\\02_out\\gee\\data_lookup_table.csv"))
## hti license dates
lic_dates_hti <- readr::read_csv(paste0(wdir,"\\01_data\\01_in\\wwi\\HTI_LICENSE_DATES.csv"),col_types = cols(license_date = col_date("%m/%d/%Y")))
## supplier groups
groups <- read_csv(paste0(wdir,"\\01_data\\01_in\\wwi\\ALIGNED_NAMES_GROUP_HTI.csv"))
## HTI and sample IDs
samples_hti <- read_csv(paste0(wdir,"\\01_data\\02_out\\samples\\samples_hti_id.csv"))
# hti concessions
hti <- read_sf(paste0(wdir,"\\01_data\\01_in\\klhk\\IUPHHK_HTI_TRASE_20230314_proj.shp"))
# wood supply
ws <- read_csv(paste0(wdir,"\\01_data\\01_in\\wwi\\PULP_WOOD_SUPPLY_CLEAN_ALL_ALIGNED_2020_2022.csv"))
# ownership class
hti_ownership_class <- read_csv(paste0(wdir,"\\01_data\\02_out\\tables\\hti_company_ownership_reclass.csv"))
# kabupaten
kab <- read_sf(paste0(wdir,"\\01_data\\01_in\\big\\idn_kabupaten_big.shp"))
# provinces
prov_slim <- kab %>% select(prov,prov_code) %>% st_drop_geometry() %>% distinct() %>%
  mutate(prov_code = ifelse(prov == "PAPUA",92,prov_code))
# add islands
islands <- kab %>%
  st_drop_geometry() %>%
  mutate(island = str_sub(prov_code, 1, 1)) %>%
  mutate(
    island = case_when(
      island == 1 ~ "Sumatera",
      island == 6 ~ "Kalimantan",
      island == 9 ~ "Papua"
    )
  ) %>%
  distinct(prov_code,island) %>%
  drop_na(island)
# mills
mills <- read_excel(paste0(wdir,"\\01_data\\01_in\\wwi\\MILLS_EXPORTERS_20200405.xlsx"))

## clean mill supplier
mill_supplier <- ws %>%
  filter(str_detect(SUPPLIER_ID, '^ID-WOOD-CONCESSION')) %>%
  select(supplier_id=SUPPLIER_ID,EXPORTER) %>%
  mutate(mill = case_when(EXPORTER == "OKI" ~ "app",
                          EXPORTER == "INDAH KIAT" ~ "app",
                          EXPORTER == "APRIL" ~ "april",
                          TRUE  ~ "marubeni")) %>%
  select(-EXPORTER) %>%
  mutate(supplier_id = str_replace(supplier_id,"ID-WOOD-CONCESSION-","H-")) %>%
  distinct() 

## TreeMap data
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\gaveau\\"),pattern = "*gaveau_classes.csv",full.names= TRUE)

samples_gaveau_landuse <- filenames %>% map_dfr(read_csv) %>% janitor::clean_names() 

## GFC deforestation, peat and Margono primary forest
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\gfc_peat\\"),
                 pattern = "*.csv",
                 full.names= TRUE)

samples_gfc_margono_peat <- filenames %>% map_dfr(read_csv) %>% janitor::clean_names() 

## GFC deforestation (modified by TreeMap)
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\gfc_ttm\\"),
                 pattern = "*.csv",
                 full.names= TRUE)

samples_gfc_ttm <- filenames %>% map_dfr(read_csv) %>%janitor::clean_names() 

# pulp conversion from forest (indonesia wide)
pulp_for_id <- read_csv(paste0(wdir,"\\01_data\\02_out\\gee\\gaveau\\pulp_annual_defor_forest_id.csv")) %>%
  select(-`system:index`,-constant,-.geo)

# pulp conversion from non-forest (indonesia wide)
pulp_nonfor_id <- read_csv(paste0(wdir,"\\01_data\\02_out\\gee\\gaveau\\pulp_annual_defor_non-forest_id.csv")) %>%
  select(-`system:index`,-constant,-.geo)

## clean hti concession names
## Note: 3 supplying concessions - PT OKI PULP & PAPER MILLS & PT WANA SUBUR SAWIT INDAH,
## and PT MUTAIARA SABUK KHATULISTIWA are not HTI concessions but IPK concessions [wood utilization permit]
## and Hutan Alam included as they are suppliers to pulp mills
hti_concession_names <- hti %>%
  st_drop_geometry() %>%
  select(supplier_id=ID,supplier=namaobj) %>%
  mutate(supplier_label = paste0(supplier," (",supplier_id,")"))

## clean license dates
hti_dates_clean <- lic_dates_hti %>%
  mutate(YEAR = year(license_date)) %>%
  select(supplier_id=HTI_ID,license_year=YEAR)

## clean mill supplier
mill_supplier <- ws %>%
  filter(str_detect(SUPPLIER_ID, '^ID-')) %>%
  select(supplier_id=SUPPLIER_ID,EXPORTER) %>%
  mutate(mill = case_when(EXPORTER == "OKI" ~ "app",
                          EXPORTER == "INDAH KIAT" ~ "app",
                          EXPORTER == "APRIL" ~ "april",
                          TRUE  ~ "marubeni")) %>%
  select(-EXPORTER) %>%
  distinct() %>%
  mutate(supplier_id = str_replace(supplier_id,"ID-WOOD-CONCESSION-","H-"))

# list of supplying concessions
mill_supplier_list <- mill_supplier %>%
  select(supplier_id) %>%
  pull()

other_concessions <- hti_concession_names %>%
  filter(supplier_id %in% mill_supplier_list == FALSE) %>%
  select(supplier_id) %>%
  mutate(mill="none")

mill_supplier <- mill_supplier %>%
  rbind(other_concessions) %>%
  as.data.table()

mill_supplier <- dcast(mill_supplier, formula = supplier_id ~ mill, fun.aggregate = length) 
mill_supplier$all <- "1" # 1 value for all concessions

# clean supplier groups
supplier_groups <- groups %>%
  select(supplier_id = id,supplier_group=group) %>%
  mutate(supplier_group = ifelse(is.na(supplier_group),"OTHER",supplier_group))

################################################################################
## Create data on land use changes
################################################################################

## Identify pixels that eventually become pulp
gaveau_pulp_sids <- samples_gaveau_landuse %>%
  select(sid,timberdeforestation_2022) %>%
  lazy_dt() %>%
  as.data.table() %>%
  dt_pivot_longer(cols = c(-sid),
                  names_to = 'year',
                  values_to = 'class') %>%
  as_tibble() %>%
  filter(class == "3") %>%
  distinct() %>%
  pull(sid)

# Create pixel level dataset starting from primary forest detected by Treemap Margono mask
samples_df <- samples_gfc_margono_peat %>%
  lazy_dt() %>%
  select(sid, primary,lossyear) %>%
  mutate(start_for = ifelse(primary == 100 & !is.na(primary),"Y","N")) %>% 
  left_join(samples_hti, by = "sid") %>%
  drop_na(sid) %>%
  mutate(island_name = case_when(
    island == 1 ~ "Balinusa",
    island == 2 ~ "Kalimantan",
    island == 3 ~ "Maluku",
    island == 4 ~ "Papua",
    island == 5 ~ "Sulawesi",
    island == 6 ~ "Sumatera",
    TRUE ~ NA
  )) %>%
  select(-island) %>%
  rename(island = island_name) %>%
  as_tibble()

### Join to hti concession license dates and names
samples_df <- samples_df %>% 
  as_tibble() %>%
  add_column(rand = runif(nrow(.))) %>%
  lazy_dt() %>%
  rename(supplier_id = ID) %>% 
  left_join(hti_dates_clean,by="supplier_id") %>%
  left_join(hti_concession_names,by="supplier_id") %>%
  mutate(pulp = ifelse(sid %in% gaveau_pulp_sids,"Y","N"))

## create table of annual pulp
supplier_year_tbl <- hti_concession_names %>%
  select(supplier_id) %>%
  group_by(supplier_id) %>%
  summarise(start = min(2001),
            end = max(2022)) %>%
  mutate(year = Map(seq, start, end)) %>%
  unnest(cols =year) %>%
  mutate(unique=1) %>%
  select(supplier_id,year)

## Calculate annual pulp planted areas
gaveau_annual_pulp <- samples_gaveau_landuse %>%
  lazy_dt() %>%
  as.data.table() %>%
  dt_pivot_longer(cols = c(-sid),
                  names_to = 'year',
                  values_to = 'class') %>%
  mutate(year = str_replace(year,"timberdeforestation_", ""),year = as.double(year)) %>%
  mutate(gav_class = ifelse(class == 3,"Pulp","Others")) %>%
  left_join(samples_hti,by="sid") %>%
  as_tibble() %>%
  group_by(supplier_id=ID,year,gav_class) %>%
  summarize(n = n()) %>%
  group_by(supplier_id,year) %>%
  mutate(shr_gav_lu_areas = prop.table(n)*100) %>%
  filter(gav_class != "Others")

# calculate forest areas in 2000
gaveau_forest_2000 <- samples_gfc_margono_peat %>%
  lazy_dt() %>%
  left_join(samples_hti,by="sid") %>%
  group_by(supplier_id=ID,primary) %>%
  summarize(forest_area_ha = n()) %>%
  filter(!is.na(primary)) %>%
  as_tibble() %>%
  rowwise() %>% 
  transmute(supplier_id, primary, forest_area_ha,year = list(seq(2000, 2022))) %>% 
  unnest_longer(year)

conc_area <- samples_hti %>%
  group_by(supplier_id=ID) %>%
  summarize(conc_area_ha = n())

# list of codes of forest
forest_loss_codes <- c(101:122,401:422,601:622)

# merge and calculate remaining forest areas in each year
gaveau_annual_forest <- samples_gfc_ttm %>%
  lazy_dt() %>%
  left_join(samples_hti,by="sid") %>%
  mutate(year = round(gfc_ttm %% 100)+2000) %>%
  as_tibble() %>%
  filter(gfc_ttm %in% forest_loss_codes) %>%
  group_by(supplier_id=ID,island,year) %>%
  summarize(n = n()) %>%
  arrange(-desc(supplier_id),year) %>%
  group_by(supplier_id,island) %>% 
  mutate(cum_floss = cumsum(n)) %>%
  right_join(gaveau_forest_2000,by=c("supplier_id","year")) %>%
  arrange(-desc(supplier_id),year) %>%
  group_by(supplier_id) %>%
  fill(cum_floss,island,.direction="down") %>%
  mutate(cum_floss = ifelse(is.na(cum_floss),0,cum_floss),
         rem_forest_area_ha = forest_area_ha - cum_floss) %>%
  select(supplier_id,island,year,rem_forest_area_ha) %>%
  as_tibble() %>%
  right_join(supplier_year_tbl,by=c("supplier_id","year")) %>%
  arrange(-desc(supplier_id),year) %>%
  group_by(supplier_id) %>%
  fill(island,.direction="updown") %>%
  mutate(rem_forest_area_ha = ifelse(is.na(rem_forest_area_ha),0,rem_forest_area_ha))

# combine areas
gaveau_annual_lc <- gaveau_annual_forest %>%
  full_join(conc_area,by="supplier_id") %>%
  full_join(gaveau_annual_pulp,by=c("supplier_id","year")) %>%
  mutate(pulp_area_ha = ifelse(is.na(n),0,n)) %>%
  arrange(-desc(supplier_id),year) %>%
  group_by(supplier_id) %>%
  fill(conc_area_ha,rem_forest_area_ha,.direction = "updown") %>%
  mutate(other_land_ha = conc_area_ha - (rem_forest_area_ha + pulp_area_ha)) %>%
  select(year,supplier_id,island,pulp_area_ha,rem_forest_area_ha,other_land_ha,conc_area_ha) %>%
  pivot_longer(cols = -c(supplier_id,island,year),
               names_to = 'class',
               values_to = 'area_ha') %>%
  filter(class != "conc_area_ha") %>%
  mutate(class_desc = case_when(
    class == "pulp_area_ha" ~ "Cleared for pulp", # pulp planted area (need to check)
    class == "rem_forest_area_ha" ~ "Forest",
    class == "other_land_ha" ~ "Non-forest")) %>%
  select(year,supplier_id,island,class_desc,area_ha) %>%
  left_join(hti_dates_clean,by="supplier_id") %>%
  left_join(mill_supplier,by="supplier_id") %>%
  mutate(
    zdc_year = case_when(
      app == 1 ~ 2013,
      app == 0 & april == 1 ~ 2015,
      april == 0 & app == 0 & marubeni == 1 ~ 2019,
      TRUE ~ 0
    )
  ) %>%
  mutate(zdc_year = ifelse(zdc_year ==0,NA_real_,zdc_year))

hti_gav_annual_lc <- gaveau_annual_lc %>%
  left_join(hti_concession_names,by="supplier_id") 

write_csv(hti_gav_annual_lc,paste0(wdir,"\\01_data\\02_out\\tables\\hti_land_use_change_areas.csv"))

################################################################################
## calculate hti conversion timing
################################################################################

## identify pixels that eventually become pulp
gaveau_pulp_sids <- samples_gaveau_landuse %>%
  select(sid,timberdeforestation_2022) %>%
  lazy_dt() %>%
  as.data.table() %>%
  dt_pivot_longer(cols = c(-sid),
                  names_to = 'year',
                  values_to = 'class') %>%
  as_tibble() %>%
  filter(class == "3") %>%
  distinct() %>%
  pull(sid)

# create pixel level dataset starting from primary forest detected by Treemap Margono mask
samples_df <- samples_gfc_margono_peat %>%
  lazy_dt() %>%
  select(sid, primary,lossyear) %>%
  mutate(start_for = ifelse(primary == 100 & !is.na(primary),"Y","N")) %>% 
  left_join(samples_hti, by = "sid") %>%
  filter(!is.na(island)) %>%
  mutate(island_name = case_when(
    island == 1 ~ "Balinusa",
    island == 2 ~ "Kalimantan",
    island == 3 ~ "Maluku",
    island == 4 ~ "Papua",
    island == 5 ~ "Sulawesi",
    island == 6 ~ "Sumatera",
    TRUE ~ "None"
  )) %>%
  select(-island) %>%
  rename(island = island_name) %>%
  as_tibble()

samples_df <- samples_df %>% 
  as_tibble() %>%
  add_column(rand = runif(nrow(.))) %>%
  lazy_dt() %>%
  rename(supplier_id = ID) %>% 
  left_join(hti_dates_clean,by="supplier_id") %>%
  left_join(hti_concession_names,by="supplier_id") %>%
  left_join(samples_gfc_ttm,by="sid") %>%
  mutate(pulp = ifelse(sid %in% gaveau_pulp_sids,"Y","N"))

# gaveau pulp conversion
gaveau_annual_pulp <- samples_gaveau_landuse %>%
  lazy_dt() %>%
  as.data.table() %>%
  dt_pivot_longer(cols = c(-sid),
                  names_to = 'year',
                  values_to = 'class') %>%
  as_tibble() %>%
  filter(class != "0") %>%
  mutate(year = str_replace(year,"timberdeforestation_", ""),year = as.double(year))

# deforestation for pulp (1 - non-forest to pulp,2 - forest to pulp)
hti_pulp_conv <- gaveau_annual_pulp %>%
  lazy_dt() %>%
  # as.data.table() %>%
  arrange(sid,year) %>%
  group_by(sid) %>%
  mutate(conv_type = class - lag(class, default = first(class))) %>%
  ungroup() %>%
  left_join(samples_df,by="sid") %>%
  group_by(year,supplier_id,supplier,supplier_label,license_year,island,conv_type) %>%
  summarize(area_ha = n()) %>%
  filter(conv_type != 0) %>%
  as_tibble()

# other deforestation (GFC) # conversion type = 3
hti_other_conv <- samples_df %>%
  filter(pulp == "N" & start_for == "Y" & !is.na(lossyear)) %>%
  mutate(year = lossyear + 2000, conv_type = 3) %>%
  group_by(year,supplier_id,supplier,supplier_label,license_year,island,conv_type) %>%
  summarize(area_ha = n()) %>%
  as_tibble()

# Other deforestation using TTM's modified GFC layer
# conversion from forest (incl. peat forests)
# 101-1xx = Annual forest on mineral soil loss. The last two digit represent the year of loss.
# 401-4xx Mangrove forest loss, where xx is the year when the mangrove forest loss happen.
# 601-6xx = Peat swamp forest loss. The last two digit represent the year of loss.
hti_other_conv <- samples_df %>%
  filter(pulp == "N" & start_for == "Y" & gfc_ttm > 100 & gfc_ttm <= 122 |
           pulp == "N" & start_for == "Y" & gfc_ttm > 400 & gfc_ttm <= 422 |
           pulp == "N" & start_for == "Y" & gfc_ttm > 600 & gfc_ttm <= 622) %>%
  as_tibble() %>%
  mutate(gfc_ttm = as.character(gfc_ttm),
         gfc_ttm = str_sub(gfc_ttm,2, -1),
         year = as.double(gfc_ttm) + 2000,
         conv_type = 3) %>%
  group_by(year,supplier_id,supplier,supplier_label,license_year,island,conv_type) %>%
  summarize(area_ha = n()) %>%
  as_tibble()

# annual deforestation outside concessions (from forest and non-forest)
id_pulp_conv_for <- pulp_for_id %>%
  left_join(islands,by="prov_code") %>%
  select(-prov,-kab,-kab_code,-prov_code,-type) %>%
  dt_pivot_longer(cols = -c(island),
                  names_to = 'year',
                  values_to = 'area_ha') %>%
  as_tibble() %>%
  filter(area_ha != "0") %>%
  mutate(year = str_replace(year,"deforestation_", ""),year = as.double(year)) %>%
  group_by(island,year) %>%
  summarize(area_ha = sum(area_ha)) %>%
  mutate(conv_type = "forest") 

id_pulp_conv_nonfor <- pulp_nonfor_id %>%
  left_join(islands,by="prov_code") %>%
  select(-prov,-kab,-kab_code,-prov_code,-type) %>%
  dt_pivot_longer(cols = -c(island),
                  names_to = 'year',
                  values_to = 'area_ha') %>%
  as_tibble() %>%
  filter(area_ha != "0") %>%
  mutate(year = str_replace(year,"deforestation_", ""),year = as.double(year)) %>%
  group_by(island,year) %>%
  summarize(area_ha = sum(area_ha)) %>%
  mutate(conv_type = "non-forest") 

id_pulp_conv_hti <- hti_pulp_conv %>%
  group_by(island,year,conv_type) %>%
  summarize(area_ha = sum(area_ha))

pulp_conv_outside_hti <- id_pulp_conv_for %>%
  bind_rows(id_pulp_conv_nonfor) %>%
  mutate(conv_type = ifelse(conv_type == "forest",2,1)) %>%
  left_join(id_pulp_conv_hti,by=c("year","conv_type","island")) %>%
  mutate(area_ha.y = ifelse(is.na(area_ha.y),0,area_ha.y),
         area_ha = area_ha.x - area_ha.y,
         area_ha = ifelse(area_ha < 50,0,area_ha)) %>% # remove minor differences due to area and sample based calculations
  select(-area_ha.x,-area_ha.y) %>%
  arrange(year) %>%
  mutate(supplier_id = NA,supplier=NA,supplier_label=NA,license_year=NA) %>%
  select(year,island,supplier_id,supplier,supplier_label,license_year,conv_type,area_ha)

# merge conversion (hti)
hti_conv <- hti_pulp_conv %>%
  bind_rows(hti_other_conv) %>% # TTM GFC modified data
  left_join(mill_supplier,by="supplier_id") %>%
  mutate(
    zdc_year = case_when(
      app == 1 ~ 2013,
      app == 0 & april == 1 ~ 2015,
      april == 0 & app == 0 & marubeni == 1 ~ 2019, 
      TRUE ~ 0
    )
  ) %>%
  mutate(zdc_year = ifelse(zdc_year ==0,NA_real_,zdc_year)) %>%
  arrange(year,supplier_id) %>%
  print()

# merge conversion (hti and non-hti areas)
hti_nonhti_conv <- hti_pulp_conv %>%
  bind_rows(pulp_conv_outside_hti) %>% # deforestation for pulp outside concessions
  left_join(mill_supplier,by="supplier_id") %>%
  mutate(
    zdc_year = case_when(
      app == 1 ~ 2013,
      app == 0 & april == 1 ~ 2015,
      april == 0 & app == 0 & marubeni == 1 ~ 2019, 
      TRUE ~ 0
    )
  ) %>%
  mutate(zdc_year = ifelse(zdc_year ==0,NA_real_,zdc_year)) %>%
  arrange(year,supplier_id) %>%
  print()

# forest areas in 2022
hti_for_areas <- samples_gfc_ttm %>%
  filter(gfc_ttm == 100 | gfc_ttm == 400 | gfc_ttm == 600) %>%
  lazy_dt() %>%
  left_join(samples_df,by="sid") %>% 
  group_by(supplier_id,supplier,supplier_label,license_year,island) %>%
  summarize(area_ha = n()) %>%
  as_tibble()

# other conversion
hti_nonpulp_conv_areas <- hti_other_conv %>%
  group_by(supplier_id,supplier,supplier_label,license_year,island) %>%
  summarize(area_ha = sum(area_ha)) %>%
  print()

# generate deforestation timing plot
hti_conv_timing <- gaveau_annual_pulp %>%
  lazy_dt() %>%
  arrange(sid,year) %>%
  group_by(sid) %>%
  mutate(conv_type = class - lag(class, default = first(class))) %>%
  ungroup() %>%
  left_join(samples_df,by="sid") %>%
  filter(conv_type != 0) %>%
  drop_na(license_year) %>%
  arrange(year,supplier_id) %>%
  as_tibble() %>%
  left_join(mill_supplier,by="supplier_id") %>%
  mutate(conv_time = ifelse(year >= 2015,"Deforestation for pulp after 2015","Deforestation for pulp from 2001-2015")) %>%
  group_by(supplier_id,supplier,supplier_label,license_year,island,april,app,marubeni,all,conv_type,conv_time) %>%
  summarize(area_ha = n()) %>%
  bind_rows(hti_for_areas) %>%
  mutate(class = ifelse(is.na(conv_time),"Remaining forest",conv_time)) %>%
  select(-conv_time) %>%
  bind_rows(hti_nonpulp_conv_areas) %>%
  mutate(class = ifelse(is.na(class),"Deforestation not for pulp",class)) %>%
  arrange(-desc(supplier_id)) %>%
  left_join(supplier_groups,by="supplier_id") %>%
  group_by(supplier_id) %>%
  mutate(app = zoo::na.locf(app, na.rm = FALSE),
         april = zoo::na.locf(april, na.rm = FALSE),
         marubeni = zoo::na.locf(marubeni, na.rm = FALSE),
         app = ifelse(is.na(app),0,app),
         april = ifelse(is.na(april),0,april),
         marubeni = ifelse(is.na(marubeni),0,marubeni),
         supplier_group = ifelse(supplier_group == "UNKNOWN" | is.na(supplier_group),"OTHER",supplier_group)) %>%
  group_by() %>%
  mutate(all = ifelse(is.na(all),1,all)) %>%
  print()

# write to csv
write_csv(hti_conv_timing,paste0(wdir,"\\01_data\\02_out\\tables\\hti_grps_deforestation_timing.csv"))
write_csv(hti_nonhti_conv,paste0(wdir,"\\01_data\\02_out\\tables\\idn_pulp_conversion_hti_nonhti_gaveau.csv"))
