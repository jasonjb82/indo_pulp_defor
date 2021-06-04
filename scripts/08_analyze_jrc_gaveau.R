#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Authors: Robert Heilmayr and Jason Jon Benedict
# Project: indo_pulp_defor
# Input files:
#
# ToDo
# Clean up plot to match what's being produced in 07_analyze_gee_data.R
# Convert n in frequency table to hectares
# Confirm Gaveau data is being interpreted correctly
# Confirm JRC data is being interpreted correctly. E.g. confirm that 0 in defyear means no deforestation observed in 1990-2019?
# Seems like JRC data only covers locations that started as forest in 1990? If so, need to add deforested points to ensure final ha = total area within concessions 
# Add qc checks
# Add three binary columns indicating whether concession supplies APRIL, APP, Marubeni mills
# Update <2013 cut-off to match earliest ZDC of their downstream mill
# 
#
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Set-up --------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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




## credentials ----------------------------------------------

aws.signature::use_credentials()
bucket <- "trase-storage"
Sys.setenv("AWS_DEFAULT_REGION" = "eu-west-1")

## set working directory -------------------------------------

wdir <- "remote"

## read data -------------------------------------------------

# increase memory size
memory.limit(size=56000)

## data lookup table
lu_table <- read_csv(paste0(wdir,"\\01_data\\02_out\\gee\\data_lookup_table.csv"))

## hti license dates
lic_dates_hti <- readr::read_csv(paste0(wdir,"\\01_data\\01_in\\wwi\\HTI_LICENSE_DATES.csv"),
                                 col_types = cols(license_date = col_date("%m/%d/%Y")))

## HTI and sample IDs
samples_hti <- read_csv(paste0(wdir,"\\01_data\\02_out\\samples\\samples_hti_id.csv"))

# hti concessions
hti <- read_sf(paste0(wdir,"\\01_data\\01_in\\klhk\\IUPHHK_HT_proj.shp"))

## JRC

## Annual changes 
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\jrc\\annual_changes\\"),
                 pattern = "*.csv",
                 full.names= TRUE)

samples_jrc_tmf <- filenames %>%
  map_dfr(read_csv, .id = "jrc_tmf") %>%
  janitor::clean_names() %>%
  select(-system_index,-geo)

## Deforestation year
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\jrc\\deforestation_year\\"),
                 pattern = "*.csv",
                 full.names= TRUE)

samples_jrc_defyr <- filenames %>%
  map_dfr(read_csv, .id = "jrc_def_yr") %>%
  janitor::clean_names() %>%
  select(-system_index,-geo)

## Gaveau data
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\gaveau\\"),
                 pattern = "*.csv",
                 full.names= TRUE)

samples_gaveau_landuse <- filenames %>%
  map_dfr(read_csv, .id = "gaveau") %>%
  janitor::clean_names() %>%
  select(-system_index,-geo)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Clean / prep data --------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Identify pixels that were cleared for pulp at some point in the time series
# gaveau_pulp <- samples_gaveau_landuse %>% 
#   mutate(ends_pulp = as.integer(id_2019==4)) %>% # Gaveau class 4 is industrial pulp clearing
#   select(sid, ends_pulp)
gaveau_pulp$ever_pulp <- samples_gaveau_landuse %>% 
  select(starts_with("id")) %>% 
  apply(1, function(c) as.integer(any (c==4))) # Gaveau class 4 is industrial pulp clearing

gaveau_pulp <- gaveau_pulp %>% 
  select(sid, ever_pulp)

### Create island mapping
island_tab <- tibble("island_code" = c(1, 2, 3, 4, 5, 6), "island" = c("balinusa", "kalimantan", "maluku", "papua", "sulawesi", "sumatera"))

### Identify pixels that started as forest in 1990
### NOTE: dataset currently doesn't include any pixels that are not forested at t0
forest_sids <- samples_jrc_tmf %>% 
  filter(dec1990 %in% c(1,2)) %>% # 1 = undisturbed tropical moist forest; 2 = degraded tmf
  pull(sid)

### Create pixel-level dataset starting with JRC deforestation year
### NOTE: Confirm that 0 means no deforestation observed in 1990-2019?
samples_df <- samples_jrc_defyr %>% 
  select(sid, island_code = jrc_def_yr, def_year = deforestation_year) %>%
  mutate(island_code = as.integer(island_code)) %>% 
  left_join(island_tab, by = "island_code")

### Join to gaveau, concession, island data
samples_df <- samples_df %>% 
  mutate(rand = runif(dim(samples_df)[1])) %>% 
  left_join(samples_hti,by="sid") %>%
  rename(supplier_id = ID) %>% 
  left_join(hti_dates_clean,by="supplier_id") %>%
  left_join(hti_concession_names,by="supplier_id") %>% 
  left_join(gaveau_pulp, by = "sid") %>% 
  select(sid, island, supplier_id, def_year, ever_pulp, license_year, supplier_label, rand)
samples_df <- samples_df %>% 
  mutate(start_for = sid %in% forest_sids)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Analyze deforestation timing --------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Identify different deforestation timings
lag <- 3
samples_df <- samples_df %>% 
  mutate(def_year = ifelse(def_year==0, ifelse(start_for == 1, 2999, 0), def_year)) %>%  
  mutate(defor_time = ifelse((def_year < license_year - 3), "Deforestation >3 years before license",  # Note: works because no licenses were issued prior to 1992 so JRC fully covers period of interest
                             ifelse((def_year >= license_year - 3) & (def_year < license_year), "Deforestation in 3 years prior to license",
                                    ifelse((def_year >= license_year) & (def_year < 2013), "Deforestation on licensed concession, before 2013",
                                           ifelse((def_year >= 2013) & (def_year != 2999), "Deforestation on licensed concession, after 2013",
                                                  ifelse((def_year == 2999), "Never deforested", 0))))),
         defor_pulp = ifelse(ever_pulp==1, paste0(defor_time, ", converted to pulp plantation"), paste0(defor_time, ", not converted to pulp plantation")))


### Generate frequency table by group
group_var <- "island" # Generally either island or supplier_label
class_var <- "defor_time" # Generally either defor_time or defor_pulp
freq_tab <- samples_df %>%
  group_by(.data[[group_var]], .data[[class_var]]) %>% 
  summarize(n = n()) %>% 
  mutate(freq = n / sum(n)) %>%
  ungroup()

freq_tab

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Plotting --------------------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
### Plot frequencies
freq_tab %>% 
  ggplot() +
  aes(y = .data[[group_var]], x = n, fill = .data[[class_var]]) +
  geom_bar(stat = "identity")
