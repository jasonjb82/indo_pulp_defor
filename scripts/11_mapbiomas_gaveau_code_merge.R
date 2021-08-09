## ---------------------------------------------------------
## 
## Project: Indonesia pulp deforestation
##
## Purpose of script:
##
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Date Created: 2021-06-07
## 
## ---------------------------------------------------------
##
## Notes: Input datasets
##        1) HTI concessions (boundaries) and concession start year - from KLHK
##        2) Gaveau landuse change - commodity deforestation (2000 - 2019) (IOPP,ITP and smallholders)
##
##
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
library(tidylog)
library(data.table)
library(janitor)
library(lubridate)
library(sf)
library(scales)
library(aws.s3)
library(dtplyr)
library(testthat)
library(tidyfast)
library(writexl)

## credentials ----------------------------------------------

aws.signature::use_credentials()
bucket <- "trase-storage"
Sys.setenv("AWS_DEFAULT_REGION" = "eu-west-1")

## set working directory -------------------------------------

wdir <- "remote"

## read data -------------------------------------------------

## increase memory size
memory.limit(size=65000)

## load color palette
source("scripts\\001_color_palettes.R")

## HTI and sample IDs
samples_hti <- read_csv(paste0(wdir,"\\01_data\\02_out\\samples\\samples_hti_id.csv"))

## Gaveau data
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\gaveau\\"),
                 pattern = "*.csv",
                 full.names= TRUE)

samples_gaveau_landuse <- filenames %>%
  map_dfr(read_csv, .id = "gaveau") %>%
  janitor::clean_names() %>%
  select(-system_index,-geo)

## Mapbiomas data
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\mapbiomas\\"),
                 pattern = "*.csv",
                 full.names= TRUE)

samples_mapbiomas_landuse <- filenames %>%
  map_dfr(read_csv, .id = "mapbiomas") %>%
  janitor::clean_names() %>%
  select(-system_index,-geo)


############################################################################
# Clean / prep data --------------------------------------------------------
############################################################################

## convert to long dataset

# gaveau landuse
gaveau_lu_long <- samples_gaveau_landuse %>%
  add_column(rand = runif(nrow(.))) %>%
  lazy_dt() %>%
  left_join(samples_hti,by="sid") %>%
  select(sid,gaveau,rand,starts_with("id_")) %>%
  as.data.table() %>%
  dt_pivot_longer(cols = c(-gaveau,-sid,-rand),
                  names_to = 'year',
                  values_to = 'class') 

# mapbiomas landuse
mapbiomas_lu_long <- samples_mapbiomas_landuse %>%
  add_column(rand = runif(nrow(.))) %>%
  lazy_dt() %>%
  left_join(samples_hti,by="sid") %>%
  select(sid,mapbiomas,rand,starts_with("classification_")) %>%
  as.data.table() %>%
  dt_pivot_longer(cols = c(-mapbiomas,-sid,-rand),
                  names_to = 'year',
                  values_to = 'class')

## first year of pulp for each dataset (99 if never pulp)
gaveau_first_pulp <- gaveau_lu_long %>%
   lazy_dt() %>%
   #filter(rand < 0.01) %>%
   #filter(sid <= 300000) %>%
   mutate(year = str_replace(year,"id_", "")) %>% 
   mutate(class = ifelse(class !=4,99,class)) %>%
   arrange(sid) %>%
   group_by(sid,class) %>%
   filter(year == min(year)) %>% 
   slice(1) %>% # takes the first occurrence if there is a tie
   mutate(code_gav = substr(year,3,4),
          code_gav = as.character(code_gav),
          code_gav = ifelse(class == 99,class,code_gav),
          code_gav = str_pad(code_gav, side = "left", width = 2, pad = "0")) %>%
   ungroup() %>%
   select(sid,code_gav) %>%
   group_by(sid) %>%
   filter(code_gav == min(code_gav)) # get first year of pulp for sid
   #as_tibble()

mb_first_pulp <- mapbiomas_lu_long %>%
   lazy_dt() %>%
   #filter(rand < 0.01) %>%
   #filter(sid <= 300000) %>%
   mutate(year = str_replace(year,"classification_", "")) %>% 
   mutate(class = ifelse(class !=9,99,class)) %>%
   arrange(sid) %>%
   group_by(sid,class) %>%
   filter(year == min(year)) %>% 
   slice(1) %>% # takes the first year of pulp / non-pulp by sid
   mutate(code_mb = substr(year,3,4),
          code_mb = as.character(code_mb),
          code_mb = ifelse(class == 99,class,code_mb),
          code_mb = str_pad(code_mb, side = "left", width = 2, pad = "0")) %>%
   ungroup() %>%
   select(sid,code_mb) %>%
   group_by(sid) %>%
   filter(code_mb == min(code_mb))  # get first year of pulp for sid
   #as_tibble()


## check to see if no of samples match
no_samples_gav <- samples_gaveau_landuse %>%
   select(sid) %>%
   nrow()

no_samples_gav_fp <- gaveau_first_pulp %>%
   as_tibble() %>%
   select(sid) %>%
   nrow() 

expect_true(all.equal(no_samples_gav,no_samples_gav_fp))

## combine and get count by code
 
mb_gav_merge <- gaveau_first_pulp %>%
   left_join(mb_first_pulp,by="sid") %>%
   mutate(code = paste(code_gav,code_mb,sep="")) %>%
   group_by(code) %>%
   summarize(count=n()) %>%
   as_tibble()

## write to csv
write_xlsx(mb_gav_merge,paste0(wdir,"\\01_data\\02_out\\tables\\mapbiomas_gaveau_code_counts.xlsx"))
 
 