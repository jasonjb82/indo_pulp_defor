## ---------------------------------------------------------
## 
## Project: Indonesia pulp deforestation
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
##        2) Gaveau landuse change - pulp deforestation (2000 - 2022) from TreeMap
##        3) JRC deforestation (1990 - 2022) - Vancutsem et.al (2021) - https://www.science.org/doi/10.1126/sciadv.abe1603
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
library(aws.s3)
library(dtplyr)
library(testthat)
library(d3.format)
library(tidyfast)
library(patchwork)
library(rcartocolor)
library(showtext)
library(khroma) # palettes for color blindness

## credentials ----------------------------------------------

aws.signature::use_credentials()
bucket <- "trase-storage"
Sys.setenv("AWS_DEFAULT_REGION" = "eu-west-1")

## set working directory -------------------------------------

wdir <- "remote"

## read data -------------------------------------------------
'%ni%' <- Negate('%in%') # filter out function

## load color palette
source("scripts\\001_misc\\001_color_palettes.R")

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
ws <- read_delim(get_object(object="indonesia/wood_pulp/production/out/PULP_WOOD_SUPPLY_CLEAN_ALL_ALIGNED_2015_2022.csv", bucket), delim = ",")

# ownership class
hti_ownership_class <- read_csv(paste0(wdir,"\\01_data\\02_out\\tables\\hti_company_ownership_reclass.csv"))

# kabupaten
kab <- read_sf(paste0(wdir,"\\01_data\\01_in\\big\\idn_kabupaten_big.shp"))
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
mills <- s3read_using(read_excel, object = "indonesia/wood_pulp/logistics/out/mills/MILLS_EXPORTERS_20200405.xlsx", bucket = bucket)

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

## JRC
## Annual changes 
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\jrc\\annual_changes\\"),pattern = "*.csv",full.names= TRUE)

samples_jrc_tmf <- filenames %>%
  map_dfr(read_csv) %>%
  janitor::clean_names() 

## Deforestation year
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\jrc\\deforestation_year\\"),
                 pattern = "*.csv",
                 full.names= TRUE)

samples_jrc_defyr <- filenames %>%
  map_dfr(read_csv) %>%
  janitor::clean_names() 

## Gaveau data
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\gaveau\\"),pattern = "*gaveau_classes.csv",full.names= TRUE)

samples_gaveau_landuse <- filenames %>%
  map_dfr(read_csv) %>%
  janitor::clean_names() 

## GFC deforestation, peat and Margono primary forest
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\gfc_peat\\"),
                 pattern = "*.csv",
                 full.names= TRUE)

samples_gfc_margono_peat <- filenames %>%
  map_dfr(read_csv) %>%
  janitor::clean_names() 

## GFC deforestation (modified by TreeMap)
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\gfc_ttm\\"),
                 pattern = "*.csv",
                 full.names= TRUE)

samples_gfc_ttm <- filenames %>%
  map_dfr(read_csv) %>%
  janitor::clean_names() 

# pulp conversion from forest (indonesia wide)
pulp_for_id <- read_csv(paste0(wdir,"\\01_data\\02_out\\gee\\gaveau\\pulp_annual_defor_forest_id.csv")) %>%
  select(-`system:index`,-constant,-.geo)

# pulp conversion from non-forest (indonesia wide)
pulp_nonfor_id <- read_csv(paste0(wdir,"\\01_data\\02_out\\gee\\gaveau\\pulp_annual_defor_non-forest_id.csv")) %>%
  select(-`system:index`,-constant,-.geo)

############################################################################
# Clean / prep data --------------------------------------------------------
############################################################################

## clean hti concession names
## Note: 2 supplying concessions - PT OKI PULP & PAPER MILLS (H-0656) & PT WANA SUBUR SAWIT INDAH (H-0657) are
## not HTI concessions but IPK concessions [wood utilization permit] included as they are suppliers to pulp mills
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

#############################################################################
# Combine datasets for analysis  --------------------------------------------
#############################################################################

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

### Join to gaveau, concession, jrc & gfc year of deforestation
samples_df <- samples_df %>% 
  as_tibble() %>%
  add_column(rand = runif(nrow(.))) %>%
  lazy_dt() %>%
  rename(supplier_id = ID) %>% 
  left_join(hti_dates_clean,by="supplier_id") %>%
  left_join(hti_concession_names,by="supplier_id") %>%
  left_join(samples_gfc_ttm,by="sid") %>%
  left_join(samples_jrc_defyr,by="sid") %>%
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

# Option of using deforestation for other areas within concessions from Hansen GFC (lossyear) / JRC TMF (def_yr)
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

# # other deforestation (JRC) # conversion type = 3
# hti_other_conv <- samples_df %>%
#   filter(pulp == "N" & start_for == "Y" & def_yr >= 2001) %>%
#   mutate(year = def_yr, conv_type = 3) %>%
#   group_by(year,supplier_id,supplier,supplier_label,license_year,island,conv_type) %>%
#   summarize(area_ha = n()) %>%
#   as_tibble()

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
  bind_rows(hti_other_conv) %>% # GFC Hansen / JRC deforestation within concessions / TTM GFC modified data
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

# write to csv
write_csv(hti_nonhti_conv,paste0(wdir,"\\01_data\\02_out\\tables\\idn_pulp_conversion_hti_nonhti_gaveau.csv"))

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
  #mutate(defor_time = case_when(year >= 2013 & year >= license_year & app == 1 ~ "Deforestation post-permit and after first ZDC of downstream mill",
  #                              year >= 2015 & year >= license_year & app == 0 & april == 1 ~ "Deforestation post-permit and after first ZDC of downstream mill",
  #                              year >= 2019 & year >= license_year & app == 0 & april == 0 & marubeni == 1  ~ "Deforestation post-permit and after first ZDC of downstream mill",
  #                              year >= license_year ~ "Deforestation post-permit",
  #                              year < license_year  ~ "Deforestation pre-permit",
  #                              TRUE ~ NA)) %>%
  mutate(conv_time = case_when(year >= 2013 & app == 1 ~ "Deforestation for pulp after first ZDC of downstream mill",
                                year >= 2015 & app == 0 & april == 1 ~ "Deforestation for pulp after first ZDC of downstream mill",
                                year >= 2019 & app == 0 & april == 0 & marubeni == 1  ~ "Deforestation for pulp after first ZDC of downstream mill",
                                TRUE ~ "Deforestation for pulp before first ZDC of downstream mill")) %>%
  group_by(supplier_id,supplier,supplier_label,license_year,island,april,app,marubeni,all,conv_type,conv_time) %>%
  summarize(area_ha = n()) %>%
  bind_rows(hti_for_areas) %>%
  mutate(class = ifelse(is.na(conv_time),"Never deforested",conv_time)) %>%
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
write_csv(hti_conv_timing,paste0(wdir,"\\01_data\\02_out\\tables\\hti_grps_zdc_pulp_conv_areas.csv"))

#########################################################################
# Plotting --------------------------------------------------------------
#########################################################################

## set up theme
theme_plot <- theme(text = element_text(family = "DM Sans",colour="#3A484F"),
                    panel.background = element_rect(colour=NA,fill=NA),
                    panel.grid.minor = element_blank(),
                    panel.grid.major.y = element_line(color="grey70",linetype="dashed",linewidth=0.35),
                    plot.title = element_text(hjust = 0.5),
                    axis.line.x = element_line(),
                    axis.ticks.x = element_blank(),
                    axis.ticks.y = element_blank(),
                    panel.spacing = unit(2, "lines"),
                    axis.text.x = element_text(size = 9, color = "grey30",angle = 45,hjust=1),
                    axis.text.y = element_text(size = 9, color = "grey30"),
                    axis.title.x = element_text(size = 10, color = "grey30"),
                    axis.title.y = element_text(size = 10, color = "grey30"),
                    strip.text.x = element_text(size = 12, face = "bold",color="grey30"),
                    strip.background = element_rect(color=NA, fill=NA),
                    legend.key = element_rect(linewidth = 12, fill = "white", colour = NA),
                    legend.key.height = unit(10, "pt"),
                    legend.key.width = unit(10, "pt"),
                    legend.text = element_text(size = 8,colour="grey30"),
                    legend.title = element_blank(),
                    legend.position="bottom",
                    legend.box="horizontal",
                    plot.margin=unit(c(0.5,1.5,0.5,0.5),"cm"))

# set up theme
theme_plot2 <- theme(text = element_text(family = "DM Sans",colour="#3A484F"),
                     panel.background = element_rect(colour=NA,fill=NA),
                     panel.grid.minor = element_blank(),
                     panel.grid.major.x= element_line(color="grey70",linetype="dashed",size=0.35),
                     plot.title = element_text(hjust = 0.5),
                     axis.line.x = element_line(),
                     axis.ticks.x = element_blank(),
                     axis.ticks.y = element_blank(),
                     panel.spacing = unit(2, "lines"),
                     axis.text.x = element_text(size = 9, color = "grey30"),
                     axis.text.y = element_text(size = 9, color = "grey30"),
                     axis.title.x = element_text(size = 10, color = "grey30"),
                     axis.title.y = element_text(size = 10, color = "grey30"),
                     strip.text.x = element_text(size = 12, face = "bold",color="grey30"),
                     strip.background = element_rect(color=NA, fill=NA),
                     legend.key.height = unit(12, "pt"),
                     legend.key.width = unit(12, "pt"),
                     legend.text = element_text(size = 9,colour="grey30"),
                     legend.title = element_blank(),
                     legend.position="bottom",
                     legend.box="horizontal",
                     plot.margin=unit(c(0.5,1.5,0.5,0.5),"cm"))

options(crayon.enabled = FALSE)

## Deforestation for pulp within concessions
p1 <- hti_conv %>%
  mutate(conv_type = factor(conv_type,levels=c(3,1,2))) %>%
  #filter(app == 1) %>%
  #filter(supplier_id == "H-0526") %>%
  # filter(island == "Kalimantan") %>%
  # filter(conv_type %in% c(2,3)) %>%
  ggplot() +
  aes(y = area_ha, x = year, fill=as.factor(conv_type),color=as.factor(conv_type)) +
  geom_col() +
  xlab("\nYear") +
  ylab("Area (ha)") + 
  scale_y_continuous(expand=c(0,0),labels = d3_format(".2~s",suffix = ""))+
  scale_x_continuous(expand=c(0,0),breaks=seq(2000,2022,by=1)) +
  scale_fill_manual(values=c("#CC79A7","#E69F00","#009E73"),
                    breaks = c(3,1,2),
                    labels = c("Other deforestation\nwithin concessions","Non forest to pulp","Forest to pulp"))+ 
  scale_color_manual(values=c("#CC79A7","#E69F00","#009E73"),
                     breaks = c(3,1,2),
                     labels = c("Other deforestation\nwithin concessions","Non forest to pulp","Forest to pulp"))+ 
  guides(fill = guide_legend(nrow = 1,reverse = TRUE),color = guide_legend(nrow = 1,reverse = TRUE),keyheight = 10) +
  #facet_wrap(~supplier_label,ncol=1,scales="free") +
  theme_plot

p1

p1_island <- hti_conv %>%
  mutate(conv_type = factor(conv_type,levels=c(3,1,2))) %>%
  #filter(app == 1) %>%
  #filter(supplier_id == "H-0526") %>%
  filter(conv_type == 2) %>%
  ggplot() +
  aes(y = area_ha, x = year, fill=as.factor(island),color=as.factor(island)) +
  geom_col() +
  xlab("\nYear") +
  ylab("Area (ha)") + 
  scale_y_continuous(expand=c(0,0),labels = d3_format(".2~s",suffix = ""))+
  scale_x_continuous(expand=c(0,0),breaks=seq(2000,2022,by=1)) +
  scale_fill_manual(values=c("#CC79A7","#E69F00","#009E73"))+ 
  scale_color_manual(values=c("#CC79A7","#E69F00","#009E73"))+ 
  guides(fill = guide_legend(nrow = 1,reverse = TRUE),color = guide_legend(nrow = 1,reverse = TRUE),keyheight = 10) +
  #facet_wrap(~supplier_label,ncol=1,scales="free") +
  theme_plot

p1_island


## Deforestation for pulp within and outside HTI concessions
## Note: Island level deforestation (Sumatera, Kalimantan and Papua) should match the plots on the Nusantara Atlas blog post at the link below -
## https://nusantara-atlas.org/pulp-and-paper-driven-deforestation-in-indonesia-accelerates-in-2022/

p2 <- hti_nonhti_conv %>%
  #filter(april == 1) %>%
  #filter(supplier_id == "H-0565") %>%
  filter(island == "Papua") %>%
  ggplot() +
  aes(y = area_ha, x = year, fill=as.factor(conv_type),color=as.factor(conv_type)) +
  geom_col() +
  xlab("\nYear") +
  ylab("Area (ha)") + 
  scale_y_continuous(expand=c(0,0),labels = d3_format(".2~s",suffix = ""))+
  scale_x_continuous(expand=c(0,0),breaks=seq(2000,2022,by=1)) +
  scale_fill_manual(values=c("#E69F00","#009E73"),
                    breaks = c(1,2),
                    labels = c("Non forest to pulp","Forest to pulp"))+ 
  scale_color_manual(values=c("#E69F00","#009E73"),
                     breaks = c(1,2),
                     labels = c("Non forest to pulp","Forest to pulp"))+ 
  guides(fill = guide_legend(nrow = 1,reverse = TRUE),color = guide_legend(nrow = 1,reverse = TRUE),keyheight = 10) +
  #facet_wrap(~supplier_label,ncol=1,scales="free") +
  theme_plot

p2

p2_island <- hti_nonhti_conv %>%
  #filter(april == 1) %>%
  #filter(supplier_id == "H-0565") %>%
  # filter(island == "Papua") %>%
  filter(conv_type == 2) %>% 
  ggplot() +
  aes(y = area_ha, x = year, fill=as.factor(island),color=as.factor(island)) +
  geom_col() +
  xlab("\nYear") +
  ylab("Area (ha)") + 
  scale_y_continuous(expand=c(0,0),labels = d3_format(".2~s",suffix = ""))+
  scale_x_continuous(expand=c(0,0),breaks=seq(2000,2022,by=1)) +
  scale_fill_manual(values=c("#CC79A7","#E69F00","#009E73"))+ 
  scale_color_manual(values=c("#CC79A7","#E69F00","#009E73"))+ 
  guides(fill = guide_legend(nrow = 1,reverse = TRUE),color = guide_legend(nrow = 1,reverse = TRUE),keyheight = 10) +
  #facet_wrap(~supplier_label,ncol=1,scales="free") +
  theme_plot

p2_island

## Deforestation timing plot

top_5_hti_deforesters_pp_after_zdc <- hti_conv_timing %>%
  filter(class == "Deforestation for pulp after first ZDC of downstream mill" & conv_type == 2) %>%
  arrange(-area_ha) %>%
  slice(1:5) %>%
  print()

## manual reordering
order <- c("SINAR MAS","ROYAL GOLDEN EAGLE / TANOTO","SUMITOMO",
           "KORINDO","DJARUM","MARUBENI","GOVERNMENT","ADR","OTHER")

## set plot order
plot_order_deft_pulp <- c(
  "Never deforested",
  "Deforestation not for pulp",
  "Deforestation for pulp before first ZDC of downstream mill",
  "Deforestation for pulp after first ZDC of downstream mill")

## Generate frequency table by group
group_var <- "supplier_group" # Generally either island, supplier_group or supplier_label
mill_var <- "all" # Generally either april,app,marubeni or all (all concessions)

freq_tab <- hti_conv_timing %>%
  left_join(hti_ownership_class,by=c("supplier_id")) %>%
  filter(supplier_id != "H-0657" & supplier_id != "H-0656") %>% # remove 2 IPKs (Non HTI suppliers)
  filter(!!sym(mill_var) == 1) %>%
  filter(conv_type == 2 | is.na(conv_type)) %>%
  #filter(island == "kalimantan") %>% # filter to island if required
  group_by(.data[[group_var]],linked_group,ownership_class,class) %>% 
  summarize(area_ha = sum(area_ha)) %>% 
  mutate(freq = area_ha / sum(area_ha)) %>%
  ungroup()

freq_tab

## plot frequencies
freq_plot <- freq_tab %>% 
  as_tibble() %>%
  mutate(label_order = factor(!!sym(group_var),rev(order))) %>%
  ggplot() +
  aes(y = label_order, x = area_ha, fill = factor(class,levels=plot_order_deft_pulp)) +
  geom_bar(stat = "identity",position = position_stack(reverse = TRUE)) +
  theme_plot2 +
  xlab("") + ylab("") +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 10)) +
  scale_x_continuous(labels = d3_format(".2~s",suffix = " ha"),expand = c(0,0)) +
  guides(fill = guide_legend(nrow = 2)) +
  scale_fill_manual(values = cols,name ="Group",
                    breaks=plot_order_deft_pulp,labels=plot_order_deft_pulp)

freq_plot

# stacked percent plot
freq_conv_perc_plot <- freq_tab %>% 
  as_tibble() %>%
  filter(class != "Never deforested") %>%
  mutate(label_order = factor(!!sym(group_var),rev(order))) %>%
  ggplot() +
  aes(y = label_order, x = area_ha, fill = factor(class,levels=rev(plot_order_deft_pulp))) +
  geom_bar(stat = "identity",position = "fill") +
  theme_plot2 +
  xlab("") + ylab("")+
  scale_x_continuous(labels = percent,expand = c(0,0)) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 10)) +
  guides(fill = guide_legend(nrow = 4)) +
  scale_fill_manual(values = cols,name ="Group",
                    breaks=plot_order_deft_pulp,labels=plot_order_deft_pulp) +
  theme(legend.position = "none")

freq_conv_perc_plot

# merging plots
freq_comb <- (freq_plot + plot_layout(guides = "collect") & theme(legend.position = "bottom")) + (freq_conv_perc_plot + theme(legend.position = "none")) 
freq_comb

## save plot to png
ggsave(freq_comb,file=paste0(wdir,"\\01_data\\02_out\\plots\\001_figures\\supplier_groups_defor_class_plot.png"), dpi=400, w=10, h=5,type="cairo-png",limitsize = FALSE)
