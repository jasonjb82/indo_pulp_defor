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
##        1) HTI concessions (boundaries) and concession start year - from KLHK
##        2) Gaveau landuse change - pulp deforestation (2000 - 2022)
##        3) JRC deforestation (1990 - 2020)
##        4) Wood types
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
library(concordance)
library(rcartocolor)
library(vistime)
library(showtext)
library(khroma) # palettes for color blindness

#showtext_auto()
#showtext_opts(dpi = 300)

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
hti <- read_sf(paste0(wdir,"\\01_data\\01_in\\klhk\\IUPHHK_HTI_20230314_proj.shp"))

# wood supply
ws <- read_delim(get_object(object="indonesia/wood_pulp/production/out/PULP_WOOD_SUPPLY_CLEAN_ALL_ALIGNED_2015_2021.csv", bucket), delim = ",")

# kabupaten
kab <- read_sf(paste0(wdir,"\\01_data\\01_in\\big\\idn_kabupaten_big.shp"))
prov_slim <- kab %>% select(prov,prov_code) %>% st_drop_geometry() %>% distinct() %>%
  mutate(prov_code = ifelse(prov == "PAPUA",92,prov_code))

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
  map_dfr(read_csv, .id = "jrc_tmf_ac") %>%
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
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\gaveau\\"),pattern = "*2022.csv",full.names= TRUE)

samples_gaveau_landuse <- filenames %>%
  map_dfr(read_csv, .id = "gaveau") %>%
  janitor::clean_names() %>%
  select(-system_index,-geo)

## GFC deforestation, peat and margono primary forest
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\gfc_peat\\"),
                 pattern = "*.csv",
                 full.names= TRUE)

samples_gfc_margono_peat <- filenames %>%
  map_dfr(read_csv, .id = "gfc_marg_peat") %>%
  janitor::clean_names() 

# pulp conversion from forest (indonesia wide)
pulp_for_id <- read_csv(paste0(wdir,"\\01_data\\02_out\\gee\\gaveau\\pulp_annual_defor_forest_id.csv")) %>%
  select(-`system:index`,-constant)

# pulp conversion from non-forest (indonesia wide)
pulp_nonfor_id <- read_csv(paste0(wdir,"\\01_data\\02_out\\gee\\gaveau\\pulp_annual_defor_non-forest_id.csv")) %>%
  select(-`system:index`,-constant)

############################################################################
# Clean / prep data --------------------------------------------------------
############################################################################

## create island mapping
island_tab <- tibble("island_code" = c(1, 2, 3, 4, 5, 6), "island" = c("balinusa", "kalimantan", "maluku", "papua", "sulawesi", "sumatera"))

## clean hti concession names
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
  lazy_dt() %>%
  as.data.table() %>%
  dt_pivot_longer(cols = c(-gaveau,-sid),
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
  select(sid, island_code = gfc_marg_peat,primary,lossyear) %>%
  mutate(island_code = as.integer(island_code),start_for = ifelse(primary == 100 & !is.na(primary),"Y","N")) %>% 
  left_join(island_tab, by = "island_code")

### Join to gaveau, concession, jrc & gfc year of deforestation
samples_df <- samples_df %>% 
  as_tibble() %>%
  add_column(rand = runif(nrow(.))) %>%
  lazy_dt() %>%
  left_join(samples_hti,by="sid") %>%
  rename(supplier_id = ID) %>% 
  left_join(hti_dates_clean,by="supplier_id") %>%
  left_join(hti_concession_names,by="supplier_id") %>%
  left_join(samples_jrc_defyr,by="supplier_id") %>%
  mutate(pulp = ifelse(sid %in% gaveau_pulp_sids,"Y","N"))
  
# gaveau pulp conversion
gaveau_annual_pulp <- samples_gaveau_landuse %>%
  lazy_dt() %>%
  as.data.table() %>%
  dt_pivot_longer(cols = c(-gaveau,-sid),
                  names_to = 'year',
                  values_to = 'class') %>%
  as_tibble() %>%
  select(-gaveau) %>%
  filter(class != "0") %>%
  mutate(year = str_replace(year,"timberdeforestation_", ""),year = as.double(year))

# deforestation for pulp
hti_pulp_conv <- gaveau_annual_pulp %>%
  lazy_dt() %>%
  as.data.table() %>%
  arrange(sid,year) %>%
  group_by(sid) %>%
  mutate(conv_type = class - lag(class, default = first(class))) %>%
  ungroup() %>%
  left_join(samples_df,by="sid") %>%
  group_by(year,supplier_id,supplier,supplier_label,license_year,island,conv_type) %>%
  summarize(area_ha = n()) %>%
  filter(conv_type != 0) %>%
  as_tibble()

# other deforestation (GFC)
hti_other_conv <- samples_df %>%
  filter(pulp == "N" & start_for == "Y" & !is.na(lossyear)) %>%
  mutate(year = lossyear + 2000, conv_type = 3) %>%
  group_by(year,supplier_id,supplier,supplier_label,license_year,island,conv_type) %>%
  summarize(area_ha = n()) %>%
  as_tibble()

# other deforestation (JRC)
hti_other_conv <- samples_df %>%
  filter(pulp == "N" & start_for == "Y" & deforestation_year >= 2001) %>%
  mutate(year = deforestation_year, conv_type = 3) %>%
  group_by(year,supplier_id,supplier,supplier_label,license_year,island,conv_type) %>%
  summarize(area_ha = n()) %>%
  as_tibble()

# annual deforestation outside concessions (from forest and non-forest)

pulp_defor_for_outside_hti <- pulp_for_id %>%
  select(-prov,-kab,-kab_code,-prov_code,-type,-.geo) %>%
  dt_pivot_longer(names_to = 'year',
               values_to = 'area_ha') %>%
  as_tibble() %>%
  filter(area_ha != "0") %>%
  mutate(year = str_replace(year,"deforestation_", ""),year = as.double(year)) %>%
  group_by(year) %>%
  summarize(area_ha = sum(area_ha)) %>%
  mutate(conv_type = "forest") 

pulp_defor_nonfor_outside_hti <- pulp_nonfor_id %>%
  select(-prov,-kab,-kab_code,-prov_code,-type,-.geo) %>%
  dt_pivot_longer(names_to = 'year',
                  values_to = 'area_ha') %>%
  as_tibble() %>%
  filter(area_ha != "0") %>%
  mutate(year = str_replace(year,"deforestation_", ""),year = as.double(year)) %>%
  group_by(year) %>%
  summarize(area_ha = sum(area_ha)) %>%
  mutate(conv_type = "non-forest") 

pulp_defor_outside_hti <- pulp_defor_for_outside_hti %>%
  bind_rows(pulp_defor_nonfor_outside_hti) %>%
  arrange(year) %>%

# merge deforestation
hti_conv <- hti_pulp_conv %>%
  bind_rows(hti_other_conv) %>%
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

#########################################################################
# Plotting --------------------------------------------------------------
#########################################################################

## set up theme
theme_plot <- theme(text = element_text(family = "DM Sans",colour="#3A484F"),
                    panel.background = element_rect(colour=NA,fill=NA),
                    panel.grid.minor = element_blank(),
                    panel.grid.major.y = element_line(color="grey70",linetype="dashed",size=0.35),
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
                    legend.key = element_rect(size = 12, fill = "white", colour = NA),
                    legend.key.height = unit(10, "pt"),
                    legend.key.width = unit(10, "pt"),
                    legend.text = element_text(size = 8,colour="grey30"),
                    legend.title = element_blank(),
                    legend.position="bottom",
                    legend.box="horizontal",
                    plot.margin=unit(c(0.5,1.5,0.5,0.5),"cm"))

options(crayon.enabled = FALSE)

hti_conv$conv_type = factor(hti_conv$conv_type, levels = c(3,1,2) )

p1 <- hti_conv %>%
  #filter(conv_type != 3) %>%
  #filter(supplier_id == "H-0565") %>%
  ggplot() +
  aes(y = area_ha, x = year, fill=as.factor(conv_type),color=as.factor(conv_type)) +
  geom_col() +
  xlab("\nYear") +
  ylab("Area (ha)") + 
  scale_y_continuous(expand=c(0,0),labels = d3_format(".2~s",suffix = ""))+
  scale_x_continuous(expand=c(0,0),breaks=seq(2000,2022,by=1)) +
  scale_fill_manual(values=c("#c194d4","orange1","yellowgreen"),
                    labels = c("Other deforestation","Non forest to pulp","Forest to pulp"))+ 
  scale_color_manual(values=c("#c194d4","orange1","yellowgreen"),
                     labels = c("Other deforestation","Non forest to pulp","Forest to pulp"))+ 
  guides(fill = guide_legend(nrow = 1,reverse = TRUE),color = guide_legend(nrow = 1,reverse = TRUE),keyheight = 10) +
  #facet_wrap(~supplier_label,ncol=1,scales="free") +
  theme_plot

p1

ggsave(p1,file="D:\\hti_annual_defor_type.png",dpi=300, w=8, h=6,type="cairo-png",limitsize = FALSE)
ggsave(p1,file=paste0(wdir,"\\01_data\\02_out\\plots\\000_data_exploration\\gaveau_defor\\gav_lu_traj.png"),dpi=300, w=6, h=12,type="cairo-png",limitsize = FALSE)

