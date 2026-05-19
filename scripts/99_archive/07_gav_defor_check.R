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

## JRC
## Annual changes 
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\jrc\\annual_changes\\"),pattern = "*.csv",full.names= TRUE)

samples_jrc_tmf <- filenames %>%
  map_dfr(read_csv, .id = "jrc_tmf_ac") %>%
  janitor::clean_names() %>%
  select(-system_index,-geo)

## Gaveau data
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\gaveau\\"),pattern = "*.csv",full.names= TRUE)

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
  filter(str_detect(SUPPLIER_ID, '^H-')) %>%
  select(supplier_id=SUPPLIER_ID,EXPORTER) %>%
  mutate(mill = case_when(EXPORTER == "OKI" ~ "app",
                          EXPORTER == "INDAH KIAT" ~ "app",
                          EXPORTER == "APRIL" ~ "april",
                          TRUE  ~ "marubeni")) %>%
  select(-EXPORTER) %>%
  distinct() 

# clean supplier groups
supplier_groups <- groups %>%
  select(supplier_id = id,supplier_group=group) %>%
  mutate(supplier_group = ifelse(is.na(supplier_group),"OTHER",supplier_group))

## identify pixels that started as forest in 2000 (based on TreeMaps modified Margono data)
forest_sids <- samples_gfc_margono_peat %>% 
  filter(primary == 100) %>% # 10: Water, 100: Forest, 300: Non-forest
  pull(sid)

#############################################################################
# Analyze Gaveau data -------------------------------------------------------
#############################################################################


gaveau_annual_pulp <- samples_gaveau_landuse %>%
  lazy_dt() %>%
  as.data.table() %>%
  dt_pivot_longer(cols = c(-gaveau,-sid),
                  names_to = 'year',
                  values_to = 'class') %>%
  as_tibble() %>%
  #select(-gaveau) %>%
  filter(class != "0") %>%
  mutate(year = str_replace(year,"timberdeforestation_", ""),year = as.double(year)) %>%
  mutate(island = case_when(gaveau == "1" ~ "Kalimantan",
                            gaveau == "2" ~ "Papua",
                            gaveau == "3" ~ "Sumatera",
                            TRUE ~ "None")) 

class_change <- gaveau_annual_pulp %>%
  arrange(sid,island,year) %>%
  group_by(island,sid) %>%
  mutate(conv_type = class - lag(class, default = first(class))) %>%
  ungroup() %>%
  group_by(year,island,conv_type) %>%
  summarize(area_ha = n()) %>%
  filter(conv_type != 0)


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

# Pulp conversion from forest and non-forest areas

p1 <- class_change %>%
  #filter(gaveau == 2) %>% # 1 = kalimantan, 2 = papua, 3= sumatera
  mutate(conv_type = as.character(conv_type)) %>%
  ggplot() +
  aes(y = area_ha, x = year, fill=conv_type) +
  xlab("\nYear") +
  ylab("Area (ha)") + 
  scale_y_continuous(expand=c(0,0),labels = d3_format(".2~s",suffix = ""))+
  scale_x_continuous(expand=c(0,0),breaks=seq(2000,2022,by=1)) +
  scale_fill_manual(values=c( "#e7298a","pink"),labels = c("Non-forest to pulpwood","Forest to pulpwood"))+ 
  geom_col() +
  facet_wrap(~island,ncol=1,scales="free") +
  theme_plot
p1

ggsave(p1,file=paste0(wdir,"\\01_data\\02_out\\plots\\000_data_exploration\\gaveau_defor\\gaveau_pulp_defor1.png"),dpi=300, w=8, h=12,type="cairo-png",limitsize = FALSE)

# Annual landuse traj
gav_lu_traj <- gaveau_pulp %>%
  group_by(year,island,class) %>%
  summarize(area_ha = n()) %>%
  drop_na(year)

p2 <- gav_lu_traj %>%
  ggplot() +
  aes(y = area_ha, x = year, fill=as.factor(class)) +
  geom_col() +
  xlab("\nYear") +
  ylab("Area (ha)") + 
  scale_y_continuous(expand=c(0,0),labels = d3_format(".2~s",suffix = ""))+
  scale_x_continuous(expand=c(0,0),breaks=seq(2000,2022,by=1)) +
  scale_fill_manual(values=c( "yellowgreen","orange3","lightpink"),labels = c("Forest","Non forest","Pulp"))+ 
  guides(fill = guide_legend(nrow = 1),keyheight = 10) +
  facet_wrap(~island,ncol=1,scales="free") +
  theme_plot

p2

ggsave(p2,file=paste0(wdir,"\\01_data\\02_out\\plots\\000_data_exploration\\gaveau_defor\\gav_lu_traj.png"),dpi=300, w=6, h=12,type="cairo-png",limitsize = FALSE)

