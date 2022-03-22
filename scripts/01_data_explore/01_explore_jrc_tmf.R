## ---------------------------------------------------------
##
## Purpose of script: Exploring JRC TMF dataset annual change data within hti concessions
##
## Author: Jason Benedict
##
## Date Created: 2021-03-30
## 
## ---------------------------------------------------------
##
## Notes:
##   
##
## ---------------------------------------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------------------------------------

## load packages
library(tidyverse)
library(readxl)
library(tidylog)
library(data.table)
library(janitor)
library(lubridate)
library(myutil)
library(sf)
library(scales)
library(aws.s3)
library(showtext)
library(khroma) # palettes for color blindness
library(d3.format)


## credentials ------------------------------------------------

aws.signature::use_credentials()
bucket <- "trase-storage"
Sys.setenv("AWS_DEFAULT_REGION" = "eu-west-1")

## set working directory -------------------------------------

wdir <- "remote"

## read data and clean ---------------------------------------

# results
results <- read_delim(get_object(object="SUBNATIONAL/INDONESIA/WOOD_PULP/V3.0.1/INDONESIA_WOOD_PULP_V3_0_1.csv", "trase-results"), delim = ";", quote = "'", skip = 1) %>%
  mutate(supplier_id = str_replace(TRASE_ID_2, "ID-CONCESSION-", "H-"),year=TIME)

# wood supply
ws_hti <- read_delim(get_object(object="indonesia/wood_pulp/production/out/PULP_WOOD_SUPPLY_CLEAN_ALL_ALIGNED_2015_2019.csv", bucket), delim = ",")

# annual deforestation
annual_defor_hti <- read_csv(paste0(wdir,'\\01_data\\01_in\\gee\\annual_defor_hti.csv'))

# landuse in 2019
lu_2019_hti <- read_csv(paste0(wdir,'\\01_data\\01_in\\gee\\lu_classes_2019_by_hti.csv'))

# annual woodpulp planted areas in concessions
wp_annual_hti <- read_csv(paste0(wdir,'\\01_data\\01_in\\gee\\annual_woodpulp_areas_hti.csv'))

# hti license dates
lic_dates_hti <- readr::read_csv(paste0(wdir,"\\01_data\\01_in\\wwi\\HTI_LICENSE_DATES.csv"),
                                 col_types = cols(license_date = col_date("%m/%d/%Y")))

## jrc dataset
# forest cover changes since 1990 (JRC TMF dataset)
fc_jrc_hti <- read_csv(paste0(wdir, '\\01_data\\01_in\\gee\\jrc\\jrc_tmf_year_hti.csv'))

# deforested areas since 1990 (JRC TMF dataset)
def_jrc_hti <- read_csv(paste0(wdir, '\\01_data\\01_in\\gee\\jrc\\jrc_def_year_hti.csv'))

# degraded areas since 1990 (JRC TIMF dataset)
deg_jrc_hti <- read_csv(paste0(wdir, '\\01_data\\01_in\\gee\\jrc\\jrc_deg_tmf_year_hti.csv'))

# regrowth areas since 1990 (JRC TIMF dataset)
reg_jrc_hti <- read_csv(paste0(wdir, '\\01_data\\01_in\\gee\\jrc\\jrc_reg_tmf_year_hti.csv'))

# perm or seasonal water since 1990 (JRC TIMF dataset)
psw_jrc_hti <- read_csv(paste0(wdir, '\\01_data\\01_in\\gee\\jrc_perm_sw_tmf_year_hti.csv'))

# other LU since 1990 (JRC TIMF dataset)
oth_jrc_hti <- read_csv(paste0(wdir, '\\01_data\\01_in\\gee\\jrc_other_tmf_year_hti.csv'))


## merge and create annual changes map ------------------

# get active concessions in 2019
active_htis <- ws_hti %>%
  filter(YEAR == 2019 & str_detect(SUPPLIER_ID, "^H-")) %>%
  distinct(SUPPLIER_ID) %>%
  pull(SUPPLIER_ID)

# woodpulp planted areas
wp_gav_clean_hti <- wp_annual_hti %>%
  select(supplier_id=ID,starts_with("id")) %>%
  pivot_longer(cols = starts_with("id"),
               names_to = 'year',
               values_to = 'area_ha') %>%
  mutate(year = str_replace(year,"id_", "")) %>%
  mutate(year = as.double(year),source="gaveau") %>%
  mutate(class="Woodpulp planted area")


# area of forests in jrc dataset
fc_jrc_clean_hti <- fc_jrc_hti %>%
  select(supplier_id=ID,starts_with("Dec")) %>%
  pivot_longer(cols = starts_with("Dec"),
               names_to = 'year',
               values_to = 'area_ha') %>%
  mutate(year = str_replace(year,"Dec", "")) %>%
  mutate(year = as.double(year),source="jrc tmf") %>%
  mutate(class="04 - Tropical Moist Forest (TMF)")

# deforestation from jrc dataset
def_jrc_clean_hti <- def_jrc_hti %>%
  select(supplier_id=ID,starts_with("Dec")) %>%
  pivot_longer(cols = starts_with("Dec"),
               names_to = 'year',
               values_to = 'area_ha') %>%
  mutate(year = str_replace(year,"Dec", "")) %>%
  mutate(year = as.double(year),source="jrc tmf") %>%
  mutate(class = "01 - Deforestation")
  #mutate(diff = deforested_area_ha - lag(deforested_area_ha))

# degradation area from jrc dataset
deg_jrc_clean_hti <- deg_jrc_hti %>%
  select(supplier_id=ID,starts_with("Dec")) %>%
  pivot_longer(cols = starts_with("Dec"),
               names_to = 'year',
               values_to = 'area_ha') %>%
  mutate(year = str_replace(year,"Dec", "")) %>%
  mutate(year = as.double(year),source="jrc tmf") %>%
  mutate(class = "02 - Degradation")

# regrowth from jrc dataset
reg_jrc_clean_hti <- reg_jrc_hti %>%
  select(supplier_id=ID,starts_with("Dec")) %>%
  pivot_longer(cols = starts_with("Dec"),
               names_to = 'year',
               values_to = 'area_ha') %>%
  mutate(year = str_replace(year,"Dec", "")) %>%
  mutate(year = as.double(year),source="jrc tmf") %>%
  mutate(class = "03 - Regrowth")

# permanent or seasonal water  from jrc dataset
psw_jrc_clean_hti <- psw_jrc_hti %>%
  select(supplier_id=ID,starts_with("Dec")) %>%
  pivot_longer(cols = starts_with("Dec"),
               names_to = 'year',
               values_to = 'area_ha') %>%
  mutate(year = str_replace(year,"Dec", "")) %>%
  mutate(year = as.double(year),source="jrc tmf") %>%
  mutate(class = "Permanent / Seasonal Water")

# other landuse  from jrc dataset
oth_jrc_clean_hti <- oth_jrc_hti %>%
  select(supplier_id=ID,starts_with("Dec")) %>%
  pivot_longer(cols = starts_with("Dec"),
               names_to = 'year',
               values_to = 'area_ha') %>%
  mutate(year = str_replace(year,"Dec", "")) %>%
  mutate(year = as.double(year),source="jrc tmf") %>%
  mutate(class = "04 - Other landuse")


# merge
jrc_areas_hti <- fc_jrc_clean_hti %>%
  bind_rows(def_jrc_clean_hti) %>%
  bind_rows(deg_jrc_clean_hti) %>%
  bind_rows(reg_jrc_clean_hti) %>%
  bind_rows(wp_gav_clean_hti) %>%
  #bind_rows(psw_jrc_clean_hti) %>%
  #bind_rows(oth_jrc_clean_hti) %>%
  left_join(lic_dates_clean_hti,by="supplier_id") %>%
  left_join(hti_in_rpbbi,by=c("supplier_id")) %>%
  left_join(select(results,supplier_id,supplier_grp=`SUPPLIER GROUP`,supplier=`WOOD SUPPLIER`),by="supplier_id") %>%
  distinct() %>%
  drop_na(first_year_rpbbi)


# visualizing data -------------------------------------

# set up theme
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
                    legend.key.height = unit(12, "pt"),
                    legend.key.width = unit(12, "pt"),
                    legend.text = element_text(size = 9,colour="grey30"),
                    legend.title = element_blank(),
                    legend.position="bottom",
                    legend.box="horizontal",
                    plot.margin=unit(c(0.5,1.5,0.5,0.5),"cm"))

options(crayon.enabled = FALSE)

# selecting hti's for plot
#hti_selected <- c("H-0540","H-0536","H-0517","H-0476","H-0484","H-0471","H-0461","H-0494")

#jrc_areas_hti <- jrc_areas_hti %>%
#  filter(supplier_id %in% hti_selected)

p <- ggplot(subset(jrc_areas_hti,class != "Woodpulp planted area"),aes(year,area_ha)) +
     geom_area(aes(fill= class), position = 'stack') +
     scale_x_continuous(expand=c(0,0),breaks=seq(1990,2019,by=1)) +
     scale_y_continuous(labels = d3_format(".2~s",suffix = "ha"),expand = c(0,0.5)) +
     #geom_vline(aes(xintercept=as.numeric(first_year_rpbbi),color="First year RPBBI"),linetype="dashed",size=0.5)+
     geom_vline(aes(xintercept=as.numeric(license_year),color="License year"),size=0.5)+
     geom_point(data=subset(jrc_areas_hti,class == "Woodpulp planted area"),aes(x=year,y=area_ha,shape=class),color="black",size=1.5)+
     ylab("") +
     xlab("") +
     #scale_fill_muted() +
     scale_fill_manual(values=c("lightpink", "orange3", "yellowgreen", "seagreen4"))+ 
     scale_shape_manual(values=17,labels="Woodpulp planted area")+ 
     scale_color_manual(values = c("License year" = "palevioletred4")) +
     facet_wrap(~supplier,ncol=5,scales="free") +
     theme_plot

p

# export to csv
ggsave(p,file=paste0(wdir,"\\01_data\\02_out\\plots\\hti_jrc_class_areas.png"), dpi=400, w=30, h=50,type="cairo-png",limitsize = FALSE)

