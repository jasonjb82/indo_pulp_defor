## ---------------------------------------------------------
##
## Purpose of script: Analysing GEE extracted datasets - JRC, GFC, peat, Gaveau maps and burn areas
##
## Author: Jason Benedict
##
## Date Created: 2021-04-22
## 
## ---------------------------------------------------------
##
## Notes: Input data
##
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
library(sf)
library(scales)
library(aws.s3)
library(showtext)
library(khroma) # palettes for color blindness
library(nngeo)

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
  
## Transition map
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\jrc\\transition_map\\"),
                 pattern = "*.csv",
                 full.names= TRUE)

samples_jrc_tmap <- filenames %>%
  map_dfr(read_csv, .id = "jrc_tmap") %>%
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

## GFC and Peat
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\gfc_peat\\"),
                 pattern = "*.csv",
                 full.names= TRUE)

samples_gfc_peat <- filenames %>%
  map_dfr(read_csv, .id = "gfc_peat") %>%
  janitor::clean_names()


## Burn areas
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\burn_areas\\"),
                 pattern = "*.csv",
                 full.names= TRUE)

samples_klhk_ba <- filenames %>%
  map_dfr(read_csv, .id = "burn_areas") %>%
  janitor::clean_names() %>%
  select(-system_index,-geo)


## cleaning data ---------------------------------------------

# get clean hti concession names
hti_concession_names <- hti %>%
  st_drop_geometry() %>%
  select(supplier_id=ID,supplier=NAMOBJ) %>%
  mutate(supplier_label = paste0(supplier," (",supplier_id,")"))

# clean license dates
hti_dates_clean <- lic_dates_hti %>%
  mutate(YEAR = year(license_date)) %>%
  select(supplier_id=HTI_ID,license_year=YEAR)

# clean gaveau pulp areas
gaveau_pulp <- samples_gaveau_landuse %>%
  left_join(samples_hti,by="sid") %>%
  select(supplier_id=ID,gaveau,starts_with("id_")) %>%
  #filter(supplier_id == "H-0363") %>%
  pivot_longer(cols = starts_with("id_"),
               names_to = 'year',
               values_to = 'class') %>%
  mutate(year = str_replace(year,"id_", ""),year = as.double(year)) %>%
  mutate(gav_class = ifelse(class == 4,"Pulp","Others")) %>%
  group_by(supplier_id,year,gav_class) %>%
  summarize(n = n()) %>%
  group_by(supplier_id,year) %>%
  mutate(shr_gav_lu_areas = prop.table(n)*100) %>%
  filter(gav_class != "Others")

## Analysis and visualization ----------------------------

## JRC annual changes
jrc_ac <- samples_jrc_tmf %>%
  left_join(samples_hti,by="sid") %>%
  select(supplier_id=ID,jrc_tmf,starts_with("dec")) %>%
  #filter(supplier_id == "H-0363") %>%
  filter(jrc_tmf == 2) %>% # filter by island (1 - balinusa, 2 - kalimantan, 3 - maluku, 4 - papua, 5 - sulawesi, 6 - sumatera)
  pivot_longer(cols = starts_with("dec"),
               names_to = 'year',
               values_to = 'class') %>%
  filter(class <= 4) %>% # removing other land uses and permanent/seasonal water
  mutate(year = str_replace(year,"dec", "")) %>%
  mutate(year = as.double(year)) %>%
  group_by(jrc_tmf,supplier_id,year,class) %>%
  summarize(n = sum(class)) %>%
  group_by(jrc_tmf,supplier_id,year) %>%
  mutate(shr_class = prop.table(n)*100) %>%
  left_join(hti_dates_clean,by="supplier_id") %>%
  left_join(subset(lu_table,dataset =="jrc tmf annual changes"),by="class") %>%
  mutate(class_desc = ifelse(class == 5 | class == 6,"others",class_desc )) %>%
  left_join(hti_concession_names,by="supplier_id") %>%
  left_join(select(gaveau_pulp,supplier_id,year,shr_gav_lu_areas,gav_class),by=c("year","supplier_id"))
 
# visualizing jrc annual changes  -----------------------

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


p <- ggplot(data=jrc_ac,aes(year,shr_class)) +
  geom_area(aes(fill= as.character(class_desc)), position = 'stack') +
  scale_x_continuous(expand=c(0,0),breaks=seq(1990,2019,by=1)) +
  scale_y_continuous(labels = d3_format(".2~s",suffix = "%"),expand = c(0,0)) +
  geom_vline(aes(xintercept=as.numeric(license_year),color="License year"),size=0.5)+
  geom_point(data=jrc_ac,aes(x=year,y=shr_gav_lu_areas,shape=gav_class),color="black",size=1.5)+
  ylab("") +
  xlab("") +
  #scale_fill_muted() +
  scale_fill_manual(values=c("lightpink", "orange3", "yellowgreen","seagreen4"))+ 
  scale_shape_manual(values=17,labels="Woodpulp planted area",na.translate=FALSE)+ 
  scale_color_manual(values = c("License year" = "palevioletred4")) +
  facet_wrap(~supplier_label,ncol=5,scales="free") +
  theme_plot

p

# export to csv
ggsave(p,file=paste0(wdir,"\\01_data\\02_out\\plots\\kali_hti_jrc_class_areas.png"), dpi=400, w=35, h=75,type="cairo-png",limitsize = FALSE)

  