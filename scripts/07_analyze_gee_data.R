## ---------------------------------------------------------
##
## Purpose of script: Analysing GEE extracted datasets - JRC, GFC, peat, Gaveau maps and burn areas from KLHK
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
library(d3.format)

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

# wood supply
ws <- read_delim(get_object(object="indonesia/wood_pulp/production/out/PULP_WOOD_SUPPLY_CLEAN_ALL_ALIGNED_2015_2019.csv", bucket), delim = ",")

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
  select(-system_index,-geo,-burn_areas) %>%
  pivot_longer(cols = starts_with("x"),
               names_to = 'year',
               values_to = 'burn_area') %>%
  mutate(year = as.numeric(str_sub(year, 2,6)))


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
  left_join(select(gaveau_pulp,supplier_id,year,shr_gav_lu_areas,gav_class),by=c("year","supplier_id")) %>%
  ungroup()

# filter by island
jrc_ac_island <-  jrc_ac %>%
  filter(jrc_tmf == 2)  # filter by island (1 - balinusa, 2 - kalimantan, 3 - maluku, 4 - papua, 5 - sulawesi, 6 - sumatera)
 
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

## Analyzing deforestation

# get unique sid's and island values
jrc_island <- jrc_ac %>%
  select(supplier_id,island_code=jrc_tmf) %>%
  distinct()

# get remaining tmf in 2019
hti_rem_tmf <- jrc_ac %>%
  filter(class_desc == "undisturbed tropical moist forest (tmf)" & year == 2019) %>%
  select(supplier_id,n) %>%
  mutate(rem_tmf_area_ha = n)
  
# get expanded list of concession and years  
hti_all_years <- hti_concession_names %>%
  mutate(from = 1982,to = 2019) %>%
  rowwise() %>% 
  do(data_frame(supplier_id = .$supplier_id, supplier=.$supplier, supplier_label=.$supplier_label,year=.$from:.$to))

# calculate deforestation before, after and 3 yrs before license year
jrc_defor <- samples_jrc_defyr %>%
  left_join(samples_hti,by="sid") %>%
  group_by(supplier_id=ID,year=deforestation_year) %>%
  tally() %>%
  filter(year > 0) %>%
  mutate(area_ha = n) %>%
  left_join(hti_dates_clean,by="supplier_id") %>%
  right_join(hti_all_years,by=c("supplier_id","year")) %>%
  mutate(area_ha = ifelse(is.na(area_ha),0,area_ha)) %>%
  group_by(supplier_id) %>%
  fill(license_year, .direction = "downup") %>%
  mutate(yrs_after_lic = year - license_year) %>%
  arrange(supplier_id,year) %>%
  group_by(supplier_id) %>% 
  mutate(cum_defor_area_ha = cumsum(area_ha)) %>%
  group_by(supplier_id,supplier,supplier_label,license_year) %>%
  summarize(def_3yrs_before_licyr = sum(area_ha[yrs_after_lic < 0 & yrs_after_lic >= -3]),
            def_before_licyr = sum(area_ha[yrs_after_lic < -3]),
            def_after_licyr = sum(area_ha[yrs_after_lic >= 0])) %>%
  left_join(hti_rem_tmf,by="supplier_id") %>%
  pivot_longer(cols= c(rem_tmf_area_ha,starts_with("def")),
               values_to = "area_ha") %>%
  arrange(desc(name),area_ha) %>%
  ungroup() %>%
  left_join(jrc_island,by="supplier_id") %>%
  group_by(supplier_id,supplier,supplier_label) %>% mutate(id = row_number()) %>%
  ungroup()

# plot bar

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

# set island code (1 - balinusa, 2 - kalimantan, 3 - maluku, 4 - papua, 5 - sulawesi, 6 - sumatera)
code = 1

o <- jrc_defor %>% 
  filter(island_code == code) %>% # select island code
  filter(name == "rem_tmf_area_ha") %>% 
  pull("supplier_label")

def_ba_plot <- jrc_defor %>% 
  mutate(name = factor(name,c('def_after_licyr','def_3yrs_before_licyr','def_before_licyr','rem_tmf_area_ha'))) %>%
  filter(island_code == code) %>%
  mutate(supplier_label = factor(supplier_label,o)) %>% 
  ggplot() +
  aes(y = supplier_label, x = area_ha, fill = name) +
  geom_bar(stat = "identity") +
  #coord_flip() +
  theme_plot2 +
  xlab("") + ylab("")+
  scale_x_continuous(labels = d3_format(".2~s",suffix = " ha"),expand = c(0,0)) +
  scale_fill_manual(values=c("seagreen4","palevioletred","slateblue1","yellow3"),
  breaks=c('rem_tmf_area_ha','def_before_licyr','def_3yrs_before_licyr','def_after_licyr'),
  labels= c("Remaining TMF","Deforestation before license year (more than 3 yrs)","Deforestation 3 yrs before license year","Deforestation after license year"))

def_ba_plot

# export to png
ggsave(def_ba_plot,file=paste0(wdir,"\\01_data\\02_out\\plots\\jrc_deforestation\\balinusa_def_areas_ba_licyr_plot.png"),
       dpi=400, w=15, h=6,type="cairo-png",limitsize = FALSE)


# hansen annual deforestation
hansen_annual_defor <- samples_gfc_peat %>%
  left_join(samples_hti,by="sid") %>%
  select(ID,sid,lossyear) %>%
  group_by(supplier_id=ID,year=lossyear+2000) %>%
  tally() %>%
  mutate(area_ha = n) %>%
  left_join(hti_dates_clean,by="supplier_id") %>%
  right_join(hti_all_years,by=c("supplier_id","year")) %>%
  group_by(supplier_id) %>%
  fill(license_year, .direction = "downup") %>%
  select(supplier_id,year,defor_hansen_area_ha=area_ha)

# jrc annual deforestation
jrc_annual_defor <- samples_jrc_defyr %>%
  left_join(samples_hti,by="sid") %>%
  group_by(supplier_id=ID,year=deforestation_year) %>%
  tally() %>%
  filter(year > 0) %>%
  mutate(area_ha = n) %>%
  left_join(hti_dates_clean,by="supplier_id") %>%
  right_join(hti_all_years,by=c("supplier_id","year")) %>%
  mutate(area_ha = ifelse(is.na(area_ha),0,area_ha)) %>%
  group_by(supplier_id) %>%
  fill(license_year, .direction = "downup") %>%
  mutate(yrs_after_lic = year - license_year) %>%
  arrange(supplier_id,year) %>%
  select(supplier_id,supplier,supplier_label,year,license_year,defor_jrc_area_ha=area_ha) 


# set island code (1 - balinusa, 2 - kalimantan, 3 - maluku, 4 - papua, 5 - sulawesi, 6 - sumatera)
code = 1

# plot
def_hansen_jrc <- jrc_annual_defor %>%
  left_join(hansen_annual_defor,by=c("supplier_id","year")) %>%
  left_join(jrc_island,by="supplier_id") %>%
  #pivot_longer(cols = starts_with("defor"),values_to="area_ha") %>%
  #replace(is.na(.), 0) %>%
  filter(island_code == code) %>%
  ggplot() +
  geom_col(aes(x=year,y=defor_jrc_area_ha,fill=island_code)) +
  geom_line(aes(x=year,y=defor_hansen_area_ha,color=island_code),size=0.25) +
  geom_point(aes(x=year,y=defor_hansen_area_ha,color=island_code),size=0.75) +
  geom_vline(aes(xintercept=as.numeric(license_year),color="License year"),size=0.5)+
  xlab("") + ylab("") +
  scale_x_continuous(expand=c(0,0),breaks=seq(1982,2019,by=2)) +
  facet_wrap(~supplier_label,ncol=3,scales="free") +
  scale_y_continuous(expand=c(0,0.01)) +
  scale_fill_manual(labels="JRC deforestation",values="palevioletred2",guide=guide_legend()) +
  scale_color_manual(labels=c("Hansen deforestation","License year"),values=c("royalblue","black"),guide=guide_legend()) +
  theme_plot

def_hansen_jrc
  
# export to png
ggsave(def_hansen_jrc,file=paste0(wdir,"01_data\\02_out\\plots\\jrc_deforestation\\jrc_vs_hansen\\malu_jrc_vs_hansen_defor.png"),
       dpi=400, w=20, h=8,type="cairo-png",limitsize = FALSE)


### Analysis of APRIL wood suppliers ###

# get list of APRIL suppliers
ws_april <- ws %>%
  filter(EXPORTER_GROUP == "ROYAL GOLDEN EAGLE / TANOTO") %>%
  distinct() %>%
  pull(SUPPLIER_ID)

# filter deforestation to APRIL suppliers
jrc_defor_april <- jrc_defor %>%
  filter(supplier_id %in% ws_april)


# Deforestation before-after plot

o <- jrc_defor_april %>% 
  filter(name == "rem_tmf_area_ha") %>% 
  pull("supplier_label") 

o1 <- jrc_defor_april %>% 
  filter(name == "rem_tmf_area_ha") %>% 
  pull("supplier_id") 

# Only select suppliers from Top 15 (tmf remaining)
selected <- o1[45:31]

def_april_ba_plot <- jrc_defor_april %>% 
  filter(supplier_id %in% selected) %>%
  #top_n(10,rem_tmf) %>%
  mutate(name = factor(name,c('def_after_licyr','def_3yrs_before_licyr','def_before_licyr','rem_tmf_area_ha'))) %>%
  #filter(island_code == code) %>%
  mutate(supplier_label = factor(supplier_label,o)) %>% 
  ggplot() +
  aes(y = supplier_label, x = area_ha, fill = name) +
  geom_bar(stat = "identity") +
  #coord_flip() +
  theme_plot2 +
  xlab("") + ylab("")+
  scale_x_continuous(labels = d3_format(".2~s",suffix = " ha"),expand = c(0,0)) +
  scale_fill_manual(values=c("seagreen4","palevioletred","slateblue1","yellow3"),
                    breaks=c('rem_tmf_area_ha','def_before_licyr','def_3yrs_before_licyr','def_after_licyr'),
                    labels= c("Remaining TMF","Deforestation before license year (more than 3 yrs)","Deforestation 3 yrs before license year","Deforestation after license year"))

def_april_ba_plot

# export to png
ggsave(def_april_ba_plot,file=paste0(wdir,"\\01_data\\02_out\\plots\\jrc_deforestation\\APRIL\\april_top15_def_areas_ba_licyr_plot.png"),
       dpi=400, w=16, h=10,type="cairo-png",limitsize = FALSE)


# Annual deforestation plot

# pick 3 concessions
ws_april_selected <- c("H-0536","H-0476","H-0402")

jrc_ac_april <- jrc_ac %>%
  filter(supplier_id %in% ws_april_selected)

p <- ggplot(data=jrc_ac_april,aes(year,shr_class)) +
  geom_area(aes(fill= as.character(class_desc)), position = 'stack') +
  scale_x_continuous(expand=c(0,0),breaks=seq(1990,2019,by=2)) +
  scale_y_continuous(labels = d3_format(".2~s",suffix = "%"),expand = c(0,0)) +
  geom_vline(aes(xintercept=as.numeric(license_year),color="License year"),size=0.5)+
  geom_point(data=jrc_ac_april,aes(x=year,y=shr_gav_lu_areas,shape=gav_class),color="black",size=1.5)+
  ylab("") +
  xlab("") +
  #scale_fill_muted() +
  scale_fill_manual(values=c("lightpink", "orange3", "yellowgreen","seagreen4"))+ 
  scale_shape_manual(values=17,labels="Woodpulp planted area",na.translate=FALSE)+ 
  scale_color_manual(values = c("License year" = "palevioletred4")) +
  facet_wrap(~supplier_label,ncol=3,scales="free") +
  theme_plot

p

# export to png
ggsave(p,file=paste0(wdir,"\\01_data\\02_out\\plots\\APRIL\\april_hti_jrc_class_areas.png"), dpi=400, w=18, h=5,type="cairo-png",limitsize = FALSE)
