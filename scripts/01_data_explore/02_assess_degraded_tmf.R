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
##        2) Gaveau landuse change - mapped land use change from 2001 - 2019 (IOPP,ITP and smallholders)
##        3)
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
library(ggsankey)

## credentials ----------------------------------------------

aws.signature::use_credentials()
bucket <- "trase-storage"
Sys.setenv("AWS_DEFAULT_REGION" = "eu-west-1")

## set working directory -------------------------------------

wdir <- "remote"

## read data -------------------------------------------------

## increase memory size
memory.limit(size=60000)

## load color palette
source("scripts\\001_color_palettes.R")

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
  map_dfr(read_csv, .id = "jrc_tmf_ac") %>%
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


############################################################################
# Clean / prep data --------------------------------------------------------
############################################################################

## clean hti concession names
hti_concession_names <- hti %>%
  st_drop_geometry() %>%
  select(supplier_id=ID,supplier=NAMOBJ) %>%
  mutate(supplier_label = paste0(supplier," (",supplier_id,")"))

## clean license dates
hti_dates_clean <- lic_dates_hti %>%
  mutate(YEAR = year(license_date)) %>%
  select(supplier_id=HTI_ID,license_year=YEAR)

## gaveau iopp,itp or smallholder
gaveau_landuse_2019 <- samples_gaveau_landuse %>%
  lazy_dt() %>%
  left_join(samples_hti,by="sid") %>%
  select(sid,supplier_id=ID,gaveau,starts_with("id_")) %>%
  as.data.table() %>%
  dt_pivot_longer(cols = c(-sid,-supplier_id,-gaveau),
                  names_to = 'year',
                  values_to = 'class') %>%
  as_tibble() %>%
  mutate(year = str_replace(year,"id_", ""),year = as.double(year)) %>%
  filter(year == 2019) %>%
  filter(class != 0 & class != 1 & class !=2)


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

## degraded tmf in at least one year
## melt annual changes dataset into long dataset
jrc_ac_long <- samples_jrc_tmf %>%
  as.data.table() %>%
  dt_pivot_longer(cols = c(-jrc_tmf_ac,-sid),
                  names_to = 'year',
                  values_to = 'class')

## join with HTI's and get areas by class
jrc_kali_degtmf <- samples_jrc_tmf %>%
  filter(jrc_tmf_ac == 2 | jrc_tmf_ac == 6)

jrc_kali_degtmf$deg <- (rowSums(jrc_kali_degtmf[,startsWith(names(jrc_kali_degtmf),"dec")]==2) >= 1) # jrc tmf class 2 is degraded tmf

# select columns
jrc_kali_degtmf <- jrc_kali_degtmf %>% 
  select(sid,deg) %>%
  filter(deg == 1)

## Join to gaveau pulp & concession
samples_gav_deg_df <- jrc_kali_degtmf %>% 
  as_tibble()%>%
  add_column(rand = runif(nrow(.))) %>%
  lazy_dt() %>%
  left_join(samples_hti,by="sid") %>%
  rename(supplier_id = ID) %>% 
  left_join(hti_dates_clean,by="supplier_id") %>%
  left_join(hti_concession_names,by="supplier_id") %>% 
  left_join(select(gaveau_landuse_2019,sid,class),by="sid") %>% 
  left_join(mill_supplier,by="supplier_id") %>%
  select(sid,supplier_id,supplier_label,deg,class,rand,app,april,marubeni) %>%
  as_tibble()
  
#########################################################################
### Checking ------------------------------------------------------------
#########################################################################

deg_lu <- samples_gav_deg_df %>%
  mutate(class = ifelse(is.na(class),0,class)) %>%
  filter(april == 1) %>%
  left_join(subset(lu_table,dataset =="gaveau landuse"),by="class") %>%
  mutate(class_desc = ifelse(is.na(class_desc),"others",class_desc)) %>%
  group_by(supplier_id,supplier_label,class_desc) %>%
  tally() %>%
  mutate(area_ha = n) %>%
  select(-n) %>%
  print()

### Plotting

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
  
deg_lu_plot <- deg_lu %>% 
  ggplot() +
  aes(y = reorder(supplier_label,area_ha), x = area_ha, fill = class_desc) +
  geom_bar(stat = "identity",position = position_fill(reverse = TRUE)) +
  theme_plot2 +
  xlab("") + ylab("")+
  #scale_x_continuous(labels = d3_format(".2~s",suffix = " ha"),expand = c(0,0)) +
  scale_x_continuous(labels = scales::percent_format()) +
  guides(fill = guide_legend(nrow = 1)) 

deg_lu_plot

## export to png
ggsave(deg_lu_plot,file=paste0(wdir,"\\01_data\\02_out\\plots\\APRIL\\april_degraded_tmf_gav_lu.png"), dpi=300, w=12, h=8,type="cairo-png",limitsize = FALSE)

## jrc tmf annual change transition plot

df <- samples_jrc_tmf %>%
  lazy_dt() %>%
  left_join(samples_hti,by="sid") %>%
  rename(supplier_id = ID) %>% 
  #filter(jrc_tmf_ac == 6) %>%
  filter(supplier_id == "H-0494") %>%
  as_tibble() %>%
  make_long(dec1990,dec1991,dec1992,dec1993,dec1994,dec1995,dec1996,dec1997,dec1998,dec1999,dec2000,
            dec2001,dec2002,dec2003,dec2004,dec2005,dec2006,dec2007,dec2008,dec2009,dec2010,dec2011,
            dec2012,dec2013,dec2014,dec2015,dec2016,dec2017,dec2018,dec2019) %>%
  mutate(x = str_replace(x,"dec", ""),next_x = str_replace(next_x,"dec",""),x = as.double(x),next_x = as.double(next_x)) 


lu_traj <- ggplot(df, aes(x = x, next_x = next_x, node = node, next_node = next_node, fill = factor(node), label = node)) +
  geom_sankey(flow.alpha = .75,node.color = "gray20",width=0.0001) +
  #geom_sankey_label(size = 3, color = "white", fill = "gray40") +
  scale_fill_manual(values=c("seagreen4","orange3","lightpink","yellowgreen","cornflowerblue","#F8F899"),
                    labels= c("Undisturbed TMF","Degraded TMF","Deforested land","Forest regrowth","Permanent or seasonal water",
                              "Other land cover"))+ 
  theme_sankey(base_size = 12) +
  scale_x_continuous(expand=c(0,0),breaks=seq(1990,2019,by=2)) +
  labs(x = NULL) +
  theme(text = element_text(family = "DM Sans",colour="#3A484F"),
        #legend.position = "none",
        plot.title = element_text(hjust = .5,size=9),
        legend.position = "bottom",
        legend.title = element_blank(),
        axis.text.x = element_text(size = 9, color = "grey30")) +
  guides(fill = guide_legend(override.aes = list(alpha = 0.75))) 

lu_traj

## export to png
ggsave(lu_traj,file=paste0(wdir,"\\01_data\\02_out\\plots\\jrc_ac_transition\\pt_tanjung_redeb_hutani_jrc_ac_transition.png"), dpi=300, w=10, h=6,type="cairo-png",limitsize = FALSE)



### Get JRC class in 2019 on Gaveau LU

# gaveau classes in 2019
gaveau_class_2019 <- samples_gaveau_landuse %>%
  lazy_dt() %>%
  left_join(samples_hti,by="sid") %>%
  select(sid,ID,id_2019) %>%
  as_tibble()

jrc_pulp_plot <- samples_jrc_tmf %>%
  lazy_dt() %>%
  select(sid,dec2019) %>%
  left_join(samples_hti,by="sid") %>%
  rename(supplier_id = ID) %>% 
  left_join(select(gaveau_class_2019,sid,id_2019),by="sid") %>% 
  left_join(hti_concession_names,by="supplier_id") %>% 
  left_join(mill_supplier,by="supplier_id") %>%
  select(sid,supplier_id,supplier_label,class=dec2019,gav_class=id_2019,app,april,marubeni) %>%
  filter(gav_class == 4 & april == 1) %>%
  left_join(subset(lu_table,dataset =="jrc tmf annual changes"),by="class") %>%
  as_tibble() %>%
  group_by(supplier_id,supplier_label,class_desc) %>%
  summarise(n = n()) %>%
  mutate(area_ha = n) %>%
  ggplot() +
  aes(y = reorder(supplier_label,area_ha), x = area_ha, fill = class_desc) +
  geom_bar(stat = "identity",position = position_fill(reverse = TRUE)) +
  theme_plot2 +
  xlab("") + ylab("")+
  scale_x_continuous(labels = scales::percent_format()) +
  guides(fill = guide_legend(nrow = 2)) 

## export to png
ggsave(jrc_pulp_plot,file=paste0(wdir,"\\01_data\\02_out\\plots\\APRIL\\april_gav_pulp_jrc_class.png"), dpi=300, w=12, h=8,type="cairo-png",limitsize = FALSE)
