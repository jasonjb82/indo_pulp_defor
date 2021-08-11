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
library(readxl)
library(rcartocolor)

## credentials ----------------------------------------------

aws.signature::use_credentials()
bucket <- "trase-storage"
Sys.setenv("AWS_DEFAULT_REGION" = "eu-west-1")

## set working directory -------------------------------------

wdir <- "remote"

## read data -------------------------------------------------

mb_gav_code <- read_excel(paste0(wdir,"\\01_data\\02_out\\tables\\mapbiomas_gaveau_code_counts.xlsx"))

############################################################################
# Clean / prep data --------------------------------------------------------
############################################################################

mb_gav_code_shares <- mb_gav_code %>% 
   mutate(shares = prop.table(count)) %>%
   mutate(code_gav = substr(code,1,2),
          code_mb = substr(code,3,4)) %>%
   filter(code_gav != "99" & code_mb != "99") %>%
   #filter(code_gav != "00" & code_mb != "00") %>%
   select(-code,-count) 

############################################################################
# Plot share heatmap -------------------------------------------------------
############################################################################

# set up theme
theme_heatmap <- theme(text = element_text(family = "DM Sans",colour="#3A484F"),
                     panel.background = element_rect(colour=NA,fill=NA),
                     panel.grid.minor = element_blank(),
                     panel.grid.major = element_blank(),
                     plot.title = element_text(hjust = 0.5),
                     axis.line.x = element_blank(),
                     axis.ticks.x = element_blank(),
                     axis.ticks.y = element_blank(),
                     panel.spacing = unit(2, "lines"),
                     axis.text.x = element_text(size = 9, color = "grey30"),
                     axis.text.y = element_text(size = 9, color = "grey30"),
                     axis.title.x = element_text(size = 10, color = "grey30"),
                     axis.title.y = element_text(size = 10, color = "grey30"),
                     strip.text.x = element_text(size = 12, face = "bold",color="grey30"),
                     strip.background = element_rect(color=NA, fill=NA),
                     legend.key.height = unit(20, "pt"),
                     legend.key.width = unit(10, "pt"),
                     legend.text = element_text(size = 9,colour="grey30"),
                     #legend.title = element_blank(),
                     legend.position="right",
                     legend.box="vertical",
                     plot.margin=unit(c(0.5,1.5,0.5,0.5),"cm"))

options(crayon.enabled = FALSE)

# create plot
agplot <- ggplot(data=mb_gav_code_shares)+
   aes(y=code_gav,x=code_mb,fill=shares) +
   geom_tile() +
   scale_fill_carto_c(palette = "Sunset",direction = -1, name= "Share of overlap") +
   theme_heatmap +
   guides(colour = guide_legend()) +
   xlab("\nMapbiomas") +
   ylab("Gaveau\n")

agplot

# export plot to png
ggsave(agplot,file=paste0(wdir,"\\01_data\\02_out\\plots\\mapbiomas_review\\gav_mb_agreement_plot_v1.png"), dpi=400, w=8, h=5,type="cairo-png",limitsize = FALSE)


############################################################################
# Create data for observable comparison plot -------------------------------
############################################################################

### Read data

# hti concessions
hti <- read_sf(paste0(wdir,"\\01_data\\01_in\\klhk\\IUPHHK_HT_proj.shp"))

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

## clean hti concession names
hti_concession_names <- hti %>%
   st_drop_geometry() %>%
   select(supplier_id=ID,supplier=NAMOBJ) %>%
   mutate(supplier_label = paste0(supplier," (",supplier_id,")"))

### Clean data

## annual pulp planted areas - gaveau
gaveau_annual_pulp <- samples_gaveau_landuse %>%
   lazy_dt() %>%
   left_join(samples_hti,by="sid") %>%
   select(supplier_id=ID,gaveau,starts_with("id_")) %>%
   as.data.table() %>%
   dt_pivot_longer(cols = c(-supplier_id,-gaveau),
                   names_to = 'year',
                   values_to = 'class') %>%
   as_tibble() %>%
   mutate(year = str_replace(year,"id_", ""),year = as.double(year)) %>%
   mutate(gav_class = ifelse(class == 4,"Pulp","Others")) %>%
   group_by(supplier_id,year,gav_class) %>%
   summarize(n = n()) %>%
   group_by(supplier_id,year) %>%
   mutate(area_ha = n) %>%
   filter(gav_class != "Others")

## annual pulp planted areas - mapbiomas
mapbiomas_annual_pulp <- samples_mapbiomas_landuse %>%
   lazy_dt() %>%
   left_join(samples_hti,by="sid") %>%
   select(supplier_id=ID,mapbiomas,starts_with("classification_")) %>%
   as.data.table() %>%
   dt_pivot_longer(cols = c(-supplier_id,-mapbiomas),
                   names_to = 'year',
                   values_to = 'class') %>%
   as_tibble() %>%
   mutate(year = str_replace(year,"classification_", ""),year = as.double(year)) %>%
   mutate(mb_class = ifelse(class == 9,"Pulp","Others")) %>%
   group_by(supplier_id,year,mb_class) %>%
   summarize(n = n()) %>%
   group_by(supplier_id,year) %>%
   mutate(area_ha = n) %>%
   filter(mb_class != "Others")

annual_pulp_comb <- gaveau_annual_pulp %>%
   full_join(mapbiomas_annual_pulp,by=c("supplier_id","year")) %>%
   select(supplier_id,year,Gaveau=area_ha.x,Mapbiomas=area_ha.y) %>%
   pivot_longer(cols = -c(supplier_id,year),
                names_to = 'dataset',
                values_to = 'area_ha') %>%
   left_join(select(hti_concession_names,supplier_id,supplier_label),by="supplier_id") %>%
   select(supplier_id,supplier_label,year,area_ha,dataset)

# write to csv
write_csv(annual_pulp_comb,paste0(wdir,"\\01_data\\02_out\\tables\\mapbiomas_gaveau_pulp_areas.csv"))
