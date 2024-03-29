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
library(yardstick)

## credentials ----------------------------------------------

aws.signature::use_credentials()
bucket <- "trase-storage"
Sys.setenv("AWS_DEFAULT_REGION" = "eu-west-1")

## set working directory -------------------------------------

wdir <- "remote"

## read data -------------------------------------------------

# code counts
mb_gav_code <- read_excel(paste0(wdir,"\\01_data\\02_out\\tables\\mapbiomas_gaveau_code_counts.xlsx"))

# idn wide samples 4x4 km
# gaveau
gav_samples <- read_csv(paste0(wdir,"\\01_data\\02_out\\gee\\idn\\idn_gaveau_classes_sampled.csv"))
# mapbiomas
mb_samples <- read_csv(paste0(wdir,"\\01_data\\02_out\\gee\\idn\\idn_mapbiomas_classes_sampled.csv"))

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

############################################################################
### Create confusion matrix ################################################
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

## reclass to match

### Gaveau class values
## 1 - Forest
## 2 - Non Forest
## 3 - IOPP - Industrial Palm Oil Plantation
## 4 - ITP - Industrial Timber Plantation
## 5 - Smallholder

yr <- 2019

# gaveau
gav_rc <- gaveau_lu_long %>%
   lazy_dt() %>%
   #filter(sid <= 1000000) %>%
   mutate(year = str_replace(year,"id_", "")) %>% 
   filter(year == yr) %>%
   mutate(reclass = case_when(class == 1 | class == 2 ~ "3 - Other", # all other classes
                              class == 4 ~ "1 - Pulp", # pulp
                              class == 3 | class == 5 ~ "2 - Oil palm", # oil palm
                              TRUE ~ "3 - Other")) %>% as_tibble()
### MapBiomas class values
## 3    Forest 
## 5    Mangrove
## 9    Planted Forest
## 13   Natural Non Forest 
## 21   Other Agriculture Land
## 25   Other Non Vegetated Area
## 30   Mining
## 31   Aquaculture
## 33   Water
## 35   Palm Oil

# mapbiomas
mb_rc <- mapbiomas_lu_long %>%
   lazy_dt() %>%
   #filter(sid <= 1000000) %>%
   mutate(year = str_replace(year,"classification_", "")) %>% 
   filter(year == yr) %>%
   mutate(reclass = case_when(class == 3 | class == 5 | class == 13 | class == 21 | class == 25 | class == 30 | class == 31 | class == 33 ~ "3 - Other", # all other classes
                              class == 9 ~ "1 - Pulp", # pulp
                              class == 35 ~ "2 - Oil palm", # oil palm
                              TRUE ~ "3 - Other")) %>% as_tibble()

# merge datasets
merge_df <- gav_rc %>%
   left_join(mb_rc,by="sid") 

# convert levels as factors
merge_df$reclass.x <- as.factor(merge_df$reclass.x)
levels(merge_df$reclass.x) <- c("1 - Pulp","2 - Oil palm","3 - Other")
merge_df$reclass.y <- as.factor(merge_df$reclass.y)

# create conversion matrix
cm <- conf_mat(merge_df,reclass.x,reclass.y)

# create plot
cm_plot <- autoplot(cm, type = "heatmap") +
   xlab("\nGaveau") + ylab("Mapbiomas\n") +
   theme_heatmap +
   theme(legend.position = "none") +
   ggtitle(paste0("Confusion matrix for classes in ",yr,"")) +
   scale_fill_gradient(low = "pink", high = "cyan")

cm_plot

# save to png
ggsave(cm_plot,file=paste0(wdir,"\\01_data\\02_out\\plots\\mapbiomas_review\\gav_mb_cm_plot_2019.png"), dpi=400, w=8, h=5,type="cairo-png",limitsize = FALSE)

####################################
#### Indonesia wide ################
####################################

yr = 2019

# gaveau landuse
gaveau_idn_lu_long <- gav_samples %>%
   select(sid,starts_with("id_")) %>%
   pivot_longer(cols = c(-sid),
                   names_to = 'year',
                   values_to = 'class') %>%
   mutate(year = str_replace(year,"id_", "")) %>% 
   filter(year == yr) %>%
   mutate(reclass = case_when(class == 1 | class == 2 ~ "3 - Other", # all other classes
                              class == 4 ~ "1 - Pulp", # pulp
                              class == 3 | class == 5 ~ "2 - Oil palm", # oil palm
                              TRUE ~ "3 - Other"))

# mapbiomas landuse
mapbiomas_idn_lu_long <- mb_samples %>%
   select(sid,starts_with("classification_")) %>%
   pivot_longer(cols = c(-sid),
                names_to = 'year',
                values_to = 'class') %>%
   mutate(year = str_replace(year,"classification_", "")) %>% 
   filter(year == yr) %>%
   mutate(reclass = case_when(class == 3 | class == 5 | class == 13 | class == 21 | class == 25 | class == 30 | class == 31 | class == 33 ~ "3 - Other", # all other classes
                              class == 9 ~ "1 - Pulp", # pulp
                              class == 35 ~ "2 - Oil palm", # oil palm
                              TRUE ~ "3 - Other"))

# merge datasets
merge_cm_df <- gaveau_idn_lu_long %>%
   left_join(mapbiomas_idn_lu_long,by="sid") 

# convert levels as factors
merge_cm_df$reclass.x <- as.factor(merge_cm_df$reclass.x)
levels(merge_cm_df$reclass.x) <- c("1 - Pulp","2 - Oil palm","3 - Other")
merge_cm_df$reclass.y <- as.factor(merge_cm_df$reclass.y)

# create conversion matrix
idn_cm <- conf_mat(merge_cm_df,reclass.x,reclass.y)

# create data.frame
idn_cm_df <- as.data.frame(idn_cm[1]) %>%
   pivot_wider(names_from = "table.Truth",values_from = "table.Freq")

# create plot
idn_cm_plot <- autoplot(idn_cm, type = "heatmap") +
   xlab("\nGaveau") + ylab("Mapbiomas\n") +
   theme_heatmap +
   theme(legend.position = "none") +
   ggtitle(paste0("Confusion matrix for classes in ",yr,"")) +
   scale_fill_gradient(low = "pink", high = "cyan")

idn_cm_plot

# save to png
ggsave(idn_cm_plot,file=paste0(wdir,"\\01_data\\02_out\\plots\\mapbiomas_review\\idn_gav_mb_cm_plot_2019.png"), dpi=400, w=8, h=5,type="cairo-png",limitsize = FALSE)
# save to csv
write_csv(idn_cm_df,paste0(wdir,"\\01_data\\02_out\\plots\\mapbiomas_review\\idn_gav_mb_cm_plot_2019.csv"))
