## ---------------------------------------------------------
## 
## Project: Indonesia pulp deforestation
##
## Purpose of script: Create plots of annual land cover change within HTI concessions
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
library(dtplyr)
library(testthat)
library(d3.format)
library(tidyfast)
library(patchwork)
library(concordance)
library(rcartocolor)
library(vistime)
library(extrafont)
library(showtext)
library(khroma) # palettes for color blindness

## set working directory -------------------------------------

wdir <- "remote"

## read data -------------------------------------------------
'%ni%' <- Negate('%in%') # filter out function

## load color palette
source("scripts\\001_misc\\001_color_palettes.R")

colorBlind8  <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## data lookup table
lu_table <- read_csv(paste0(wdir,"\\01_data\\02_out\\gee\\data_lookup_table.csv"))

## hti license dates
lic_dates_hti <- readr::read_csv(paste0(wdir,"\\01_data\\01_in\\wwi\\HTI_LICENSE_DATES.csv"),col_types = cols(license_date = col_date("%m/%d/%Y")))

## supplier groups
groups <- read_csv(paste0(wdir,"\\01_data\\01_in\\wwi\\ALIGNED_NAMES_GROUP_HTI.csv"))

## HTI and sample IDs
samples_hti <- read_csv(paste0(wdir,"\\01_data\\02_out\\samples\\samples_hti_id.csv"))

# hti concessions
hti <- read_sf(paste0(wdir,"\\01_data\\01_in\\klhk\\IUPHHK_HTI_20230314_proj.shp")) %>%
  filter(ID != "H-0553")

# wood supply
ws <- read_csv(paste0(wdir,"\\01_data\\01_in\\wwi\\PULP_WOOD_SUPPLY_CLEAN_ALL_ALIGNED_2020_2022.csv"))

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
mills <- read_excel(paste0(wdir,"\\01_data\\01_in\\wwi\\MILLS_EXPORTERS_20200405.xlsx"))

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

## GFC deforestation (modified by TreeMap)
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\gfc_ttm\\"),
                 pattern = "*.csv",
                 full.names= TRUE)

samples_gfc_ttm <- filenames %>%
  map_dfr(read_csv) %>%
  janitor::clean_names() 

## GFC deforestation, peat and Margono primary forest
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\gfc_peat\\"),
                 pattern = "*.csv",
                 full.names= TRUE)

samples_gfc_margono_peat <- filenames %>%
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
  drop_na(sid) %>%
  mutate(island_name = case_when(
    island == 1 ~ "Balinusa",
    island == 2 ~ "Kalimantan",
    island == 3 ~ "Maluku",
    island == 4 ~ "Papua",
    island == 5 ~ "Sulawesi",
    island == 6 ~ "Sumatera",
    TRUE ~ NA
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
  left_join(samples_jrc_defyr,by="supplier_id") %>%
  mutate(pulp = ifelse(sid %in% gaveau_pulp_sids,"Y","N"))

## melt annual changes dataset into long dataset
jrc_ac_long <- samples_jrc_tmf %>%
  as.data.table() %>%
  dt_pivot_longer(cols = c(-sid),
                  names_to = 'year',
                  values_to = 'class')

## join with HTI's and get areas by class
jrc_ac <- jrc_ac_long %>% 
  lazy_dt() %>%
  left_join(samples_hti,by="sid") %>%
  group_by(ID,island,year,class) %>%
  summarize(n = n()) %>%
  mutate(shr_class = prop.table(n)*100) 

## create table of annual pulp
supplier_year_tbl <- hti_concession_names %>%
  select(supplier_id) %>%
  group_by(supplier_id) %>%
  summarise(start = min(2001),
            end = max(2022)) %>%
  mutate(year = Map(seq, start, end)) %>%
  unnest(cols =year) %>%
  mutate(unique=1) %>%
  select(supplier_id,year)

## annual pulp planted areas - gaveau
gaveau_annual_pulp <- samples_gaveau_landuse %>%
  lazy_dt() %>%
  as.data.table() %>%
  dt_pivot_longer(cols = c(-sid),
                  names_to = 'year',
                  values_to = 'class') %>%
  mutate(year = str_replace(year,"timberdeforestation_", ""),year = as.double(year)) %>%
  mutate(gav_class = ifelse(class == 3,"Pulp","Others")) %>%
  left_join(samples_hti,by="sid") %>%
  as_tibble() %>%
  #select(supplier_id=ID,starts_with("timberdeforestation_")) %>%
  group_by(supplier_id=ID,year,gav_class) %>%
  summarize(n = n()) %>%
  group_by(supplier_id,year) %>%
  mutate(shr_gav_lu_areas = prop.table(n)*100) %>%
  filter(gav_class != "Others")

# # write to csv
# write_csv(gaveau_annual_pulp,paste0(wdir,"\\01_data\\02_out\\tables\\gaveau_annual_pulp_areas.csv"))

gav_tbl <- gaveau_annual_pulp %>%
  mutate(dataset = "gaveau",sh_area = shr_gav_lu_areas,pulp_area = n) %>%
  select(supplier_id,year,sh_area,pulp_area,dataset) 

## join with downstream mill supplier, license year,concession names and annual change class names
hti_jrc_ac <- jrc_ac %>%
  rename(supplier_id=ID) %>%
  left_join(subset(lu_table,dataset =="jrc tmf annual changes"),by="class") %>%
  mutate(class_desc = ifelse(class_desc == "permanent or seasonal water" | class_desc == "other land cover" | is.na(class_desc),"other land cover",class_desc )) %>%
  group_by(supplier_id,class_desc,year,island) %>%
  summarize(area_class = sum(n),shr_class=sum(shr_class)) %>%
  left_join(hti_dates_clean,by="supplier_id") %>%
  left_join(mill_supplier,by="supplier_id") %>%
  left_join(hti_concession_names,by="supplier_id") %>%
  mutate(year = str_replace(year,"dec", "")) %>%
  mutate(year = as.double(year)) %>%
  select(-supplier) %>%
  left_join(select(gaveau_annual_pulp,supplier_id,year,shr_gav_lu_areas,gav_lu_areas=n,gav_class),by=c("year","supplier_id")) %>%
  mutate(
    zdc_year = case_when(
      app == 1 ~ 2013,
      app == 0 & april == 1 ~ 2015,
      april == 0 & app == 0 & marubeni == 1 ~ 2019,
      TRUE ~ 0
    )
  ) %>%
  as_tibble() %>%
  mutate(zdc_year = ifelse(zdc_year ==0,NA_real_,zdc_year))

# Gaveau annual LU classes
# annual landuse

# calculate forest areas in 2000
gaveau_forest_2000 <- samples_gfc_margono_peat %>%
  lazy_dt() %>%
  left_join(samples_hti,by="sid") %>%
  group_by(supplier_id=ID,primary) %>%
  summarize(forest_area_ha = n()) %>%
  filter(!is.na(primary)) %>%
  as_tibble() %>%
  rowwise() %>% 
  transmute(supplier_id, primary, forest_area_ha,year = list(seq(2000, 2022))) %>% 
  unnest_longer(year)

conc_area <- samples_hti %>%
  group_by(supplier_id=ID) %>%
  summarize(conc_area_ha = n())

# list of codes of forest
forest_loss_codes <- c(101:122,401:422,601:622)

# merge and calculate remaining forest areas in each year
gaveau_annual_forest <- samples_gfc_ttm %>%
  lazy_dt() %>%
  left_join(samples_hti,by="sid") %>%
  mutate(year = round(gfc_ttm %% 100)+2000) %>%
  as_tibble() %>%
  filter(gfc_ttm %in% forest_loss_codes) %>%
  group_by(supplier_id=ID,island,year) %>%
  summarize(n = n()) %>%
  arrange(-desc(supplier_id),year) %>%
  group_by(supplier_id,island) %>% 
  mutate(cum_floss = cumsum(n)) %>%
  right_join(gaveau_forest_2000,by=c("supplier_id","year")) %>%
  #mutate(cum_floss = ifelse(is.na(cum_floss),0,cum_floss)) %>%
  arrange(-desc(supplier_id),year) %>%
  group_by(supplier_id) %>%
  fill(cum_floss,island,.direction="down") %>%
  mutate(cum_floss = ifelse(is.na(cum_floss),0,cum_floss),
         rem_forest_area_ha = forest_area_ha - cum_floss) %>%
  select(supplier_id,island,year,rem_forest_area_ha) %>%
  as_tibble() %>%
  right_join(supplier_year_tbl,by=c("supplier_id","year")) %>%
  arrange(-desc(supplier_id),year) %>%
  #mutate(rem_forest_area_ha = ifelse(is.na(rem_forest_area_ha),NA,rem_forest_area_ha)) %>%
  group_by(supplier_id) %>%
  fill(island,.direction="updown") %>%
  mutate(rem_forest_area_ha = ifelse(is.na(rem_forest_area_ha),0,rem_forest_area_ha))
  

# combine areas
gaveau_annual_lc <- gaveau_annual_forest %>%
  full_join(conc_area,by="supplier_id") %>%
  full_join(gaveau_annual_pulp,by=c("supplier_id","year")) %>%
  mutate(pulp_area_ha = ifelse(is.na(n),0,n)) %>%
  arrange(-desc(supplier_id),year) %>%
  group_by(supplier_id) %>%
  fill(conc_area_ha,rem_forest_area_ha,.direction = "updown") %>%
  mutate(other_land_ha = conc_area_ha - (rem_forest_area_ha + pulp_area_ha)) %>%
  select(year,supplier_id,island,pulp_area_ha,rem_forest_area_ha,other_land_ha,conc_area_ha) %>%
  pivot_longer(cols = -c(supplier_id,island,year),
              names_to = 'class',
              values_to = 'area_ha') %>%
  filter(class != "conc_area_ha") %>%
  mutate(class_desc = case_when(
    class == "pulp_area_ha" ~ "Cleared for pulp", # pulp planted area (need to check)
    class == "rem_forest_area_ha" ~ "Forest",
    class == "other_land_ha" ~ "Non-forest")) %>%
  select(year,supplier_id,island,class_desc,area_ha) %>%
  left_join(hti_dates_clean,by="supplier_id") %>%
  left_join(mill_supplier,by="supplier_id") %>%
  mutate(
    zdc_year = case_when(
      app == 1 ~ 2013,
      app == 0 & april == 1 ~ 2015,
      april == 0 & app == 0 & marubeni == 1 ~ 2019,
      TRUE ~ 0
    )
  ) %>%
  mutate(zdc_year = ifelse(zdc_year ==0,NA_real_,zdc_year))
  
#########################################################################
# Plotting --------------------------------------------------------------
#########################################################################

## set up theme
theme_plot <- theme(text = element_text(family = "DM Sans",colour="#3A484F"),
                    panel.background = element_rect(colour=NA,fill=NA),
                    panel.grid.minor = element_blank(),
                    panel.grid.major.y = element_line(color="grey70",linetype="dashed",linewidth=0.35),
                    plot.title = element_text(hjust = 0.5, face="bold"),
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


# Annual land classes plot (TMF + Gaveau pulp areas)
## filter for plot (by supplier)
# hti_jrc_ac_comb <- hti_jrc_ac %>%
#   as_tibble() %>%
#   filter(supplier_id == "H-0527")
# 
# hti_jrc_ac_plot <- ggplot(hti_jrc_ac_comb,aes(year,area_class)) +
#   geom_area(aes(fill= as.character(class_desc)), position = position_stack(reverse = T)) +
#   scale_x_continuous(expand=c(0,0),breaks=seq(1990,2022,by=1)) +
#   scale_y_continuous(labels = d3_format(".2~s",suffix = "ha"),expand = c(0,0)) +
#   geom_vline(aes(xintercept=as.numeric(license_year),color="License\nyear"),linewidth=0.5)+
#   geom_vline(aes(xintercept=as.numeric(zdc_year),color="Earliest ZDC year\nof downstream mill"),linewidth=0.5)+
#   #geom_point(data=jrc_ac_comb,aes(x=year,y=shr_gav_lu_areas,shape=gav_class),color="black",size=1.5)+
#   geom_point(data=hti_jrc_ac_comb,aes(x=year,y=gav_lu_areas,shape=gav_class),color="black",size=1.5)+
#   ylab("") +
#   xlab("") +
#   scale_fill_manual(values=c("#CC79A7", "#E69F00", "#F0E442","#999999","#009E73"),
#                    breaks = c("deforested land","degraded tmf","forest regrowth","other land cover","undisturbed tropical moist forest (tmf)"),
#                    labels = c("Deforested land","Degraded tropical moist forest","Forest regrowth","Other land cover","Undisturbed tropical moist forest"))+ 
#   scale_shape_manual(values=c(1),labels=c("Area cleared for pulpwood"),na.translate=FALSE)+ 
#   scale_color_manual(values = c("#000000","#0072B2")) +
#   #facet_wrap(~supplier_label,ncol=2,scales="free") +
#   guides(fill = guide_legend(nrow = 2),color = guide_legend(nrow=1),shape = guide_legend(nrow=2),keyheight = 10) +
#   theme_plot
# 
# hti_jrc_ac_plot

# Creating plots of individual concessions using a loop
# 
# class_descs <- unique(hti_jrc_ac$class_desc)
# years <- unique(hti_jrc_ac$year)
# supplier_ids <- unique(hti_jrc_ac$supplier_id)
# combinations <- expand.grid(year = years, class_desc = class_descs,supplier_id=supplier_ids) %>%
#   as_tibble() %>%
#   mutate(class_desc = as.character(class_desc),supplier_id = as.character(supplier_id))
# 
# hti_jrc_ac_complete <- full_join(hti_jrc_ac,combinations,c("year"="year","class_desc"="class_desc","supplier_id"="supplier_id")) %>%
#   drop_na(class_desc) %>%
#   complete(year,class_desc,
#   fill = list(shr_class = 0, shr_gav_lu_areas=0)) %>%
#   group_by(supplier_id) %>%
#   fill(supplier_label, island,app,april,marubeni,none,all,zdc_year,license_year,gav_class, .direction="downup")

# jrc_hti_ac_complete <- jrc_hti_ac %>%
#   ungroup() %>%
#   #filter(supplier_id == "H-0657" | supplier_id == "H-0318") %>%
#   complete(year,class_desc,
#            fill = list(shr_class = 0, shr_gav_lu_areas=0)) %>%
#   fill(supplier_id,supplier_label,island,license_year,.direction = "downup") %>%
#   mutate(zdc_year = ifelse(zdc_year == 0,NA,zdc_year)) %>%
#   group_by(supplier_id) %>%
#   mutate(zdc_year = min(zdc_year))
# 
# concessions <- hti_jrc_ac_complete %>%
#   #filter(app == 1 | april == 1 | marubeni == 1) %>%
#   filter(island == 4) %>%
#   distinct(supplier_label) %>%
#   pull(supplier_label) %>%
#   print()
# 
# hti_plots = list()
# 
# for(concession_ in concessions) {
#   hti_plots[[concession_]] = ggplot(hti_jrc_ac_complete %>% filter(supplier_label == concession_),aes(year,area_class)) +
#     geom_area(aes(fill= as.character(class_desc)), position = position_stack(reverse = T)) +
#     scale_x_continuous(expand=c(0,0),breaks=seq(1990,2022,by=1)) +
#     scale_y_continuous(labels = d3_format(".2~s",suffix = "ha"),expand = c(0,0)) +
#     geom_vline(aes(xintercept=as.numeric(license_year),color="License\nyear"),size=0.5)+
#     geom_vline(aes(xintercept=as.numeric(zdc_year),color="Earliest ZDC year\nof downstream mill"),size=0.5)+
#     geom_point(data=hti_jrc_ac %>% filter(supplier_label == concession_),aes(x=year,y=gav_lu_areas,shape=gav_class),color="black",size=1.5)+
#     ylab("") +
#     xlab("") +
#     ggtitle(paste0(concession_)) +
#     scale_fill_manual(values=c("#CC79A7", "#E69F00", "#F0E442","#999999","#009E73"),
#                       breaks = c("deforested land","degraded tmf","forest regrowth","other land cover","undisturbed tropical moist forest (tmf)"),
#                       labels = c("Deforested land","Degraded tropical moist forest","Forest regrowth","Other land cover","Undisturbed tropical moist forest"))+ 
#     scale_shape_manual(values=c(1),labels=c("Area cleared for pulpwood"),na.translate=FALSE)+ 
#     scale_color_manual(values = c("#000000","#0072B2")) +
#     guides(fill = guide_legend(nrow = 3),color = guide_legend(nrow=1),shape = guide_legend(nrow=2),keyheight = 10) +
#     theme_plot
#   
#   print(hti_plots[[concession_]])
#   ggsave(hti_plots[[concession_]], file=paste0("D:\\",gsub(" ","_",concession_),"_JRC_AnnualChanges.png"), dpi=400, w=10, h=6,device="png")
# }

# ## filter by supplier
# gav_ac_comb <- gaveau_annual_lc %>%
#   left_join(hti_concession_names,by="supplier_id") %>%
#   mutate(area_ha = ifelse(is.na(area_ha),0,area_ha)) %>%
#   filter(supplier_id == "H-0469")
# 
# gav_ac_comb$class_desc <- factor(gav_ac_comb$class_desc, levels = c("Forest", "Non-forest", "Cleared for pulp"))
# 
# gav_ac_plot <- gav_ac_comb %>%
#   ggplot(aes(year,area_ha)) +
#   geom_area(aes(fill= as.factor(class_desc))) +
#   scale_x_continuous(expand=c(0.0,0),breaks=seq(2001,2022,by=1),limits=c(2001,2022)) +
#   scale_y_continuous(labels = d3_format(".2~s",suffix = "ha"),expand = c(0,0)) +
#   geom_vline(aes(xintercept=as.numeric(license_year),color="License\nyear"),linewidth=0.5)+
#   geom_vline(aes(xintercept=as.numeric(zdc_year),color="Earliest ZDC year\nof downstream mill"),linewidth=0.5)+
#   ggtitle(unique(gav_ac_comb$supplier_label)) +
#   ylab("") +
#   xlab("") +
#   scale_fill_manual(values=c("#009E73","#F0E442","#CC79A7"),
#                     breaks = c("Forest","Non-forest","Cleared for pulp"),
#                     labels = c("Forest","Non-forest","Cleared for pulp"))+
#   scale_shape_manual(values=c(1),labels=c("Pulpwood area (TreeMap)"),na.translate=FALSE)+
#   scale_color_manual(values = c("#000000","#0072B2")) +
#   #facet_wrap(~supplier_label,ncol=2,scales="free") +
#   guides(fill = guide_legend(nrow = 1),color = guide_legend(nrow=1),shape = guide_legend(nrow=2),keyheight = 10) +
#   theme_plot 
# 
# gav_ac_plot
# 
# ggsave(gav_ac_plot,file="D:\\gav_plot_test.png",dpi=400, w=10, h=6)

# create plots by loop
hti_gav_annual_lc <- gaveau_annual_lc %>%
  left_join(hti_concession_names,by="supplier_id") 

concessions <- hti_gav_annual_lc %>%
  #filter(app == 0 & april == 0 & marubeni == 1) %>%
  filter(all == 1) %>%
  #filter(island == 4) %>%
  distinct(supplier_label) %>%
  pull(supplier_label) %>%
  print()

hti_gav_annual_lc$class_desc <- factor(hti_gav_annual_lc$class_desc, levels = c("Forest", "Non-forest", "Cleared for pulp"))

hti_plots = list()

for(concession_ in concessions) {
  hti_plots[[concession_]] = ggplot(hti_gav_annual_lc %>% filter(supplier_label == concession_),aes(year,area_ha)) +
    geom_area(aes(fill= as.factor(class_desc)), position = position_stack(reverse = F)) +
    scale_x_continuous(expand=c(0,0),breaks=seq(2001,2022,by=1),limits = c(2001,2022)) +
    scale_y_continuous(labels = d3_format(".2~s",suffix = "ha"),expand = c(0,0)) +
    geom_vline(aes(xintercept=as.numeric(license_year),color="License\nyear"),size=0.5)+
    geom_vline(aes(xintercept=as.numeric(zdc_year),color="Earliest ZDC year\nof downstream mill"),size=0.5)+
    #geom_point(data=hti_jrc_ac %>% filter(supplier_label == concession_),aes(x=year,y=gav_lu_areas,shape=gav_class),color="black",size=1.5)+
    ylab("") +
    xlab("") +
    ggtitle(paste0(concession_)) +
    scale_fill_manual(values=c("#009E73","#F0E442","#CC79A7"),
                      breaks = c("Forest","Non-forest","Cleared for pulp"),
                      labels = c("Forest","Non-forest","Cleared for pulp"))+
    #scale_shape_manual(values=c(1),labels=c("Pulpwood area (TreeMap)"),na.translate=FALSE)+
    scale_color_manual(values = c("#000000","#0072B2")) +
    guides(fill = guide_legend(nrow = 1),color = guide_legend(nrow=1),shape = guide_legend(nrow=2),keyheight = 10) +
    theme_plot
  
  print(hti_plots[[concession_]])
  ggsave(hti_plots[[concession_]], file=paste0("D:\\",gsub(" ","_",concession_),"_Gaveau_AnnualChanges.png"), dpi=400, w=10, h=6,device="png")
}
