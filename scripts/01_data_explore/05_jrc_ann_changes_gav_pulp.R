## ---------------------------------------------------------
## 
## Project: Indonesia pulp deforestation
##
## Purpose of script: Produce plots of JRC annual changes for each HTI concession
##
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Date Created: 2023-01-28
## 
## ---------------------------------------------------------
##
## Notes: Input datasets
##        1) HTI concessions (boundaries) and concession start year - from KLHK
##        2) Gaveau landuse change - commodity deforestation (2000 - 2020)
##        3) JRC deforestation (1990 - 2021)
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
#library(d3.format)
library(tidyfast)
library(patchwork)

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
lic_dates_hti <- readr::read_csv(paste0(wdir,"\\01_data\\01_in\\wwi\\HTI_LICENSE_DATES.csv"),
                                 col_types = cols(license_date = col_date("%m/%d/%Y")))

## supplier groups
groups <- read_csv(paste0(wdir,"\\01_data\\01_in\\wwi\\ALIGNED_NAMES_GROUP_HTI.csv"))

## HTI and sample IDs
samples_hti <- read_csv(paste0(wdir,"\\01_data\\02_out\\samples\\samples_hti_id.csv"))

# hti concessions
hti <- read_sf(paste0(wdir,"\\01_data\\01_in\\klhk\\IUPHHK_HT_proj.shp"))

# wood supply
ws <- read_delim(get_object(object="indonesia/wood_pulp/production/out/PULP_WOOD_SUPPLY_CLEAN_ALL_ALIGNED_2015_2019.csv", bucket), delim = ",")

# kabupaten
kab <- read_sf(paste0(wdir,"\\01_data\\01_in\\big\\idn_kabupaten_big.shp"))
prov_slim <- kab %>% select(prov,prov_code) %>% st_drop_geometry() %>% distinct() %>%
  mutate(prov_code = ifelse(prov == "PAPUA",92,prov_code))

## JRC
## Annual changes 
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\jrc\\annual_changes\\"),
                 pattern = "*2021.csv",
                 full.names= TRUE)

samples_jrc_tmf <- filenames %>%
  map_dfr(read_csv, .id = "jrc_tmf_ac") %>%
  janitor::clean_names() %>%
  select(-system_index,-geo)

## Deforestation year
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\jrc\\deforestation_year\\"),
                 pattern = "*2021.csv",
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

## first year gaveau assigns as pulp
gaveau_pulp_styr <- samples_gaveau_landuse %>%
  lazy_dt() %>%
  left_join(samples_hti,by="sid") %>%
  select(supplier_id=ID,gaveau,starts_with("id_")) %>%
  as.data.table() %>%
  dt_pivot_longer(cols = c(-supplier_id,-gaveau),
                  names_to = 'year',
                  values_to = 'class') %>%
  as_tibble() %>%
  filter(class == 4) %>%
  mutate(year = str_replace(year,"id_", ""),year = as.double(year)) %>% 
  group_by(supplier_id) %>% 
  slice(which.min(year)) 

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

## create island mapping
island_tab <- tibble("island_code" = c(1, 2, 3, 4, 5, 6), "island" = c("balinusa", "kalimantan", "maluku", "papua", "sulawesi", "sumatera"))

## identify pixels that were cleared for pulp at some point in the time series
# gaveau
gaveau_pulp <- samples_gaveau_landuse %>%
  lazy_dt() %>%
  left_join(samples_hti,by="sid") %>%
  as_tibble()

# TRUE/FALSE if sample is ever on pulp clearing 
# gaveau
gaveau_pulp$ever_pulp <- (rowSums(gaveau_pulp[,startsWith(names(gaveau_pulp),"id_")]==4) >= 1) # Gaveau class 4 is industrial pulp clearing
# select columns
# gaveau
gaveau_pulp <- gaveau_pulp %>% 
  select(sid,ever_pulp)

## identify pixels that started as forest in 1990
## NOTE: dataset currently doesn't include any pixels that are not forested at t0
forest_sids <- samples_jrc_tmf %>% 
  filter(dec1990 == 1) %>% # 1 = undisturbed tropical moist forest; 2 = degraded tmf
  pull(sid)

## create pixel-level dataset starting with JRC deforestation year
## NOTE: 0 means no deforestation observed in 1990-2019
samples_df <- samples_jrc_defyr %>% 
  lazy_dt() %>%
  select(sid, island_code = jrc_def_yr, def_year = deforestation_year) %>%
  mutate(island_code = as.integer(island_code)) %>% 
  left_join(island_tab, by = "island_code")

### Join to gaveau, concession, island data, mill supplied, first year pulp
samples_df <- samples_df %>% 
  as_tibble()%>%
  add_column(rand = runif(nrow(.))) %>%
  lazy_dt() %>%
  left_join(samples_hti,by="sid") %>%
  rename(supplier_id = ID) %>% 
  left_join(hti_dates_clean,by="supplier_id") %>%
  left_join(hti_concession_names,by="supplier_id") %>% 
  left_join(gaveau_pulp, by = "sid") %>% 
  left_join(gaveau_pulp_styr) %>%
  mutate(def_year = ifelse(def_year == 0 & ever_pulp == 1,year,def_year), # reclassifying def_year to year pulp assigned by Gaveau/MapBiomas if def_year = 0
         year = ifelse(is.na(year),2999,year)) %>%
  select(sid, island, supplier_id, def_year, ever_pulp, license_year, start_pulp=year,supplier_label,rand) %>% 
  mutate(start_for = sid %in% forest_sids)

###########################################################################
# Analyze annual changes --------------------------------------------------
###########################################################################

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
  mutate(shr_gav_lu_areas = prop.table(n)*100) %>%
  filter(gav_class != "Others")


## melt annual changes dataset into long dataset
jrc_ac_long <- samples_jrc_tmf %>%
  as.data.table() %>%
  #melt(measure = patterns("^dec"), value.name = "class",variable.name = "year")
  dt_pivot_longer(cols = c(-jrc_tmf_ac,-sid),
                  names_to = 'year',
                  values_to = 'class')

## join with HTI's and get areas by class
jrc_ac <- jrc_ac_long %>% 
  lazy_dt() %>%
  left_join(samples_hti,by="sid") %>%
  group_by(ID,year,jrc_tmf_ac,class) %>%
  summarize(n = n()) %>%
  mutate(shr_class = prop.table(n)*100) 

## create table of annual pulp for gaveau
supplier_year_tbl <- hti_concession_names %>%
  select(supplier_id) %>%
  group_by(supplier_id) %>%
  summarise(start = min(1990),
            end = max(2020)) %>%
  mutate(year = Map(seq, start, end)) %>%
  unnest(cols =year) %>%
  mutate(unique=1) %>%
  select(supplier_id,year)

gav_tbl <- gaveau_annual_pulp %>%
  mutate(dataset = "gaveau",sh_area = shr_gav_lu_areas) %>%
  select(supplier_id,year,sh_area,dataset) 

## join with downstream mill supplier, license year,concession names and annual change class names
jrc_hti_ac <- jrc_ac %>%
  rename(supplier_id=ID,island_code=jrc_tmf_ac) %>%
  left_join(subset(lu_table,dataset =="jrc tmf annual changes"),by="class") %>%
  mutate(class_desc = ifelse(class_desc == "permanent or seasonal water" | class_desc == "other land cover","other land cover",class_desc )) %>%
  group_by(supplier_id,class_desc,year,island_code) %>%
  summarize(n = sum(n),shr_class=sum(shr_class)) %>%
  left_join(hti_dates_clean,by="supplier_id") %>%
  left_join(mill_supplier,by="supplier_id") %>%
  left_join(hti_concession_names,by="supplier_id") %>%
  mutate(year = str_replace(year,"dec", "")) %>%
  mutate(year = as.double(year)) %>%
  select(-supplier) %>%
  #left_join(supplier_year_tbl,by=c("year","supplier_id")) %>%
  #left_join(select(pulp_area_tbl,supplier_id,year,sh_area,dataset),by=c("year","supplier_id")) %>%
  left_join(select(gaveau_annual_pulp,supplier_id,year,shr_gav_lu_areas,gav_class),by=c("year","supplier_id")) %>%
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


## garbage collection (clearing memory)
gc()
rm(samples_hti)
rm(samples_jrc_defyr)
rm(samples_jrc_tmf)

## filter or aggregate for plot (by island, downstream mill, etc)
jrc_ac_comb <- jrc_hti_ac %>%
  as_tibble() %>%
  filter(island_code == 4) %>%
  filter(april == 1 | app == 1 | marubeni == 1)

## QA checks

## Check 1 - check if area before and after area aggregation is equal

# get supplier ID
area_test_supp_id <- jrc_ac_comb %>%
  slice(1) %>%
  select(supplier_id) %>%
  pull

# area pre area calculation
ac_supp_pre_area <- jrc_ac %>%
  as_tibble() %>%
  filter(ID == area_test_supp_id) %>%
  group_by(ID) %>%
  summarize(area_ha = sum(n))

# area post area calculation
ac_supp_post_area <- jrc_ac_comb %>%
  as_tibble() %>%
  filter(supplier_id == area_test_supp_id) %>%
  group_by(supplier_id) %>%
  summarize(area_ha = sum(n))

expect_true(all.equal(ac_supp_pre_area[1,2],ac_supp_post_area[1,2]))

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

## plotting
ac_plot <- ggplot(jrc_ac_comb,aes(year,shr_class)) +
  geom_area(aes(fill= as.character(class_desc)), position = position_stack(reverse = T)) +
  scale_x_continuous(expand=c(0,0),breaks=seq(1990,2021,by=1)) +
  #scale_y_continuous(labels = d3_format(".2~s",suffix = "%"),expand = c(0,0)) +
  geom_vline(aes(xintercept=as.numeric(license_year),color="License\nyear"),size=0.5)+
  geom_vline(aes(xintercept=as.numeric(zdc_year),color="Earliest ZDC year\nof downstream mill"),size=0.5)+
  #geom_line(data=jrc_ac_comb,aes(x=year,y=sh_area,shape=dataset),fill="white",color="grey20",size=0.2)+
  #geom_point(data=jrc_ac_comb,aes(x=year,y=sh_area,shape=1),fill="white",color="black",size=1)+
  geom_point(data=jrc_ac_comb,aes(x=year,y=shr_gav_lu_areas,shape=gav_class),color="black",size=1.5)+
  #geom_point(data=jrc_ac_comb,aes(x=year,y=shr_mb_lu_areas,shape=mb_class),color="blue",size=1.5)+
  ylab("") +
  xlab("") +
  #scale_fill_muted() +
  scale_fill_manual(values=c("lightpink", "orange3", "yellowgreen","#F8F899","seagreen4"))+ 
  scale_shape_manual(values=c(1,2),labels=c("Area cleared for pulp - Gaveau","Wood pulp planted area - MapBiomas"),na.translate=FALSE)+ 
  scale_color_manual(values = c("palevioletred4","#064383")) +
  facet_wrap(~supplier_label,ncol=2,scales="free") +
  guides(fill = guide_legend(nrow = 2),color = guide_legend(nrow=1),shape = guide_legend(nrow=2),keyheight = 10) +
  theme_plot

ac_plot

## export to png
ggsave(ac_plot,file=paste0(wdir,"\\01_data\\02_out\\plots\\APRIL\\suma_april_suppliers_jrc_ac_pulp_gav_mb.png"), dpi=400, w=12, h=42,type="cairo-png",limitsize = FALSE)
ggsave(ac_plot,file=paste0(wdir,"\\01_data\\02_out\\plots\\APRIL\\kali_april_suppliers_jrc_ac_pulp_gav_mb.png"), dpi=400, w=12, h=12,type="cairo-png",limitsize = FALSE)


# Creating plots of individual concessions using a loop

concessions = unique(jrc_ac_comb$supplier_label)
hti_plots = list()

for(concession_ in concessions) {
  hti_plots[[concession_]] = ggplot(jrc_ac_comb %>% filter(supplier_label == concession_),aes(year,shr_class)) +
    geom_area(aes(fill= as.character(class_desc)), position = position_stack(reverse = T)) +
    scale_x_continuous(expand=c(0,0),breaks=seq(1990,2021,by=1)) +
    scale_y_continuous(labels = d3_format(".2~s",suffix = "%"),expand = c(0,0)) +
    geom_vline(aes(xintercept=as.numeric(license_year),color="License\nyear"),size=0.5)+
    geom_vline(aes(xintercept=as.numeric(zdc_year),color="Earliest ZDC year\nof downstream mill"),size=0.5)+
    geom_point(data=jrc_ac_comb %>% filter(supplier_label == concession_),aes(x=year,y=shr_gav_lu_areas,shape=gav_class),color="black",size=1.5)+
    ylab("") +
    xlab("") +
    #scale_fill_muted() +
    scale_fill_manual(values=c("lightpink", "orange3", "yellowgreen","#F8F899","seagreen4"))+ 
    scale_shape_manual(values=1,labels=c("Area cleared\nfor pulp (TreeMap"),na.translate=FALSE)+ 
    scale_color_manual(values = c("palevioletred4","#064383")) +
    guides(fill = guide_legend(nrow = 3),color = guide_legend(nrow=1),shape = guide_legend(nrow=2),keyheight = 10) +
    theme_plot
    
  print(hti_plots[[concession_]])
  ggsave(hti_plots[[concession_]], file=paste0("D:\\",concession_,"_JRC_AnnualChanges.png"), width = 20, height = 12, units = "cm", dpi=400)
}
