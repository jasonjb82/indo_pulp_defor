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
##        2) Gaveau landuse change - commodity deforestation (2000 - 2020) (IOPP,ITP and smallholders)
##        3) JRC deforestation (1990 - 2020)
##        4) MapBiomas land use classification (2000 - 2019)
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
                 pattern = "*.csv",
                 full.names= TRUE)

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
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\gaveau\\"),
                 pattern = "*.csv",
                 full.names= TRUE)

samples_gaveau_landuse <- filenames %>%
  map_dfr(read_csv, .id = "gaveau") %>%
  janitor::clean_names() %>%
  select(-system_index,-geo)

## Mapbiomas data
# filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\mapbiomas\\"),
#                  pattern = "*.csv",
#                  full.names= TRUE)
# 
# samples_mapbiomas_landuse <- filenames %>%
#   map_dfr(read_csv, .id = "mapbiomas") %>%
#   janitor::clean_names() %>%
#   select(-system_index,-geo)

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

## first year mapbiomas assigns as pulp
# mapbiomas_pulp_styr <- samples_mapbiomas_landuse %>%
#   lazy_dt() %>%
#   left_join(samples_hti,by="sid") %>%
#   select(supplier_id=ID,mapbiomas,starts_with("classification_")) %>%
#   as.data.table() %>%
#   dt_pivot_longer(cols = c(-supplier_id,-mapbiomas),
#                   names_to = 'year',
#                   values_to = 'class') %>%
#   as_tibble() %>%
#   filter(class == 9) %>%
#   mutate(year = str_replace(year,"classification_", ""),year = as.double(year)) %>% 
#   group_by(supplier_id) %>% 
#   slice(which.min(year)) 

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

# mapbiomas
# mapbiomas_pulp <- samples_mapbiomas_landuse %>%
#   lazy_dt() %>%
#   left_join(samples_hti,by="sid") %>%
#   as_tibble()

# TRUE/FALSE if sample is ever on pulp clearing 
# gaveau
gaveau_pulp$ever_pulp <- (rowSums(gaveau_pulp[,startsWith(names(gaveau_pulp),"id_")]==4) >= 1) # Gaveau class 4 is industrial pulp clearing
# mapbiomas
# mapbiomas_pulp$ever_pulp <- (rowSums(mapbiomas_pulp[,startsWith(names(mapbiomas_pulp),"classification_")]==9) >= 1) # MapBiomas class 9 is plantations

# select columns
# gaveau
gaveau_pulp <- gaveau_pulp %>% 
  select(sid,ever_pulp)

# mapbiomas
#mapbiomas_pulp <- mapbiomas_pulp %>% 
#  select(sid,ever_pulp)

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
  #left_join(mapbiomas_pulp, by = "sid") %>% 
  #left_join(mapbiomas_pulp_styr) %>%
  mutate(def_year = ifelse(def_year == 0 & ever_pulp == 1,year,def_year), # reclassifying def_year to year pulp assigned by Gaveau/MapBiomas if def_year = 0
         year = ifelse(is.na(year),2999,year)) %>%
  select(sid, island, supplier_id, def_year, ever_pulp, license_year, start_pulp=year,supplier_label,rand) %>% 
  mutate(start_for = sid %in% forest_sids)

###########################################################################
# Analyze deforestation timing --------------------------------------------
###########################################################################

### Identify different deforestation timings
lag <- 5
defort_df <- samples_df %>% 
  left_join(mill_supplier,by="supplier_id") %>%
  left_join(supplier_groups,by="supplier_id") %>%
  mutate(supplier_group = ifelse(supplier_group == "OTHER" & ever_pulp == "FALSE","OTHER - NO PLANTED PULP",
         ifelse(supplier_group == "OTHER" & ever_pulp == "TRUE","OTHER - WITH PLANTED PULP",supplier_group))) %>%
  group_by(supplier_id) %>%
  mutate(mgmt_year = ifelse(start_pulp < license_year ,max(license_year - lag),
                            ifelse(start_pulp > license_year,license_year,start_pulp))) %>%
  mutate(def_year = ifelse(def_year== 0, ifelse(start_for == 1, 2999, 0), def_year)) %>%  
  mutate(defor_time = case_when(def_year >= 2013 & def_year >= mgmt_year & app == 1 & def_year != 2999 ~ "Deforestation post-permit / post-first planting and after first ZDC of downstream mill",
         def_year >= 2015 & def_year >= mgmt_year & app == 0 & april == 1 & def_year != 2999 ~ "Deforestation post-permit / post-first planting and after first ZDC of downstream mill",
         def_year >= 2019 & def_year >= mgmt_year & app == 0 & april == 0 & marubeni == 1 & def_year != 2999 ~ "Deforestation post-permit / post-first planting and after first ZDC of downstream mill", # still needs confirmation
         def_year >= mgmt_year & def_year != 2999 ~ "Deforestation post-permit / post-first planting",
         def_year < mgmt_year & def_year != 2999 ~ "Deforestation pre-permit and first planting",
         def_year == 2999  ~ "Never deforested",TRUE ~ 0)) %>%
  mutate(defor_pulp = ifelse(ever_pulp==1, paste0(defor_time, ", converted to pulp plantation"), paste0(defor_time, ", not converted to pulp plantation")))

## QA checks

## Check 1 - check if no of samples are same before deforestation timing reclass
test_supp_id = "H-0262"
expect_true(all.equal(dim(subset(as.data.frame(samples_df),supplier_id==test_supp_id))[1],dim(subset(as.data.frame(defort_df),supplier_id==test_supp_id))[1]))

## Generate frequency table by group
group_var <- "supplier_group" # Generally either island, supplier_group or supplier_label
class_var <- "defor_pulp" # Generally either defor_time or defor_pulp
mill_var <- "all" # Generally either april,app,marubeni or all (all concessions)
freq_tab <- defort_df %>%
  filter(!!sym(mill_var) == 1) %>%
  #filter(island == "kalimantan") %>% # filter to island if required
  group_by(.data[[group_var]], .data[[class_var]]) %>% 
  summarize(area_ha = n()) %>% 
  mutate(freq = area_ha / sum(area_ha)) %>%
  ungroup()

freq_tab

# Check 2 - check if total area of deforestation timings and frequency table are equal (for supplier group)
condition = grepl("supplier_group", names(as_tibble(freq_tab)))

select.vars <- function (df, cond=condition[1]){
  if(cond){
    df %>%
      select(supplier_group,area_ha)
  } else {
    df %>%
      select(island,area_ha)
  }
}

# area_test_supp_id <- select.vars(freq_tab) %>%
#   slice(1) %>%
#   select(!!sym(group_var)) %>%
#   mutate(id = str_extract_all(supplier_group,"(?<=\\().+?(?=\\))")[[1]]) %>%
#   pull
supplier_grp = "SINAR MAS"

test_defort_area = defort_df %>%
  filter(supplier_group == supplier_grp) %>%
  as_tibble() %>%
  group_by(supplier_group) %>%
  tally(sort=TRUE) %>%
  mutate(area_ha = n) %>%
  summarize(area_ha = sum(area_ha))

test_freq_tab_area <- freq_tab %>%
  as_tibble() %>%
  #mutate(supplier_id = stringr::str_extract(string = supplier_label,pattern = "(?<=\\().*(?=\\))")) %>%
  filter(supplier_group == supplier_grp) %>%
  group_by(supplier_group) %>%
  summarize(area_ha = sum(area_ha))

expect_true(all.equal(test_defort_area[1,1],test_freq_tab_area[1,2]))
  
#########################################################################
# Plotting --------------------------------------------------------------
#########################################################################

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

# ## ordering by never deforested areas
# order <- freq_tab %>% 
#   as_tibble() %>%
#   filter(!!sym(class_var) == "Never deforested, not converted to pulp plantation" | !!sym(class_var) == "Never deforested") %>% 
#   arrange(-area_ha) %>%
#   pull(!!sym(group_var))

# manual reordering
order <- c("SINAR MAS","ROYAL GOLDEN EAGLE / TANOTO","GOVERNMENT","MARUBENI","GOVERNEMENT","SUMITOMO",
           "KORINDO","DJARUM","OTHER - WITH PLANTED PULP", "OTHER - NO PLANTED PULP")

## set plot order
plot_order_deft_pulp <- c(
  "Never deforested",
  "Never deforested, not converted to pulp plantation",
  "Deforestation pre-permit and first planting",
  "Deforestation pre-permit and first planting, converted to pulp plantation",
  "Deforestation pre-permit and first planting, not converted to pulp plantation",
  "Deforestation post-permit / post-first planting",
  "Deforestation post-permit / post-first planting, converted to pulp plantation",
  "Deforestation post-permit / post-first planting, not converted to pulp plantation",
  "Deforestation post-permit / post-first planting and after first ZDC of downstream mill",
  "Deforestation post-permit / post-first planting and after first ZDC of downstream mill, converted to pulp plantation",
  "Deforestation post-permit / post-first planting and after first ZDC of downstream mill, not converted to pulp plantation"
)

## plot frequencies
freq_plot <- freq_tab %>% 
  as_tibble() %>%
  mutate(label_order = factor(!!sym(group_var),rev(order))) %>%
  ggplot() +
  aes(y = label_order, x = area_ha, fill = factor(!!sym(class_var),levels=plot_order_deft_pulp)) +
  geom_bar(stat = "identity",position = position_stack(reverse = TRUE)) +
  theme_plot2 +
  xlab("") + ylab("")+
  scale_x_continuous(labels = d3_format(".2~s",suffix = " ha"),expand = c(0,0)) +
  guides(fill = guide_legend(nrow = 4)) +
  scale_fill_manual(values = cols_alpha,name ="Group",
  breaks=plot_order_deft_pulp,labels=plot_order_deft_pulp)

freq_plot

## save plot to png
#ggsave(freq_plot,file=paste0(wdir,"\\01_data\\02_out\\plots\\jrc_deforestation\\supplier_groups_defort_pulp.png"), dpi=400, w=15, h=10,type="cairo-png",limitsize = FALSE)

## plot frequencies (for pulp converted areas only)

freq_conv_plot <- freq_tab %>% 
  as_tibble() %>%
  filter(stringr::str_detect(defor_pulp, ', converted to pulp plantation')) %>%
  mutate(label_order = factor(!!sym(group_var),rev(order))) %>%
  ggplot() +
  aes(y = label_order, x = area_ha, fill = factor(!!sym(class_var),levels=plot_order_deft_pulp)) +
  geom_bar(stat = "identity",position = fill(reverse = TRUE)) +
  theme_plot2 +
  xlab("") + ylab("")+
  scale_x_continuous(labels = d3_format(".2~s",suffix = " ha"),expand = c(0,0)) +
  guides(fill = guide_legend(nrow = 4)) +
  scale_fill_manual(values = cols_alpha,name ="Group",
                    breaks=plot_order_deft_pulp,labels=plot_order_deft_pulp) +
  theme(legend.position = "none")

freq_conv_plot

## plot frequencies (for pulp converted areas only) - percent

freq_conv_perc_plot <- freq_tab %>% 
  as_tibble() %>%
  filter(stringr::str_detect(defor_pulp, ', converted to pulp plantation')) %>%
  mutate(label_order = factor(!!sym(group_var),rev(order))) %>%
  ggplot() +
  aes(y = label_order, x = area_ha, fill = factor(!!sym(class_var),levels=plot_order_deft_pulp)) +
  geom_bar(stat = "identity",position = "fill") +
  theme_plot2 +
  xlab("") + ylab("")+
  scale_x_continuous(labels = percent,expand = c(0,0)) +
  guides(fill = guide_legend(nrow = 4)) +
  scale_fill_manual(values = cols_alpha,name ="Group",
                    breaks=plot_order_deft_pulp,labels=plot_order_deft_pulp) +
  theme(legend.position = "none")

freq_conv_perc_plot

## save plot to png
#ggsave(freq_conv_plot,file=paste0(wdir,"\\01_data\\02_out\\plots\\jrc_deforestation\\supplier_groups_defort_pulp_conv_only.png"), dpi=400, w=15, h=10,type="cairo-png",limitsize = FALSE)

# merging plots
freq_comb <- (freq_plot + plot_layout(guides = "collect") & theme(legend.position = "bottom")) + (freq_conv_perc_plot + theme(legend.position = "none")) 
freq_comb

## save plot to png
ggsave(freq_comb,file=paste0(wdir,"\\01_data\\02_out\\plots\\jrc_deforestation\\supplier_groups_defort_pulp.png"), dpi=400, w=15, h=7,type="cairo-png",limitsize = FALSE)

###########################################################################
# Analyze annual changes --------------------------------------------------
###########################################################################

## TODO: add earliest year of downstream mill ZDC, add more QA tests

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
  mutate(shr_mb_lu_areas = prop.table(n)*100) %>%
  filter(mb_class != "Others")

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

## create table of annual pulp for gaveau and mapbiomas dataset
supplier_year_tbl <- hti_concession_names %>%
  select(supplier_id) %>%
  group_by(supplier_id) %>%
  summarise(start = min(1990),
            end = max(2019)) %>%
  mutate(year = Map(seq, start, end)) %>%
  unnest(cols =year) %>%
  mutate(unique=1) %>%
  select(supplier_id,year)

gav_tbl <- gaveau_annual_pulp %>%
  mutate(dataset = "gaveau",sh_area = shr_gav_lu_areas) %>%
  select(supplier_id,year,sh_area,dataset) 

# mb_tbl <- mapbiomas_annual_pulp %>%
#   mutate(dataset = "mapbiomas",sh_area = shr_mb_lu_areas) %>%
#   select(supplier_id,year,sh_area,dataset)   
# 
# merged_pulp_areas <- gav_tbl %>%
#   bind_rows(mb_tbl)
# 
# pulp_area_tbl <- supplier_year_tbl %>%
#   left_join(merged_pulp_areas,by =c("supplier_id","year"))


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
  #left_join(select(mapbiomas_annual_pulp,supplier_id,year,shr_mb_lu_areas,mb_class),by=c("year","supplier_id")) %>%
  mutate(
    zdc_year = case_when(
      app == 1 ~ 2013,
      app == 0 & april == 1 ~ 2015,
      april == 0 & app == 0 & marubeni == 1 ~ 2019,
      TRUE ~ 0
    )
  ) %>%
  mutate(zdc_year = ifelse(zdc_year ==0,NA,zdc_year))


## garbage collection (clearing memory)
gc()
rm(samples_hti)
rm(samples_jrc_defyr)
rm(samples_jrc_tmf)

## filter or aggregate for plot (by island, downstream mill, etc)
jrc_ac_comb <- jrc_hti_ac %>%
  as_tibble() %>%
  filter(island_code == 2) %>%
  filter(april == 1)

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
                    legend.key.height = unit(12, "pt"),
                    legend.key.width = unit(12, "pt"),
                    legend.text = element_text(size = 9,colour="grey30"),
                    legend.title = element_blank(),
                    legend.position="bottom",
                    legend.box="horizontal",
                    plot.margin=unit(c(0.5,1.5,0.5,0.5),"cm"))

options(crayon.enabled = FALSE)

## plotting
ac_plot <- ggplot(subset(jrc_ac_comb,is.na(dataset)| dataset == "gaveau"),aes(year,shr_class)) +
  geom_area(aes(fill= as.character(class_desc)), position = position_stack(reverse = T)) +
  scale_x_continuous(expand=c(0,0),breaks=seq(1990,2019,by=1)) +
  scale_y_continuous(labels = d3_format(".2~s",suffix = "%"),expand = c(0,0)) +
  geom_vline(aes(xintercept=as.numeric(license_year),color="License\nyear"),size=0.5)+
  geom_vline(aes(xintercept=as.numeric(zdc_year),color="Earliest ZDC year\nof downstream mill"),size=0.5)+
  #geom_line(data=jrc_ac_comb,aes(x=year,y=sh_area,shape=dataset),fill="white",color="grey20",size=0.2)+
  geom_point(data=jrc_ac_comb,aes(x=year,y=sh_area,shape=dataset),fill="white",color="black",size=1)+
  #geom_point(data=jrc_ac_comb,aes(x=year,y=shr_gav_lu_areas,shape=gav_class),color="black",size=1.5)+
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


## Figure X (deforestation for plantation establishment)

lag <- 2
defort_df <- samples_df %>% 
  left_join(mill_supplier,by="supplier_id") %>%
  left_join(supplier_groups,by="supplier_id") %>%
  mutate(supplier_group = ifelse(supplier_group == "OTHER" & ever_pulp == "FALSE","OTHER - NO PLANTED PULP",
                                 ifelse(supplier_group == "OTHER" & ever_pulp == "TRUE","OTHER - WITH PLANTED PULP",supplier_group))) %>%
  group_by(supplier_id) %>%
  mutate(def_year = ifelse(def_year== 0, ifelse(start_for == 1, 2999, 0), def_year)) %>%  
  mutate(defor_time = case_when(def_year >= license_year & def_year != 2999 ~ "Deforestation post-permit",
                                def_year < license_year - lag & def_year != 2999 ~ "Deforestation pre-permit (< 2 years)",
                                def_year < license_year & def_year != 2999 ~ "Deforestation pre-permit (> 2 years)",
                                def_year == 2999  ~ "Never deforested",TRUE ~ 0)) %>%
  mutate(defor_pulp = ifelse(ever_pulp==1, paste0(defor_time, ", converted to pulp plantation"), paste0(defor_time, ", not converted to pulp plantation")))

## Generate frequency table by group
class_var <- "defor_pulp" # Generally either defor_time or defor_pulp
mill_var <- "all" # Generally either april,app,marubeni or all (all concessions)
freq_tab <- defort_df %>%
  filter(!!sym(mill_var) == 1) %>%
  group_by(.data[[class_var]]) %>% 
  summarize(area_ha = n()) %>% 
  mutate(freq = area_ha / sum(area_ha)) %>%
  ungroup() %>%
  as_tibble()

# Total area cleared for establishment of plantation
freq_tab %>%
  filter(stringr::str_detect(defor_pulp, ', converted to pulp plantation')) %>%
  mutate(pc_shr = prop.table(area_ha)*100) %>%
  select(-freq) %>% write.csv(.,file = paste0(wdir,"\\01_data\\02_out\\tables\\defor_plantation_est.csv"))

freq_tab %>% filter(stringr::str_detect(defor_pulp, ', converted to pulp plantation')) %>% group_by() %>%
  summarize(area_ha = sum(area_ha)) %>% print()

## set plot order
plot_order <- c(
  "Deforestation pre-permit (> 2 years), converted to pulp plantation",
  "Deforestation pre-permit (< 2 years), converted to pulp plantation",
  "Deforestation post-permit, converted to pulp plantation")

labels <- c("Deforestation pre-permit (> 2 years),\nconverted to pulp plantation",
          "Deforestation pre-permit (< 2 years),\nconverted to pulp plantation", 
           "Deforestation post-permit,\nconverted to pulp plantation") 

dt_plot <- freq_tab %>% 
  filter(stringr::str_detect(defor_pulp, ', converted to pulp plantation')) %>%
  ggplot() +
  aes(y = factor(defor_pulp,levels=rev(plot_order)), x = area_ha, fill = factor(defor_pulp,levels=plot_order)) +
  geom_bar(position="stack", stat="identity",  width=0.75) +
  theme_plot2 +
  xlab("") + ylab("")+
  scale_x_continuous(labels = d3_format(".2~s",suffix = " ha"),expand = c(0,0)) +
  scale_fill_manual(values=c("#EB4A40", "#94764d","#7827c2"))+ 
  guides(fill = guide_legend(nrow = 4)) +
  theme(legend.position = "none") +
  scale_y_discrete(labels = rev(labels))

dt_plot

ggsave(dt_plot,file=paste0(wdir,"\\01_data\\02_out\\plots\\001_figures\\fig_0X_defor_plantation_est.png"), 
       dpi=400, w=8, h=4,type="cairo-png",limitsize = FALSE)


## Fig X - Cumulative plantation development and deforestation (by province)

# cumulative plantation development by province
gaveau_annual_pulp_supplier <- samples_gaveau_landuse %>%
  left_join(samples_hti,by="sid") %>%
  select(supplier_id=ID,gaveau,starts_with("id_")) %>%
  pivot_longer(cols = starts_with("id_"),
               names_to = 'year',
               values_to = 'class') %>%
  mutate(year = str_replace(year,"id_", ""),year = as.double(year)) %>%
  mutate(gav_class = ifelse(class == 4,"Pulp","Others")) %>%
  group_by(supplier_id,year,gav_class) %>%
  summarize(n = n()) %>%
  filter(gav_class != "Others")

cum_pulp_prov <- gaveau_annual_pulp_supplier %>%
  left_join(select(hti,ID,Kode_Prov),by=c("supplier_id"="ID")) %>%
  select(-geometry,-supplier_id) %>%
  group_by(year,prov_code=Kode_Prov) %>%
  summarize(area_ha = sum(n)) %>%
  left_join(prov_slim,by="prov_code") %>%
  distinct()

plot_order <- c(
  "ACEH","SUMATERA UTARA","RIAU","JAMBI","SUMATERA SELATAN","SUMATERA BARAT","LAMPUNG",
  "KALIMANTAN BARAT","KALIMANTAN TENGAH","KALIMANTAN SELATAN","KALIMANTAN TIMUR","KALIMANTAN UTARA","PAPUA")

cpd_prov_plot <- ggplot(data=cum_pulp_prov,aes(year,area_ha)) +
  geom_bar(stat="identity",aes(fill = factor(prov,levels=plot_order))) +
  scale_x_continuous(expand=c(0,0),breaks=seq(2000,2020,by=1)) +
  scale_y_continuous(labels = d3_format(".2~s",suffix = " ha"),expand = c(0,0),breaks = seq(0,4000000,by=500000),limits=c(0,3000000)) +
  scale_fill_manual(values=c("#666666", "#ed8f8a","#94764d","#dfc398","#e7298a", "#ff7f00",
                             "#ffed6f","#7827c2","#80b1d3","#1f78b4","#bc80bd","#fccde5","#66c2a4"))+ 
  ylab("") +
  xlab("") +
  theme_plot

cpd_prov_plot

ggsave(cpd_prov_plot,file=paste0(wdir,"\\01_data\\02_out\\plots\\001_figures\\fig_0X_pulp_areas_province.png"), 
       dpi=400, w=10, h=6,type="cairo-png",limitsize = FALSE)

# annual plantation expansion by province
pulp_area_peryr <-  samples_gaveau_landuse %>%
  left_join(samples_hti,by="sid") %>%
  select(supplier_id=ID,sid,gaveau,starts_with("id_")) %>%
  pivot_longer(cols = starts_with("id_"),
               names_to = 'year',
               values_to = 'class') %>%
  mutate(year = str_replace(year,"id_", ""),year = as.double(year)) %>%
  filter(class == 4) %>%
  group_by(gaveau,sid,supplier_id) %>% 
  filter(year == min(year) & year > 2000) %>%
  group_by(year,supplier_id) %>%
  summarize(n = n()) %>%
  print()

pulp_area_peryr_prov <- pulp_area_peryr %>%
  left_join(select(hti,ID,Kode_Prov),by=c("supplier_id"="ID")) %>%
  select(-geometry,-supplier_id) %>%
  group_by(year,prov_code=Kode_Prov) %>%
  summarize(area_ha = sum(n)) %>%
  left_join(prov_slim,by="prov_code") %>%
  distinct() %>%
  print()

pap_prov_plot <- ggplot(data=pulp_area_peryr_prov,aes(year,area_ha)) +
  geom_bar(stat="identity",aes(fill = factor(prov,levels=plot_order))) +
  scale_x_continuous(expand=c(0,0),breaks=seq(2001,2020,by=1)) +
  scale_y_continuous(labels = d3_format(".2~s",suffix = " ha"),expand = c(0,0),breaks = seq(0,250000,by=50000),limits=c(0,240000)) +
  #scale_fill_manual(values=c("#666666", "#ed8f8a","#94764d","#dfc398","#e7298a", "#ff7f00",
  #                           "#ffed6f","#7827c2","#80b1d3","#1f78b4","#bc80bd","#fccde5","#66c2a4"))+ 
  scale_fill_manual(values=c("#120112", "#5d0e5e","#9b189e","#df65b0","#c994c7", "#d4b9da","#e7e1ef",
  "#082882","#3b48b3","#1d91c0","#22b3ab","#ace6ce",
  "#ffff33"))+ 
  ylab("") +
  xlab("") +
  theme_plot

pap_prov_plot

ggsave(pap_prov_plot,file=paste0(wdir,"\\01_data\\02_out\\plots\\001_figures\\fig_0X_pulp_areas_yr_province.png"), 
       dpi=400, w=10, h=6,type="cairo-png",limitsize = FALSE)

# pulp deforestation by province

def_pulp_year <- samples_df %>%
  filter(def_year >= 2001 & def_year < 2999 & ever_pulp == TRUE) %>%
  as_tibble() %>%
  #group_by(def_year,island) %>%
  left_join(select(hti,ID,Kode_Prov),by=c("supplier_id"="ID")) %>%
  select(-geometry,-supplier_id) %>%
  group_by(def_year,prov_code=Kode_Prov) %>%
  summarize(area_ha = n()) %>%
  left_join(prov_slim,by="prov_code") %>%
  distinct() %>%
  print()


dp_plot <- ggplot(data=def_pulp_year,aes(def_year,area_ha)) +
  geom_bar(stat="identity",aes(fill = factor(prov,levels=plot_order))) +
  scale_x_continuous(expand=c(0,0),breaks=seq(2001,2020,by=1)) +
  scale_y_continuous(labels = d3_format(".2~s",suffix = " ha"),expand = c(0,0),breaks = seq(0,250000,by=50000),limits=c(0,200000)) +
  scale_fill_manual(values=c("#120112", "#5d0e5e","#9b189e","#df65b0","#c994c7", "#d4b9da","#e7e1ef",
                             "#082882","#3b48b3","#1d91c0","#22b3ab","#ace6ce",
                             "#ffff33"))+ 
  theme_plot

dp_plot

ggsave(dp_plot,file=paste0(wdir,"\\01_data\\02_out\\plots\\001_figures\\fig_0X_defor_pulp_yr_province.png"),w=10, h=6,limitsize = FALSE)


##################################################
#### Info requests ###############################
##################################################

## 1. Top 10 other HTI's never deforested areas

top10_others <- defort_df %>%
  as_tibble() %>%
  filter(defor_time == "Never deforested") %>%
  group_by(supplier_id,defor_time) %>%
  summarize(area_ha = n()) %>% 
  mutate(freq = area_ha / sum(area_ha)) %>%
  ungroup() %>%
  left_join(hti_concession_names,by="supplier_id") %>%
  left_join(supplier_groups,by="supplier_id") %>%
  filter(supplier_group == "OTHER") %>%
  arrange(-area_ha) %>%
  select(supplier_id,supplier_label,area_ha) %>%
  top_n(10,area_ha)

write_csv(top10_others,file=paste0(wdir,"\\01_data\\02_out\\tables\\top10_neverdeforested.csv"))

## 2. All HTI's never deforested areas

hti_remfor <- defort_df %>%
  as_tibble() %>%
  filter(defor_time == "Never deforested") %>%
  group_by(supplier_id,defor_time) %>%
  summarize(area_ha = n()) %>% 
  mutate(freq = area_ha / sum(area_ha)) %>%
  ungroup() %>%
  left_join(hti_concession_names,by="supplier_id") %>%
  left_join(supplier_groups,by="supplier_id") %>%
  arrange(-area_ha) %>%
  select(supplier_id,supplier_group,supplier_label,area_ha) 

write_csv(hti_remfor,file=paste0(wdir,"\\01_data\\02_out\\tables\\all_hti_neverdeforested.csv"))
