## ---------------------------------------------------------
## 
## Project: Indonesia pulp deforestation
##
## Purpose of script: 
##
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Date Created: 2025-0X-0X
## 
## ---------------------------------------------------------
##
## Notes: Input datasets
##
##
## ---------------------------------------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------------------------------------

### Load packages
library(stringr)
library(naniar)
library(tidyverse)
library(readxl)
library(tidylog)
library(janitor)
library(lubridate)
library(sf)
library(scales)
library(d3.format)
library(patchwork)
library(extrafont)
library(showtext)
library(khroma) # palettes for color blindness

## set working directory -------------------------------------

wdir <- "remote"

## read data -------------------------------------------------
'%ni%' <- Negate('%in%') # filter out function

# license dates of concessions
lic_dates_hti <- readr::read_csv(paste0(wdir,"\\01_data\\01_in\\wwi\\HTI_LICENSE_DATES.csv"),col_types = cols(license_date = col_date("%m/%d/%Y")))
# IOPP and ITP data
ioppitp_2024 <- st_read(paste0(wdir,"\\01_data\\01_in\\gaveau\\data.gdb"), layer = "ioppitp_hti_union")
# treeMap gridcode classes
ttm_gc <- read_csv(paste0(wdir,"\\01_data\\02_out\\gee\\ttm_classes_by_concession.csv"))
# hti concessions
hti <- read_sf(paste0(wdir,"\\01_data\\01_in\\klhk\\IUPHHK_HTI_20230314_proj.shp")) %>%
  filter(ID != "H-0553")
# wood supply
ws <- read_csv(paste0(wdir,"\\01_data\\01_in\\wwi\\PULP_WOOD_SUPPLY_CLEAN_ALL_ALIGNED_2020_2022.csv"))

# clean data -------------------------------------------------

# HTI concession names
hti_concession_names <- hti %>%
  st_drop_geometry() %>%
  select(id=ID,supplier=namaobj) %>%
  mutate(supplier_label = paste0(supplier," (",id,")"))

## clean license dates
hti_dates_clean <- lic_dates_hti %>%
  mutate(license_year = year(license_date)) %>%
  select(id=HTI_ID,license_year)

## clean mill supplier
mill_supplier <- ws %>%
  filter(str_detect(SUPPLIER_ID, '^ID-')) %>%
  select(supplier_id=SUPPLIER_ID,EXPORTER) %>%
  mutate(mill = case_when(EXPORTER == "OKI" ~ "app",
                          EXPORTER == "INDAH KIAT" ~ "app",
                          EXPORTER == "APRIL" ~ "april",
                          EXPORTER == "LONTAR PAPYRUS" ~ "app",
                          EXPORTER == "TOBA PULP LESTARI" ~ "april",
                          EXPORTER == "TANJUNG ENIM LESTARI" ~ "marubeni", 
                          TRUE  ~ NA)) %>%
  select(-EXPORTER) %>%
  distinct() %>%
  mutate(id = str_replace(supplier_id,"ID-WOOD-CONCESSION-","H-")) %>%
  select(id,mill)

# list of supplying concessions
mill_supplier_list <- mill_supplier %>%
  select(id) %>%
  pull()

other_concessions <- hti_concession_names %>%
  filter(id %in% mill_supplier_list == FALSE) %>%
  select(id) %>%
  mutate(mill="none")

mill_supplier <- mill_supplier %>%
  rbind(other_concessions) %>%
  as.data.table() %>%
  dcast(., formula = id ~ mill, fun.aggregate = length) %>%
  mutate(all = 1,
         zdc_year = case_when(
           app == 1 ~ 2013,
           app == 0 & april == 1 ~ 2015,
           april == 0 & app == 0 & marubeni == 1 ~ 2019,
           TRUE ~ 0),
         zdc_year = ifelse(zdc_year ==0,NA_real_,zdc_year)
  ) 

# calculate concession areas
hti_areas <- hti %>%
  select(id=ID) %>%
  mutate(area_ha = as.double(units::set_units(st_area(.), "hectare")),
                landuse = "Concession",year=2001) %>%
  st_drop_geometry() %>%
  complete(id, year = 2001:2024) %>%
  arrange(year,id,landuse) %>%
  # carry last known area_ha forward to missing years
  group_by(id) %>%
  fill(landuse,area_ha, .direction = "updown") %>%
  ungroup()

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

# create pulp conversion table
ioppitp_conversion <- ioppitp_2024 %>% 
  st_drop_geometry() %>%
  mutate(prev_land_cover = str_sub(as.character(cnvrsn_), 2, 2),
         comm_type = str_sub(as.character(cnvrsn_), 1,1),
         prev_land_cover = case_when(prev_land_cover == 1 ~ "Forest",
                                     prev_land_cover == 4 ~ "Mangrove Forest",
                                     prev_land_cover == 6 ~ "Peat Swamp Forest",
                                     prev_land_cover == 3 ~ "Non Forest",
                                     prev_land_cover == 0  & comm_type == 2 ~ "Already palm in 2000",
                                     prev_land_cover == 0  & comm_type == 3 ~ "Already pulp in 2000",
                                     TRUE ~ "Others")) %>%
  mutate(conv_type = ifelse(CmpDrvn > 0, "Rapid", "Indirect"), #indirect means ultimately converted
         comm_desc = ifelse(comm_type == 2 | comm_type == 4,"Palm","Pulp")) %>%
  rename(id = ID)

# calculate total forest areas
concession_total_forest <- ttm_gc %>%
  mutate(class = 
           case_when(
             grid_code %in% c(101:124,401:424,601:624) ~ "Forest",
             grid_code %in% c(100,400,600) ~ "Forest",
             TRUE ~ "Non Forest"
           )) %>%
  group_by(id,class) %>%
  summarize(area_ha = sum(area_ha))

# full forest trajectory
concession_forest_traj <- ttm_gc %>%
  mutate(class = 
           case_when(
             grid_code %in% c(101:124,401:424,601:624) ~ "Forest",
             grid_code %in% c(100,400,600) ~ "Forest",
             TRUE ~ "Non Forest"
           )) %>%
  #filter(class == "Forest") %>%
  mutate(year = as.character(grid_code),
         year = str_sub(grid_code,2, -1),
         year = as.double(year) + 2000) %>%
  group_by(year,id,class) %>%
  summarize(area_ha = sum(area_ha)) %>%
  filter(year > 2000) %>%
  group_by(id,class) %>%
  mutate(total_area_ha = cumsum(area_ha)) %>%
  left_join(concession_total_forest,by=c("id","class")) %>%
  mutate(total_area_ha = area_ha.y - total_area_ha) %>%
  select(year,id,landuse=class,area_ha=total_area_ha) 

# calculate pulp annual trajectory
concession_pulp_traj <- ioppitp_conversion %>%
  filter(id != "") %>%
  as_tibble() %>%
  filter(comm_desc == "Pulp") %>%
  group_by(year,id,comm_desc) %>%
  summarize(pulp_area_ha = sum(as.double(Area_Ha))) %>%
  group_by(id) %>%
  mutate(total_annual_pulp_area_ha = cumsum(pulp_area_ha))

itp_traj_tbl <- concession_pulp_traj %>%
  mutate(year = as.integer(year)) %>%
  filter(year > 2000) %>%
  rename(landuse = comm_desc) %>%
  complete(landuse,year = 2001:2024) %>%
  arrange(year,id,landuse) %>%
  # carry last known area_ha forward to missing years
  group_by(id,landuse) %>%
  fill(landuse,total_annual_pulp_area_ha, .direction = "down") %>%
  mutate(area_ha = ifelse(is.na(total_annual_pulp_area_ha),0,total_annual_pulp_area_ha)) %>%
  select(year,id,landuse,area_ha)


# Create expanded table of all landuses
lu_expanded_tbl <- concession_fnf_traj %>%
  bind_rows(itp_traj_tbl) %>%
  bind_rows(hti_areas) %>%
  complete(year = 2001:2024) %>%
  arrange(year,id,landuse) %>%
  # carry last known area_ha forward to missing years
  group_by(id,landuse) %>%
  fill(landuse,area_ha, .direction = "updown") %>%
  mutate(area_ha = ifelse(is.na(area_ha),0,area_ha)) %>%
  select(year,id,landuse,area_ha) %>%
  pivot_wider(id_cols = c(year,id),names_from=landuse,values_from=area_ha,values_fill = 0) %>%
  mutate(`Non Forest` = Concession - (Forest + Pulp)) %>%
  select(-Concession) %>%
  pivot_longer(cols = c(-year,-id),names_to = 'landuse',values_to = 'area_ha') %>%
  left_join(hti_dates_clean,by="id") %>%
  left_join(mill_supplier,by="id") %>%
  left_join(hti_concession_names,by="id")

# Sort labelling of landuse
lu_expanded_tbl$landuse <- factor(lu_expanded_tbl$landuse, levels = c("Forest", "Non Forest", "Pulp"))

# Create plots
ggplot(lu_expanded_tbl %>% filter(id == "H-0416"),aes(year,area_ha)) +
  geom_area(aes(fill= as.factor(landuse)), position = position_stack(reverse = F)) +
  scale_x_continuous(expand=c(0,0),breaks=seq(2001,2024,by=1),limits = c(2001,2024)) +
  scale_y_continuous(labels = d3_format(".2~s",suffix = "ha"),expand = c(0,0)) +
  ylab("") +
  xlab("") +
  #ggtitle(paste0(concession_)) +
  scale_fill_manual(values=c("#009E73","#F0E442","#CC79A7"),
                    breaks = c("Forest","Non Forest","Pulp"),
                    labels = c("Forest","Non Forest","Pulp"))+
  #scale_shape_manual(values=c(1),labels=c("Pulpwood area (TreeMap)"),na.translate=FALSE)+
  #scale_color_manual(values = c("#000000","#0072B2")) +
  guides(fill = guide_legend(nrow = 1),color = guide_legend(nrow=1),shape = guide_legend(nrow=2),keyheight = 10) +
  theme_plot

