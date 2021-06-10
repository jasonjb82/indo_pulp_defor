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
## Todo: 
## Clean up plot to match what's being produced in 07_analyze_gee_data.R
## Convert n in frequency table to hectares - DONE
## Confirm Gaveau data is being interpreted correctly - DONE
## Confirm JRC data is being interpreted correctly. E.g. confirm that 0 in defyear means no deforestation observed in 1990-2019? - DONE
## Seems like JRC data only covers locations that started as forest in 1990? If so, need to add deforested points to ensure final ha = total area within concessions 
## Add qc checks
## Add three binary columns indicating whether concession supplies APRIL, APP, Marubeni mills - DONE
## Update <2013 cut-off to match earliest ZDC of their downstream mill - DONE
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

## credentials ----------------------------------------------

aws.signature::use_credentials()
bucket <- "trase-storage"
Sys.setenv("AWS_DEFAULT_REGION" = "eu-west-1")

## set working directory -------------------------------------

wdir <- "remote"

## read data -------------------------------------------------

## increase memory size
memory.limit(size=40000)

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
gaveau_pulp <- samples_gaveau_landuse %>%
  lazy_dt() %>%
  left_join(samples_hti,by="sid") %>%
  as_tibble()

# TRUE/FALSE if sample is ever on pulp clearing  
gaveau_pulp$ever_pulp <- (rowSums(gaveau_pulp[,startsWith(names(gaveau_pulp),"id_")]==4) >= 1) # Gaveau class 4 is industrial pulp clearing

# select columns
gaveau_pulp <- gaveau_pulp %>% 
  select(sid,ever_pulp)

## identify pixels that started as forest in 1990
## NOTE: dataset currently doesn't include any pixels that are not forested at t0
forest_sids <- samples_jrc_tmf %>% 
  filter(dec1990 %in% c(1,2)) %>% # 1 = undisturbed tropical moist forest; 2 = degraded tmf
  pull(sid)

## create pixel-level dataset starting with JRC deforestation year
## NOTE: 0 means no deforestation observed in 1990-2019
samples_df <- samples_jrc_defyr %>% 
  lazy_dt() %>%
  select(sid, island_code = jrc_def_yr, def_year = deforestation_year) %>%
  mutate(island_code = as.integer(island_code)) %>% 
  left_join(island_tab, by = "island_code")

### Join to gaveau, concession, island data, mill supplied
samples_df <- samples_df %>% 
  as_tibble()%>%
  add_column(rand = runif(nrow(.))) %>%
  #mutate(rand = runif(dim(samples_df)[1])) %>% 
  lazy_dt() %>%
  left_join(samples_hti,by="sid") %>%
  rename(supplier_id = ID) %>% 
  left_join(hti_dates_clean,by="supplier_id") %>%
  left_join(hti_concession_names,by="supplier_id") %>% 
  left_join(gaveau_pulp, by = "sid") %>% 
  #left_join(mill_supplier,by="supplier_id") %>%
  select(sid, island, supplier_id, def_year, ever_pulp, license_year, supplier_label,rand) %>% 
  mutate(start_for = sid %in% forest_sids)

###########################################################################
# Analyze deforestation timing --------------------------------------------
###########################################################################

### Identify different deforestation timings
lag <- 3
defort_df <- samples_df %>% 
  left_join(mill_supplier,by="supplier_id") %>%
  mutate(def_year = ifelse(def_year==0, ifelse(start_for == 1, 2999, 0), def_year)) %>%  
  mutate(defor_time = ifelse((def_year < license_year - lag), paste0("Deforestation >",lag," years before license"),  # Note: works because no licenses were issued prior to 1992 so JRC fully covers period of interest
         ifelse((def_year >= license_year - lag) & (def_year < license_year), paste0("Deforestation in ",lag," years prior to license"),
             ifelse((def_year >= license_year) & (def_year < 2013) & (april == 1), "Deforestation on licensed concession, before earliest ZDC of downstream mill",
                   ifelse((def_year >= 2013) & (def_year != 2999) & (april == 1), "Deforestation on licensed concession, after earliest ZDC of downstream mill",
                        ifelse((def_year >= license_year) & (def_year < 2015) & (april == 0 & app == 1), "Deforestation on licensed concession, before earliest ZDC of downstream mill",
                            ifelse((def_year >= 2015) & (def_year != 2999) & (april == 0 & app == 1), "Deforestation on licensed concession, after earliest ZDC of downstream mill",
                                ifelse((def_year >= license_year) & (def_year < 2018) & (april == 0 & app == 0 & marubeni == 1), "Deforestation on licensed concession, before earliest ZDC of downstream mill", # need to confirm with Brian ZDC yr
                                    ifelse((def_year >= 2018) & (def_year != 2999) & (april == 0 & app == 0 & marubeni == 1), "Deforestation on licensed concession, after earliest ZDC of downstream mill", # need to confirm with Brian ZDC yr
                                       ifelse((def_year != 2999) & (april == 0 & app == 0 & marubeni == 0), "Deforestation on concession after license",
                                          ifelse((def_year == 2999), "Never deforested", 0)))))))))),
         defor_pulp = ifelse(ever_pulp==1, paste0(defor_time, ", converted to pulp plantation"), paste0(defor_time, ", not converted to pulp plantation")))
  

## QA checks

## Check 1 - check if no of samples are same before deforestation timing reclass
test_supp_id = "H-0262"
expect_true(all.equal(dim(subset(as.data.frame(samples_df),supplier_id==test_supp_id))[1],dim(subset(as.data.frame(defort_df),supplier_id==test_supp_id))[1]))

## Generate frequency table by group
group_var <- "supplier_label" # Generally either island or supplier_label
class_var <- "defor_time" # Generally either defor_time or defor_pulp
mill_var <- "april" # Generally either april,app,marubeni or all (all concessions)
freq_tab <- defort_df %>%
  filter(!!sym(mill_var) == 1) %>%
  filter(island == "kalimantan") %>% # filter to island if required
  group_by(.data[[group_var]], .data[[class_var]]) %>% 
  summarize(area_ha = n()) %>% 
  mutate(freq = area_ha / sum(area_ha)) %>%
  ungroup()

freq_tab

# Check 2 - check if total area for deforestation timings and frequency table are equal (if supplier label)
condition = grepl("supplier_label", names(as_tibble(freq_tab)))

select.vars <- function (df, cond=condition[1]){
  if(cond){
    df %>%
      select(supplier_label,area_ha)
  } else {
    df %>%
      select(island,area_ha)
  }
}

area_test_supp_id <- select.vars(freq_tab) %>%
  slice(1) %>%
  select(!!sym(group_var)) %>%
  mutate(id = str_extract_all(supplier_label,"(?<=\\().+?(?=\\))")[[1]]) %>%
  pull

test_defort_area = defort_df %>%
  filter(supplier_id == area_test_supp_id) %>%
  as_tibble() %>%
  group_by(supplier_id) %>%
  tally(sort=TRUE) %>%
  mutate(area_ha = n) %>%
  summarize(area_ha = sum(area_ha))

test_freq_tab_area <- freq_tab %>%
  as_tibble() %>%
  mutate(supplier_id = stringr::str_extract(string = supplier_label,pattern = "(?<=\\().*(?=\\))")) %>%
  filter(supplier_id == area_test_supp_id) %>%
  group_by(supplier_id) %>%
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

## ordering by never deforested areas
order <- freq_tab %>% 
  as_tibble() %>%
  filter(!!sym(class_var) == "Never deforested, not converted to pulp plantation" | !!sym(class_var) == "Never deforested") %>% 
  arrange(-area_ha) %>%
  pull(!!sym(group_var))

## set plot order
plot_order_deft_pulp <- c(
  "Never deforested",
  "Never deforested, not converted to pulp plantation",
  "Never deforested, converted to pulp plantation",
  "Deforestation >3 years before license",
  "Deforestation >3 years before license, converted to pulp plantation",
  "Deforestation >3 years before license, not converted to pulp plantation",
  "Deforestation in 3 years prior to license",
  "Deforestation in 3 years prior to license, converted to pulp plantation",
  "Deforestation in 3 years prior to license, not converted to pulp plantation",
  "Deforestation on licensed concession, before earliest ZDC of downstream mill",
  "Deforestation on licensed concession, before earliest ZDC of downstream mill, converted to pulp plantation",
  "Deforestation on licensed concession, before earliest ZDC of downstream mill, not converted to pulp plantation",
  "Deforestation on licensed concession, after earliest ZDC of downstream mill",
  "Deforestation on licensed concession, after earliest ZDC of downstream mill, converted to pulp plantation",
  "Deforestation on licensed concession, after earliest ZDC of downstream mill, not converted to pulp plantation",
  "Deforestation on concession after license",
  "Deforestation on concession after license, converted to pulp plantation",
  "Deforestation on concession after license, not converted to pulp plantation")

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
  guides(fill = guide_legend(nrow = 5)) +
  scale_fill_manual(values = cols_alpha,name ="Group",
  breaks=plot_order_deft_pulp,labels=plot_order_deft_pulp)

freq_plot

## save plot to png
ggsave(freq_plot,file=paste0(wdir,"\\01_data\\02_out\\plots\\APRIL\\kali_april_defort_pulp.png"), dpi=400, w=15, h=8,type="cairo-png",limitsize = FALSE)


###########################################################################
# Analyze annual changes --------------------------------------------------
###########################################################################

## TODO: add earliest year of downstream mill ZDC, add more QA tests

## annual pulp planted areas
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
  left_join(select(gaveau_annual_pulp,supplier_id,year,shr_gav_lu_areas,gav_class),by=c("year","supplier_id"))

## garbage collection (clearing memory)
gc()

## filter or aggregate for plot (by island, downstream mill, etc)
jrc_ac_comb <- jrc_hti_ac %>%
  as_tibble() %>%
  #filter(island_code == 5) %>%
  filter(marubeni == 1)

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
                    legend.key.height = unit(12, "pt"),
                    legend.key.width = unit(12, "pt"),
                    legend.text = element_text(size = 9,colour="grey30"),
                    legend.title = element_blank(),
                    legend.position="bottom",
                    legend.box="horizontal",
                    plot.margin=unit(c(0.5,1.5,0.5,0.5),"cm"))

options(crayon.enabled = FALSE)

## plotting
ac_plot <- ggplot(data=jrc_ac_comb,aes(year,shr_class)) +
  geom_area(aes(fill= as.character(class_desc)), position = 'stack') +
  scale_x_continuous(expand=c(0,0),breaks=seq(1990,2019,by=1)) +
  scale_y_continuous(labels = d3_format(".2~s",suffix = "%"),expand = c(0,0)) +
  geom_vline(aes(xintercept=as.numeric(license_year),color="License year"),size=0.5)+
  geom_point(data=jrc_ac_comb,aes(x=year,y=shr_gav_lu_areas,shape=gav_class),color="black",size=1.5)+
  ylab("") +
  xlab("") +
  #scale_fill_muted() +
  scale_fill_manual(values=c("lightpink", "orange3", "yellowgreen","#F8F899","seagreen4"))+ 
  scale_shape_manual(values=17,labels="Woodpulp planted area",na.translate=FALSE)+ 
  scale_color_manual(values = c("License year" = "palevioletred4")) +
  facet_wrap(~supplier_label,ncol=5,scales="free") +
  theme_plot

ac_plot

## export to png
ggsave(ac_plot,file=paste0(wdir,"\\01_data\\02_out\\plots\\marubeni_suppliers_jrc_annual_changes.png"), dpi=400, w=30, h=30,type="cairo-png",limitsize = FALSE)
