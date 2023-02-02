## ---------------------------------------------------------
## 
## Project: Indonesia pulp deforestation
##
## Purpose of script: Create summary figure for paper
##
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Date Created: 2022-04-01
## 
## ---------------------------------------------------------
##
## Notes: Input datasets
##        1) HTI concessions (boundaries) and concession start year - from KLHK
##        2) Gaveau landuse change - commodity deforestation (2000 - 2020) (IOPP,ITP and smallholders)
##        3) JRC deforestation (1990 - 2020)
##        4) Wood types
##        5) Pulp mill capacities
##        6) Wood pulp prices
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
library(concordance)
library(rcartocolor)
library(vistime)
library(khroma) # palettes for color blindness

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
source("scripts\\001_misc\001_color_palettes.R")

## data lookup table
lu_table <- read_csv(paste0(wdir,"\\01_data\\02_out\\gee\\data_lookup_table.csv"))

## hti license dates
lic_dates_hti <- readr::read_csv(paste0(wdir,"\\01_data\\01_in\\wwi\\HTI_LICENSE_DATES.csv"),col_types = cols(license_date = col_date("%m/%d/%Y")))

## supplier groups
groups <- read_csv(paste0(wdir,"\\01_data\\01_in\\wwi\\ALIGNED_NAMES_GROUP_HTI.csv"))

## HTI and sample IDs
samples_hti <- read_csv(paste0(wdir,"\\01_data\\02_out\\samples\\samples_hti_id.csv"))

# hti concessions
hti <- read_sf(paste0(wdir,"\\01_data\\01_in\\klhk\\IUPHHK_HT_proj.shp"))

# wood supply
ws <- read_delim(get_object(object="indonesia/wood_pulp/production/out/PULP_WOOD_SUPPLY_CLEAN_ALL_ALIGNED_2015_2019.csv", bucket), delim = ",")

# silk data
silk_data <- read_csv(paste0(wdir,"\\01_data\\01_in\\silk\\WOOD_EXPORTS_SILK_MERGED.csv"))

# wood species lookup table
wood_species <- read_csv(paste0(wdir,"\\01_data\\01_in\\silk\\SILK_PULP_WOOD_SPECIES.csv"))

# policy timeline
policy_tl <- read_csv(paste0(wdir,"\\01_data\\01_in\\tables\\policy_timeline.csv"))

# policy timeline (updated)
policy_tl <- read_csv(paste0(wdir,"\\01_data\\01_in\\tables\\policy_timeline_cats.csv")) %>%
  mutate(year_col = as.Date(year_proper,format="%d/%m/%Y"))

# pulp prices (FRED)
pulp_prices <- read_csv(paste0(wdir,"\\02_literature\\pulp_prices\\FRED\\WPU0911_yearly.csv"))

# deforestation within concessions (Trase)
alldefor_conc <- read_csv(paste0(wdir,"\\01_data\\01_in\\jrc\\jrc_def_year_hti.csv"))

# Indonesia deforestation
alldefor_idn <- read_csv(paste0(wdir,"\\01_data\\01_in\\jrc\\jrc_def_areas_year_idn.csv"))

# annual pulp capacities
pulp_cap <- read_csv(paste0(wdir,"\\01_data\\01_in\\tables\\annual_pulp_cap.csv"))

# kabupaten
kab <- read_sf(paste0(wdir,"\\01_data\\01_in\\big\\idn_kabupaten_big.shp"))
prov_slim <- kab %>% select(prov,prov_code) %>% st_drop_geometry() %>% distinct() %>%
  mutate(prov_code = ifelse(prov == "PAPUA",92,prov_code))

# mills
mills <- s3read_using(read_excel, object = "indonesia/wood_pulp/logistics/out/mills/MILLS_EXPORTERS_20200405.xlsx", bucket = bucket)

## JRC
## Annual changes 
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\jrc\\annual_changes\\"),pattern = "*.csv",full.names= TRUE)

samples_jrc_tmf <- filenames %>%
  map_dfr(read_csv, .id = "jrc_tmf_ac") %>%
  janitor::clean_names() %>%
  select(-system_index,-geo)

## Deforestation year
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\jrc\\deforestation_year\\"),pattern = "*.csv",full.names= TRUE)

samples_jrc_defyr <- filenames %>%
  map_dfr(read_csv, .id = "jrc_def_yr") %>%
  janitor::clean_names() %>%
  select(-system_index,-geo)

## Gaveau data
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\gaveau\\"),pattern = "*.csv",full.names= TRUE)

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


# # using gaveau's forest in 2000
# gaveau_for_2000 <- samples_gaveau_landuse %>%
#   pivot_longer(cols = starts_with("id_"),
#                names_to = 'year',
#                values_to = 'class') %>%
#   mutate(year = str_replace(year,"id_", ""),year = as.double(year)) %>%
#   filter(class == 1) %>%
#   filter(year == 2000)
# 
# forest_sids <- gaveau_for_2000 %>% 
#   pull(sid)

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
# Create figures
###########################################################################

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
                    axis.text.x = element_text(size = 8, color = "grey30",angle = 0, face="bold"),
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
                    legend.direction="horizontal",
                    plot.margin=unit(c(0.1,1.5,0.1,0.5),"cm"))

options(crayon.enabled = FALSE)

# Panel A - Pulp contributions to deforestation through time -----------

# Left axis: Stacked bar plot breaking total Indonesian deforestation (top bar, from vancutsem) into
# deforestation happening within fiber concessions (middle bar, current trase figure?) and for direct conversion
# to pulp plantations (bottom bar, gaveau).
# Right axis: Possibly overlay pulp price curve on right axis if it doesn't get too messy?

# idn deforestation (jrc)
defor_idn_year <- alldefor_idn %>%
  pivot_longer(cols=c(-'country'),names_to="year",values_to="area_m2") %>%
  mutate(year = as.double(year),area_ha = area_m2*0.0001) %>%
  filter(year >= 2001) %>%
  select(year,area_ha) %>%
  mutate(name = "Deforestation in Indonesia")

# hti concession deforestation (JRC)
defor_hti_year <- samples_jrc_defyr %>%
  rename(year = deforestation_year) %>%
  group_by(year) %>%
  summarize(area_ha = n()) %>%
  filter(year >= 2001) %>%
  mutate(name = "Deforestation\nwithin HTI concessions")

# pulp deforestation (Gaveau)
defor_pulp_year <- samples_df %>%
  filter(def_year >= 2001 & def_year < 2999 & ever_pulp == TRUE) %>%
  as_tibble() %>%
  #group_by(def_year,island) %>%
  left_join(select(hti,ID),by=c("supplier_id"="ID")) %>%
  select(-geometry,-supplier_id) %>%
  rename(year=def_year) %>%
  group_by(start_for,year) %>%
  summarize(area_ha = n()) %>%
  mutate(name = "Deforestation for pulp\nwithin HTI concessions")

# pulp prices
pulp_prices_clean <- pulp_prices %>%
  select(DATE,PPI=WPU0911) %>%
  filter(between(DATE, as.Date("2000-01-01"),as.Date("2019-12-31"))) %>%
  mutate(year = year(DATE),PPI = as.double(PPI)) %>%
  select(year,PPI)

# merge deforestation df's and pulp prices
defor_pp_comb <- defor_idn_year %>%
  rbind(defor_hti_year) %>%
  rbind(defor_pulp_year) %>%
  left_join(pulp_prices_clean,by="year") %>%
  filter(year < 2020)


def_plot_order <- c("Deforestation for pulp\nwithin HTI concessions","Deforestation\nwithin HTI concessions","Deforestation in Indonesia")

# create dual-axis plot
pa_scale_factor <- 0.01
defor_pp_plot <- ggplot(data = defor_pp_comb, aes(x = year))+
  geom_bar(stat="identity",position = "stack",aes(y = area_ha/1000000,fill=factor(name,levels=rev(def_plot_order)))) +
  geom_line(aes(y = PPI*pa_scale_factor,color="Producer Price Index")) +
  geom_point(aes(y = PPI*pa_scale_factor,color="Producer Price Index")) +
  ylab("Area (million ha)\n") +
  xlab("") +
  scale_fill_manual(values=c("#c194d4","#a6e1f5","#fcd483"))+ 
  scale_color_manual(NULL, values = "black") +
  scale_x_continuous(breaks = seq(from = 2001, to = 2019, by =1),expand=c(0,0)) +
  scale_y_continuous(sec.axis = sec_axis(~ .*1, labels = number_format(scale=1/pa_scale_factor),
                                         name="Producer Price Index\n"), 
                     limits = c(0,2.5),
                     expand = c(0,0)) +
  theme_plot 

defor_pp_plot


# Modified deforestation plot
# pulp deforestation (Gaveau)
defor_pulp_year <- samples_df %>%
  filter(def_year >= 2001 & def_year < 2999 & ever_pulp == TRUE) %>%
  as_tibble() %>%
  #group_by(def_year,island) %>%
  left_join(select(hti,ID),by=c("supplier_id"="ID")) %>%
  select(-geometry,-supplier_id) %>%
  rename(year=def_year) %>%
  group_by(start_for,year) %>%
  summarize(area_ha = n()) 
  
defor_pulp_year <- defor_pulp_year %>%
  ungroup() %>%
  add_row(start_for = c(FALSE,FALSE),year = c(2021,2022), area_ha = c(0,0))


defor_plot <- ggplot(data = defor_pulp_year, aes(x = year))+
  geom_bar(stat="identity",position = "stack",aes(y = area_ha/1000),fill="grey20") +
  ylab("Area (1000 ha)\n") +
  xlab("\nDeforestation within HTI concessions") +
  scale_fill_manual(values=c("#c194d4","#a6e1f5"))+ 
  scale_color_manual(NULL, values = "black") +
  scale_x_continuous(breaks = seq(from = 2001, to = 2022, by =1)) +
  scale_y_continuous(limits = c(0,200),expand = c(0,0)) +
  theme_plot 

defor_plot


# Panel B - Industrial capacity ----------------------------------------

# Left axis: Total planted pulp bar chart. I dropped in a prior figure that shows this by province, 
# but we'll probably need to simplify for this already complicated figure. Maybe do this by island?
# Right axis: Total installed mill capacity line chart. We'll probably need to dig up these values for
# the 2000-2015 period, maybe WWI or Auriga has these numbers?

# mill_caps_ts <- mills %>% select(MILL_ID,START_OPER_YEAR,PULP_CAP_2019_MTPY) %>% 
#   distinct(MILL_ID,START_OPER_YEAR,PULP_CAP_2019_MTPY) %>%
#   mutate(YEAR = START_OPER_YEAR) %>% 
#   complete(MILL_ID,YEAR = seq(min(YEAR), 2020, by = 1)) %>% 
#   fill(PULP_CAP_2019_MTPY) %>%
#   mutate(CAP = ifelse(is.na(START_OPER_YEAR),NA,PULP_CAP_2019_MTPY)) %>%
#   group_by(MILL_ID) %>%
#   fill(CAP, .direction = "down") %>%
#   select(MILL_ID,YEAR,CAP) %>%
#   group_by(YEAR) %>%
#   summarize(CAP = sum(CAP,na.rm=T))

# cumulative plantation development by island
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

cum_pulp <- gaveau_annual_pulp_supplier %>%
  left_join(select(hti,ID,Kode_Prov),by=c("supplier_id"="ID")) %>%
  select(-geometry,-supplier_id) %>%
  group_by(year,prov_code=Kode_Prov) %>%
  summarize(area_ha = sum(n)) %>%
  left_join(prov_slim,by="prov_code") %>%
  distinct() %>%
  mutate(island = str_sub(prov_code, 1, 1)) %>%
  mutate(
    island = case_when(
      island == 1 ~ "Sumatera", island == 2 ~ "Riau Archipelago",
      island == 3 ~ "Jawa", island == 5 ~ "Balinusa",
      island == 6 ~ "Kalimantan", island == 7 ~ "Sulawesi",
      island == 8 ~ "Maluku", island == 9 ~ "Papua"
    )
  ) %>%
  group_by(year,island) %>%
  summarize(area_ha = sum(area_ha))

cum_pulp <- gaveau_annual_pulp_supplier %>%
  left_join(select(hti,ID,Kode_Prov),by=c("supplier_id"="ID")) %>%
  select(-geometry,-supplier_id) %>%
  group_by(year) %>%
  summarize(area_ha = sum(n))

# combine mill capacity and planted palm areas
# mill_caps_pp <- cum_pulp %>%
#   left_join(mill_caps_ts,by=c("year"="YEAR")) %>%
#   rename(cap = CAP) %>%
#   mutate(area_mha = area_ha/1000000) %>%
#   filter(year <= 2019 & year > 2000)

# combine mill capacity and planted pulp areas
annual_caps_pp <- cum_pulp %>%
  left_join(pulp_cap,by="year") %>%
  rename(cap = capacity_mtpy) %>%
  mutate(area_mha = area_ha/1000000) %>%
  filter(year <= 2019 & year > 2000) %>%
  mutate(name = "indonesia")

# create dual-axis plot
pb_scale_factor <- 0.25
mc_pp_plot <- ggplot(data = annual_caps_pp, aes(x = year))+
  geom_col(aes(y = area_mha,fill=name)) +
  geom_line(aes(y = cap*pb_scale_factor,color="Pulp production capacity")) +
  geom_point(aes(y = cap*pb_scale_factor,color="Pulp production capacity")) +
  ylab("Planted pulp (million ha)\n") +
  xlab("") +
  scale_fill_manual(values = "#a89671", label = "Planted pulp") +
  scale_color_manual(NULL, values = "black") +
  scale_x_continuous(breaks = seq(from = 2001, to = 2019, by =1),expand=c(0,0)) +
  scale_y_continuous(sec.axis = sec_axis(~ .*1, labels = number_format(scale=1/pb_scale_factor),
                                         name="Pulp production capacity (MTPY)\n"), 
                                         limits = c(0,3.5),
                                         expand = c(0,0)) +
  theme_plot 

mc_pp_plot

# Panel C - Wood supply transition -------------------------------------

# Stacked bar breaking pulpwood volumes into MTH / plantation sources (probably simplify categories from current figure). 
# We really need to go back to 2001  which I think was a continued issue - for now we can put a 0 placeholder 
# in for early years; but I'm pretty sure either Auriga or Chris have these numbers in previous reports 
# which we can discuss during our call

## clean and summarize ---------------------------------------

# get tax IDs for the 6 main pulp mills

### 013418579092000 - PT. RIAU ANDALAN PULP AND PAPER (APRIL)
### 010005668092000 - PT. INDAH KIAT PULP & PAPER TBK
### 021212279073000 - PT. INTIGUNA PRIMATAMA (APRIL)
### 013575964092000 - PT. TANJUNGENIM LESTARI PULP AND PAPER
### 012197950054000 - PT. TOBA PULP LESTARI TBK
### 011159654092001 - PT. LONTAR PAPYRUS PULP & PAPER INDUSTRY
### 032034811312001 - PT. OKI PULP & PAPER MILLS

npwp_pulp_mills <- mills %>%
  select(EXPORTER_ID) %>%
  distinct() %>%
  pull(EXPORTER_ID) %>%
  print()

silk_pulp <- silk_data %>%
  filter((HS_NUMBER %like% "470329" | HS_NUMBER  %like% "4702")  & !str_detect(DESKRIPSI, 'SAMPLE') & 
           NPWP %in% npwp_pulp_mills & BERAT_BERSIH_KG > 150) %>%
  mutate(YEAR = year(TGL_INVOICE),
         YEAR = ifelse(is.na(YEAR),year(TGL_TTD),YEAR),
         HS_NUMBER = ifelse(HS_NUMBER %like% 4702,470200,470329)) %>%
  filter(YEAR >= 2013) %>%
  left_join(select(mills,EXPORTER_ID,MILL_GROUP,MILL_NAME),by=c("NPWP"="EXPORTER_ID"))


silk_pulp_clean <- silk_pulp %>%
  separate_rows(SCIENTIFIC_NAMES,sep=";") %>%
  group_by(NPWP,NAMA_EKSPORTIR,PROPINSI,KABUPATEN_KOTA,NO_ETPIK,NAMA_IMPORTIR,NEGARA_IMPORTIR,PELABUHAN_MUAT,PELABUHAN_BONGKAR,
           NEGARA_TUJUAN,NO_INVOICE,SKEMA_KERJASAMA,NO_V_LEGAL,TRANSPORTASI,TGL_INVOICE,KETERANGAN,PEJABAT_TTD,TEMPAT_TTD,
           DIGITAL_SIGN,LOKASI_STUFFING,NO,HS_NUMBER,HS_PRINTED,DESKRIPSI,NUMBER_OF_UNIT,HARVEST_COUNTRY,ID,YEAR,CURRENCY,
           VOLUME_M3,BERAT_BERSIH_KG,VALUE) %>%
  mutate(PROP_BERAT_BERSIH = prop.table(BERAT_BERSIH_KG),PROP_VALUE=prop.table(VALUE)) %>%
  ungroup()%>%
  mutate(PROP_BERAT_BERSIH_KG = PROP_BERAT_BERSIH*BERAT_BERSIH_KG, PROP_VALUE_UNIT = PROP_VALUE*VALUE) %>%
  mutate(SCIENTIFIC_NAMES = str_trim(SCIENTIFIC_NAMES,side="both")) %>%
  left_join(wood_species,by="SCIENTIFIC_NAMES") %>%
  mutate(MAJOR_WOOD_SPECIES = ifelse(SPECIES_GENERIC != "ACACIA" & SPECIES_GENERIC != "EUCALYPTUS", "OTHERS", SPECIES_GENERIC))

# get annual exports by HS codes
silk_annual_pulp_exports_hs <- silk_pulp_clean %>%
  filter(YEAR >= 2015) %>%
  group_by(YEAR,HS_NUMBER) %>%
  summarize(PULP_EXPORTS = sum(PROP_BERAT_BERSIH_KG/1000)) %>%
  print()

# get annual exports by mill
silk_annual_pulp_exports_mill <- silk_pulp_clean %>%
  filter(YEAR >= 2015) %>%
  group_by(YEAR,MILL_NAME) %>%
  summarize(PULP_EXPORTS = sum(PROP_BERAT_BERSIH_KG/1000)) %>%
  arrange(-desc(MILL_NAME)) %>%
  print()

# check that totals are same after splitting rows for wood species
s1 <- silk_pulp %>% filter(YEAR == 2018) %>%
  group_by() %>%
  summarize(VOLUME = sum(BERAT_BERSIH_KG/1000,na.rm = TRUE)) %>%
  print()

s2 <- silk_pulp_clean %>% filter(YEAR == 2018) %>%
  group_by() %>%
  summarize(VOLUME = sum(PROP_BERAT_BERSIH_KG/1000,na.rm = TRUE)) %>%
  print()

expect_equal(s1$VOLUME,s2$VOLUME)

# get rank of species by year
pulp_species_year <- silk_pulp_clean %>%
  group_by(YEAR,SPECIES_GENERIC) %>%
  summarize(TOTAL_VOLUME_TONNES = sum(PROP_BERAT_BERSIH_KG/1000)) %>%
  arrange(YEAR,-TOTAL_VOLUME_TONNES)

# get total exports by year
exports_year <- silk_pulp %>%
  group_by(YEAR) %>%
  summarize(EXPORT_TONS = sum(BERAT_BERSIH_KG/1000)) %>%
  print()

# plot by general wood species
silk_pulp_species <- silk_pulp_clean %>%
  select(YEAR,PROP_BERAT_BERSIH_KG,ID,MAJOR_WOOD_SPECIES,MILL_NAME,SPECIES_CLEAN) %>% 
  mutate(
    SPECIES_GENERAL = case_when(
      SPECIES_CLEAN == "ACACIA MANGIUM" ~ "ACACIA MANGIUM",
      SPECIES_CLEAN == "ACACIA CRASSICARPA" ~ "ACACIA CRASSICARPA",
      SPECIES_CLEAN == "EUCALYPTUS PELLITA" ~ "EUCALYPTUS PELLITA",
      MAJOR_WOOD_SPECIES == "ACACIA" & (SPECIES_CLEAN != "ACACIA MANGIUM" | SPECIES_CLEAN != "ACACIA CRASSICARPA") ~ "ACACIA (OTHERS)",
      MAJOR_WOOD_SPECIES == "EUCALYPTUS" & SPECIES_CLEAN != "EUCALYPTUS PELLITA" ~ "EUCALYPTUS (OTHERS)",
      TRUE ~ MAJOR_WOOD_SPECIES
    )
  )

## checking shipments and composition of species
f <- function(x)setNames(wood_species$GENERAL_CLASS, wood_species$SCIENTIFIC_NAMES)[x] 
vars_to_process=c("TYPE1","TYPE2","TYPE3","TYPE4","TYPE5","TYPE6")

silk_species_shipments <- silk_pulp %>%
  mutate(MIXED = ifelse(str_detect(SCIENTIFIC_NAMES,";"),"Y","N")) %>%
  group_by(NPWP,NAMA_EKSPORTIR,PROPINSI,KABUPATEN_KOTA,NO_ETPIK,NAMA_IMPORTIR,NEGARA_IMPORTIR,PELABUHAN_MUAT,PELABUHAN_BONGKAR,
           NEGARA_TUJUAN,NO_INVOICE,SKEMA_KERJASAMA,NO_V_LEGAL,TRANSPORTASI,TGL_INVOICE,KETERANGAN,PEJABAT_TTD,TEMPAT_TTD,
           DIGITAL_SIGN,LOKASI_STUFFING,NO,HS_NUMBER,HS_PRINTED,DESKRIPSI,NUMBER_OF_UNIT,HARVEST_COUNTRY,ID,YEAR,CURRENCY,
           VOLUME_M3,BERAT_BERSIH_KG,VALUE,MIXED) %>%
  ungroup() %>%
  mutate(SCIENTIFIC_NAMES = str_trim(SCIENTIFIC_NAMES,side="both")) %>%
  separate(SCIENTIFIC_NAMES,c("TYPE1","TYPE2","TYPE3","TYPE4","TYPE5","TYPE6"),";") %>%
  mutate_at(.vars=vars_to_process,funs(f)) %>%
  mutate(WOODTYPE_EXPORT=pmap_chr(list(TYPE1,TYPE2,TYPE3,TYPE4,TYPE5,TYPE6), ~paste(sort(c(...)), collapse = ","))) %>%
  separate_rows(WOODTYPE_EXPORT, sep = ",") %>%
  group_by(NPWP,NAMA_EKSPORTIR,PROPINSI,KABUPATEN_KOTA,NO_ETPIK,NAMA_IMPORTIR,NEGARA_IMPORTIR,PELABUHAN_MUAT,PELABUHAN_BONGKAR,
           NEGARA_TUJUAN,NO_INVOICE,SKEMA_KERJASAMA,NO_V_LEGAL,TRANSPORTASI,TGL_INVOICE,KETERANGAN,PEJABAT_TTD,TEMPAT_TTD,
           DIGITAL_SIGN,LOKASI_STUFFING,NO,HS_NUMBER,HS_PRINTED,DESKRIPSI,NUMBER_OF_UNIT,HARVEST_COUNTRY,ID,YEAR,CURRENCY,
           VOLUME_M3,BERAT_BERSIH_KG,VALUE,MIXED) %>%
  summarise(WOODTYPE_EXPORT = paste(unique(WOODTYPE_EXPORT), collapse = ","))

# create species shipments by year
woodtype_exports_yr <- silk_species_shipments %>%
  select(YEAR,BERAT_BERSIH_KG,ID,WOODTYPE_EXPORT,MIXED) %>%
  group_by(YEAR,ID,WOODTYPE_EXPORT,MIXED) %>%
  summarize(TONS = sum(BERAT_BERSIH_KG/1000)) %>%
  group_by(YEAR,ID,MIXED) %>%
  group_by(YEAR,WOODTYPE_EXPORT) %>%
  summarize(NO_SHIPMENTS=n(),TONS=sum(TONS)) %>%
  group_by(YEAR) %>%
  mutate(PERC_SHIPMENT = prop.table(NO_SHIPMENTS)*100, PERC_VOLUME = prop.table(TONS)*100) %>%
  mutate(WOODTYPE_GENERAL = case_when(grepl("ACACIA", WOODTYPE_EXPORT) ~ "Plantation",
                                      grepl("EUCALYPTUS", WOODTYPE_EXPORT) ~ "Plantation",
                                      grepl("MIXED TROPICAL HARDWOODS", WOODTYPE_EXPORT, ignore.case = TRUE) ~"Mixed Tropical Hardwoods")) %>%
  #complete(YEAR = seq(2001, 2012, by = 1)) %>%
  ungroup() %>%
  add_row(YEAR = seq(2001,2012,by=1), TONS=0) %>% # temporary fix
  add_row(YEAR = c(2020,2022), TONS = 0) %>% # temporary fix
  mutate(TONS = ifelse(is.na(TONS),0,TONS), WOODTYPE_GENERAL = ifelse(is.na(WOODTYPE_GENERAL),"Plantation",WOODTYPE_GENERAL))

# check yearly shipments
yearly_shipments_total <- woodtype_exports_yr %>%
  group_by(YEAR) %>%
  summarize(TONS = sum(TONS))

wt_plot <- ggplot(data=woodtype_exports_yr) +
  geom_bar(stat="identity",position="stack",aes(x=YEAR,y=TONS/1000000,fill=as.factor(WOODTYPE_GENERAL))) +
  scale_x_continuous(breaks = seq(from = 2001, to = 2022, by =1)) +
  xlab("") +
  scale_y_continuous(name="Pulp exports (Million tons)\n",
                     limits=c(0,6),
                     breaks=seq(0,6, by=1),
                     #labels = d3_format(".3~s"),
                     expand = c(0,0)) + 
  theme_plot +
  labs(fill = "\n") +
  scale_fill_manual(values=c("#66c2a4","#ed8f8a"))+ 
  guides(fill = guide_legend(title.position = "top",nrow=1)) + 
  ggtitle("") 

wt_plot

# Panel D - Timeline of key developments in the sector & government ----

# We can discuss what all we'd like to include during our call next week, but here are a few ideas to seed the figure
# https://www.dropbox.com/s/fzy0s7eg62h2r8a/policy_timeline.xlsx?dl=0


g.gantt <- gather(policy_tl, "state", "date", 2:3) %>% 
  mutate(date = as.Date(date, "%d/%m/%Y"))

range <-  c(as.Date("2001-01-01"), as.Date("2019-01-01"))

tl_plot <- ggplot(g.gantt, aes(date, label, group=label)) +
  geom_line(arrow = arrow(length=unit(0.5,"cm"), ends="last", type = "open"), size = 2,color="#ed8f8a",alpha=0.85) +
  geom_text(data = g.gantt[1:5,],
            aes(label=label,family = "DM Sans"), 
            x = as.Date("2018-06-01"),size=3,color="grey20",
            hjust=1, vjust=-1 ) + 
  labs(x="", y=NULL, title="") +
  theme_classic() +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y",limits=range,expand=c(0,1)) +
  theme(text = element_text(family = "DM Sans",colour="#3A484F"),
        axis.line.y = element_blank(),
        panel.grid.major.x = element_line(colour="grey90", size=5),
        axis.text.x = element_text(size = 9, color = "grey30",angle = 45,hjust=1),
        plot.margin=unit(c(0.1,1.5,0.1,0.5),"cm"),
        axis.text.y = element_blank(),
        axis.ticks = element_blank())

tl_plot

# Modified policy TL

df <- policy_tl[with(policy_tl, order(year)), ]

type_levels <- c("Indonesian government", "Companies","International governments")
type_colors <- c("#0070C0", "#00B050", "#DE8600")

df$type <- factor(df$type, levels=type_levels, ordered=TRUE)

#positions <- c(0.5, -0.5, 1.0, -1.0, 1.5, -1.5)
#directions <- c(1, -1)

positions <- c(0.5)
directions <- c(1,-1)

line_pos <- data.frame(
  "year"=unique(df$year),
  "position"=rep(positions, length.out=length(unique(df$year))),
  "direction"=rep(directions, length.out=length(unique(df$year)))
)

df <- merge(x=df, y=line_pos, by="year", all = TRUE)
df <- df[with(df, order(year, type)), ]

text_offset <- 0.1
df$year_count <- ave(df$year==df$year, df$year, FUN=cumsum)
df$text_position <- (df$year_count * text_offset * df$direction) + df$position
head(df)


year_date_range <- seq(min(df$year_col) , max(df$year_col), by='year')
year_date_range <- as.Date(
  intersect(
    ceiling_date(year_date_range, unit="year"),
    floor_date(year_date_range, unit="year")
  ),  origin = "1970-01-01"
)
year_format <- format(year_date_range, '%Y')
year_df <- data.frame(year_date_range, year_format)

#### PLOT ####

tl_df <- subset(df,!is.na(event)) %>%
  as_tibble()

range <-  c(as.Date("2001-01-01"), as.Date("2022-01-01"))

tl_plot <- ggplot(tl_df,aes(x=year_col,y=0, col=type, label=type)) + 
  scale_color_manual(values=type_colors, labels=type_levels, drop = FALSE,guide = "legend",name="") + 
  theme_classic() + 
  scale_x_continuous(breaks = seq(from = 2001, to = 2022, by =1)) +
  #scale_x_date(date_breaks = "1 year", date_labels = "%Y",limits= range) +
  geom_hline(yintercept=0,color = "black", size=0.3) + # Plot horizontal black line for timeline
  geom_segment(data=tl_df[tl_df$year_count == 1,], aes(y=0.8,yend=0,xend=year_col), 
               color='grey70', alpha=0.5,size=0.85,linetype='dotted') +
  geom_point(aes(y=0), size=2,color="black") + # scatter points 
  geom_point(aes(y=text_position), size=2,alpha=0.75) + # scatter points 
  geom_text(aes(y=text_position + 0.03,x=year_col-35,label=stringr::str_wrap(event,15)),size=2.75,hjust =1, family= "DM Sans",
            fontface = "bold",show.legend = FALSE) +
  geom_text(data=year_df, aes(x=year_date_range,y=-0.03,label=year_format, fontface="bold"),size=2.75, color='black', family = "DM Sans") +
  theme(text = element_text(family = "DM Sans"),
        #panel.grid.major.x = element_line(colour="grey95", size=6),
        axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x =element_blank(),
        axis.ticks.x =element_blank(),
        axis.line.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom") 


tl_plot

# merge plot using patchwork
comb_plot <- defor_plot + wt_plot + tl_plot + plot_layout(ncol=1)
comb_plot

#ggsave(comb_plot,file="D:/comb_plot.png", dpi=400, w=11, h=14,type="cairo-png") 
ggsave(comb_plot,file=paste0(wdir,"\\01_data\\02_out\\plots\\001_figures\\fig_0X_summary_figure_updated.png"), dpi=400, w=11, h=14,type="cairo-png") 


# merge plot using patchwork
comb_plot <- defor_pp_plot + mc_pp_plot + wt_plot + tl_plot + plot_layout(ncol=1)
comb_plot

ggsave(comb_plot,file=paste0(wdir,"\\01_data\\02_out\\plots\\001_figures\\fig_0X_summary_figure.png"), dpi=400, w=9, h=13,type="cairo-png") 



# Quick stats for draft paper

# Many of these forests were cleared to make room for industrial acacia and eucalyptus plantations, 
# which expanded by XX hectares between 2000 and 2015

cum_pulp[16,2] - cum_pulp[1,2]
