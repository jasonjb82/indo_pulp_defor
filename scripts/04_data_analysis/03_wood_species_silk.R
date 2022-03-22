## ---------------------------------------------------------
##
## Purpose of script:
##
## Author: Jason Benedict
##
## Date Created: 2021-04-08
## 
## ---------------------------------------------------------
##
## Notes:
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
library(myutil)
library(sf)
library(scales)
library(aws.s3)
library(testthat)
library(concordance)
library(rcartocolor)
library(d3.format)
library(khroma) # palettes for color blindness

## credentials ----------------------------------------------

aws.signature::use_credentials()
bucket <- "trase-storage"
Sys.setenv("AWS_DEFAULT_REGION" = "eu-west-1")

## set working directory -------------------------------------

wdir <- "remote"

## read data -------------------------------------------------

# silk data
silk_data <- read_csv(paste0(wdir,"\\01_data\\01_in\\silk\\WOOD_EXPORTS_SILK_MERGED.csv"))

# wood species lookup table
wood_species <- read_csv(paste0(wdir,"\\01_data\\01_in\\silk\\SILK_PULP_WOOD_SPECIES.csv"))

# mills
mills <- s3read_using(read_excel, object = "indonesia/wood_pulp/logistics/out/mills/MILLS_EXPORTERS_20200405.xlsx", bucket = bucket)

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
                    legend.direction="horizontal",
                    plot.margin=unit(c(0.5,1.5,0.5,0.5),"cm"))

options(crayon.enabled = FALSE)

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

p1 <-   ggplot(data=silk_pulp_species) +
  geom_bar(stat="identity",position="stack",aes(x=YEAR,y=PROP_BERAT_BERSIH_KG/1000,fill=as.factor(SPECIES_GENERAL))) +
  scale_x_continuous(breaks = seq(from = 2012, to = 2019, by =1)) +
  xlab("") +
  scale_y_continuous(name="Pulp (Tons)\n",
                     limits=c(0,6000000),
                     breaks=seq(0,6000000, by=1000000),
                     labels= scales::comma,expand = c(0,0)) + 
  theme_plot +
  labs(fill = "\n") +
  scale_fill_bright() +
  guides(fill = guide_legend(title.position = "top")) + 
  #facet_wrap(~MILL_NAME,nrow=2) +
  ggtitle("") 

p1

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
  #select(SCIENTIFIC_NAMES) %>%
  mutate(SCIENTIFIC_NAMES = str_trim(SCIENTIFIC_NAMES,side="both")) %>%
  separate(SCIENTIFIC_NAMES,c("TYPE1","TYPE2","TYPE3","TYPE4","TYPE5","TYPE6"),";") %>%
  mutate_at(.vars=vars_to_process,funs(f)) %>%
  #mutate(SPECIES_EXPORT = pmap_chr(select(., TYPE1,TYPE2,TYPE3,TYPE4,TYPE5,TYPE6), ~toString(unique(na.omit(c(...)))))) %>%
  mutate(SPECIES_EXPORT=pmap_chr(list(TYPE1,TYPE2,TYPE3,TYPE4,TYPE5,TYPE6), ~paste(sort(c(...)), collapse = ","))) %>%
  separate_rows(SPECIES_EXPORT, sep = ",") %>%
  group_by(NPWP,NAMA_EKSPORTIR,PROPINSI,KABUPATEN_KOTA,NO_ETPIK,NAMA_IMPORTIR,NEGARA_IMPORTIR,PELABUHAN_MUAT,PELABUHAN_BONGKAR,
           NEGARA_TUJUAN,NO_INVOICE,SKEMA_KERJASAMA,NO_V_LEGAL,TRANSPORTASI,TGL_INVOICE,KETERANGAN,PEJABAT_TTD,TEMPAT_TTD,
           DIGITAL_SIGN,LOKASI_STUFFING,NO,HS_NUMBER,HS_PRINTED,DESKRIPSI,NUMBER_OF_UNIT,HARVEST_COUNTRY,ID,YEAR,CURRENCY,
           VOLUME_M3,BERAT_BERSIH_KG,VALUE,MIXED) %>%
  summarise(SPECIES_EXPORT = paste(unique(SPECIES_EXPORT), collapse = ","))

# create species shipments by year
species_shipments_yr <- silk_species_shipments %>%
  #mutate(SPECIES_EXPORT = ifelse(SPECIES_EXPORT == "ACACIA, OTHERS" | SPECIES_EXPORT == "OTHERS, ACACIA", "ACACIA, OTHERS",
  #              ifelse(SPECIES_EXPORT == "ACACIA, EUCALYPTUS, OTHERS" | SPECIES_EXPORT == "ACACIA, OTHERS, EUCALYPTUS", "ACACIA, EUCALYPTUS, OTHERS",
  #              ifelse(SPECIES_EXPORT == "ACACIA, EUCALYPTUS" | SPECIES_EXPORT == "EUCALYPTUS, ACACIA", "ACACIA, EUCALYPTUS",
  #              ifelse(SPECIES_EXPORT == "EUCALTYPUS, OTHERS" | SPECIES_EXPORT == "OTHERS, EUCALYPTUS","EUCALYPTUS,OTHERS",SPECIES_EXPORT))))) %>%
  select(YEAR,BERAT_BERSIH_KG,ID,SPECIES_EXPORT,MIXED) %>%
  group_by(YEAR,ID,SPECIES_EXPORT,MIXED) %>%
  summarize(TONS = sum(BERAT_BERSIH_KG/1000)) %>%
  group_by(YEAR,ID,MIXED) %>%
  group_by(YEAR,SPECIES_EXPORT) %>%
  summarize(NO_SHIPMENTS=n(),TONS=sum(TONS)) %>%
  group_by(YEAR) %>%
  mutate(PERC_SHIPMENT = prop.table(NO_SHIPMENTS)*100, PERC_VOLUME = prop.table(TONS)*100)

# check yearly shipments
yearly_shipments_total <- species_shipments_yr %>%
  group_by(YEAR) %>%
  summarize(TONS = sum(TONS))

p2 <- ggplot(data=species_shipments_yr) +
  geom_bar(stat="identity",position="stack",aes(x=YEAR,y=TONS,fill=as.factor(SPECIES_EXPORT))) +
  scale_x_continuous(breaks = seq(from = 2012, to = 2019, by =1)) +
  xlab("") +
  scale_y_continuous(name="Pulp (Tons)\n",
                     limits=c(0,6000000),
                     breaks=seq(0,6000000, by=1000000),
                     labels = d3_format(".3~s"),
                     expand = c(0,0)) + 
  theme_plot +
  labs(fill = "\n") +
  scale_fill_carto_d(palette="Safe") +
  guides(fill = guide_legend(title.position = "top",nrow=5)) + 
  #facet_wrap(~MILL_NAME,nrow=2) +
  ggtitle("") 

p2


ggsave(p2,file=paste0(wdir,"\\01_data\\02_out\\plots\\silk_exports_woodtypes.png"), dpi=400, w=8, h=10,type="cairo-png") 

# create species shipments by downstream group and year

species_shipments_grp_yr <- silk_species_shipments %>%
  select(YEAR,NPWP,NAMA_EKSPORTIR,BERAT_BERSIH_KG,ID,SPECIES_EXPORT,MIXED) %>%
  group_by(YEAR,NPWP,NAMA_EKSPORTIR,ID,SPECIES_EXPORT,MIXED) %>%
  summarize(TONS = sum(BERAT_BERSIH_KG/1000)) %>%
  group_by(YEAR,NPWP,NAMA_EKSPORTIR,ID,MIXED) %>%
  group_by(YEAR,NPWP,NAMA_EKSPORTIR,SPECIES_EXPORT) %>%
  summarize(NO_SHIPMENTS=n(),TONS=sum(TONS)) %>%
  group_by(YEAR) %>%
  mutate(PERC_SHIPMENT = prop.table(NO_SHIPMENTS)*100, PERC_VOLUME = prop.table(TONS)*100) %>%
  left_join(select(mills,MILL_GROUP,EXPORTER_ID),by=c("NPWP"="EXPORTER_ID"))

# check yearly shipments
yearly_shipments_total <- species_shipments_grp_yr %>%
  group_by(YEAR) %>%
  summarize(TONS = sum(TONS))

mill_order <- c("SINAR MAS","ROYAL GOLDEN EAGLE / TANOTO","MARUBENI")

species_shipments_grp_yr <- arrange(transform(species_shipments_grp_yr,MILL_GROUP=factor(MILL_GROUP,levels=mill_order)),MILL_GROUP) 

p3 <- ggplot(data=species_shipments_grp_yr) +
  geom_bar(stat="identity",position="stack",aes(x=YEAR,y=TONS,fill=as.factor(SPECIES_EXPORT))) +
  scale_x_continuous(breaks = seq(from = 2012, to = 2019, by =1)) +
  xlab("") +
  scale_y_continuous(name="Pulp (Tons)\n",
                     limits=c(0,4000000),
                     breaks=seq(0,4000000, by=1000000),
                     labels = d3_format(".3~s"),
                     expand = c(0,0)) + 
  theme_plot +
  labs(fill = "\n") +
  scale_fill_carto_d(palette="Safe") +
  guides(fill = guide_legend(title.position = "top",nrow=3)) + 
  facet_wrap(~MILL_GROUP,nrow=1, scales = "free") +
  ggtitle("") 

p3


ggsave(p3,file=paste0(wdir,"\\01_data\\02_out\\plots\\mth_exports\\silk_exports_group_woodtypes.png"), dpi=400, w=16, h=8,type="cairo-png") 
