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
library(svglite)
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
source("scripts\\001_misc\\001_color_palettes.R")
colorBlind8  <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## data lookup table
lu_table <- read_csv(paste0(wdir,"\\01_data\\02_out\\gee\\data_lookup_table.csv"))

## hti license dates
lic_dates_hti <- readr::read_csv(paste0(wdir,"\\01_data\\01_in\\wwi\\HTI_LICENSE_DATES.csv"),col_types = cols(license_date = col_date("%m/%d/%Y")))

## supplier groups
groups <- read_csv(paste0(wdir,"\\01_data\\01_in\\wwi\\ALIGNED_NAMES_GROUP_HTI.csv"))

# hti concessions
hti <- read_sf(paste0(wdir,"\\01_data\\01_in\\klhk\\IUPHHK_HT_proj.shp"))

# wood supply
ws <- read_delim(get_object(object="indonesia/wood_pulp/production/out/PULP_WOOD_SUPPLY_CLEAN_ALL_ALIGNED_2015_2022.csv", bucket), delim = ",")

# wood species lookup table
wood_species <- read_csv(paste0(wdir,"\\01_data\\01_in\\silk\\SILK_PULP_WOOD_SPECIES.csv"))

# policy timeline (updated)
policy_tl <- read_csv(paste0(wdir,"\\01_data\\01_in\\tables\\policy_timeline_cats_rev1.csv")) %>%
  mutate(year_col = as.Date(year_proper,format="%d/%m/%Y"))

# deforestation within concessions
hti_nonhti_conv <- read_csv(paste0(wdir,"\\01_data\\02_out\\tables\\idn_deforestation_hti_nonhti_treemap.csv"))

# timber for pulp production (Obidzinski Dermawan)
timber_for_pulp <- read_csv(paste0(wdir,"\\01_data\\01_in\\obidzinski_dermawan\\plot_data.csv"))

# pulp exports (WITS)
pulp_exports <- read_csv(paste0(wdir,"\\01_data\\01_in\\tables\\pulp_exports_wits.csv"))

# pulp production data (MoEF)
pulp_production <- read_excel(paste0(wdir,"\\01_data\\01_in\\tables\\annual_pulp_shr_prod.xlsx"))

# kabupaten
kab <- read_sf(paste0(wdir,"\\01_data\\01_in\\big\\idn_kabupaten_big.shp"))
prov_slim <- kab %>% select(prov,prov_code) %>% st_drop_geometry() %>% distinct() %>%
  mutate(prov_code = ifelse(prov == "PAPUA",92,prov_code))

# mills
mills <- s3read_using(read_excel, object = "indonesia/wood_pulp/logistics/out/mills/MILLS_EXPORTERS_20200405.xlsx", bucket = bucket)

############################################################################
# Clean / prep data --------------------------------------------------------
############################################################################


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
                    #panel.spacing = unit(2, "lines"),
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

island_order <- c(
  "Sumatera",
  "Kalimantan",
  "Papua")

defor_plot <- hti_nonhti_conv %>%
  filter(conv_type == 2) %>% 
  ggplot() +
  aes(y = area_ha, x = year, fill=factor(island,levels=rev(island_order)),color=factor(island,levels=rev(island_order))) +
  geom_col() +
  xlab("\nYear") +
  ylab("Pulp-driven deforestation (ha)") + 
  scale_y_continuous(expand=c(0,0),labels = d3_format(".2~s",suffix = ""))+
  scale_x_continuous(expand=c(0,1),breaks=seq(2001,2023,by=1),labels = seq(2001,2023,by=1)) +
  scale_fill_manual(values=c(colorBlind8[7],colorBlind8[3],colorBlind8[5]),
                    breaks=island_order,labels=island_order)+ 
  scale_color_manual(values=c(colorBlind8[7],colorBlind8[3],colorBlind8[5]),
                     breaks=island_order,labels=island_order)+ 
  guides(fill = guide_legend(nrow = 1,reverse = FALSE),color = guide_legend(nrow = 1,reverse = FALSE),keyheight = 10) +
  #facet_wrap(~supplier_label,ncol=1,scales="free") +
  theme_plot

defor_plot

# Panel B - Wood supply transition -------------------------------------

# Stacked bar breaking pulpwood volumes into MTH / plantation sources (probably simplify categories from current figure). 

# O-D ratios
timber_for_pulp_od <- timber_for_pulp %>%
  pivot_wider(
    names_from = label,
    values_from = c(year_digitized,timber_m3)
  ) %>%
  mutate(timber_m3_mth = timber_m3_total - timber_m3_plantation) %>%
  select(year,timber_m3_plantation,timber_m3_mth) %>%
  pivot_longer(cols = c(-year),
                  names_to = 'woodtype',
                  values_to = 'annual_prod_mtpy') %>%
  mutate(woodtype = ifelse(woodtype == "timber_m3_plantation","Plantation","Mixed Tropical Hardwoods")) %>%
  group_by(year) %>%
  mutate(ratio = annual_prod_mtpy / sum(annual_prod_mtpy)) %>%
  ungroup() %>%
  select(year,woodtype,ratio)

# plantation_mth_ratio <- timber_plantation_silk %>%
#   bind_rows(timber_for_pulp_od) %>%
#   select(year,woodtype,ratio) %>%
#   filter(year > 2000) %>%
#   mutate(ratio = ifelse(is.nan(ratio),0,ratio)) %>%
#   arrange(year) 

# # merge ratio and WITS exports
# pulp_ratio <- pulp_exports %>%
#   group_by(year) %>%
#   summarize(vols_tonnes = sum(exports_kg)/1000) %>%
#   right_join(plantation_mth_ratio,by="year") %>%
#   mutate(vols_ton_ratio = vols_tonnes*ratio,
#          vols_ton_ratio = ifelse(is.na(vols_tonnes),0,vols_ton_ratio))

# # clean pulp ratio (temporary fix using O-D and SILK data ratios)
# pulp_ratio_clean <- pulp_ratio %>%
#   drop_na(vols_tonnes) %>%
#   select(year,woodtype,ratio) %>%
#   add_row(year = c(2020,2021,2022),woodtype=c("Plantation","Plantation","Plantation"),ratio=c(1,1,1)) %>%
#   group_by() %>% tidyr::complete(year = min(year):2022, woodtype) %>%
#   mutate(ratio = ifelse(is.na(ratio) & year > 2016,0,ratio)) %>%
#   group_by(woodtype) %>%
#   fill(ratio, .direction = c("down")) %>%
#  print()

# # modified pulp production
# pulp_prod_modified <- pulp_ratio_clean %>%
#   left_join(pulp_production,by="year") %>%
#   mutate(prod_woodtype = ratio*annual_prod_mtpy) %>%
#   select(year,woodtype,ratio,annual_prod_mtpy,prod_woodtype)

# pulp production with ratios
pulp_prod_modified <- pulp_production %>%
  select(year,annual_prod_mtpy,total_pulp_mth,total_pulp_plantation) %>%
  pivot_longer(cols = c(-year,-annual_prod_mtpy),
               names_to = 'woodtype',
               values_to = 'ratio') %>%
  mutate(prod_woodtype = ratio*annual_prod_mtpy,
         woodtype = ifelse(woodtype == "total_pulp_plantation","Plantation","Mixed Tropical Hardwoods"),
         ratio = ifelse(is.na(ratio),0,ratio))

# merge OD data and KLHK data
pulp_prod_ratio_merged <- timber_for_pulp_od %>%
  full_join(pulp_prod_modified,by=c("year","woodtype")) %>%
  filter(year > 2000) %>%
  mutate(ratio = ifelse(!is.na(ratio.x),ratio.x,ratio.y),
         annual_prod_mtpy = ratio*annual_prod_mtpy) %>%
  select(year,woodtype,annual_prod_mtpy,ratio) %>%
  print()


wt_plot <- ggplot(pulp_prod_ratio_merged) +
  geom_bar(stat="identity",position="stack",aes(x=year,y=annual_prod_mtpy,fill=as.factor(woodtype))) +
  scale_x_continuous(breaks = seq(from = 2001, to = 2023, by =1)) +
  xlab("") +
  scale_y_continuous(name="Pulp production (Million tonnes)\n",
                     limits=c(0,10),
                     breaks=seq(0,19, by=1),
                     #labels = d3_format(".3~s"),
                     expand = c(0,0)) + 
  theme_plot +
  labs(fill = "\n") +
  scale_fill_manual(values=c(colorBlind8[4],colorBlind8[2]))+ 
  guides(fill = guide_legend(title.position = "top",nrow=1)) + 
  ggtitle("") 

wt_plot

# Panel C - Timeline of key developments in the sector & government ----

# We can discuss what all we'd like to include during our call next week, but here are a few ideas to seed the figure
# https://www.dropbox.com/s/fzy0s7eg62h2r8a/policy_timeline.xlsx?dl=0

df <- policy_tl[with(policy_tl, order(year)), ]

type_levels <- c("Indonesian government", "Companies","International governments")
#type_colors <- c("#0070C0", "#00B050", "#DE8600")
#type_fill <- c("#0070C0", "#00B050", "#DE8600")

type_colors <- c(colorBlind8[4],colorBlind8[6],colorBlind8[8])
type_fill <- c(colorBlind8[4],colorBlind8[6],colorBlind8[8])
type_shape <- c(16)

df$type <- factor(df$type, levels=type_levels, ordered=TRUE)

positions <- c(0.5)
#directions <- c(1,-1)
directions <- unique(df$direction)

line_pos <- data.frame(
  "year"=unique(df$year),
  "position"=rep(positions, length.out=length(unique(df$year))),
  "direction"=rep(directions, length.out=length(unique(df$year)))
)

df <- merge(x=df, y=line_pos, by="year", all = TRUE)
df <- df[with(df, order(year, type)), ]

text_offset <- 0.1
df$year_count <- ave(df$year==df$year, df$year, FUN=cumsum)
df$text_position <- df$type_cat
head(df)

#### PLOT ####

tl_df <- df %>%
  mutate(direction = as.factor(direction.x),
         text_position_mod = case_when(
           event == "Omnibus Law for Job Creation" ~ 0.1,
           event == "PT Phoenix mill proposed" ~ 1.6,
           event == "REDD+ agreement with Norway" ~ 3.5,
           event == "Indonesia withdraws from Norway REDD+" ~ 3.5,
           event == "Norway REDD+ restart" ~ 3.5,
           TRUE ~ text_position
         ),
         text_position = ifelse(row_cat == 32,3.5,text_position),
         text_position = ifelse(row_cat == 11,1,text_position),
         text_position = ifelse(row_cat == 21,1.5,text_position),
         text_position = ifelse(row_cat == 22,2,text_position),
         text_position = ifelse(row_cat == 23,2.5,text_position)
  )

tl_plot <- ggplot(tl_df,aes(x=year,y=0, col=type, label=type,shape=direction)) + 
  geom_segment(data=subset(tl_df,row_cat==11), aes(y=text_position,yend=1,x=min(year),xend=max(year),group=1), 
               alpha=1,linewidth=1.75,linetype='solid',color=c(colorBlind8[4])) +
  geom_segment(data=subset(tl_df,row_cat==21), aes(y=text_position,yend=1.5,x=min(year),xend=max(year),group=1), 
               alpha=1,linewidth=1.75,linetype='solid',color=c(colorBlind8[6])) +
  geom_segment(data=subset(tl_df,row_cat==22), aes(y=text_position,yend=2,x=min(year),xend=max(year),group=1), 
               alpha=1,linewidth=1.75,linetype='solid',color=c(colorBlind8[6])) +
  geom_segment(data=subset(tl_df,row_cat==23), aes(y=text_position,yend=2.5,x=min(year),xend=max(year),group=1), 
               alpha=1,linewidth=1.75,linetype='solid',color=c(colorBlind8[6])) +
  geom_segment(data=subset(tl_df,row_cat==31), aes(y=text_position,yend=3,x=min(year),xend=max(year),group=1), 
               alpha=1,linewidth=1.75,linetype='solid',color=c(colorBlind8[8])) +
  geom_segment(data=subset(tl_df,row_cat==32), aes(y=text_position,yend=3.5,x=min(year),xend=max(year),group=1), 
               alpha=1,linewidth=1.75,linetype='solid',color=c(colorBlind8[8])
  ) +
  ylab("\n")+
  scale_color_manual(values=type_colors, labels=type_levels, drop = FALSE,guide = guide_legend(reverse = TRUE),name="",na.translate=FALSE) + 
  scale_fill_manual(values=type_fill, labels=type_levels, drop = FALSE,guide = "legend",name="",na.translate=FALSE) + 
  scale_shape_manual(values=type_shape, labels=type_levels, drop = TRUE,guide = FALSE,name="",na.translate=FALSE,) +
  theme_classic() + 
  scale_x_continuous(expand=c(0,0.5),breaks=seq(2001,2023,by=1)) +
  scale_y_discrete(expand=c(0,0.2))+
  geom_point(aes(y=text_position), size=4.5,alpha=0.75) + # scatter points 
  geom_point(data=tl_df[tl_df$direction.x == 0,],aes(y=text_position), size=4.5,alpha=1) + # scatter points 
  ggrepel::geom_text_repel(aes(y=text_position_mod+0.05,x=year,label=stringr::str_wrap(event,25)),size=2.75,hjust =0,vjust=-1.25, family= "DM Sans",
                           fontface = "bold",show.legend = FALSE,min.segment.length = 2.5) +
  #geom_text(data=year_df, aes(x=as.double(year_format),y=-0.03,label=year_format, fontface="bold"),size=2.75, color='black', family = "DM Sans") +
  theme(text = element_text(family = "DM Sans"),
        panel.grid.major.x = element_line(colour="grey95", size=6),
        axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        #axis.title.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x =element_text(vjust=5,color = "grey30",angle = 0, face="bold"),
        axis.ticks.x =element_blank(),
        axis.line.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom") 


tl_plot

# merge plot using patchwork
comb_plot <- defor_plot / wt_plot / tl_plot
comb_plot <- comb_plot +
  plot_annotation(tag_levels="A") & 
  theme(plot.tag = element_text(face = 'bold', size=12))
comb_plot

##ggsave(comb_plot,file="D:/comb_plot.png", dpi=400, w=11, h=14,type="cairo-png") 
ggsave(comb_plot,file=paste0(wdir,"\\01_data\\02_out\\plots\\001_figures\\fig_0X_summary_figure_updated.png"), dpi=400, w=12, h=15,type="cairo-png") 
ggsave(comb_plot,file=paste0(wdir,"\\01_data\\02_out\\plots\\001_figures\\fig_0X_summary_figure_rev1.svg"), dpi=400, w=12, h=15) 
