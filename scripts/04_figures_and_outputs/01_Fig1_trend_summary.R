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
##        1) Pulp driven deforestation (TreeMap)
##        2) Pulp production and share of MTH vs plantation wood sources (RPBBI, KLHK)
##        3) Pulp mill capacities (RPBBI, company sustainability reports)
##        4) Wood pulp prices (WDI)
##        5) Policy events (multiple sources)
##
##
## ---------------------------------------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------------------------------------

### Load packages
library(stringr)
library(data.table)
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
library(tidyfast)
library(patchwork)
library(rcartocolor)
library(showtext)
library(svglite)
library(khroma) # palettes for color blindness

font_add_google(name = "DM Sans", family = "DM Sans")
showtext_auto()
showtext_opts(dpi = 400)

## set working directory -------------------------------------

wdir <- "remote"
data_dir <- "/01_data/"

## read data -------------------------------------------------

## load color palette
colorBlind8  <- c("#999999", "#E69F00", "#56B4E9", "#009E73", 
                  "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# policy timeline (updated)
policy_tl <- read_csv(paste0(wdir,data_dir,"/01_in/tables/policy_timeline_cats_rev1.csv")) %>%
  mutate(year_col = as.Date(year_proper,format="%d/%m/%Y"))

# pulp conversion from forest (Indonesia wide) - TreeMap
pulp_for_id <- read_csv(paste0(wdir,data_dir,"/02_out/gee/gaveau/pulp_annual_defor_forest_id.csv")) %>%
  select(-`system:index`,-constant,-.geo)

# pulp conversion from non-forest (Indonesia wide) - TreeMap
pulp_nonfor_id <- read_csv(paste0(wdir,data_dir,"/02_out/gee/gaveau/pulp_annual_defor_non-forest_id.csv")) %>%
  select(-`system:index`,-constant,-.geo)

# timber for pulp production (Obidzinski Dermawan)
timber_for_pulp <- read_csv(paste0(wdir,data_dir,"/01_in/obidzinski_dermawan/plot_data.csv"))

# pulp prices (FRED)
pulp_prices <- read_csv(paste0(wdir,data_dir,"/01_in/tables/WPU0911_FRED.csv"))

# pulp production data (MoEF)
pulp_production <- read_excel(paste0(wdir,data_dir,"/01_in/tables/annual_pulp_shr_prod.xlsx"))

# kabupaten
kab <- read_sf(paste0(wdir,data_dir,"/01_in/big/idn_kabupaten_big.shp"))

# get table of islands
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

############################################################################
# Clean / prep data --------------------------------------------------------
############################################################################

id_pulp_conv_for <- pulp_for_id %>%
  left_join(islands,by="prov_code") %>%
  select(-prov,-kab,-kab_code,-prov_code,-type) %>%
  dt_pivot_longer(cols = -c(island),
                  names_to = 'year',
                  values_to = 'area_ha') %>%
  as_tibble() %>%
  filter(area_ha != "0") %>%
  mutate(year = str_replace(year,"deforestation_", ""),year = as.double(year)) %>%
  group_by(island,year) %>%
  summarize(area_ha = sum(area_ha)) %>%
  mutate(conv_type = "forest") 

id_pulp_conv_nonfor <- pulp_nonfor_id %>%
  left_join(islands,by="prov_code") %>%
  select(-prov,-kab,-kab_code,-prov_code,-type) %>%
  dt_pivot_longer(cols = -c(island),
                  names_to = 'year',
                  values_to = 'area_ha') %>%
  as_tibble() %>%
  filter(area_ha != "0") %>%
  mutate(year = str_replace(year,"deforestation_", ""),year = as.double(year)) %>%
  group_by(island,year) %>%
  summarize(area_ha = sum(area_ha)) %>%
  mutate(conv_type = "non-forest") 


pulp_prices_clean <- pulp_prices %>%
  select(DATE,PPI=WPU0911) %>%
  mutate(DATE = as.Date(DATE,format="%m/%d/%Y")) %>%
  #filter(between(DATE, as.Date("2000-01-01"),as.Date("2022-12-31"))) %>%
  mutate(year = year(DATE),PPI = as.double(PPI)) %>%
  select(year,PPI)


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

# merge deforestation df's and pulp prices
defor_price_comb <- id_pulp_conv_for %>%
  bind_rows(id_pulp_conv_nonfor) %>%
  left_join(pulp_prices_clean,by="year") %>%
  filter(year < 2023 & conv_type == "forest") %>%
  group_by(year,island) %>%
  summarize(area_ha = sum(area_ha),PPI=max(PPI))

# scale factor to match axis'
pa_scale_factor <- 0.5

defor_pp_plot <- ggplot(data = defor_price_comb, aes(x = year))+
  geom_bar(stat="identity",position = "stack",aes(y = area_ha/1000,
                                                  fill=factor(island,levels=rev(island_order)))) +
  geom_line(aes(y = PPI*pa_scale_factor,color="Producer Price Index")) +
  geom_point(aes(y = PPI*pa_scale_factor,color="Producer Price Index")) +
  ylab("Pulp-drive deforestation (Kha)\n") +
  xlab("") +
  scale_fill_manual(values=c(colorBlind8[7],colorBlind8[3],colorBlind8[5]),
                    breaks=island_order,labels=island_order)+ 
  scale_color_manual(values=c("black"))+ 
  scale_x_continuous(breaks = seq(from = 2001, to = 2022, by =1),expand=c(0,1)) +
  scale_y_continuous(sec.axis = sec_axis(~ .*1, labels = number_format(scale=1/pa_scale_factor),
                                         name="Producer Price Index\n"), 
                     limits = c(0,150),
                     expand = c(0,0)) +
  guides(fill = guide_legend(nrow = 1,reverse = FALSE),color = guide_legend(nrow = 1,reverse = TRUE),keyheight = 10) +
  theme_plot 


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
                     expand = c(0,0)) + 
  theme_plot +
  labs(fill = "\n") +
  scale_fill_manual(values=c(colorBlind8[4],colorBlind8[2]))+ 
  guides(fill = guide_legend(title.position = "top",nrow=1)) + 
  ggtitle("") 


# Panel C - Timeline of key developments in the sector & government ----

df <- policy_tl[with(policy_tl, order(year)), ]

type_levels <- c("Indonesian government", "Companies","International governments")

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
  theme(text = element_text(family = "DM Sans"),
        panel.grid.major.x = element_line(colour="grey95", size=6),
        axis.line.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x =element_text(vjust=5,color = "grey30",angle = 0, face="bold"),
        axis.ticks.x =element_blank(),
        axis.line.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom") 



# merge plot using patchwork
comb_plot <- defor_pp_plot / wt_plot / tl_plot
comb_plot <- comb_plot +
  plot_annotation(tag_levels="A") & 
  theme(plot.tag = element_text(face = 'bold', size=12))


## save to image format
ggsave(comb_plot,file=paste0(wdir,"/01_data/04_results/figures/f1_summary_figure.png"), dpi=400, w=12, h=15) 
ggsave(comb_plot,file=paste0(wdir,"/01_data/04_results/figures/f1_summary_figure.svg"), dpi=400, w=12, h=15) 
