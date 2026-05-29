## ---------------------------------------------------------
## 
## Project: Indonesia pulp deforestation
##
## Purpose of script: Calculate statistics for paper
##
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Date Created: 2023-09-01
## 
## ---------------------------------------------------------
##
## Notes: 
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
library(khroma) # palettes for color blindness
library(patchwork)

'%ni%' <- Negate('%in%') # filter out function

## set working directory -------------------------------------

wdir <- "remote"
data_dir <- "/01_data/"

## read data -------------------------------------------------

# choose projection: Cylindrical Equal Area
indonesian_crs <- "+proj=cea +lon_0=115.0 +lat_ts=0 +x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"

# kabupaten
kab <- read_sf(paste0(wdir,data_dir,"/01_in/big/idn_kabupaten_big.shp"))

# hti concessions
hti <- read_sf(paste0(wdir,data_dir,"01_in/klhk/IUPHHK_HTI_TRASE_20230314_proj.shp"))

# wood supply (2015-2022)
ws_2015_2022 <- read_csv(paste0(wdir,data_dir,"/02_out/tables/ws_merge_clean_2015_2022.csv"))

# add islands
islands <- kab %>%
  st_drop_geometry() %>%
  mutate(island = str_sub(prov_code, 1, 1)) %>%
  mutate(
    island = case_when(
      island == 1 ~ "SUMATRA", island == 2 ~ "RIAU ARCHIPELAGO",
      island == 3 ~ "JAVA", island == 5 ~ "BALI AND NUSA TENGGARA",
      island == 6 ~ "KALIMANTAN", island == 7 ~ "SULAWESI",
      island == 8 ~ "MALUKU", island == 9 ~ "PAPUA"
    )
  ) %>%
  distinct(prov_code, island)

# pulpwood conversion from forest and non-forest within and outside hti concessions
hti_nonhti_conv <- read_csv(paste0(wdir,data_dir,"/02_out/tables/idn_pulp_conversion_hti_nonhti_treemap.csv"))

# treemap annual expansion stats
id_annual_exp_stats <- read_csv(paste0(wdir,data_dir,"/02_out/tables/id_annual_expansion_stats_ttm.csv"))

# kalimantan annual pulp forest expansion stats
kali_annual_pulp_exp_stats <- read_csv(paste0(wdir,data_dir,"/02_out/tables/kali_annual_pulp_exp_stats_ttm.csv"))

# pulpwood areas (Indonesia and within HTI)
pw_area_hti <- read_csv(paste0(wdir,data_dir,"/02_out/gee/pulp_annual_area_hti_only.csv")) 

pw_annual_area_id <- read_csv(paste0(wdir,data_dir,"/02_out/gee/pulp_annual_area_id.csv")) 

pw_2000 <- pw_annual_area_id %>%
  select(pulp_2000) %>%
  group_by() %>%
  summarize(area_ha = sum(pulp_2000))

# reclasses ownership groups
groups_reclass_hti <- read_csv(paste0(wdir,data_dir,"/01_in/tables/ALIGNED_NAMES_GROUP_HTI_reclassed.csv"))

# hti pulp conversion with timing information
zdc_hti_conv <- read_csv(paste0(wdir,data_dir,"02_out/tables/hti_grps_deforestation_timing.csv"))

# Gaveau annual pulp areas (within HTI)
gaveau_annual_pulp <- read_csv(paste0(wdir,data_dir,"/02_out/tables/gaveau_annual_pulp_areas.csv"))

# Expansion on soil type (Gaveau)
pulp_ttm_soil_type <- read_csv(paste0(wdir,data_dir,"/02_out/gee/gaveau/idn_pulp_annual_expansion_peat_mineral_soils.csv"))

# HTI concession names
hti_concession_names <- hti %>%
  st_drop_geometry() %>%
  select(supplier_id=ID,supplier=namaobj) %>%
  mutate(supplier_label = paste0(supplier," (",supplier_id,")"))

## GFC deforestation (modified by TreeMap)
samples_gfc_ttm <- read_csv(paste0(wdir,data_dir,"/02_out/tables/samples_gfc_ttm.csv"))

# Parameters from MAI analysis
mai_df <- read_csv(paste0(wdir,data_dir,"/04_results/key_parameters.csv"))

# mill capacities
cap_df <- read_excel(paste0(wdir,data_dir,"/01_in/wwi/MILLS_EXPORTERS_20200405.xlsx"))

# RS accuracy assessment stats
rs_acc_df <- read_csv(paste0(wdir,data_dir,"/04_results/rs_accuracy_paper_stats.csv"))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Overarching trends in pulp expansion, deforestation, peat conversion -------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

## Line 24:
# Estimated area of pulp expansion 2001-2011
pulp_defor_row <- rs_acc_df %>% filter(stat_name == "defor_2001_2011")
forest_loss_pulp_ha <- pulp_defor_row %>%
  pull(estimated_area_kha) * 1000

pulp_def_share_2001_2011 <- id_annual_exp_stats %>%
  filter(year < 2012) %>%
  group_by() %>%
  summarize(forest_loss_ha = sum(forest_loss_ha)) %>%
  mutate(shr_pulp_forest_loss = forest_loss_pulp_ha/forest_loss_ha*100)

# # Share of palm deforestation over total annual deforestation (2001-2011)
# palm_def_share_2001_2011 <- id_annual_exp_stats %>%
#   filter(year < 2012) %>%
#   group_by() %>%
#   summarize(forest_loss_palm_ha = sum(forest_loss_palm_ha),
#             forest_loss_ha = sum(forest_loss_ha)) %>%
#   mutate(shr_palm_forest_loss = forest_loss_palm_ha/forest_loss_ha*100) %>%
#   print()

cat(sprintf(paste0(
  "\nPaper sentence, line 24:\n",
  "Between 2001 and 2011, \033[1m%s\033[0m (95%% CI: \033[1m%s\033[0m–\033[1m%s\033[0m) hectares of rainforest were directly\n",
  "converted to pulpwood plantations (SI Section 1), representing \033[1m%.0f\033[0m%% of\n",
  "Indonesian primary forest loss.\n\n"),
  formatC(round(pulp_defor_row$estimated_area_kha * 1000, -3), format = "f", digits = 0, big.mark = ","),
  formatC(round(pulp_defor_row$ci95_lower_kha      * 1000, -3), format = "f", digits = 0, big.mark = ","),
  formatC(round(pulp_defor_row$ci95_upper_kha      * 1000, -3), format = "f", digits = 0, big.mark = ","),
  pulp_def_share_2001_2011$shr_pulp_forest_loss
))

## Line 38

# annual_conv <- pw_annual_area_id %>%
#   select(-`system:index`,-constant,-kab,-kab_code,-prov_code,-.geo,-type) %>%
#   pivot_longer(cols = -c(prov),
#                names_to = 'year',
#                values_to = 'area_ha') %>%
#   mutate(year = as.numeric(gsub("[^0-9]", "", year))) %>%
#   group_by(year) %>%
#   summarize(area_ha = sum(area_ha)) %>%
#   mutate(area_pulp_ha = area_ha - lag(area_ha, default = first(area_ha))) %>%
#   print()
# 
# annual_conv <- id_annual_pulp_stats %>%
#   mutate(area_ha = total_forest_loss_pulp_ha + total_nonforest_loss_pulp_ha)


# table of total pulp areas each year
annual_pulp_areas <- pw_annual_area_id %>%
  select(constant,starts_with("pulp_")) %>%
  pivot_longer(cols = -c(constant),
               names_to = 'year',values_to = 'area_ha') %>%
  mutate(year = as.double(str_replace(year,"pulp_",""))) %>%
  group_by(year) %>%
  summarize(area_ha = sum(area_ha)-5000) %>% # GEE calculations adjustment
  mutate(annual_pulp_area = area_ha - lag(area_ha, default = first(area_ha))) %>%
  left_join(id_annual_exp_stats, by="year") %>%
  select(year,annual_pulp_expansion_area_ha=annual_pulp_area,forest_loss_ha,forest_loss_pulp_ha,nonforest_loss_pulp_ha,annual_pulp_area_ha=area_ha)

annual_conv <- annual_pulp_areas %>%
  group_by(year) %>%
  summarize(area_ha = sum(forest_loss_pulp_ha)) 
 
# annual_conv %>% 
#   ggplot(aes(x = year, y = area_ha)) +
#   geom_bar(stat = "identity") 

# Line 14 / 100: Over the following six years, pulp-driven deforestation declined by 95% 
conv_2011 = annual_conv %>% filter(year == 2011) %>% pull(area_ha)
conv_2017 = annual_conv %>% filter(year==2017) %>% pull(area_ha)
early_change <- (conv_2017 - conv_2011) / conv_2011
cat(sprintf(paste0(
  "\nPaper sentence, line 36 (also lines 14, 124, 204):\n",
  "we describe how these four elements interacted over a period of time (2011-2017)\n",
  "when pulp-driven deforestation fell by \033[1m%.0f\033[0m%%\n\n"),
  abs(early_change) * 100
))

# Line 16 / 101: Indonesia has since seen ... a 372% increase in pulp-driven deforestation... 
# Between 2017 and 2022, the annual rate of conversion of primary forests to pulp plantations increased 372%
conv_2022 = annual_conv %>% filter(year==2022) %>% pull(area_ha)
late_change <- (conv_2022 - conv_2017) / conv_2017

# Conversion of peat between 2017 and 2022
annual_pulp_conv <- pulp_ttm_soil_type %>%
  select(-`system:index`,-constant,-kab,-kab_code,-prov_code,-.geo,-type) %>%
  pivot_longer(cols = -c(prov),
               names_to = 'year',
               values_to = 'area_ha') %>%
  mutate(class = str_extract(year, "[^_]+"),
         year = as.numeric(gsub("[^0-9]", "", year))) %>%
  ungroup() %>%
  group_by(year,class) %>%
  summarize(area_ha = sum(area_ha), .groups = "keep")

pulp_conv_2017 = annual_pulp_conv %>% filter(class == "peat" & year==2017) %>% pull(area_ha)
pulp_conv_2022 = annual_pulp_conv %>% filter(class == "peat" & year==2022) %>% pull(area_ha)
overall_pulp_change <- (pulp_conv_2022 - pulp_conv_2017) / pulp_conv_2017
cat(sprintf(paste0(
  "\nPaper sentence, line 125:\n",
  "Between 2017 and 2022, the annual rate of conversion of primary forests to pulpwood\n",
  "plantations increased from \033[1m%s\033[0m ha/year to \033[1m%s\033[0m ha/year (\033[1m%.0f\033[0m%% increase), while\n",
  "pulp-driven conversion of peatlands increased from \033[1m%s\033[0m ha/year to \033[1m%s\033[0m ha/year\n",
  "(\033[1m%.0f\033[0m%% increase).\n\n"),
  formatC(round(conv_2017,       -2), format = "f", digits = 0, big.mark = ","),
  formatC(round(conv_2022,       -2), format = "f", digits = 0, big.mark = ","),
  late_change * 100,
  formatC(round(pulp_conv_2017,  -2), format = "f", digits = 0, big.mark = ","),
  formatC(round(pulp_conv_2022,  -2), format = "f", digits = 0, big.mark = ","),
  overall_pulp_change * 100
))


pulp_exp_row <- rs_acc_df %>% filter(stat_name == "pulp_expansion_2001_2011")
cat(sprintf(paste0(
  "\nPaper sentence, line 81:\n",
  "Many of these forests were cleared to make room for industrial acacia and eucalyptus\n",
  "plantations, which expanded by \033[1m%.2f\033[0m (\033[1m%.2f\033[0m–\033[1m%.2f\033[0m) million hectares between 2001 and 2011.\n\n"),
  pulp_exp_row$estimated_area_kha / 1e3,
  pulp_exp_row$ci95_lower_kha     / 1e3,
  pulp_exp_row$ci95_upper_kha     / 1e3
))


# Although deforestation rates in 2022 were still XX% lower than during the 2011 peak, major economic, ecological and policy changes call into question whether the sector will ever be able to achieve its desired end to deforestation 
overall_change <- (conv_2022 - conv_2011) / conv_2011
cat(sprintf(paste0(
  "\nPaper sentence, line 131:\n",
  "While pulp-driven deforestation rates in 2022 were still \033[1m%.0f\033[0m%% lower than the 2011 peak.\n\n"),
  abs(overall_change) * 100
))


# more of Indonesia’s forests were converted to new pulpwood plantations than to industrial oil palm plantations in 2022.
defor_2022 <- id_annual_exp_stats %>%
  filter(year == 2022) %>%
  summarize(pulp_ha = sum(forest_loss_pulp_ha), palm_ha = sum(forest_loss_palm_ha))
cat(sprintf(paste0(
  "\nValidation - paper claim (line ~130):\n",
  "more of Indonesia’s forests were converted to new\n",
  "pulpwood plantations than to industrial oil palm plantations in 2022:\n",
  "\033[1m%s\033[0m\n\n"),
  ifelse(defor_2022$pulp_ha > defor_2022$palm_ha, "TRUE", "FALSE")
))


# Line 85: pulp plantations now supply nearly all of Indonesia’s 47 million m3 of annual pulpwood demand (Figure 1). 
current_wood_demand <- ws_2015_2022 %>% filter(YEAR == 2022) %>% pull(VOLUME_M3) %>% sum()
cat(sprintf(paste0(
  "\nPaper sentence, line 85:\n",
  "As a result of this combination of pulpwood plantation expansion and intensification,\n",
  "plantations now supply nearly all of Indonesia's \033[1m%.0f\033[0m million m3 of annual pulpwood demand.\n\n"),
  current_wood_demand / 1e6
))

# The expansion of pulp processing infrastructure into Kalimantan is particularly important 
#since the region has been responsible for XX%  of pulp-driven deforestation since 2017
kali_pulp_driven_defor <- kali_annual_pulp_exp_stats %>%
  left_join(annual_conv,by="year") %>%
  filter(year >= 2017) %>%
  group_by() %>%
  summarize(shr_kali_pulp_defor = sum(forest_loss_ha)/sum(area_ha)*100)
cat(sprintf(paste0(
  "\nPaper sentence, line 138:\n",
  "The expansion of pulp processing infrastructure into Kalimantan is particularly\n",
  "important since the region has been responsible for \033[1m%.0f\033[0m%% of pulp-driven\n",
  "deforestation since 2017.\n\n"),
  kali_pulp_driven_defor$shr_kali_pulp_defor
))


# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# # Plantation yield changes -----------------------------------------------
# #%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# # Line 67: Many of these forests were cleared to make room for industrial acacia and eucalyptus plantations, which expanded by ~1.62 million hectares between 2000 and 2015 

# annual_pulp <- annual_pulp_areas %>%
#   group_by(year) %>%
#   summarize(area_ha = sum(annual_pulp_area_ha)) %>%
#   print()

# pulp_2000 = annual_pulp %>% filter(year == 2000) %>% pull(area_ha)
# pulp_2015 = annual_pulp %>% filter(year==2015) %>% pull(area_ha)
# pulp_change <- (pulp_2015 - pulp_2000) %>% 
#   print()



# # Line 109: we find little evidence that plantation yields have increased over the past XX years 

# # Line 151: We find that 3 million hectares of primary forests, XX% of which are on peat soils,
# # still exist within Indonesia’s assigned industrial forest concessions
# undrained_peat_areas_hti <- samples_gfc_ttm %>%
#   filter(gfc_ttm == 600 | gfc_ttm == 400 | gfc_ttm == 100) %>%
#   group_by(gfc_ttm) %>%
#   summarize(area_ha = n()) %>%
#   ungroup() %>%
#   group_by() %>%
#   mutate(shr_class = prop.table(area_ha)*100) %>%
#   print()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Description of ZDC violations -----------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Line 85: Although the impact of these types of voluntary commitments has been called into question in other settings (Garrett et al. 2019), we find that only XX hectares (XX percent) of pulpwood plantations established between 2015 and 2022 violated these no deforestation commitments (SIXX). 
# total_violations <- zdc_hti_conv %>% 
#   filter(conv_type == 2) %>% # only forest to pulp conversion
#   group_by(class) %>% 
#   summarise(area_ha = sum(area_ha)) %>% 
#   filter(class == "Deforestation for pulp after 2015") %>% 
#   pull(area_ha) %>% 
#   print()

# # Area of pulp-driven deforestation since APRIL's ZDC 
# pulp_defor_after_zdc <- hti_nonhti_conv %>%
#   filter(conv_type == 2) %>%
#   filter(year >= 2015, year <= 2022) %>% 
#   pull(area_ha) %>% 
#   sum()

# # Area of pulp expansion 
# pulp_expansion <- hti_nonhti_conv %>%
#   # filter(conv_type == 2) %>%
#   filter(year >= 2013, year <= 2022) %>% 
#   pull(area_ha) %>% 
#   sum()

# pulp_2013 = annual_pulp %>% filter(year==2013) %>% pull(area_ha)
# pulp_2022 = annual_pulp %>% filter(year==2022) %>% pull(area_ha)
# pulp_expansion_2 <- pulp_2022- pulp_2013  ## TODO: Jason - why don't these two measures of pulp expansion match up better?

# violations_shr <- (total_violations / pulp_expansion) %>% print()

# # Line 88: In addition, we find that XX percent of these violations occurred in concessions controlled by external suppliers, rather than directly within concessions controlled by NDPE-committed pulp producers. 
# indirect_violations <- zdc_hti_conv %>%
#   filter(conv_type == 2) %>%
#   filter(supplier_group %in% c("SINAR MAS", "MARUBENI", "ROYAL GOLDEN EAGLE / TANOTO")) %>% 
#   group_by(class) %>% 
#   summarise(area_ha = sum(area_ha)) %>% 
#   filter(class == "Deforestation for pulp after 2015") %>% 
#   pull(area_ha) %>% 
#   print()

# indirect_shr <- (indirect_violations / total_violations) %>% 
#   print()

# # Among the XX pulpwood producers with the largest violations, XX.
# group_data <- zdc_hti_conv %>% 
#   select(supplier_id, supplier, supplier_group) %>% 
#   distinct()

# violations_df <- zdc_hti_conv %>% 
#   filter(conv_type == 2) %>% # only forest to pulp conversion
#   filter(class == "Deforestation for pulp after first ZDC of downstream mill") %>% 
#   group_by(supplier_id) %>% 
#   summarise(violations_ha = sum(area_ha)) %>% 
#   arrange(desc(violations_ha)) %>% 
#   ungroup() %>% 
#   left_join(group_data, by = "supplier_id") %>% 
#   print()

# n = 5
# top_violations <- violations_df %>% 
#   top_n(n, violations_ha) %>% 
#   pull(violations_ha) %>% 
#   sum()

# top_violations / total_violations

# Line 95 to 98
# While the three major pulp-producing conglomerates publicly claim ownership of concessions
# with relatively few deforestation events (XX ha), prior NGO investigations indicate they are affiliated
# with concessions responsible for XX% of all deforestation in the pulp sector during this period

ownership_defor <- hti_nonhti_conv %>%
  left_join(groups_reclass_hti,by=c("supplier_id"="id")) %>%
  filter(conv_type == 2 & year >= 2015) %>%
  #filter(year > 2012 & conv_type == 2) %>%
  # drop_na(supplier_id) %>%
  group_by(group_reclassed) %>%
  summarize(area_ha = sum(area_ha)) %>%
  group_by() %>%
  mutate(share = prop.table(area_ha)*100)

total_defor_2015_2022 <- ownership_defor %>% pull(area_ha) %>% sum()
app_april_ha  <- ownership_defor %>% filter(group_reclassed == "Owned or acknowledged") %>% pull(area_ha)
linked_ha  <- ownership_defor %>% filter(group_reclassed == "NGO-linked") %>% pull(area_ha)
linked_pct <- ownership_defor %>% filter(group_reclassed == "NGO-linked") %>% pull(share)
external_pct  <- ownership_defor %>% filter(group_reclassed == "Indirect supplier" | is.na(group_reclassed)) %>% pull(share) %>% sum()

cat(sprintf(paste0(
  "\nPaper paragraph, line ~101:\n",
  "Despite the sector's ambitious goals, we find that \033[1m%s\033[0m hectares of forests were\n",
  "directly converted to pulpwood plantations between 2015 and 2022. Concessions\n",
  "officially claimed by APP and APRIL had little pulp-driven deforestation after 2015\n",
  "(\033[1m%s\033[0m ha). However, APP and APRIL's parent conglomerates, the Sinar Mas Group and\n",
  "the Royal Golden Eagle Group (RGE), have suspected indirect ownership links to\n",
  "concessions that were responsible for \033[1m%s\033[0m ha (\033[1m%.0f\033[0m%%) of pulp-driven deforestation\n",
  "during this period. The remaining \033[1m%.0f\033[0m%% of pulp-driven deforestation occurred in\n",
  "concessions controlled by external suppliers or outside of concessions.\n\n"),
  formatC(round(total_defor_2015_2022, -2), format = "f", digits = 0, big.mark = ","),
  formatC(round(app_april_ha,          -2), format = "f", digits = 0, big.mark = ","),
  formatC(round(linked_ha,          -2), format = "f", digits = 0, big.mark = ","),
  linked_pct,
  external_pct
))

# ## Create supplier list for Brian to fill in indirect control
# defor_by_supplier <- zdc_hti_conv %>%
#   filter(conv_type == 2) %>%
#   # filter(class == "Deforestation for pulp after first ZDC of downstream mill") %>% 
#   group_by(supplier_id) %>% 
#   summarise(pulp_defor_ha = sum(area_ha))
# 
# supplier_index = zdc_hti_conv %>% 
#   select(supplier_id, supplier, supplier_group, island) %>% 
#   unique()
# 
# defor_by_supplier <- supplier_index %>% 
#   left_join(defor_by_supplier, by = "supplier_id")
# 
# defor_by_supplier <- defor_by_supplier %>% 
#   arrange(desc(pulp_defor_ha))
# 
# defor_by_supplier <- defor_by_supplier %>% 
#   drop_na()
# # %>% 
# #   filter(!(supplier_group %in% c("SINAR MAS", "ROYAL GOLDEN EAGLE / TANOTO")))
# 
# defor_by_supplier %>% 
#   write_csv(paste0(wdir, '/01_data/02_out/tables/supplier_defor_list.csv'))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Capacity expansions -----------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Expansion tonnage and baseline capacity: from planned_expansions.xlsx and MILLS_EXPORTERS
oki_exp_mt       <- 4.2
rapp_exp_mt      <- 1.33
rappbctmp_exp_mt <- 1.3
phoenix_exp_mt   <- 1.7
total_exp_mt     <- oki_exp_mt + rapp_exp_mt + rappbctmp_exp_mt + phoenix_exp_mt
baseline_cap_mt  <- cap_df %>%
  select(MILL_ID, PULP_CAP_MTPY) %>%
  distinct() %>%
  pull(PULP_CAP_MTPY) %>%
  sum()
cap_change <- total_exp_mt / baseline_cap_mt

cat(sprintf("Capacity expansion: %.2f Mt (%.1f%% increase over baseline)\n",
            total_exp_mt, cap_change * 100))

# Wood demand, plantation area, and productivity scenarios: authoritative calculations
# from 22_pulp_expansion_scenarios.R (run that script first to generate scenario_stats.csv)
scenario_stats <- read_csv(paste0(wdir, "/01_data/04_results/scenario_stats.csv"),
                           show_col_types = FALSE)

cat(sprintf("New annual wood demand from expansion: %.1f million m3\n",
            scenario_stats$new_wood_demand_mm3))

# Area needed assuming no productivity improvements (uses current sector MAI)
area_demand_historical <- scenario_stats$new_wood_demand_mm3 / mai_df$dmai
cat(sprintf("Area needed at historical productivity: %.2f million ha\n",
            area_demand_historical))

# Area needed accounting for projected productivity growth (with CI); from 22_pulp_expansion_scenarios.R
cat(sprintf("Additional plantation area needed (with yield growth): %.2f million ha (%.2f-%.2f million ha)\n",
            scenario_stats$area_demand_central_mha,
            scenario_stats$area_demand_low_mha,
            scenario_stats$area_demand_high_mha))
cat(sprintf("Productivity growth rate: %.1f%% per year (%.1f%%-%.1f%%)\n",
            scenario_stats$mai_growth_central_pct,
            scenario_stats$mai_growth_lb_pct,
            scenario_stats$mai_growth_ub_pct))
cat(sprintf("Projected deforestation from expansion: %s ha (%s-%s ha)\n",
            formatC(scenario_stats$defor_central_ha, format = "d", big.mark = ","),
            formatC(scenario_stats$defor_low_ha,     format = "d", big.mark = ","),
            formatC(scenario_stats$defor_high_ha,    format = "d", big.mark = ",")))
cat(sprintf("Projected peatland conversion: %s ha (%s-%s ha)\n",
            formatC(scenario_stats$peat_central_ha,  format = "d", big.mark = ","),
            formatC(scenario_stats$peat_low_ha,      format = "d", big.mark = ","),
            formatC(scenario_stats$peat_high_ha,     format = "d", big.mark = ",")))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Remaining forests in plantations ---------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# We find that XX hectares of primary forests, and XX hectares of undrained peatlands, still exist within Indonesia’s assigned industrial forest concessions 
## TODO: Jason - do you have these data from WWI to be able to explore these results? Might also be a good visual for another supplementary figure?


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Other ideas? -----------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Total area of post-permit deforestation

# deforestation for pulp (1 - non-forest to pulp,2 - forest to pulp)
# nodefor_pulp_sids <- gaveau_annual_pulp %>% 
#   filter(year == 2017,
#          class == 1) %>% 
#   pull(sid)

# samples_df %>%
#   filter(start_for == "Y" & !is.na(lossyear) & (sid %in% nodefor_pulp_sids)) %>% 
#   pull(lossyear) %>% 
#   hist()

# test <- hti_conv %>% filter(year > 2015, conv_type == 3) %>% arrange(desc(area_ha))
# test <- hti_conv %>%
#   filter(year > 2015, 
#          conv_type == 3) %>% 
#   group_by(supplier) %>% 
#   summarise(area_ha = sum(area_ha)) %>% 
#   arrange(desc(area_ha))

# test$area_ha %>% sum()

# test

# SI 1 stats

# Area of pulpwood in Indonesia and within HTI
pulp_area_hti <- pw_area_hti %>%
  #filter(ID != "H-0657" & ID != "H-0656") %>%
  distinct(ID,pulp_2022) %>%
  group_by() %>%
  summarize(area_ha = sum(pulp_2022)) %>%
  print()

pulp_area_id <- pw_annual_area_id %>%
  distinct(prov,pulp_2022) %>%
  group_by() %>%
  summarize(area_ha = sum(pulp_2022)) %>%
  print()

pulp_area_hti/pulp_area_id * 100

# Share of pulpwood expansion in HTI
hti_pulpwood_expansion <- hti_nonhti_conv %>%
  filter(year == 2022) %>%
  mutate(type = ifelse(is.na(supplier),"Non HTI","HTI")) %>%
  group_by(type) %>%
  summarize(area_ha = sum(area_ha)) %>%
  mutate(share = prop.table(area_ha)*100) %>%
  print()

# list of HTE plantations
hti_hte_plantations <- c("H-0344","H-0361","H-0319","H-0526","H-0365","H-0405")

pulpwood_expansion_hti_hte <- hti_nonhti_conv %>%
  filter(year == 2022) %>%
  mutate(type = ifelse(is.na(supplier),"Non HTI","HTI"),
         type = ifelse(supplier_id %in% hti_hte_plantations,"HTI/HTE",type)) %>%
  group_by(type) %>%
  summarize(area_ha = sum(area_ha)) %>%
  mutate(share = prop.table(area_ha)*100) %>%
  print()

# Pulpwood share by woodtype
pw_share <- ws_2015_2022 %>%
  filter(YEAR == 2022) %>%
  group_by(TYPE) %>%
  summarize(VOLUME_M3 = sum(VOLUME_M3)) %>%
  mutate(SHARE = prop.table(VOLUME_M3)*100) %>%
  print()

# Share of active pulpwood suppliers in 2022
active_hti_suppliers <- ws %>%
  mutate(supplier_id = str_replace(SUPPLIER_ID,"ID-WOOD-CONCESSION-","H-")) %>%
  filter(YEAR == 2022) %>%
  full_join(hti_concession_names,by="supplier_id") %>%
  select(supplier_id,VOLUME_M3) %>%
  mutate(active_supplier = ifelse(!is.na(VOLUME_M3),"yes","no")) %>%
  distinct(supplier_id,active_supplier) %>%
  group_by(active_supplier) %>%
  summarize(count = n()) %>%
  mutate(share = prop.table(count)*100) %>%
  print()
  

# SI5 stats

## In 2022, existing concessions that could allow for the future expansion of pulpwood plantations
## contain XX million ha of natural forests, 3 million ha of pulpwood plantations, 
## and *5.5* million ha of other cleared lands. *2.9* million ha of forests
## (17% of the total, within-concession forest area) are located within existing HTI concessions

hti_conc_area <- hti %>%
  mutate(area_ha = as.double(units::set_units(st_area(.), "hectare"))) %>%
  st_drop_geometry() %>%
  select(supplier_id=ID,area_ha) %>%
  mutate(class = "Concession Area") %>%
  print()

hti_conc_lu_areas <- zdc_hti_conv %>%
  group_by(supplier_id,class) %>%
  summarize(area_ha = sum(area_ha)) %>%
  bind_rows(hti_conc_area) %>%
  pivot_wider(names_from ="class",
              values_from = area_ha) %>%
  mutate_if(is.numeric, ~replace_na(., 0)) %>%
  mutate(`Other Land Cover` = `Concession Area` - `Remaining forest` - (`Deforestation not for pulp` + `Deforestation for pulp after 2015` + `Deforestation for pulp from 2001-2015`)) %>%
  pivot_longer(cols = -c(supplier_id),
               names_to='class',
               values_to = 'area_ha') %>%
  group_by(class) %>%
  summarize(area_Mha = sum(area_ha)/1000000) %>%
  print()

## We restrict our analysis to these two islands since they produce more than 
## XX% of all pulpwood throughout our study period  

pulp_share_island <- pw_annual_area_id %>%
  mutate(island = str_sub(prov_code, 1, 1)) %>%
  mutate(
    island = case_when(
      island == 1 ~ "SUMATRA", island == 2 ~ "RIAU ARCHIPELAGO",
      island == 3 ~ "JAVA", island == 5 ~ "BALI AND NUSA TENGGARA",
      island == 6 ~ "KALIMANTAN", island == 7 ~ "SULAWESI",
      island == 8 ~ "MALUKU", island == 9 ~ "PAPUA"
    )
  ) %>%
  select(island,contains("pulp_")) %>%
  pivot_longer(cols = -c(island),
               names_to = 'year',
               values_to = 'area_ha') %>%
  mutate(year = str_extract(year, "(?<=_).*"),
         year = as.integer(year)) %>%
  filter(area_ha > 0) %>%
  group_by(island) %>%
  summarize(area_ha = sum(area_ha)) %>%
  group_by() %>%
  mutate(share = prop.table(area_ha)*100) %>%
  print()

## Area of expansion and plantation
ann_pulp_exp <- annual_pulp_areas %>%
  mutate(Aggregate_pulp_expansion = forest_loss_pulp_ha + nonforest_loss_pulp_ha) %>%
  select(Year=year,Pulp_driven_deforestation=forest_loss_pulp_ha,
         Other_pulp_expansion=nonforest_loss_pulp_ha,
         Aggregate_pulp_expansion,Pulpwood_planted_area=annual_pulp_area_ha) %>%
  mutate(Pulp_driven_deforestation_kha=Pulp_driven_deforestation/1000,
         Other_pulp_expansion_kha=Other_pulp_expansion/1000,
         Aggregate_pulp_expansion_kha=Aggregate_pulp_expansion/1000,
         Pulpwood_planted_area_Mha=Pulpwood_planted_area/1000000) %>%
  select(-Aggregate_pulp_expansion,-Pulpwood_planted_area,-Other_pulp_expansion,
         -Pulp_driven_deforestation) %>%
  filter(Year > 2000) %>%
  print(Inf)

write_csv(ann_pulp_exp,paste0(wdir,"/01_data/02_out/tables/pulp_expansion_areas_2001_2022.csv"))

## count of unique plantation concessions that supplied any pulp mills during the period 2015-2021
## and mean area (ha) of these concessions

hti_supp_to_mills_conc_avg_area <- ws_2015_2022 %>%
  left_join(select(hti_conc_area, SUPPLIER_ID = supplier_id, area_ha), by = "SUPPLIER_ID") %>%
  filter(!str_detect(SUPPLIER_ID, "S-")) %>%
  # ensure uniqueness at the Exporter-Supplier level before summarizing
  distinct(EXPORTER_ID, SUPPLIER_ID, .keep_all = TRUE) %>% 
  group_by() %>%
  summarize(
    n = n_distinct(SUPPLIER_ID), 
    mean_conc_area_ha = mean(area_ha, na.rm = TRUE)
  ) %>%
  print(n = Inf)

