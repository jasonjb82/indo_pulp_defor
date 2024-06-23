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
library(d3.format)
library(tidyfast)
library(patchwork)
library(rcartocolor)
library(showtext)
library(khroma) # palettes for color blindness

## set working directory -------------------------------------

wdir <- "remote"

## read data -------------------------------------------------
'%ni%' <- Negate('%in%') # filter out function

# hti concessions
hti <- read_sf(paste0(wdir,"\\01_data\\01_in\\klhk\\IUPHHK_HTI_TRASE_20230314_proj.shp"))

# wood supply
ws <- read_csv(paste0(wdir,"\\01_data\\01_in\\wwi\\PULP_WOOD_SUPPLY_CLEAN_ALL_ALIGNED_2020_2022.csv"))

# pulpwood supply in 2022 
pw_supply_2022 <- read_excel(paste0(wdir, '\\01_data\\01_in\\wwi\\RPBBI_2022_compiled.xlsx')) %>%
  select(YEAR,SUPPLIER_ID,EXPORTER_ID,VOLUME_M3)

# pulpwood conversion from forest and non-forest within and outside hti concessions
hti_nonhti_conv <- read_csv(paste0(wdir,"\\01_data\\02_out\\tables\\idn_pulp_conversion_hti_nonhti_gaveau.csv"))

# temporary areas converted to pulp
annual_conv <- read_excel(paste0(wdir,"\\01_data\\01_in\\gaveau\\IDN_2001_2022 landcover change of Oil Palm and Pulpwood_05JUNE2023.xlsx"),sheet="PULPWOOD EXPANSION",skip=90) %>% clean_names() %>%
  select(year,area_ha=area_of_forest_converted_to_pulpwood_pw_each_year_ha) %>%
  mutate(year=year+2000) %>%
  drop_na(year)

# pulpwood areas (Indonesia and within HTI)
pw_area_hti <- read_csv(paste0(wdir, "\\01_data\\02_out\\gee\\pulp_annual_area_hti_only.csv")) %>%
   select(ID,pulp_2022) 

pw_area_id <- read_csv(paste0(wdir, "\\01_data\\02_out\\gee\\pulp_annual_area_id.csv")) %>%
  select(prov,pulp_2022) 

# reclasses ownership groups
groups_reclass_hti <- read_csv(paste0(wdir,"\\01_data\\01_in\\tables\\ALIGNED_NAMES_GROUP_HTI_reclassed.csv"))

# hti pulp conversion with timing information
zdc_hti_conv <- read_csv(paste0(wdir, '/01_data/02_out/tables/hti_grps_zdc_pulp_conv_areas.csv'))

# Gaveau annual pulp areas
gaveau_annual_pulp <- read_csv(paste0(wdir, '/01_data/02_out/tables/gaveau_annual_pulp_areas.csv'))

# Expansion on soil type (Gaveau)
pulp_ttm_soil_type <- read_csv(paste0(wdir,"\\01_data\\02_out\\gee\\gaveau\\idn_pulp_annual_expansion_peat_mineral_soils.csv"))

# pulp production data (MoEF)
pulp_production <- read_excel(paste0(wdir,"\\01_data\\01_in\\tables\\annual_pulp_shr_prod.xlsx"))

## GFC deforestation (modified by TreeMap)
filenames <- dir(path = paste0(wdir,"\\01_data\\02_out\\gee\\gfc_ttm\\"),
                 pattern = "*.csv",
                 full.names= TRUE)

samples_gfc_ttm <- filenames %>%
  map_dfr(read_csv) %>%
  janitor::clean_names() 

# Parameters from MAI analysis
mai_df <- read_csv(paste0(wdir, "/01_data/04_results/key_parameters.csv"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Overarching trends in pulp expansion, deforestation, peat conversion -------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
annual_conv <- hti_nonhti_conv %>%
  group_by(year,conv_type) %>%
  summarize(area_ha = sum(area_ha)) %>%
  filter(conv_type == 2)
 
annual_conv %>% 
  ggplot(aes(x = year, y = area_ha)) +
  geom_bar(stat = "identity")

# Line 11/23: Between 2001 and 2011, 735,000 hectares of rainforest were directly converted to pulp plantations, ...
annual_conv %>% 
  filter(year < 2012) %>% 
  pull(area_ha) %>% 
  sum()
  
# Line 24: ...contributing 15% of all of Indonesia’s primary forest loss over the same period 
## TODO: Jason, could you help get the denominator for this stat? Is David's forest basemap primary forest? If so, we'd want to take that, combine it with Hansen through 2011, to calculate total primary forest clearing in the same years

# Line 14 / 100: Over the following six years, pulp-driven deforestation declined by 95% 
conv_2011 = annual_conv %>% filter(year == 2011) %>% pull(area_ha)
conv_2017 = annual_conv %>% filter(year==2017) %>% pull(area_ha)
early_change <- (conv_2017 - conv_2011) / conv_2011
early_change %>% print()

# Line 16 / 101: Indonesia has since seen ... a 372% increase in pulp-driven deforestation... 
# Between 2017 and 2022, the annual rate of conversion of primary forests to pulp plantations increased 372%
conv_2022 = annual_conv %>% filter(year==2022) %>% pull(area_ha)
late_change <- (conv_2022 - conv_2017) / conv_2017
late_change %>% print()

# Although deforestation rates in 2022 were still XX% lower than during the 2011 peak, major economic, ecological and policy changes call into question whether the sector will ever be able to achieve its desired end to deforestation 
overall_change <- (conv_2022 - conv_2011) / conv_2011
overall_change %>% print()

# Conversion of pulp between 2017 and 2022
annual_pulp_conv <- pulp_ttm_soil_type %>%
  select(-`system:index`,-constant,-kab,-kab_code,-prov_code,-.geo,-type) %>%
  pivot_longer(cols = -c(prov),
               names_to = 'year',
               values_to = 'area_ha') %>%
  mutate(class = str_extract(year, "[^_]+"),
         year = as.numeric(gsub("[^0-9]", "", year))) %>%
  ungroup() %>%
  group_by(year,class) %>%
  summarize(area_ha = sum(area_ha))

pulp_conv_2017 = annual_pulp_conv %>% filter(class == "peat" & year==2017) %>% pull(area_ha)
pulp_conv_2022 = annual_pulp_conv %>% filter(class == "peat" & year==2022) %>% pull(area_ha)
overall_pulp_change <- (pulp_conv_2022 - pulp_conv_2017) / pulp_conv_2017
overall_pulp_change %>% print()

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Emissions -----------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Line 27: In addition, the combination of land use conversion, burning, and peat subsidence released an estimated XX million tons of carbon to the atmosphere 
## TODO: Work with Vivian and Carina to fill this in.


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Plantation yield changes -----------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Line 67: Many of these forests were cleared to make room for industrial acacia and eucalyptus plantations, which expanded by ~1.5 million hectares between 2000 and 2015 
annual_pulp <- gaveau_annual_pulp %>%
  group_by(gav_class, year) %>%
  summarize(n = sum(n)) %>%
  print()
pulp_2001 = annual_pulp %>% filter(year == 2000) %>% pull(n)
pulp_2015 = annual_pulp %>% filter(year==2015) %>% pull(n)
pulp_change <- (pulp_2015 - pulp_2001) %>% 
  print()


# Line 73: In addition, improvements in plantation management have led to an XX% increase in plantation yields 


# Line 74: pulp plantations now supply nearly all of Indonesia’s 47 million m3 of annual pulpwood demand (Figure 1). 
current_wood_demand <- pw_supply_2022 %>% pull(VOLUME_M3) %>% sum() %>% print()


# Line 109: we find little evidence that plantation yields have increased over the past XX years 


# Line 151: We find that 3 million hectares of primary forests, XX% of which are on peat soils,
# still exist within Indonesia’s assigned industrial forest concessions
undrained_peat_areas_hti <- samples_gfc_ttm %>%
  filter(gfc_ttm == 600 | gfc_ttm == 400 | gfc_ttm == 100) %>%
  group_by(gfc_ttm) %>%
  summarize(area_ha = n()) %>%
  ungroup() %>%
  group_by() %>%
  mutate(shr_class = prop.table(area_ha)*100) %>%
  print()


# Can we differentiate yields in euc and acacia plantations?

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Description of ZDC violations -----------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Line 85: Although the impact of these types of voluntary commitments has been called into question in other settings (Garrett et al. 2019), we find that only XX hectares (XX percent) of pulpwood plantations established between 2015 and 2022 violated these no deforestation commitments (SIXX). 
total_violations <- zdc_hti_conv %>% 
  filter(conv_type == 2) %>% # only forest to pulp conversion
  group_by(class) %>% 
  summarise(area_ha = sum(area_ha)) %>% 
  filter(class == "Deforestation for pulp after first ZDC of downstream mill") %>% 
  pull(area_ha) %>% 
  print()

# Area of pulp-driven deforestation since APRIL's ZDC 
pulp_defor_after_zdc <- hti_nonhti_conv %>%
  filter(conv_type == 2) %>%
  filter(year >= 2015, year <= 2022) %>% 
  pull(area_ha) %>% 
  sum()

# Area of pulp expansion 
pulp_expansion <- hti_nonhti_conv %>%
  # filter(conv_type == 2) %>%
  filter(year >= 2013, year <= 2022) %>% 
  pull(area_ha) %>% 
  sum()

pulp_2013 = annual_pulp %>% filter(year==2013) %>% pull(n)
pulp_2022 = annual_pulp %>% filter(year==2022) %>% pull(n)
pulp_expansion_2 <- pulp_2022- pulp_2013  ## TODO: Jason - why don't these two measures of pulp expansion match up better?

violations_shr <- (total_violations / pulp_expansion) %>% print()

# Line 88: In addition, we find that XX percent of these violations occurred in concessions controlled by external suppliers, rather than directly within concessions controlled by NDPE-committed pulp producers. 
indirect_violations <- zdc_hti_conv %>%
  filter(conv_type == 2) %>%
  filter(supplier_group %in% c("SINAR MAS", "MARUBENI", "ROYAL GOLDEN EAGLE / TANOTO")) %>% 
  group_by(class) %>% 
  summarise(area_ha = sum(area_ha)) %>% 
  filter(class == "Deforestation for pulp after first ZDC of downstream mill") %>% 
  pull(area_ha) %>% 
  print()


indirect_shr <- (indirect_violations / total_violations) %>% 
  print()

# Among the XX pulpwood producers with the largest violations, XX.
group_data <- zdc_hti_conv %>% 
  select(supplier_id, supplier, supplier_group) %>% 
  distinct()

violations_df <- zdc_hti_conv %>% 
  filter(conv_type == 2) %>% # only forest to pulp conversion
  filter(class == "Deforestation for pulp after first ZDC of downstream mill") %>% 
  group_by(supplier_id) %>% 
  summarise(violations_ha = sum(area_ha)) %>% 
  arrange(desc(violations_ha)) %>% 
  ungroup() %>% 
  left_join(group_data, by = "supplier_id") %>% 
  print()


n = 5
top_violations <- violations_df %>% 
  top_n(n, violations_ha) %>% 
  pull(violations_ha) %>% 
  sum()

top_violations / total_violations

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
  mutate(share = prop.table(area_ha)*100) %>%
  print()
  
## QUESTION: Should we break these group stats into those that are officially declared as subsidiaries, and those that have been inferred (e.g. http://awsassets.panda.org/downloads/removing_the_corporate_mask_app_assessment_2018.pdf)

## Create supplier list for Brian to fill in indirect control
defor_by_supplier <- zdc_hti_conv %>%
  filter(conv_type == 2) %>%
  # filter(class == "Deforestation for pulp after first ZDC of downstream mill") %>% 
  group_by(supplier_id) %>% 
  summarise(pulp_defor_ha = sum(area_ha))

supplier_index = zdc_hti_conv %>% 
  select(supplier_id, supplier, supplier_group, island) %>% 
  unique()

defor_by_supplier <- supplier_index %>% 
  left_join(defor_by_supplier, by = "supplier_id")

defor_by_supplier <- defor_by_supplier %>% 
  arrange(desc(pulp_defor_ha))

defor_by_supplier <- defor_by_supplier %>% 
  drop_na()
# %>% 
#   filter(!(supplier_group %in% c("SINAR MAS", "ROYAL GOLDEN EAGLE / TANOTO")))

defor_by_supplier %>% 
  write_csv(paste0(wdir, '/01_data/02_out/tables/supplier_defor_list.csv'))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Capacity expansions -----------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Summary of proposed expansions: remote/01_data/01_in/new_capacity/planned_expansions.xlsx"
## Productivity calculations largely building upon script 08_calc_mai.R
sector_mai <- mai_df$dmai # After correcting for burns. Was 21.75 in prior version

oki_exp_mt <- 4.2
rapp_exp_mt <- 1.33
rappbctmp_exp_mt <- 1.3
phoenix_exp_mt <- 1.7
total_exp_mt <- oki_exp_mt + rapp_exp_mt + rappbctmp_exp_mt + phoenix_exp_mt
baseline_production <- pulp_production %>% filter(year == 2022) %>% pull(annual_prod_mtpy) # JASON - why doesnt this number match the production you were drawing on to calculate capacity factors across time? Your table says pulp production in 2022 was 9.9 million tonnes, but here it's only 8.9
baseline_cap_mt <- 10.9 ## TODO: This should be drawn directly from RPBBI - RPBBI installed capacity in 2022
baseline_usage_shr <- baseline_production / baseline_cap_mt 

## Calculate the prior industry average conversion rate: m3 per tonne of pulp
wood_pulp_conv <- (current_wood_demand / 1000000) / baseline_production


## Double check calculations on current production
test_wood_demand <- baseline_cap_mt * baseline_usage_shr * wood_pulp_conv
(current_wood_demand / 1000000) == test_wood_demand


baseline_usage_shr = 1 # Currently using 100% cap usage


## Set assumed conversion rate for new semi-chemical pulp mill (pt phoenix). Source: https://unece.org/forestry-timber/documents/2022/01/informal-documents/supporting-tables-forest-products-conversion
chem_wood_pulp_conv <- 2.75

## Line 102: Together, these three projects would increase the country’s pulp capacity by 91% and, once fully operational, would lead to a concomitant 40 million m3 increase in the country’s annual demand for pulpwood. 
total_exp_mt
cap_change <- (total_exp_mt / baseline_cap_mt) %>% print()



# Estimate of land demand from capacity expansions
new_wood_demand <- ((oki_exp_mt + rapp_exp_mt) * baseline_usage_shr * wood_pulp_conv) + ((phoenix_exp_mt + rappbctmp_exp_mt) * baseline_usage_shr * chem_wood_pulp_conv)
new_wood_demand

# Line 103: At historical levels of plantation productivity, an additional 1.63 million hectares of plantations would be needed to meet this potential boom in pulpwood demand
# new_wood_demand <- 30600000 # m3 / y - taken from Brian's calculations in paper draft. Was for original expansion estimates without PT phoenix
(area_demand <- new_wood_demand / sector_mai) # ha

new_wood_demand / (current_wood_demand / 1000000)

## Explore scenario with continued yield improvements for five years. We've seen ~4.5% increase per year (script 08_calc_mai.R)
## "...even if companies were able to sustain the current rate of productivity 
## improvement over the next decade, increased production from existing plantations 
## would only meet 62% of the anticipated growth in pulpwood demand"
yield_growth = mai_df$yield_growth + 1
high_yield_mai <- (yield_growth^5) * mai_2021  # Updated after fixing david's data to account for burns. Was 1.049 growth rate
assumed_area_plantations <- 3050000
extra_production <- (high_yield_mai - sector_mai) * assumed_area_plantations / 1000000
extra_production / new_wood_demand

# A further 520,000 hectares of plantations would be needed to meet the remaining pulpwood demand.
(new_wood_demand - extra_production) / high_yield_mai


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
nodefor_pulp_sids <- gaveau_annual_pulp %>% 
  filter(year == 2017,
         class == 1) %>% 
  pull(sid)

samples_df %>%
  filter(start_for == "Y" & !is.na(lossyear) & (sid %in% nodefor_pulp_sids)) %>% 
  pull(lossyear) %>% 
  hist()

test <- hti_conv %>% filter(year > 2015, conv_type == 3) %>% arrange(desc(area_ha))
test <- hti_conv %>%
  filter(year > 2015, 
         conv_type == 3) %>% 
  group_by(supplier) %>% 
  summarise(area_ha = sum(area_ha)) %>% 
  arrange(desc(area_ha))

test$area_ha %>% sum()


test

# SI 1 stats

# Area of pulpwood in Indonesia and within HTI
pulp_area_hti <- pw_area_hti %>%
  #filter(ID != "H-0657" & ID != "H-0656") %>%
  distinct(ID,pulp_2022) %>%
  group_by() %>%
  summarize(area_ha = sum(pulp_2022)) %>%
  print()

pulp_area_id <- pw_area_id %>%
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
pw_share <- read_excel(paste0(wdir, '\\01_data\\01_in\\wwi\\RPBBI_2022_compiled.xlsx')) %>%
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
  
