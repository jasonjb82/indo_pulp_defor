



pw_supply_2022 <- read_excel(paste0(wdir, '\\01_data\\01_in\\wwi\\RPBBI_2022_compiled.xlsx')) %>%
  select(YEAR,SUPPLIER_ID,EXPORTER_ID,VOLUME_M3)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Overarching trends in pulp expansion, deforestation, peat conversion -------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Note: stats don't perfectly match David's numbers reported here: https://nusantara-atlas.org/pulp-and-paper-driven-deforestation-in-indonesia-accelerates-in-2022/
annual_conv_hti <- hti_pulp_conv %>% 
  filter(conv_type == 2) %>% 
  group_by(year) %>% 
  summarise(area_hti = sum(area_ha))

annual_conv_nohti <- pulp_defor_outside_hti %>% 
  filter(conv_type == 2) %>% 
  group_by(year) %>% 
  summarise(area_nohti = sum(area_ha))

annual_conv <- annual_conv_hti %>% 
  left_join(annual_conv_nohti, by = "year") %>% 
  mutate(area = area_nohti + area_hti)  

annual_conv %>% 
  ggplot(aes(x = year, y = area)) +
  geom_bar(stat = "identity")


# Line 23: Between 2001 and 2011, 735,000 hectares of rainforest were directly converted to pulp plantations, contributing 15% of all of Indonesia’s primary forest loss over the same period 
annual_conv %>% 
  filter(year < 2012) %>% 
  pull(area) %>% 
  sum()

# Line 93: Although pulp-driven deforestation declined by XX% between XX and XX, recent economic trends and policy debates highlight the fragility of this progress. 
conv_2011 = annual_conv %>% filter(year == 2011) %>% pull(area)
conv_2017 = annual_conv %>% filter(year==2017) %>% pull(area)
early_change <- (conv_2017 - conv_2011) / conv_2011
early_change %>% print()

# Line 94: Between 2017 and 2022, the annual rate of conversion of primary forests to pulp plantations increased 370%, while pulp-driven conversion of peatlands increased XX%. 
conv_2022 = annual_conv %>% filter(year==2022) %>% pull(area)
late_change <- (conv_2022 - conv_2017) / conv_2017
late_change %>% print()


# Although deforestation rates in 2022 were still XX% lower than during the 2011 peak, major economic, ecological and policy changes call into question whether the sector will ever be able to achieve its desired end to deforestation 
overall_change <- (conv_2022 - conv_2011) / conv_2011
overall_change %>% print()

# Line 70: Many of these forests were cleared to make room for industrial acacia and eucalyptus plantations, which expanded by ~1.1 million hectares between 2000 and 2015 
test <- gaveau_annual_pulp %>%
  group_by(gav_class, year) %>%
  summarize(n = sum(n)) %>% 
  print()



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Emissions -----------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Line 28: In addition, the combination of land use conversion, burning, and peat subsidence released an estimated XX million tons of carbon to the atmosphere 




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Plantation yield changes -----------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Line 73: In addition, improvements in plantation management have led to an XX% increase in plantation yields 


# Line 74: pulp plantations now supply nearly all of Indonesia’s 47 million m3 of annual pulpwood demand (Figure 1). 
current_wood_demand <- pw_supply_2022 %>% pull(VOLUME_M3) %>% sum() %>% print()


# Line 109: we find little evidence that plantation yields have increased over the past XX years 



# Can we differentiate yields in euc and acacia plantations?

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Description of ZDC violations -----------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Line 85: Although the impact of these types of voluntary commitments has been called into question in other settings (Garrett et al. 2019), we find that only XX hectares (XX percent) of pulpwood plantations established between 2015 and 2022 violated these no deforestation commitments (SIXX). 

# Line 88: In addition, we find that XX percent of these violations occurred in concessions controlled by external suppliers, rather than directly within concessions controlled by NDPE-committed pulp producers. 

# Among the XX pulpwood producers with the largest violations, XX.


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Capacity expansions -----------------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Summary of proposed expansions: remote/01_data/01_in/new_capacity/planned_expansions.xlsx"
## Productivity calculations largely building upon script 08_calc_mai.R
sector_mai <- 21.75

oki_exp_mt <- 4.2
rapp_exp_mt <- 2
phoenix_exp_mt <- 1.7
total_exp_mt <- oki_exp_mt + rapp_exp_mt + phoenix_exp_mt
baseline_cap_mt <- 9.3 ## TODO: Check this with Brian. Doesn't match (mills$PULP_CAP_2019_MTPY %>% sum())

## Line 102: Together, these three projects would increase the country’s pulp capacity by 91% and, once fully operational, would lead to a concomitant 40 million m3 increase in the country’s annual demand for pulpwood. 
total_exp_mt
cap_change <- (total_exp_mt / baseline_cap_mt) %>% print()

# Estimate of land demand from capacity expansions
new_wood_demand <- (current_wood_demand * cap_change) %>% print()

# Line 103: At current levels of plantation productivity, an additional 1.82 million hectares of plantations would be needed to meet this potential boom in pulpwood demand
# new_wood_demand <- 30600000 # m3 / y - taken from Brian's calculations in paper draft. Was for original expansion estimates without PT phoenix
(area_demand <- new_wood_demand / sector_mai) # ha


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Remaining forests in plantations ---------------------------------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# We find that XX hectares of primary forests, and XX hectares of undrained peatlands, still exist within Indonesia’s assigned industrial forest concessions 



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

