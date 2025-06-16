#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Author: Robert Heilmayr
# Project: Indonesia pulp deforestation
# Date: 6-2-2025
# Purpose: Estimate the deforestation elasticity
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# load packages --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(tidyverse)
library(fixest)
library(janitor)
library(modelsummary)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# load data --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
wdir <- "remote/"

# Deforestation data
defor_df <- read_csv(paste0(wdir, "/01_data/02_out/tables/tbl_long_pulp_clearing_gfc_forest.csv"))

# Transport costs
trnsprt_cst_df <- read_csv(paste0(wdir, "/01_data/02_out/tables/centroids_mills_cost.csv")) %>% 
  rename(pixel_id = id)

# Bleached Hardwood Kraft, Acacia, from Indonesia (net price) and South America from RISI
# TODO: I believe this is for tonnes of BHKP - check with Brian. Need to account for processing costs
risi_prices <- readxl::read_excel(paste0(wdir,"/01_data/01_in/wwi/Fastmarkets_2025_01_14-103617.xlsx"),skip=4) %>%
  clean_names() %>% 
  select(date,net_price=mid_3, sa_net_price = mid_2)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# clean and merge data --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Join transport cost data to defor_df
defor_df <- defor_df %>%
  left_join(trnsprt_cst_df, by = "pixel_id")

# Add total pulp expansion variable
defor_df <- defor_df %>%
  mutate(pulp_exp_ha = pulp_forest_ha + pulp_non_forest_ha) 

# RISI annual pulp prices
risi_prices_annual <- risi_prices %>%
  mutate(date = as.Date(date,format="%m/%d/%Y"),
         year = year(date),
         month = month(date)) %>%
  group_by(year,month) %>%
  summarize(indo_net_price = mean(net_price),
            sa_net_price = mean(sa_net_price)) %>%
  # filter(year <= 2023 & !is.na(risi_monthly_net_price)) %>%
  group_by(year) %>%
  summarize(indo_prices = mean(indo_net_price),
            sa_prices = mean(sa_net_price)) %>%  # Note - missing a few observations for SA in 2001
  select(year, sa_prices)

# calculate deviation in pulp prices from last five year rolling average
risi_prices_annual <- risi_prices_annual %>%
  mutate(sa_prices_dev = sa_prices - zoo::rollmean(sa_prices, k = 5, fill = NA, align = "right")) %>%
  filter(!is.na(sa_prices))

# Add RISI prices to defor_df
defor_df <- defor_df %>%
  left_join(risi_prices_annual, by = "year")

# Calculate interaction between prices and transport costs
# defor_df <- defor_df %>%
#   mutate(plant_prices_dev = sa_prices_dev  * (1/cost_usd_perton))
defor_df <- defor_df %>%
  mutate(net_prices = sa_prices - cost_usd_perton,
         net_prices_dev = net_prices - zoo::rollmean(net_prices, k = 5, fill = NA, align = "right"))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# integrate productivity variations  --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
grid_gaez <- read_csv(paste0(wdir, "/01_data/02_out/tables/gaez_grid_share.csv"))
grid_gaez <- grid_gaez %>% 
  mutate(noLimitations = (class_3_pct + class_6_pct) / 100,
         hydromorphic = (class_27_pct + class_28_pct) / 100,
         other = (class_25_pct + class_26_pct + class_32_pct + class_33_pct) / 100) %>% 
  select(pixel_id, noLimitations, hydromorphic, other)
# TODO: add check that we're getting 100% areas

hti_mai <- read_csv(paste0(wdir, "/01_data/02_out/tables/hti_mai.csv"))

hti_gaez <- read_csv(paste0(wdir, "/01_data/02_out/tables/gaez_hti_areas.csv")) %>% 
  select(-total_area_ha, supplier_id = ID)
hti_gaez <- hti_gaez %>% 
  mutate(class_noLimitations = class_2 + class_3 + class_6, # tropic lowlands; sub-humid tropic lowlands; humid topic highlands  
         class_hydromorphic = class_27 + class_28, # land with ample irrigated soils, dominantly hydromorphic soils
         class_other = class_25 + class_26 + class_32 + class_33) %>%  # very steep terrain, land with severe soil/terrain limitations, water and developed, to be dropped
  select(supplier_id, class_noLimitations, class_hydromorphic, class_other)

hti_gaez <- hti_gaez %>% 
  pivot_longer(cols = starts_with("class_"),
               names_prefix = "class_",
               names_to = "class",
               values_to = "area_ha")

hti_gaez <- hti_gaez %>% 
  group_by(supplier_id) %>% 
  mutate(share = area_ha / sum(area_ha, na.rm = TRUE))

hti_gaez %>% 
  group_by(class) %>%
  summarize(area_ha = sum(area_ha, na.rm = TRUE)) %>%
  mutate(prop_area = area_ha / sum(area_ha, na.rm = TRUE)) %>%
  print()


hti_gaez <- hti_gaez %>% 
  select(-area_ha) %>% 
  pivot_wider(names_from = class, values_from = share) %>%
  left_join(hti_mai, by = "supplier_id") %>% 
  drop_na()


weighted_mod <- lm(dmai_winsorized ~ 0 + noLimitations + hydromorphic + other, data = hti_gaez, weights = ha_y)
summary(weighted_mod)

mod <- lm(dmai_winsorized ~ 0 + noLimitations + hydromorphic + other, data = hti_gaez)
summary(mod)

grid_pot_mai <- grid_gaez %>% 
  mutate(pot_mai = predict(mod, newdata = grid_gaez),
         pot_mai = ifelse(pot_mai < 0, 0, pot_mai), # winsorize negative potential production to 0
         pot_mai = pot_mai / 2.75) %>%  # Convert m3 to tonnes of pulp
  select(pixel_id, pot_mai)


# hti_gaez <- hti_gaez %>% 
#   mutate(maj_noLimitations = ifelse(noLimitations > 0.5, 1, 0),
#          maj_hydromorphic = ifelse(hydromorphic > 0.5, 1, 0),
#          maj_steep = ifelse(steep > 0.5, 1, 0))
# 
# hti_gaez %>% 
#   filter(maj_noLimitations==1) %>% 
#   pull(dmai) %>% 
#   quantile(0.9)
# 
# hti_gaez %>% 
#   filter(maj_hydromorphic==1) %>% 
#   pull(dmai) %>% 
#   quantile(0.9)
# 
# hti_gaez %>% 
#   filter(maj_steep==1) %>% 
#   pull(dmai) %>% 
#   quantile(0.9)

test_df <- defor_df %>% 
  left_join(grid_pot_mai, by = "pixel_id")
test_df <- test_df %>%
  mutate(pot_revenues = (sa_prices_dev * pot_mai) / 1000,
         pot_net_revenues = (net_prices_dev * pot_mai) / 1000)
test_df <- test_df %>% 
  mutate(post_2015 = year > 2015)

mod_1 <- feols(pulp_forest_ha ~ pot_revenues | year + pixel_id, data = test_df, vcov = "twoway")
summary(mod_1)

mod_2 <- feols(pulp_non_forest_ha ~ pot_revenues | year + pixel_id, data = test_df, vcov = "twoway")
summary(mod_2)

mod_3 <- feols(pulp_forest_ha ~ post_2015:pot_revenues | year + pixel_id, data = test_df, vcov = "twoway")
summary(mod_3)

# Variable labels
coef_map <- c("Potential revenues","Pre-2015", "Post-2015")

# Custom summary table
msummary(
  list(
    "Pulp deforestation" = mod_1,
    "Pulp expansion" = mod_2,
    "Pulp deforestation, heterogeneous over time" = mod_3
  ),
  output = "html",
  stars = TRUE,
  # coef_map = coef_map,
  gof_omit = "R2|Adj|Within|Pseudo|Log|AIC|BIC"  # remove R2 and Adj R2 and more if desired
)




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# plot basic trends --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# plot sa_prices variable in risi_prices_annual
ggplot(risi_prices_annual, aes(x = year, y = sa_prices)) +
  geom_line() +
  labs(title = "RISI South America Pulp Prices",
       x = "Year",
       y = "Price (USD)") +
  theme_minimal()

# plot total deforestation across pixel_id for each year
total_pulp_exp <- defor_df %>%
  group_by(year) %>%
  summarize(pulp_forest_ha = sum(pulp_forest_ha, na.rm = TRUE),
            pulp_non_forest_ha = sum(pulp_non_forest_ha, na.rm = TRUE),
            pulp_exp_ha = sum(pulp_exp_ha, na.rm = TRUE))
ggplot(total_pulp_exp, aes(x = year, y = pulp_forest_ha)) +
  geom_line() +
  labs(title = "Total Pulp Deforestation by Year",
       x = "Year",
       y = "Pulp Deforestation (ha)") +
  theme_minimal()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# run regressions --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Look at variation explained by price series
mod <- feols(pulp_forest_ha ~ sa_prices_dev | pixel_id, data = defor_df)
summary(mod)
mod <- feols(pulp_exp_ha ~ sa_prices_dev | pixel_id, data = defor_df)
summary(mod)

# Look at variation explained by cross-sectional variation in transport costs
mod <- feols(pulp_forest_ha ~ cost_usd_perton | year, data = defor_df)
summary(mod)
mod <- feols(pulp_exp_ha ~ cost_usd_perton | year, data = defor_df)
summary(mod)

# Look at variation explained by their interaction
mod <- feols(pulp_forest_ha ~ cost_usd_perton | year + pixel_id, data = defor_df)
summary(mod)
mod <- feols(pulp_exp_ha ~ cost_usd_perton | year + pixel_id, data = defor_df)
summary(mod)


# Look at variation explained by price series (logged)
mod <- feols(log(pulp_forest_ha) ~ sa_prices_dev | pixel_id, data = defor_df)
summary(mod)
mod <- feols(log(pulp_exp_ha) ~ sa_prices_dev | pixel_id, data = defor_df)
summary(mod)

# Look at variation explained by cross-sectional variation in transport costs  (logged)
mod <- feols(log(pulp_forest_ha) ~ cost_usd_perton | year, data = defor_df)
summary(mod)
mod <- feols(log(pulp_exp_ha) ~ cost_usd_perton | year, data = defor_df)
summary(mod)

# Look at variation explained by their interaction (logged)
mod <- feols(log(pulp_forest_ha) ~ plant_prices_dev | year + pixel_id, data = defor_df)
summary(mod)
mod <- feols(log(pulp_exp_ha) ~ plant_prices_dev | year + pixel_id, data = defor_df)
summary(mod)


# Interaction model, but exploring different time periods
mod <- feols(pulp_forest_ha ~ plant_prices_dev | year + pixel_id, data = defor_df %>% filter(year <=2013))
summary(mod)
mod <- feols(pulp_exp_ha ~ plant_prices_dev | year + pixel_id, data = defor_df %>% filter(year <=2013))
summary(mod)


mod <- feols(pulp_forest_ha ~ plant_prices_dev | year + pixel_id, data = defor_df %>% filter(year > 2013))
summary(mod)
mod <- feols(pulp_exp_ha ~ plant_prices_dev | year + pixel_id, data = defor_df %>% filter(year > 2013))
summary(mod)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# predict deforestation with no price deviation --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mod <- feols(pulp_forest_ha ~ plant_prices_dev | year + pixel_id, data = defor_df)
summary(mod)

defor_cf <- defor_df %>% 
  mutate(plant_prices_dev = 0)

defor_cf <- defor_cf %>% 
  mutate(pulp_forest_ha_cf = predict(mod, newdata = defor_cf))

# plot total deforestation across pixel_id for each year
total_pulp_exp <- defor_cf %>%
  group_by(year) %>%
  summarize(pulp_forest_ha = sum(pulp_forest_ha, na.rm = TRUE),
            pulp_forest_ha_cf = sum(pulp_forest_ha_cf, na.rm = TRUE))
ggplot(total_pulp_exp %>% filter(year > 2004)) +
  geom_line(aes(x = year, y = pulp_forest_ha), color = "red") +
  geom_line(aes(x = year, y = pulp_forest_ha_cf), color = "blue") +
  labs(title = "Total Pulp Deforestation by Year",
       x = "Year",
       y = "Pulp Deforestation (ha)") +
  theme_minimal()





mod <- feols(pulp_forest_ha ~ sa_prices_dev | pixel_id, data = defor_df)
summary(mod)

defor_cf <- defor_df %>% 
  mutate(plant_prices_dev = 0, 
         sa_prices = 0)

defor_cf <- defor_cf %>% 
  mutate(pulp_forest_ha_cf = predict(mod, newdata = defor_cf))

# plot total deforestation across pixel_id for each year
total_pulp_exp <- defor_cf %>%
  group_by(year) %>%
  summarize(pulp_forest_ha = sum(pulp_forest_ha, na.rm = TRUE),
            pulp_forest_ha_cf = sum(pulp_forest_ha_cf, na.rm = TRUE))
ggplot(total_pulp_exp %>% filter(year > 2004)) +
  geom_line(aes(x = year, y = pulp_forest_ha), color = "red") +
  geom_line(aes(x = year, y = pulp_forest_ha_cf), color = "blue") +
  labs(title = "Total Pulp Deforestation by Year",
       x = "Year",
       y = "Pulp Deforestation (ha)") +
  theme_minimal()

