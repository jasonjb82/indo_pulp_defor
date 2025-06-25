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
library(WDI)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# load data --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
wdir <- "remote/"

# Deforestation data
defor_df <- read_csv(paste0(wdir, "/01_data/02_out/tables/tbl_long_pulp_clearing_gfc_forest.csv"))

# Bleached Hardwood Kraft, Acacia, from Indonesia (net price) and South America from RISI
# Measured in nominal USD / tonnes of BHKP
risi_prices <- readxl::read_excel(paste0(wdir,"/01_data/01_in/wwi/Fastmarkets_2025_01_14-103617.xlsx"),skip=4) %>%
  clean_names() %>% 
  select(date,net_price=mid_3, sa_net_price = mid_2)

# WRQ data on pulpwood prices (USD/m3). 
# Used to convert global interannual variation in pulp prices (RISI data) into 
# local pulpwood prices to improve interpretation
wrq_prices <- readxl::read_excel(paste0(wdir,"/01_data/01_in/wwi/WRQ_pulpwood_prices.xlsx")) %>%
  clean_names() %>% 
  drop_na() %>% 
  group_by(year)
keep_years <- wrq_prices %>% 
  tally() %>% 
  filter(n==4) %>% 
  pull(year)
wrq_prices <- wrq_prices %>% 
  filter(year %in% keep_years) %>%
  summarize(wrq_indo_prices = mean(indonesia, na.rm = TRUE),
            wrq_brazil_prices = mean(brazil, na.rm = TRUE))

# FRED data on IDR to USD exchange rate (to convert global prices to local currency)
fred_idr_usd <- read_csv(paste0(wdir,"/01_data/01_in/tables/FRED_CCUSSP02IDM650N.csv")) %>%
  mutate(date = as.Date(observation_date,format="%d/%m/%Y"),
         year = year(date)) %>%
  group_by(year) %>% 
  summarize(idr_usd = mean(CCUSSP02IDM650N, na.rm = TRUE)) %>% 
  filter(year > 1999, year < 2023)

# FRED data on Indonesian CPI (to adjust for inflation, reference year = 2015)
fred_idn_cpi <- read_csv(paste0(wdir,"/01_data/01_in/tables/FRED_IDNCPIALLAINMEI.csv")) %>%
  mutate(date = as.Date(observation_date,format="%d/%m/%Y"),
         year = year(date)) %>% 
  select(year, idn_cpi = IDNCPIALLAINMEI)

# Transport costs (2006 USD / m3, used for robustness test)
trnsprt_cst_df <- read_csv(paste0(wdir, "/01_data/02_out/tables/centroids_mills_cost.csv")) %>% 
  rename(pixel_id = id) %>% 
  mutate(year = 2006)

# Data about grid cell composition along GAEZ classes
grid_gaez <- read_csv(paste0(wdir, "/01_data/02_out/tables/gaez_grid_share.csv"))

# Data about hti composition along GAEZ classes
hti_gaez <- read_csv(paste0(wdir, "/01_data/02_out/tables/gaez_hti_areas.csv")) %>% 
  select(-total_area_ha, supplier_id = ID)

# Data about hti productivity (produced in R script 08_calc_mai.R)
hti_mai <- read_csv(paste0(wdir, "/01_data/02_out/tables/hti_mai.csv"))

# Add administrative labels
grid_admin <- read_csv(paste0(wdir, "/01_data/02_out/tables/grid_10km_adm_prov_kab.csv"))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# clean price data --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Clean RISI annual pulp prices
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


# Convert longer SA pulp price series (USD/tonne) into Indonesian pulpwood prices (USD/m3)
wrq_prices <- wrq_prices %>% 
  left_join(risi_prices_annual, by = "year")
price_conversion_mod <- lm(wrq_indo_prices ~ sa_prices + 0, data = wrq_prices)
summary(price_conversion_mod)
risi_prices_annual <- risi_prices_annual %>%
  mutate(sa_prices = predict(price_conversion_mod, newdata = risi_prices_annual))

# Convert currency
risi_prices_annual <- risi_prices_annual %>%
  left_join(fred_idr_usd, by = "year") %>%
  mutate(sa_prices_idr = sa_prices * idr_usd / 1000)  # Convert from USD to thousand IDR

# Adjust for inflation
risi_prices_annual <- risi_prices_annual %>%
  left_join(fred_idn_cpi, by = "year") %>%
  mutate(sa_prices_real = sa_prices_idr / idn_cpi * 100) # Adjust for inflation - reference year is 2015

# Adjust transport costs for currency and inflation
trnsprt_cst_df <- trnsprt_cst_df %>% 
  left_join(fred_idr_usd, by = "year") %>%
  left_join(fred_idn_cpi, by = "year") %>%
  mutate(cost_kidr_perton = cost_usd_perton * idr_usd / 1000,  # Convert from USD to thousand IDR
         cost_kidr_perton_real = cost_kidr_perton / idn_cpi * 100, # Adjust for inflation, reference year is 2015
         trnsprt_cost_real = cost_kidr_perton_real / 1.142) %>%  # Convert from USD/tonne to USD/m3 (assuming 1.142 m3 per tonne)
  select(pixel_id, trnsprt_cost_real)

# # Explore similarity to FRED data
# fred_prices_annual <- read_csv(paste0(wdir,"/01_data/01_in/tables/WPU0911_annual.csv")) %>%
#   mutate(date = as.Date(observation_date,format="%d/%m/%Y"),
#          year = year(date)) %>%
#   select(year,prices = WPU0911) 
# 
# fred_prices_annual <- fred_prices_annual %>%
#   left_join(risi_prices_annual, by = "year")
# 
# mod <- lm(sa_prices ~ prices, data = fred_prices_annual)
# summary(mod)
# 
# mod <- lm(sa_prices ~ indo_prices, data = fred_prices_annual)
# summary(mod)
# 
# mod <- lm(indo_prices ~ prices, data = fred_prices_annual)
# summary(mod)
# 
# fred_prices_annual %>% 
#   ggplot(aes(x = year)) +
#   geom_line(aes(y = prices, color = "FRED PPI")) +
#   geom_line(aes(y = indo_prices, color = "RISI Indonesia")) +
#   geom_line(aes(y = sa_prices, color = "RISI South America"))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# merge datasets --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Add administrative unit labels
defor_df <- defor_df %>%
  left_join(grid_admin, by = "pixel_id")

# Join transport cost data to defor_df
defor_df <- defor_df %>%
  left_join(trnsprt_cst_df, by = "pixel_id")

# Add total pulp expansion variable
defor_df <- defor_df %>%
  mutate(pulp_exp_ha = pulp_forest_ha + pulp_non_forest_ha) 

# Add prices to defor_df
defor_df <- defor_df %>%
  left_join(risi_prices_annual, by = "year")

# Calculate prices net of transport costs
# TODO: Still need to adjust transport prices to IDR
defor_df <- defor_df %>%
  mutate(net_prices = sa_prices_real - trnsprt_cost_real)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Estimate cross-sectional variation in productivity  --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Re-assign GAEZ into aggregated classes
hti_gaez <- hti_gaez %>% 
  mutate(class_noLimitations = class_2 + class_3 + class_6, # tropic lowlands; sub-humid tropic lowlands; humid topic highlands  
         class_hydromorphic = class_27 + class_28, # land with ample irrigated soils, dominantly hydromorphic soils
         class_terrain = class_25 + class_26, # very steep terrain, land with severe soil/terrain limitations, 
         class_other = class_32 + class_33) %>%  # water and developed, to be dropped
  select(supplier_id, class_noLimitations, class_hydromorphic, class_terrain, class_other)
grid_gaez <- grid_gaez %>% 
  mutate(noLimitations = (class_3_pct + class_6_pct) / 100,
         hydromorphic = (class_27_pct + class_28_pct) / 100,
         terrain = (class_25_pct + class_26_pct) / 100, 
         other = (class_32_pct + class_33_pct)) %>% 
  select(pixel_id, noLimitations, hydromorphic, terrain, other)

# Report out proportions in grouped classes
grid_gaez %>% 
  summary()

# Recalculate removing "other" class (water and developed)
grid_gaez <- grid_gaez %>% 
  mutate(no_other_sum = noLimitations + hydromorphic + terrain,
         noLimitations = noLimitations / no_other_sum,
         hydromorphic = hydromorphic / no_other_sum,
         terrain = terrain / no_other_sum) %>% 
  select(pixel_id, noLimitations, hydromorphic, terrain)

hti_gaez <- hti_gaez %>% 
  pivot_longer(cols = starts_with("class_"),
               names_prefix = "class_",
               names_to = "class",
               values_to = "area_ha")

hti_gaez <- hti_gaez %>% 
  group_by(supplier_id)

# Report out proportions in grouped classes (included in supplement)
hti_gaez %>% 
  group_by(class) %>%
  summarize(area_ha = sum(area_ha, na.rm = TRUE)) %>%
  mutate(prop_area = area_ha / sum(area_ha, na.rm = TRUE)) %>%
  print()

hti_gaez <- hti_gaez %>% 
  filter(class != "other") %>% 
  mutate(share = area_ha / sum(area_ha, na.rm = TRUE))

hti_gaez <- hti_gaez %>% 
  select(-area_ha) %>% 
  pivot_wider(names_from = class, values_from = share) %>%
  left_join(hti_mai, by = "supplier_id") %>% 
  drop_na()

# Model DMAI as a function of GAEZ shares
weighted_mod <- lm(dmai_winsorized ~ 0 + noLimitations + hydromorphic + terrain, data = hti_gaez, weights = ha_y)
summary(weighted_mod)

mod <- lm(dmai_winsorized ~ 0 + noLimitations + hydromorphic + terrain, data = hti_gaez)
summary(mod)

# Predict potential DMAI for all grid cells
grid_pot_mai <- grid_gaez %>% 
  mutate(pot_mai = predict(mod, newdata = grid_gaez),
         pot_mai = ifelse(pot_mai < 0, 0, pot_mai)) %>%  # winsorize negative potential production to 0
  select(pixel_id, pot_mai)

# Join productivity data back to defor_df
defor_df <- defor_df %>% 
  left_join(grid_pot_mai, by = "pixel_id")

# Calculate potential revenues and net revenues
defor_df <- defor_df %>%
  mutate(pot_revenues = (sa_prices_real * pot_mai) / 1000,
         pot_net_revenues = (net_prices * pot_mai) / 1000,
         post_2015 = year > 2015)

# ,
# time_periods = case_when(
#   year <2005 ~ "2001-2005",
#   year >= 2005 & year < 2010 ~ "2005-2010",
#   year >= 2010 & year < 2015 ~ "20120-2015",
#   year >= 2015 & year < 2020 ~ "2015-2020",
#   year >= 2020 ~ "2020-2022")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Estimate elasticity of deforestation  --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mod_1 <- feols(pulp_forest_ha ~ pot_revenues | pixel_id + year, data = defor_df)
summary(mod_1)


null_mod <- feols(pulp_forest_ha ~ 1 | pixel_id + year, data = defor_df)
summary(null_mod)

mod_2 <- feols(pulp_forest_ha ~ post_2015:pot_revenues  | pixel_id + year, data = defor_df)
summary(mod_2)

mod_3 <- feols(pulp_non_forest_ha ~ pot_revenues | pixel_id + year, data = defor_df)
summary(mod_3)

mod_4 <- feols(pulp_non_forest_ha ~ post_2015:pot_revenues | pixel_id + year, data = defor_df)
summary(mod_4)


# Variable labels
coef_map <- c("Potential revenues","Pre-2015", "Post-2015")

# Custom summary table
msummary(
  list(
    "Pulp deforestation" = mod_1,
    "Pulp deforestation" = mod_2,
    "Other pulp expansion" = mod_3,
    "Other pulp expansion" = mod_4
  ),
  output = "html",
  stars = TRUE,
  # coef_map = coef_map,
  gof_omit = "R2|Adj|Within|Pseudo|Log|AIC|BIC"  # remove R2 and Adj R2 and more if desired
)



### Robustness tests
# Add transport costs
rmod <- feols(pulp_forest_ha ~ post_2015:pot_net_revenues | pixel_id + year, data = defor_df)
summary(rmod)

# Add interact between suitability and time trend
rmod <- feols(pulp_forest_ha ~ post_2015:pot_revenues + pot_mai * year  + factor(prov) * year | pixel_id + year, data = defor_df)
summary(rmod)

# Log outcome (but drop 0s)
rmod <- feols(log(pulp_forest_ha) ~ post_2015:pot_revenues  | pixel_id + year, data = defor_df)
summary(rmod)

## TODO: Pickup from here


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# plot basic trends --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# plot sa_prices variable in risi_prices_annual
ggplot(risi_prices_annual %>% filter(year > 2000, year < 2023), aes(x = year, y = sa_prices)) +
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
ggplot(total_pulp_exp %>% filter(year > 2000, year < 2023), aes(x = year, y = pulp_forest_ha)) +
  geom_line() +
  labs(title = "Total Pulp Deforestation by Year",
       x = "Year",
       y = "Pulp Deforestation (ha)") +
  theme_minimal()




#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Interpretation --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# How big of an impact does an increase in prices have?
defor_df$sa_prices_real %>% summary()
defor_df$sa_prices_real %>% sd()
defor_df$pulp_forest_ha %>% summary()
defor_df$pulp_forest_ha %>% sd()
summary(mod_1)
defor_df$sa_prices_real %>% sd() * mod_1$coefficients[1]
defor_df$sa_prices_real %>% sd() * mod_2$coefficients[1]
# Interpretation: A 1 sd increase in prices (87.5 thousand IDR) would result in 
# an extra 78 ha of pulp deforestation in a grid cell. This effect is much 
# smaller in model 2.

## Notes on interpretation of importance of this elasticity:
# Interpret change in R2 - only shifts by ~0.025 relative to model with all fixed effects
r2(mod_1)
r2(null_mod)

# Percent of decline in deforestation between 2011 and 2017 explained by price deviation
defor_cf <- test_df %>% 
  mutate(defor_price_partial = pot_revenues * mod_1$coefficients[1])

# defor_cf <- test_df %>% 
#   mutate(defor_price_partial = ((year <= 2015) * pot_revenues * mod_2$coefficients[1]) + ((year > 2015) * pot_revenues * mod_2$coefficients[2]))

total_pulp_defor <- defor_cf %>%
  group_by(year) %>%
  summarize(sa_prices = mean(sa_prices, na.rm = TRUE),
            sa_prices_dev = mean(sa_prices_dev, na.rm = TRUE),
            pulp_forest_ha_true = sum(pulp_forest_ha, na.rm = TRUE), 
            pulp_forest_ha_cf = sum(defor_price_partial, na.rm = TRUE)) %>% 
  filter(year > 2005) %>%  # Filtering out NA values for years before price deviation
  print()

partial_2017 <- total_pulp_defor %>%  filter(year == 2011) %>%  pull(pulp_forest_ha_true)


- 
  total_pulp_defor %>%
  filter(year == 2008) %>%
  select(pulp_forest_ha_true, pulp_forest_ha_cf) %>% 
  print()


change_2011_2017 <- total_pulp_defor %>%
  filter(year == 2017) %>%
  select(pulp_forest_ha_true, pulp_forest_ha_cf) - 
  total_pulp_defor %>%
  filter(year == 2011) %>%
  select(pulp_forest_ha_true, pulp_forest_ha_cf)) %>% 
  print()


# Percent of increase in deforestation between 2017 and 2022 explained by price deviation
change_2011_2017 <- total_pulp_defor %>%
  filter(year == 2022) %>%
  select(pulp_forest_ha_true, pulp_forest_ha_cf) - 
  total_pulp_defor %>%
  filter(year == 2017) %>%
  select(pulp_forest_ha_true, pulp_forest_ha_cf) %>% 
  print()

# Interpret coefficient - how big of a price increase would you need to see?



mod <- mod_1
summary(mod)
coef = mod$coefficients[1]


defor_cf <- test_df %>% 
  mutate(pulp_forest_ha_pcf = pot_revenues * coef,
         pot_revenues = 0)

defor_cf <- defor_cf %>% 
  mutate(pulp_forest_ha_bl = predict(mod, newdata = test_df),
         pulp_forest_ha_cf = predict(mod, newdata = defor_cf))

# plot total deforestation across pixel_id for each year
total_pulp_exp <- defor_cf %>%
  group_by(year) %>%
  summarize(pulp_forest_ha_true = sum(pulp_forest_ha, na.rm = TRUE), 
            pulp_forest_ha_bl = sum(pulp_forest_ha_bl, na.rm = TRUE),
            pulp_forest_ha_cf = sum(pulp_forest_ha_cf, na.rm = TRUE),
            pulp_forest_ha_pcf = sum(pulp_forest_ha_pcf, na.rm = TRUE))
ggplot(total_pulp_exp %>% filter(year > 2004)) +
  geom_line(aes(x = year, y = pulp_forest_ha_true), color = "black") +
  geom_line(aes(x = year, y = pulp_forest_ha_bl), color = "red") +
  geom_line(aes(x = year, y = pulp_forest_ha_cf), color = "blue") +
  geom_line(aes(x = year, y = pulp_forest_ha_pcf), color = "green") +
  labs(title = "Total Pulp Deforestation by Year",
       x = "Year",
       y = "Pulp Deforestation (ha)") +
  theme_minimal()





mod <- feols(pulp_forest_ha ~ pot_revenues | pixel_id, data = defor_df)
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

