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

# Transport costs (robustness test)
# TODO: confirm with jason on the units of the transport cost data
trnsprt_cst_df <- read_csv(paste0(wdir, "/01_data/02_out/tables/centroids_mills_cost.csv")) %>% 
  rename(pixel_id = id)

# Data about grid cell composition along GAEZ classes
grid_gaez <- read_csv(paste0(wdir, "/01_data/02_out/tables/gaez_grid_share.csv"))

# Data about hti composition along GAEZ classes
hti_gaez <- read_csv(paste0(wdir, "/01_data/02_out/tables/gaez_hti_areas.csv")) %>% 
  select(-total_area_ha, supplier_id = ID)

# Data about hti productivity (produced in R script 08_calc_mai.R)
hti_mai <- read_csv(paste0(wdir, "/01_data/02_out/tables/hti_mai.csv"))

# Add administrative labels
grid_admin <- read_csv(paste0(wdir, "/01_data/02_out/tables/grid_10km_adm_prov_kab.csv"))
test_df <- test_df %>%
  left_join(grid_admin, by = "pixel_id")

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


# Convert longer SA pulp price series (USD/tonne) into Indonesian pulpwood prices (USD/m3)
wrq_prices <- wrq_prices %>% 
  left_join(risi_prices_annual, by = "year")

price_conversion_mod <- lm(wrq_indo_prices ~ sa_prices + 0, data = wrq_prices)
summary(price_conversion_mod)

risi_prices_annual <- risi_prices_annual %>%
  mutate(sa_prices = predict(price_conversion_mod, newdata = risi_prices_annual))


# Convert currency?
risi_prices_annual <- risi_prices_annual %>%
  left_join(fred_idr_usd, by = "year") %>%
  mutate(sa_prices_idr = sa_prices * idr_usd / 1000)  # Convert from USD to thousand IDR


# Adjust for inflation
risi_prices_annual <- risi_prices_annual %>%
  left_join(fred_idn_cpi, by = "year") %>%
  mutate(sa_prices_real = sa_prices_idr / idn_cpi * 100) # Adjust for inflation - reference year is 2015


# calculate deviation in pulp prices from last five year rolling average
risi_prices_annual <- risi_prices_annual %>%
  mutate(sa_prices_dev = sa_prices_real - zoo::rollmean(sa_prices_real, k = 5, fill = NA, align = "right")) %>%
  filter(!is.na(sa_prices))

# Add RISI prices to defor_df
defor_df <- defor_df %>%
  left_join(risi_prices_annual, by = "year")

# Calculate interaction between prices and transport costs
# defor_df <- defor_df %>%
#   mutate(plant_prices_dev = sa_prices_dev  * (1/cost_usd_perton))
defor_df <- defor_df %>%
  mutate(net_prices = sa_prices_real - cost_usd_perton,
         net_prices_dev = net_prices - zoo::rollmean(net_prices, k = 5, fill = NA, align = "right"))





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
# integrate productivity variations  --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
grid_gaez <- grid_gaez %>% 
  mutate(noLimitations = (class_3_pct + class_6_pct) / 100,
         hydromorphic = (class_27_pct + class_28_pct) / 100,
         terrain = (class_25_pct + class_26_pct) / 100, 
         other = (class_32_pct + class_33_pct)) %>% 
  select(pixel_id, noLimitations, hydromorphic, terrain, other)
# TODO: add check that we're getting 100% areas

# Report out proportions in grouped classes
grid_gaez %>% 
  summary()

# Recalculate removing other class (water and developed)
grid_gaez <- grid_gaez %>% 
  mutate(no_other_sum = noLimitations + hydromorphic + terrain,
         noLimitations = noLimitations / no_other_sum,
         hydromorphic = hydromorphic / no_other_sum,
         terrain = terrain / no_other_sum) %>% 
  select(pixel_id, noLimitations, hydromorphic, terrain)



hti_gaez <- hti_gaez %>% 
  mutate(class_noLimitations = class_2 + class_3 + class_6, # tropic lowlands; sub-humid tropic lowlands; humid topic highlands  
         class_hydromorphic = class_27 + class_28, # land with ample irrigated soils, dominantly hydromorphic soils
         class_terrain = class_25 + class_26, # very steep terrain, land with severe soil/terrain limitations, 
         class_other = class_32 + class_33) %>%  # water and developed, to be dropped
  select(supplier_id, class_noLimitations, class_hydromorphic, class_terrain, class_other)

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


weighted_mod <- lm(dmai_winsorized ~ 0 + noLimitations + hydromorphic + terrain, data = hti_gaez, weights = ha_y)
summary(weighted_mod)

mod <- lm(dmai_winsorized ~ 0 + noLimitations + hydromorphic + terrain, data = hti_gaez)
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
         pot_net_revenues = (net_prices * pot_mai) / 1000,
         pot_revenues_ndev = sa_prices_real * pot_mai / 1000)
test_df <- test_df %>% 
  mutate(post_2015 = year > 2015,
         time_period = ifelse(year<=2011, "p1", 
                              ifelse(year<=2018, "p2", "p3")))



# mod_1 <- feols(pulp_forest_ha ~ pot_revenues | year + pixel_id, data = test_df, vcov = "twoway")
# summary(mod_1)
# 
# mod_2 <- feols(pulp_forest_ha ~ post_2015:pot_revenues | year + pixel_id, data = test_df, vcov = "twoway")
# summary(mod_3)
# 
# mod_3 <- feols(pulp_non_forest_ha ~ pot_revenues | year + pixel_id, data = test_df, vcov = "twoway")
# summary(mod_2)
# 
# mod_4 <- feols(pulp_non_forest_ha ~ post_2015:pot_revenues | year + pixel_id, data = test_df, vcov = "twoway")
# summary(mod_4)

mod_1 <- feols(pulp_forest_ha ~ pot_revenues_ndev | pixel_id + year, data = test_df)
summary(mod_1)

## Notes on interpretation of importance of this elasticity:
# Interpret change in R2 - only shifts by ~0.025 relative to model with all fixed effects
# Percent of decline in deforestation between XX and XX explained by price deviation
# Percent of increase in deforestation between XX and XX explained by price deviation
# Interpret coefficient - how big of a price increase would you need to see?
null_mod <- feols(pulp_forest_ha ~ 1 | pixel_id + year, data = test_df)
summary(null_mod)

mod_2 <- feols(pulp_forest_ha ~ post_2015:pot_revenues_ndev | pixel_id + year, data = test_df)
summary(mod_2)

mod_3 <- feols(pulp_non_forest_ha ~ pot_revenues_ndev | pixel_id + year, data = test_df)
summary(mod_3)

mod_4 <- feols(pulp_non_forest_ha ~ post_2015:pot_revenues_ndev | pixel_id + year, data = test_df)
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
rmod <- feols(pulp_forest_ha ~ post_2015:pot_net_revenues  | pixel_id + year, data = test_df)
summary(rmod)

# Use raw prices instead of price deviation
rmod <- feols(pulp_forest_ha ~ post_2015:pot_revenues_ndev  | pixel_id + year, data = test_df)
summary(rmod)

# Add province time trends
rmod <- feols(pulp_forest_ha ~ post_2015:pot_revenues + factor(prov) * year | pixel_id + year, data = test_df)
summary(rmod)

# Add interact between suitability and time trend
rmod <- feols(pulp_forest_ha ~ post_2015:pot_revenues + pot_mai * year  + factor(prov) * year | pixel_id + year, data = test_df)
summary(rmod)

# Deforestation as a share?

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

