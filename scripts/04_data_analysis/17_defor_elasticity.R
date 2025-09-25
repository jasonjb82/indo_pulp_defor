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
library(patchwork)


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
  select(date,indo_net_price=fp_plp_0045, sa_net_price = fp_plp_0056,nasc_net_price = fp_plp_0053)

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
  summarize(wrq_indo_prices = mean(indonesia, na.rm = TRUE))

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
  summarize(indo_net_price = mean(indo_net_price),
            sa_net_price = mean(sa_net_price),
            nasc_net_price = mean(nasc_net_price)) %>%
  # filter(year <= 2023 & !is.na(risi_monthly_net_price)) %>%
  group_by(year) %>%
  summarize(indo_prices = mean(indo_net_price),
            sa_prices = mean(sa_net_price),
            nasc_prices = mean(nasc_net_price)) %>%  # Note - missing a few observations for SA in 2001
  select(year, sa_prices, indo_prices, nasc_prices)


# Explore similarity to FRED data
fred_prices_annual <- read_csv(paste0(wdir,"/01_data/01_in/tables/WPU0911_annual.csv")) %>%
  mutate(date = as.Date(observation_date,format="%d/%m/%Y"),
         year = year(date)) %>%
  select(year,fred_prices = WPU0911)

risi_prices_annual <- risi_prices_annual %>%
  left_join(fred_prices_annual, by = "year")

wrq_prices <- wrq_prices %>%
  left_join(risi_prices_annual, by = "year")

# # Convert longer SA pulp price series (USD/tonne) into Indonesian pulpwood prices (USD/m3)
# wrq_prices <- wrq_prices %>% 
#   left_join(risi_prices_annual, by = "year")

sa_price_conversion_mod <- lm(wrq_indo_prices ~ sa_prices + 0, data = wrq_prices)
summary(sa_price_conversion_mod)

indo_price_conversion_mod <- lm(wrq_indo_prices ~ indo_prices + 0, data = wrq_prices)
summary(indo_price_conversion_mod)

nasc_price_conversion_mod <- lm(wrq_indo_prices ~ nasc_prices + 0, data = wrq_prices)
summary(nasc_price_conversion_mod)

fred_price_conversion_mod <- lm(wrq_indo_prices ~ fred_prices + 0, data = wrq_prices)
summary(fred_price_conversion_mod)

risi_prices_annual <- risi_prices_annual %>%
  mutate(sa_prices = predict(sa_price_conversion_mod, newdata = risi_prices_annual),
         fred_prices = predict(fred_price_conversion_mod, newdata = risi_prices_annual),
         indo_prices = predict(indo_price_conversion_mod, newdata = risi_prices_annual),
         nasc_prices = predict(nasc_price_conversion_mod, newdata = risi_prices_annual))

# Add Ind RISI prices to price series
risi_prices_annual <- risi_prices_annual %>% 
  left_join(wrq_prices %>% select(year, wrq_prices = wrq_indo_prices), by = "year")

# Convert currency
risi_prices_annual <- risi_prices_annual %>%
  left_join(fred_idr_usd, by = "year") %>%
  mutate(sa_prices_idr = sa_prices * idr_usd / 1000000,  # Convert from USD to million IDR
         indo_prices_idr = indo_prices * idr_usd / 1000000,
         nasc_prices_idr = nasc_prices * idr_usd / 1000000,
         fred_prices_idr = fred_prices * idr_usd / 1000000,
         wrq_prices_idr = wrq_prices * idr_usd / 1000000)  

# Adjust for inflation
risi_prices_annual <- risi_prices_annual %>%
  left_join(fred_idn_cpi, by = "year") %>%
  mutate(sa_prices_real = sa_prices_idr / idn_cpi * 100, # Adjust for inflation - reference year is 2015
         indo_prices_real = indo_prices_idr / idn_cpi * 100, 
         nasc_prices_real = nasc_prices_idr / idn_cpi * 100, 
         fred_prices_real = fred_prices_idr / idn_cpi * 100, 
         wrq_prices_real = wrq_prices_idr / idn_cpi * 100,
         sa_prices_dev = (sa_prices - zoo::rollmean(sa_prices, k = 5, fill = NA, align = "right")) / 1000) # Deviation in 1000 USD

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
  mutate(pot_revenues = (sa_prices_real * pot_mai),
         pot_revenues_indo = (indo_prices_real * pot_mai),
         pot_revenues_nasc = (nasc_prices_real * pot_mai),
         pot_revenues_fred = (fred_prices_real * pot_mai),
         pot_revenues_wrq = (wrq_prices_real * pot_mai),
         pot_revenues_net = (net_prices * pot_mai),
         pot_revenues_dev = (sa_prices_dev * pot_mai),
         post_2015 = year > 2015)

# ,
#          time_periods = case_when(
#             year < 2010 ~ "2000-2010",
#             year >= 2010 & year < 2017 ~ "2010-2017",
#             year >= 2017 & year < 2022 ~ "2017-2022"))


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Estimate elasticity of deforestation  --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# mod_1 <- feols(pulp_forest_ha ~ pot_revenues + pot_mai * year  | pixel_id + year + prov[year], data = defor_df)
# summary(mod_1)
# 
# null_mod <- feols(pulp_forest_ha ~ 1 + pot_mai * year  + factor(prov) * year | pixel_id + year, data = defor_df)
# summary(null_mod)
# 
# mod_2 <- feols(pulp_forest_ha ~ pot_revenues:post_2015  + pot_mai * year | pixel_id + year + prov[year], data = defor_df)
# summary(mod_2)
# 
# mod_3 <- feols(pulp_non_forest_ha ~ pot_revenues  + pot_mai * year  | pixel_id + year + prov[year], data = defor_df)
# summary(mod_3)
# 
# mod_4 <- feols(pulp_non_forest_ha ~ pot_revenues:post_2015  + pot_mai * year  | pixel_id + year + prov[year] , data = defor_df)
# summary(mod_4)


mod_1 <- feols(pulp_forest_ha ~ pot_revenues | pixel_id + year, data = defor_df)
summary(mod_1)

null_mod <- feols(pulp_forest_ha ~ 1 | pixel_id + year, data = defor_df)
summary(null_mod)

mod_2 <- feols(pulp_forest_ha ~ post_2015:pot_revenues | pixel_id + year, data = defor_df)
summary(mod_2)

mod_3 <- feols(pulp_non_forest_ha ~ pot_revenues  | pixel_id + year, data = defor_df)
summary(mod_3)

mod_4 <- feols(pulp_non_forest_ha ~ post_2015:pot_revenues | pixel_id + year, data = defor_df)
summary(mod_4)


# Variable labels
rows <- tribble(~term, ~a,  ~b, ~c,  ~d,
                'Productivity time trends', 'X',   'X', 'X', 'X',
                'Province time trends', 'X',   'X', 'X', 'X')
attr(rows, 'position') <- c(10, 11)

# Custom summary table
msummary(
  list("Pulp deforestation" = list("(1)" = mod_1, "(2)" = mod_2),
       "Other pulp expansion" = list("(3)" = mod_3, "(4)" = mod_4)),
  # output = "html",
  output = paste0(wdir, "/01_data/04_results/defor_elast_main.docx"),
  stars = c('*' = .1, '**' = .05, '***' = .01) ,
  # coef_map = coef_map,
  coef_omit = "^(?!.*revenues)",
  coef_rename = c("pot_revenues" = "Potential revenues",
                  "post_2015FALSE" = "y<=2015",
                  "post_2015TRUE" = "y>2015"),
  gof_omit = "R2|Adj|Within|Pseudo|Log|AIC|BIC|RMSE",  # remove R2 and Adj R2 and more if desired
  shape = "cbind"
  # add_rows = rows
)



### Robustness tests
# # Add transport costs
# defor_df <- defor_df %>% mutate(pot_revenues_r = pot_revenues_net)
# rmod_1 <- feols(pulp_forest_ha ~ pot_revenues_r:post_2015 | pixel_id + year, data = defor_df)
# summary(rmod_1)

# Add suitability time trend
defor_df <- defor_df %>% mutate(pot_revenues_r = pot_revenues)
rmod_1 <- feols(pulp_forest_ha ~ pot_revenues_r:post_2015 + pot_mai * year  | pixel_id + year, data = defor_df)
summary(rmod_1)

# Use Indonesian pulpwood price series instead of SA
defor_df <- defor_df %>% mutate(pot_revenues_r = pot_revenues_indo)
rmod_2 <- feols(pulp_forest_ha ~ pot_revenues_r:post_2015 | pixel_id + year, data = defor_df)
summary(rmod_2)

# # Use FRED price series
# defor_df <- defor_df %>% mutate(pot_revenues_r = pot_revenues_fred)
# rmod_4 <- feols(pulp_forest_ha ~ pot_revenues_r:post_2015 | pixel_id + year, data = defor_df)
# summary(rmod_4)

# Price deviation
defor_df <- defor_df %>% mutate(pot_revenues_r = pot_revenues_dev)
rmod_3 <- feols(pulp_forest_ha ~ pot_revenues_r:post_2015 | pixel_id + year, data = defor_df)
summary(rmod_3)

# Lagged rents
defor_df <- defor_df %>% 
  group_by(pixel_id) %>% 
  arrange(pixel_id, year) %>% 
  mutate(pot_revenues_r = lag(pot_revenues))
rmod_4 <- feols(pulp_forest_ha ~ pot_revenues_r:post_2015 | pixel_id + year, data = defor_df)
summary(rmod_4)

# IHS outcome
defor_df <- defor_df %>% 
  mutate(asinh_pulp_forest_ha = asinh(pulp_forest_ha))
defor_df <- defor_df %>% mutate(pot_revenues_r = pot_revenues)
rmod_5 <- feols(asinh_pulp_forest_ha ~ pot_revenues_r:post_2015  | pixel_id + year, data = defor_df)
summary(rmod_5)


# Variable labels
rows <- tribble(~term, ~a,  ~b, ~c,  ~d, ~e,
                'Productivity time trends', 'X',   'X', 'X', 'X', 'X',
                'Province time trends', 'X',   'X', 'X', 'X', 'X')
attr(rows, 'position') <- c(10, 11)


# Custom summary table
msummary(
  list(rmod_1, rmod_2, rmod_3, rmod_4, rmod_5),
  # list("Pulp deforestation" = list("(1)" = mod_1, "(2)" = mod_2),
  #      "Other pulp expansion" = list("(3)" = mod_3, "(4)" = mod_4)),
  # output = "html",
  output = paste0(wdir, "/01_data/04_results/defor_elast_robust.docx"),
  stars = c('*' = .1, '**' = .05, '***' = .01) ,
  # coef_map = coef_map,
  coef_omit = "^(?!.*revenues)",
  coef_rename = c("pot_revenues_r" = "Potential revenues",
                  "post_2015FALSE" = "y<=2015",
                  "post_2015TRUE" = "y>2015"),
  gof_omit = "R2|Adj|Within|Pseudo|Log|AIC|BIC|RMSE"  # remove R2 and Adj R2 and more if desired
  # shape = "cbind",
  # add_rows = rows
)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# plot basic trends --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# plot sa_prices variable in risi_prices_annual
price_plot <- ggplot(risi_prices_annual %>% filter(year > 2000, year < 2023), aes(x = year, y = sa_prices_real)) +
  geom_line() +
  labs(title = "RISI South America Pulp Prices",
       x = "Year",
       y = "Price (USD)") +
  theme_minimal()

# plot total deforestation across pixel_id for each year
total_pulp_exp <- defor_df %>%
  group_by(year) %>%
  summarize(pulp_forest_ha = sum(pulp_forest_ha, na.rm = TRUE) / 1000,
            pulp_non_forest_ha = sum(pulp_non_forest_ha, na.rm = TRUE),
            pulp_exp_ha = sum(pulp_exp_ha, na.rm = TRUE),
            pot_revenues = mean(pot_revenues, na.rm = TRUE))
rev_plot <- ggplot(total_pulp_exp %>% filter(year > 2000, year < 2023), aes(x = year, y = pot_revenues)) +
  geom_line() +
  labs(title = "Potential returns to pulpwood production",
       x = "Year",
       y = "Value (Million IDR)") +
  theme_minimal() +
  ylim(0, 12)


defor_plot <- ggplot(total_pulp_exp %>% filter(year > 2000, year < 2023), aes(x = year, y = pulp_forest_ha)) +
  geom_line() +
  labs(title = "Total Pulp Deforestation by Year",
       x = "Year",
       y = "Pulp-driven deforestation (thousand ha)") +
  theme_minimal()
rev_plot / defor_plot



#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Interpretation --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# How big of an impact does an increase in prices have?
# every 1000 IDR increase in potential returns (XX% increase relative to mean) 
# leads to an XX hectare increase in pulp-driven deforestation (XX% increase relative to mean)
1 / (defor_df$pot_revenues %>% mean())
mod_1$coefficients[1]
mod_1$coefficients[1] / (defor_df$pulp_forest_ha %>% mean())


## Notes on interpretation of importance of this elasticity:
# Interpret change in R2 - tiny shift relative to model with all fixed effects
r2(mod_1)
r2(null_mod)

# Percent of decline in deforestation between 2011 and 2017 explained by price deviation
# Price change
pot_returns_2011 <- defor_df %>% 
  filter(year == 2011) %>% 
  pull(pot_revenues) %>% 
  mean() %>% 
  print()
pot_returns_2016 <- defor_df %>% 
  filter(year == 2016) %>% 
  pull(pot_revenues) %>% 
  mean() %>% 
  print()
pot_returns_2022 <- defor_df %>% 
  filter(year == 2022) %>% 
  pull(pot_revenues) %>% 
  mean()



defor_cf <- defor_df %>% 
  mutate(defor_price_partial = pot_revenues * mod_1$coefficients[1])

# defor_cf <- test_df %>% 
#   mutate(defor_price_partial = ((year <= 2015) * pot_revenues * mod_2$coefficients[1]) + ((year > 2015) * pot_revenues * mod_2$coefficients[2]))

total_pulp_defor <- defor_cf %>%
  group_by(year) %>%
  summarize(pot_revenues = mean(pot_revenues, na.rm = TRUE),
            pulp_forest_ha_true = sum(pulp_forest_ha, na.rm = TRUE) / 1000, 
            pulp_forest_ha_cf = sum(defor_price_partial, na.rm = TRUE) / 1000) %>% 
  print()

defor_plot <- ggplot(total_pulp_defor %>% filter(year > 2000, year < 2023), aes(x = year)) +
  geom_line(aes(y = pulp_forest_ha_true)) +
  geom_line(aes(y = pulp_forest_ha_cf), linetype = 2) +
  labs(x = "Year",
       y = "Pulp-driven deforestation (thousand ha)") +
  theme_minimal(base_size = 18) +
  annotate("text", x = 2020, y = 105, label = "Deforestation as predicted\nby price variation", color = "black") +
  annotate("text", x = 2018, y = 17, label = "Observed deforestation", color = "black")
defor_plot

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

