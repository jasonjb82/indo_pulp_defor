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

# plot sa_prices variable in risi_prices_annual
ggplot(risi_prices_annual, aes(x = year, y = sa_prices)) +
  geom_line() +
  labs(title = "RISI South America Pulp Prices",
       x = "Year",
       y = "Price (USD)") +
  theme_minimal()

# Add RISI prices to defor_df
defor_df <- defor_df %>%
  left_join(risi_prices_annual, by = "year")

# Calculate plantation-gate prices
defor_df <- defor_df %>%
  mutate(plant_prices_dev = sa_prices_dev  * (1/cost_usd_perton))



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

# Look at variation explained by price series
mod <- feols(log(pulp_forest_ha) ~ sa_prices_dev | pixel_id, data = defor_df)
summary(mod)
mod <- feols(log(pulp_exp_ha) ~ sa_prices_dev | pixel_id, data = defor_df)
summary(mod)

# Look at variation explained by cross-sectional variation in transport costs
mod <- feols(log(pulp_forest_ha) ~ cost_usd_perton | year, data = defor_df)
summary(mod)
mod <- feols(log(pulp_exp_ha) ~ cost_usd_perton | year, data = defor_df)
summary(mod)




mod <- feols(pulp_forest_ha ~ plant_prices | year + pixel_id, data = defor_df)
summary(mod)


mod <- feols(pulp_forest_ha ~ plant_prices | pixel_id, data = defor_df)
summary(mod)


mod <- feols(log(pulp_forest_ha) ~ plant_prices | pixel_id, data = defor_df)
summary(mod)

