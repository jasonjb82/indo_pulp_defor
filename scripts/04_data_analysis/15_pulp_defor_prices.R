## ---------------------------------------------------------
## 
## Project: Indonesia pulp deforestation
##
## Purpose of script: Regressions of interannual changes in pulp-driven deforestation 
##                    against changes in global pulp prices
##
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Date Created: 2025-01-29
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
library(WDI)
# library(multcomp)



# define working data directory
wdir <- "remote"

## read data -------------------------------------------------------------------

# deforestation
hti_nonhti_conv <- read_csv(paste0(wdir,"\\01_data\\02_out\\tables\\idn_deforestation_hti_nonhti_treemap.csv"))

# pulp expansion
pulp_exp <- read_csv(paste0(wdir, '/01_data/01_in/gaveau/pulp_expansion.csv')) %>% 
  clean_names() %>% 
  mutate(year = year + 2000)

# Bleached Hardwood Kraft, Acacia, from Indonesia (net price) from RISI
risi_prices <- readxl::read_excel(paste0(wdir,"\\01_data\\01_in\\wwi\\Fastmarkets_2025_01_14-103617.xlsx"),skip=4) %>%
  clean_names() %>% 
  select(date,net_price=mid_3, sa_net_price = mid_2)

# pulp prices (PPI) (FRED)
fred_prices_annual <- read_csv(paste0(wdir,"\\01_data\\01_in\\tables\\WPU0911_annual.csv")) %>%
  mutate(date = as.Date(observation_date,format="%d/%m/%Y"),
         year = year(date)) %>%
  select(year,prices = WPU0911) 

# Fetch CPI data for US (indicator: "FP.CPI.TOTL")
cpi_data <- WDI(country = "US", indicator = "FP.CPI.TOTL", start = 2001, end = 2025)

# select required columns
cpi_data <- cpi_data %>%
  select(year,cpi=FP.CPI.TOTL)

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
            sa_prices = mean(sa_net_price)) # Note - missing a few observations for SA in 2001

# clean data -------------------------------------------------------------------

adjusted_data <- merge(fred_prices_annual, cpi_data, by = "year") %>% select(year,prices,cpi)
adjusted_data <- merge(risi_prices_annual, adjusted_data, by = "year")

# set base year
base_year <- 2023

# get CPI for base year
base_cpi <- adjusted_data$cpi[adjusted_data$year == base_year]

# adjust prices
adjusted_data$indo_real_price <- adjusted_data$indo_prices * (base_cpi / adjusted_data$cpi)
adjusted_data$sa_real_price <- adjusted_data$sa_prices * (base_cpi / adjusted_data$cpi)

ggplot(adjusted_data, aes(x = year)) +
  geom_line(aes(y = prices, color = "Nominal Price\n(FRED PPI)"), size = 1) +
  geom_line(aes(y = sa_real_price, color = "Real Price"), size = 1) +
  labs(title = "Commodity Prices Adjusted for Inflation",
       y = "Price",
       color = "") +
  #theme_minimal() +
  scale_color_manual(values = c("blue", "red"))

# annualize pulp deforestation
pulp_deforestation <- hti_nonhti_conv %>% 
  group_by(year) %>%
  filter(conv_type == 2) %>%
  summarize(defor_ha = sum(area_ha)) %>%
  print()

# merge datasets ---------------------------------------------------------------

pulp_defor_prices <- adjusted_data %>%
  as_tibble() %>%
  left_join(pulp_deforestation,by="year") %>%
  select(year,defor_ha,sa_real_price, prices) %>%
  print()

pulp_exp_prices <- adjusted_data %>% 
  as_tibble() %>% 
  left_join(pulp_exp, by = "year") %>% 
  select(year, new_pulp = area_of_industrial_oil_palm_pw_added_each_year_ha, pulp_defor = area_of_forest_converted_to_pulpwood_pw_each_year_ha, pulp_nodefor = non_forest_to_pulpwood, sa_real_price, prices) %>% 
  print()


# look at fred ~ defore relationship -------------------------------------------
# pulp_defor_fred <- pulp_deforestation %>% 
#   left_join(fred_prices_annual, by = "year")
# 
# pulp_defor_fred <- pulp_defor_fred %>% 
#   mutate(
#     period = case_when(
#       year <= 2011 ~ "p1",
#       year <= 2017 ~ "p2",
#       .default = "p3"
#     ),
#     ln_defor = log(defor_ha), 
#     # ln_real_price = log(real_price),
#     ln_fred_price = log(prices),
#     p1 = year <=2011
#   )

pulp_defor_prices <- pulp_defor_prices %>% 
  mutate(
    period = case_when(
      year <= 2011 ~ "p1",
      year <= 2017 ~ "p2",
      .default = "p3"
    ),
    ln_defor = log(defor_ha), 
    ln_real_price = log(sa_real_price),
    ln_fred_price = log(prices),
    p1 = year <=2011
  )

pulp_defor_fred %>% 
  ggplot(aes(x = year)) +
  geom_line(aes(y = prices), color = "blue")

pulp_defor_fred %>% 
  ggplot(aes(x = year)) +
  geom_line(aes(y = defor_ha), color = "red")

mod <- lm(ln_defor ~ ln_real_price, data = pulp_defor_prices)
summary(mod)

mod <- lm(ln_defor ~ ln_real_price, data = pulp_defor_prices %>% filter(period == "p1"))
summary(mod)

mod <- lm(ln_defor ~ ln_real_price, data = pulp_defor_prices %>% filter(period == "p2"))
summary(mod)

mod <- lm(ln_defor ~ ln_real_price, data = pulp_defor_prices %>% filter(period == "p3"))
summary(mod)

mod <- lm(ln_defor ~ ln_real_price + period, data = pulp_defor_prices)
summary(mod)

mod <- lm(ln_defor ~ ln_real_price * period, data = pulp_defor_prices)
summary(mod)

# summary(glht(mod, linfct = c("ln_fred_price + ln_fred_price:p1TRUE = 0")))


pulp_exp_prices <- pulp_exp_prices %>% 
  mutate(
    period = case_when(
      year <= 2011 ~ "p1",
      year <= 2017 ~ "p2",
      .default = "p3"
    ),
    ln_defor = log(pulp_defor),
    ln_new_pulp = log(new_pulp),
    ln_nodefor = log(pulp_nodefor),
    ln_real_price = log(sa_real_price),
    ln_fred_price = log(prices),
    p1 = year <=2011,
    ln_real_price_l1 = lag(ln_real_price)
  )


pulp_exp_prices %>% 
  group_by(period) %>% 
  summarize(mean_price = mean(sa_real_price))

mod <- lm(ln_defor ~ ln_real_price, data = pulp_exp_prices)
summary(mod)

mod <- lm(ln_new_pulp ~ ln_real_price, data = pulp_exp_prices)
summary(mod)

mod <- lm(ln_nodefor ~ ln_real_price, data = pulp_exp_prices)
summary(mod)

mod <- lm(ln_defor ~ ln_real_price:period + period, data = pulp_exp_prices)
summary(mod)

mod <- lm(ln_new_pulp ~ ln_real_price:period + period, data = pulp_exp_prices)
summary(mod)

mod <- lm(ln_nodefor ~ ln_real_price:period + period, data = pulp_exp_prices)
summary(mod)


mod <- lm(ln_defor ~ ln_real_price + lag(ln_real_price) + lag(ln_real_price, 2) + period, data = pulp_exp_prices)
summary(mod)

mod <- lm(ln_defor ~ ln_real_price + period, data = pulp_exp_prices)
summary(mod)


mod <- lm(ln_new_pulp ~ ln_real_price + period, data = pulp_exp_prices)
summary(mod)

mod <- lm(ln_nodefor ~ ln_real_price + period, data = pulp_exp_prices)
summary(mod)



mod <- lm(ln_defor ~ ln_real_price, data = pulp_exp_prices %>% filter(year < 2011))
summary(mod)

mod <- lm(ln_new_pulp ~ ln_real_price, data = pulp_exp_prices %>% filter(year < 2011))
summary(mod)

mod <- lm(ln_nodefor ~ ln_real_price, data = pulp_exp_prices %>% filter(year < 2011))
summary(mod)


mod <- lm(ln_defor ~ ln_real_price, data = pulp_exp_prices %>% filter(year >= 2011))
summary(mod)

mod <- lm(ln_new_pulp ~ ln_real_price, data = pulp_exp_prices %>% filter(year >= 2011))
summary(mod)

mod <- lm(ln_nodefor ~ ln_real_price, data = pulp_exp_prices %>% filter(year >= 2011))
summary(mod)





pulp_exp_prices %>% 
  ggplot(aes(x = year, y = sa_real_price)) +
  geom_line() +
  ylim(0, 1100)
pulp_exp_prices %>% 
  ggplot(aes(x = year, y = pulp_defor)) +
  geom_line()
