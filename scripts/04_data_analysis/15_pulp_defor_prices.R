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


## read data -------------------------------------------------------------------

# deforestation
hti_nonhti_conv <- read_csv(paste0(wdir,"\\01_data\\02_out\\tables\\idn_deforestation_hti_nonhti_treemap.csv"))

# pulp prices (PPI) (FRED)
fred_prices_annual <- read_csv(paste0(wdir,"\\01_data\\01_in\\tables\\WPU0911_annual.csv")) %>%
  mutate(date = as.Date(observation_date,format="%d/%m/%Y"),
         year = year(date)) %>%
  select(year,prices = WPU0911) 

# Fetch CPI data for Indonesia (indicator: "FP.CPI.TOTL")
cpi_data <- WDI(country = "ID", indicator = "FP.CPI.TOTL", start = 2001, end = 2025)

# select required columns
cpi_data <- cpi_data %>%
  select(year,cpi=FP.CPI.TOTL)

#risi_prices_annual <- risi_prices_clean %>%
#  group_by(year) %>%
#  summarize(prices = mean(risi_monthly_net_price))

# clean data -------------------------------------------------------------------

adjusted_data <- merge(fred_prices_annual, cpi_data, by = "year") %>% select(year,prices,cpi)
#adjusted_data <- merge(risi_prices_annual, cpi_data, by = "year")

# set base year
base_year <- 2023

# get CPI for base year
base_cpi <- adjusted_data$cpi[adjusted_data$year == base_year]

# adjust prices
adjusted_data$real_price <- adjusted_data$prices * (base_cpi / adjusted_data$cpi)

ggplot(adjusted_data, aes(x = year)) +
  geom_line(aes(y = prices, color = "Nominal Price\n(FRED PPI)"), size = 1) +
  geom_line(aes(y = real_price, color = "Real Price"), size = 1) +
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
  select(year,defor_ha,real_price) %>%
  print()
