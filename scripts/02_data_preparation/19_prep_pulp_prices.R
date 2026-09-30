## ---------------------------------------------------------
## 
## Project: Indonesia pulp deforestation
##
## Purpose of script: Build the annual pulp / pulpwood price series used by the
##    deforestation elasticity analysis, and write it out as a derived table.
##
## Author: Robert Heilmayr
##
## Date Created: 2026-09-29
## 
## ---------------------------------------------------------
##
## Notes: This script exists so that the elasticity analysis can be replicated
##    without the licensed price data. Fastmarkets (RISI) pulp prices and WRQ
##    pulpwood prices are commercially licensed and cannot be redistributed, so
##    they are read only here. The output is a derived annual series in
##    constant 2015 IDR from which the raw licensed levels are not published.
##
##    Run this before scripts/03_analysis_modelling/03_defor_elasticity.R.
##
##        Input datasets
##        1) 01_in/wwi/Fastmarkets_2025_01_14-103617.xlsx: monthly bleached
##               hardwood kraft pulp prices, nominal USD per tonne, for
##               Indonesia and South America. LICENSED - do not redistribute.
##               The Indonesian series has no observations before May 2001.
##        2) 01_in/wwi/WRQ_pulpwood_prices.xlsx: quarterly pulpwood prices,
##               USD per m3. LICENSED - do not redistribute. Used only to
##               rescale pulp prices into pulpwood-equivalent units.
##        3) 01_in/tables/idr_usd_annual_worldbank.csv: IDR per USD, annual
##               period average (World Bank PA.NUS.FCRF), 2000-2024.
##        4) 01_in/tables/FRED_IDNCPIALLAINMEI.csv: Indonesian consumer price
##               index, 2015 = 100.
##
##        Output
##        1) Annual pulp and pulpwood price series: one row per year, holding
##               only the derived columns the analysis consumes. Written to
##               02_out/tables/pulp_prices_annual_2001_2024.csv
##               Read by scripts/03_analysis_modelling/03_defor_elasticity.R
##
## ---------------------------------------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------------------------------------

### Load packages
library(tidyverse)
library(janitor)

## set working directory -------------------------------------

wdir <- "remote"

## read data -------------------------------------------------

# Bleached Hardwood Kraft, Acacia, from Indonesia (net price) and South America from RISI
# Measured in nominal USD / tonnes of BHKP
risi_prices <- readxl::read_excel(
  paste0(wdir, "/01_data/01_in/wwi/Fastmarkets_2025_01_14-103617.xlsx"),
  skip = 4
) %>%
  clean_names() %>%
  select(
    date,
    indo_net_price = fp_plp_0045,
    sa_net_price = fp_plp_0056
  )

# WRQ data on pulpwood prices (USD/m3).
# Used to convert global interannual variation in pulp prices (RISI data) into
# local pulpwood prices to improve interpretation
wrq_prices <- readxl::read_excel(paste0(
  wdir,
  "/01_data/01_in/wwi/WRQ_pulpwood_prices.xlsx"
)) %>%
  clean_names() %>%
  drop_na() %>%
  group_by(year)
keep_years <- wrq_prices %>%
  tally() %>%
  filter(n == 4) %>%
  pull(year)
wrq_prices <- wrq_prices %>%
  filter(year %in% keep_years) %>%
  summarize(wrq_indo_prices = mean(indonesia, na.rm = TRUE))

# IDR to USD exchange rate, used to convert global prices to local currency.
# World Bank official rate (PA.NUS.FCRF, LCU per US$, period average), covering
# 2000-2024.
idr_usd_annual <- read_csv(paste0(
  wdir,
  "/01_data/01_in/tables/idr_usd_annual_worldbank.csv"
)) %>%
  filter(year > 1999)

# FRED data on Indonesian CPI (to adjust for inflation, reference year = 2015)
fred_idn_cpi <- read_csv(paste0(
  wdir,
  "/01_data/01_in/tables/FRED_IDNCPIALLAINMEI.csv"
)) %>%
  mutate(
    date = as.Date(observation_date, format = "%d/%m/%Y"),
    year = year(date)
  ) %>%
  select(year, idn_cpi = IDNCPIALLAINMEI)

## clean price data -------------------------------------------

# Clean RISI annual pulp prices
risi_prices_annual <- risi_prices %>%
  mutate(
    date = as.Date(date, format = "%m/%d/%Y"),
    year = year(date),
    month = month(date)
  ) %>%
  group_by(year, month) %>%
  summarize(
    indo_net_price = mean(indo_net_price),
    sa_net_price = mean(sa_net_price)
  ) %>%
  group_by(year) %>%
  summarize(
    indo_prices = mean(indo_net_price),
    sa_prices = mean(sa_net_price)
  ) %>%
  # No na.rm here: a year missing any month yields NA and drops out. This is
  # what removes 2001 from the Indonesian series, which has no 2001 data at all.
  select(year, sa_prices, indo_prices) %>%
  filter(!is.na(year)) # drop the empty row produced by undated source rows

# Convert global (or indonesian) pulp prices (RISI) into
# local pulpwood-equivalent prices (WRQ).
# Just a constant multiplicative conversion - designed to improve interpretability
wrq_prices <- wrq_prices %>%
  left_join(risi_prices_annual, by = "year")

sa_price_conversion_mod <- lm(
  wrq_indo_prices ~ sa_prices + 0,
  data = wrq_prices
)
summary(sa_price_conversion_mod)

indo_price_conversion_mod <- lm(
  wrq_indo_prices ~ indo_prices + 0,
  data = wrq_prices
)
summary(indo_price_conversion_mod)

risi_prices_annual <- risi_prices_annual %>%
  mutate(
    sa_prices_remap = predict(
      sa_price_conversion_mod,
      newdata = risi_prices_annual
    ),
    indo_prices_remap = predict(
      indo_price_conversion_mod,
      newdata = risi_prices_annual
    )
  )

# Add Ind RISI prices to price series
risi_prices_annual <- risi_prices_annual %>%
  left_join(
    wrq_prices %>% select(year, wrq_prices = wrq_indo_prices),
    by = "year"
  )

# Convert currency
risi_prices_annual <- risi_prices_annual %>%
  left_join(idr_usd_annual, by = "year") %>%
  mutate(
    sa_prices_idr = sa_prices_remap * idr_usd / 1000000, # Convert from USD to million IDR
    indo_prices_idr = indo_prices_remap * idr_usd / 1000000
  )

# Adjust for inflation
risi_prices_annual <- risi_prices_annual %>%
  left_join(fred_idn_cpi, by = "year") %>%
  mutate(
    sa_prices_real = sa_prices_idr / idn_cpi * 100, # Adjust for inflation - reference year is 2015
    indo_prices_real = indo_prices_idr / idn_cpi * 100,
    # Real Indonesian pulp price: convert the USD/tonne price to IDR at the
    # market rate, then deflate by Indonesian CPI (2015 = 100). This measures
    # the domestic purchasing power of mill revenue, which is the price a
    # producer's capacity decision responds to.
    indo_prices_real_idr = indo_prices * idr_usd / 1e6 / idn_cpi * 100,
    sa_prices_dev = (sa_prices_remap -
      zoo::rollmean(sa_prices_remap, k = 5, fill = NA, align = "right")) /
      1000
  ) # Deviation in 1000 USD

## export to csv ----------------------------------------------

# Only the derived columns are exported. The nominal licensed levels
# (sa_prices, indo_prices) and the intermediate conversion steps stay here.
pulp_prices_annual <- risi_prices_annual %>%
  select(
    year,
    sa_prices_real,
    indo_prices_real,
    indo_prices_real_idr,
    sa_prices_dev
  )

write_csv(
  pulp_prices_annual,
  paste0(wdir, "/01_data/02_out/tables/pulp_prices_annual_2001_2024.csv")
)
