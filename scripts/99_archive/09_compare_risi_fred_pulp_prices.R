## ---------------------------------------------------------
## 
## Project: Indonesia pulp deforestation
##
## Purpose of script: Exploring RISI and FRED pulp prices
##
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Date Created: 2025-01-14
## 
## ---------------------------------------------------------
##
## Notes: 
##
## ---------------------------------------------------------

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation

## ---------------------------------------------------------

### Load packages
library(stringr)
library(tidyverse)
library(tidylog)
library(janitor)
library(lubridate)
library(scales)
library(dtplyr)
library(d3.format) # to install: devtools::install_github("dreamRs/d3.format")
library(tidyfast)
library(concordance)
library(extrafont)
library(showtext)
library(khroma) # palettes for color blindness

'%ni%' <- Negate('%in%') # filter out function

## set working directory -------------------------------------

wdir <- "remote"

## read data -------------------------------------------------

# Bleached Hardwood Kraft, Acacia, from Indonesia (net price) from RISI
risi_prices <- readxl::read_excel(paste0(wdir,"\\01_data\\01_in\\wwi\\Fastmarkets_2025_01_14-103617.xlsx"),skip=4) %>%
  clean_names() %>% select(date,net_price=mid_3)

# Pulp prices (PPI) (FRED)
fred_prices <- read_csv(paste0(wdir,"\\01_data\\01_in\\tables\\WPU0911_FRED_monthly.csv"))

## clean data -----------------------------------------------

risi_prices_clean <- risi_prices %>%
  mutate(date = as.Date(date,format="%m/%d/%Y"),
         year = year(date),
         month = month(date)) %>%
  group_by(year,month) %>%
  summarize(risi_monthly_net_price = mean(net_price)) %>%
  filter(year < 2023 & !is.na(risi_monthly_net_price))


fred_prices_clean <- fred_prices %>%
  mutate(date = as.Date(observation_date,format="%d/%m/%Y"),
         year = year(date),
         month = month(date)) %>%
  select(year,month,fred_monthly_PPI = WPU0911) %>%
  group_by(year,month) %>%
  summarize(fred_monthly_PPI = mean(fred_monthly_PPI))

merged_prices <- risi_prices_clean %>%
  left_join(fred_prices_clean,by=c("year","month")) %>%
  mutate(date_clean = ymd(paste(year,month,1, sep = "-")))


## normalize and plot -----------------------------------------
normalize_zscore <- function(x) {
  (x - mean(x)) / sd(x)
}

merged_prices$norm_data_risi <- normalize_zscore(merged_prices$risi_monthly_net_price)
merged_prices$norm_data_fred <- normalize_zscore(merged_prices$fred_monthly_PPI)

ggplot(merged_prices, aes(x = date_clean)) +
  geom_line(aes(y = norm_data_risi, color = "RISI")) +
  geom_line(aes(y = norm_data_fred, color = "FRED")) +
  labs(title = "Normalized Data", y = "Normalized Value",x = "Date") +
  scale_x_date(date_breaks = "2 year", date_labels = "%Y") +
  scale_color_manual(values = c("blue", "red"),name="")
