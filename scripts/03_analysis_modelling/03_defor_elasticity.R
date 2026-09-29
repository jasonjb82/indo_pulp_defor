#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Author: Robert Heilmayr
# Project: Indonesia pulp deforestation
# Date: 6-2-2025
# Purpose: Estimate the deforestation elasticity. Largely the foundation
#   for SI section 5, but also includes some stats on pulp price trends
#   reported in a single sentence in SI Section 4.2.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# load packages --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(tidyverse)
library(fixest)
library(janitor)
library(modelsummary)
library(patchwork)
library(sf)
library(testthat)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# load data --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
wdir <- "remote"
data_dir <- "/01_data/"

# Deforestation data
defor_df <- read_csv(paste0(
  wdir,
  data_dir,
  "/02_out/tables/tbl_long_pulp_clearing_gfc_forest.csv"
))

# Bleached Hardwood Kraft, Acacia, from Indonesia (net price) and South America from RISI
# Measured in nominal USD / tonnes of BHKP
risi_prices <- readxl::read_excel(
  paste0(wdir, data_dir, "/01_in/wwi/Fastmarkets_2025_01_14-103617.xlsx"),
  skip = 4
) %>%
  clean_names() %>%
  select(
    date,
    indo_net_price = fp_plp_0045,
    sa_net_price = fp_plp_0056,
    nasc_net_price = fp_plp_0053
  )

# WRQ data on pulpwood prices (USD/m3).
# Used to convert global interannual variation in pulp prices (RISI data) into
# local pulpwood prices to improve interpretation
wrq_prices <- readxl::read_excel(paste0(
  wdir,
  data_dir,
  "/01_in/wwi/WRQ_pulpwood_prices.xlsx"
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
  data_dir,
  "/01_in/tables/idr_usd_annual_worldbank.csv"
)) %>%
  filter(year > 1999)

# FRED data on Indonesian CPI (to adjust for inflation, reference year = 2015)
fred_idn_cpi <- read_csv(paste0(
  wdir,
  data_dir,
  "/01_in/tables/FRED_IDNCPIALLAINMEI.csv"
)) %>%
  mutate(
    date = as.Date(observation_date, format = "%d/%m/%Y"),
    year = year(date)
  ) %>%
  select(year, idn_cpi = IDNCPIALLAINMEI)

# Data about grid cell composition along GAEZ classes
grid_gaez <- read_csv(paste0(
  wdir,
  data_dir,
  "/02_out/tables/gaez_grid_share.csv"
))

# Data about hti composition along GAEZ classes
hti_gaez <- read_csv(paste0(
  wdir,
  data_dir,
  "/02_out/tables/gaez_hti_areas.csv"
)) %>%
  select(-total_area_ha, supplier_id = ID)

# Data about hti productivity (produced in R script 08_calc_mai.R)
hti_mai <- read_csv(paste0(wdir, data_dir, "/02_out/tables/hti_mai.csv"))

# Add administrative labels
grid_admin <- read_csv(paste0(
  wdir,
  data_dir,
  "/02_out/tables/grid_10km_adm_prov_kab_kec.csv"
))

# mill capacities
cap_df <- readxl::read_excel(paste0(
  wdir,
  data_dir,
  "/01_in/wwi/MILLS_EXPORTERS_20200405.xlsx"
))

# mill-level production
mill_prod <- readxl::read_excel(paste0(
  wdir,
  data_dir,
  '/01_in/wwi/MILL_PRODUCTION_2015_2024.xlsx'
))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# clean price data --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
    sa_net_price = mean(sa_net_price),
    nasc_net_price = mean(nasc_net_price)
  ) %>%
  # filter(year <= 2023 & !is.na(risi_monthly_net_price)) %>%
  group_by(year) %>%
  summarize(
    indo_prices = mean(indo_net_price),
    sa_prices = mean(sa_net_price),
    nasc_prices = mean(nasc_net_price)
  ) %>% # Note - missing a few observations for SA in 2001
  select(year, sa_prices, indo_prices, nasc_prices) %>%
  filter(!is.na(year)) # drop the empty row produced by undated source rows
# Note: nasc_prices (North American softwood) is carried through for reference
# but is not used in any reported model.

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
    indo_prices_idr = indo_prices_remap * idr_usd / 1000000,
    wrq_prices_idr = wrq_prices * idr_usd / 1000000
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
    wrq_prices_real = wrq_prices_idr / idn_cpi * 100,
    sa_prices_dev = (sa_prices_remap -
      zoo::rollmean(sa_prices_remap, k = 5, fill = NA, align = "right")) /
      1000
  ) # Deviation in 1000 USD


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# merge datasets --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Add administrative unit labels
defor_df <- defor_df %>%
  left_join(grid_admin, by = "pixel_id")

# Add total pulp expansion variable
defor_df <- defor_df %>%
  mutate(pulp_exp_ha = pulp_forest_ha + pulp_non_forest_ha)

# Add prices to defor_df
defor_df <- defor_df %>%
  left_join(risi_prices_annual, by = "year")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Estimate cross-sectional variation in productivity  --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Re-assign GAEZ into aggregated classes.
# NOTE on class 2: the HTI aggregation folds GAEZ classes 2, 3 and 6 into
# "noLimitations", while the grid aggregation below uses only classes 3 and 6.
# The asymmetry is real, and follows from the two extractions covering
# different geographies:
#   - The 10 km grid spans Sumatra and Kalimantan only (all 15 provinces in
#     grid_10km_adm_prov_kab_kec.csv), not the whole of Indonesia. HTI
#     concessions are therefore NOT a subset of the grid.
#   - GAEZ class 2 ("Tropics, lowland; sub-humid") occurs in the drier
#     southeast. In our data it appears only in five Nusa Tenggara concessions
#     (75,330 ha; 0.67% of HTI area) and in no grid cell at all: class 2 is NA
#     for all 11,712 pixels of the raw GEE extraction, so the pivot_wider in
#     18_gaez_classes_hti_centroids.R never creates a class_2_pct column.
# The shorter grid formula therefore misallocates no grid area, and pot_mai is
# unaffected. It does mean the regression is trained on concessions containing
# a land class the prediction domain cannot contain, so the training and
# prediction populations are not identical.
hti_gaez <- hti_gaez %>%
  mutate(
    class_noLimitations = class_2 + class_3 + class_6, # tropic lowlands; sub-humid tropic lowlands; humid topic highlands
    class_hydromorphic = class_27 + class_28, # land with ample irrigated soils, dominantly hydromorphic soils
    class_terrain = class_25 + class_26, # very steep terrain, land with severe soil/terrain limitations,
    class_other = class_32 + class_33
  ) %>% # water and developed, to be dropped
  select(
    supplier_id,
    class_noLimitations,
    class_hydromorphic,
    class_terrain,
    class_other
  )
grid_gaez <- grid_gaez %>%
  mutate(
    noLimitations = (class_3_pct + class_6_pct) / 100,
    hydromorphic = (class_27_pct + class_28_pct) / 100,
    terrain = (class_25_pct + class_26_pct) / 100,
    other = (class_32_pct + class_33_pct) / 100
  ) %>%
  select(pixel_id, noLimitations, hydromorphic, terrain, other)

# Report out proportions in grouped classes
grid_gaez %>%
  summary()

# Recalculate removing "other" class (water and developed)
grid_gaez <- grid_gaez %>%
  mutate(
    no_other_sum = noLimitations + hydromorphic + terrain,
    noLimitations = noLimitations / no_other_sum,
    hydromorphic = hydromorphic / no_other_sum,
    terrain = terrain / no_other_sum
  ) %>%
  select(pixel_id, noLimitations, hydromorphic, terrain)

hti_gaez <- hti_gaez %>%
  pivot_longer(
    cols = starts_with("class_"),
    names_prefix = "class_",
    names_to = "class",
    values_to = "area_ha"
  )

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

# Model DMAI as a function of GAEZ shares (SI Equation 8).
# No intercept: the three shares sum to 1 by construction, so a constant would
# be perfectly collinear with them. Without it, each coefficient is the mean
# productivity of a concession composed entirely of that AEZ class.
mod <- lm(
  dmai_winsorized ~ 0 + noLimitations + hydromorphic + terrain,
  data = hti_gaez
)
summary(mod)

# Export SI Table 8
si_table8 <- tibble(
  Coefficient = c("alpha_nl", "alpha_h", "alpha_t"),
  Interpretation = c(
    "Average productivity on lands with few agricultural limitations",
    "Average productivity on saturated soils",
    "Average productivity on terrain with topographic limitations"
  ),
  Value = sprintf(
    "%.2f",
    coef(mod)[c("noLimitations", "hydromorphic", "terrain")]
  ),
  `Standard error` = sprintf(
    "%.2f",
    summary(mod)$coefficients[
      c("noLimitations", "hydromorphic", "terrain"),
      "Std. Error"
    ]
  )
)
print(si_table8)
write_csv(
  si_table8,
  paste0(wdir, data_dir, "/04_results/tables/si_table8_aez_productivity.csv")
)

# Predict potential DMAI for all grid cells
grid_pot_mai <- grid_gaez %>%
  mutate(
    pot_mai = predict(mod, newdata = grid_gaez),
    pot_mai = ifelse(pot_mai < 0, 0, pot_mai)
  ) %>% # winsorize negative potential production to 0
  select(pixel_id, pot_mai)

# Join productivity data back to defor_df
defor_df <- defor_df %>%
  left_join(grid_pot_mai, by = "pixel_id")

# Calculate potential revenues and net revenues
defor_df <- defor_df %>%
  mutate(
    pot_revenues = (sa_prices_real * pot_mai),
    pot_revenues_indo = (indo_prices_real * pot_mai),
    pot_revenues_wrq = (wrq_prices_real * pot_mai), # computed for reference; not used in any reported model
    pot_revenues_dev = (sa_prices_dev * pot_mai),
    post_2015 = year > 2015
  )


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Estimate elasticity of deforestation  --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mod_1 <- feols(
  pulp_forest_ha ~ pot_revenues | pixel_id + year,
  data = defor_df,
  vcov = ~kec_code
)
summary(mod_1)

mod_2 <- feols(
  pulp_forest_ha ~ post_2015:pot_revenues | pixel_id + year,
  data = defor_df,
  vcov = ~kec_code
)
summary(mod_2)

mod_3 <- feols(
  pulp_non_forest_ha ~ pot_revenues | pixel_id + year,
  data = defor_df,
  vcov = ~kec_code
)
summary(mod_3)

mod_4 <- feols(
  pulp_non_forest_ha ~ post_2015:pot_revenues | pixel_id + year,
  data = defor_df,
  vcov = ~kec_code
)
summary(mod_4)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Generate summary table (SI Table 9)  --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# Suppress modelsummary's automatic significance note: the thresholds are
# already stated in the table notes below, matching the published SI. Note this
# is a global option in modelsummary >= 2.4.0; the old stars_note argument is
# silently ignored.
options(modelsummary_stars_note = FALSE)

### Format summary table
# glance_custom.fixest injects n_clusters into modelsummary's GOF machinery
glance_custom.fixest <- function(x, ...) {
  data.frame(n_clusters = length(unique(defor_df$kec_code[obs(x)])))
}

# gof_map: Num.Obs. then Num. Clusters; FE rows excluded
gof_map_defor <- tribble(
  ~raw         , ~clean            , ~fmt ,
  "nobs"       , "N. Observations" ,    0 ,
  "n_clusters" , "N. Clusters"     ,    0
)

# SI Table 10 prints the same two labels in lower case
gof_map_robust <- tribble(
  ~raw         , ~clean            , ~fmt ,
  "nobs"       , "N. observations" ,    0 ,
  "n_clusters" , "N. clusters"     ,    0
)

# Custom summary table
tbl_args <- list(
  list(
    "Pulp deforestation" = list("(1)" = mod_1, "(2)" = mod_2),
    "Other pulp expansion" = list("(3)" = mod_3, "(4)" = mod_4)
  ),
  stars = c('*' = .1, '**' = .05, '***' = .01),
  coef_omit = "^(?!.*revenues)",
  coef_rename = c(
    "pot_revenues" = "Potential revenues",
    "post_2015FALSE:pot_revenues" = "Potential revenues (y<=2015)",
    "post_2015TRUE:pot_revenues" = "Potential revenues (y>2015)"
  ),
  gof_map = gof_map_defor,
  # Clustering is on kec_code (district), not concession -- the previous note
  # said "concession", which the published SI does not.
  notes = paste(
    "All models include grid cell and year fixed effects.",
    "Standard errors clustered by district (kecamatan) in parentheses.",
    "* p < 0.1, ** p < 0.05, *** p < 0.01"
  ),
  shape = "cbind"
)

do.call(msummary, tbl_args) # display
# Writing .docx requires pandoc. Guard the call so a missing pandoc cannot halt
# the script: everything below, including SI Figure 3 and the SI Section 4.2
# capacity-utilisation statistics, sits downstream of this call.
defor_elast_main_docx <- paste0(
  wdir,
  data_dir,
  "/04_results/tables/defor_elast_main.docx"
)
tryCatch(
  do.call(msummary, c(tbl_args, list(output = defor_elast_main_docx))),
  error = function(e) {
    warning(
      "Could not write ",
      defor_elast_main_docx,
      ": ",
      conditionMessage(e),
      "\n  .docx output needs pandoc on the PATH; other outputs are unaffected.",
      call. = FALSE
    )
  }
)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Robustness table (SI Table 10)  --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Re-run base model with same coefficients
defor_df <- defor_df %>%
  mutate(pot_revenues_r = pot_revenues)
rmod_0 <- feols(
  pulp_forest_ha ~ post_2015:pot_revenues_r | pixel_id + year,
  data = defor_df,
  vcov = ~kec_code
)

# Add suitability time trend
rmod_1 <- feols(
  pulp_forest_ha ~ post_2015:pot_revenues_r + pot_mai * year | pixel_id + year,
  data = defor_df,
  vcov = ~kec_code
)
summary(rmod_1)

# Lagged rents
defor_df <- defor_df %>%
  group_by(pixel_id) %>%
  arrange(pixel_id, year) %>%
  mutate(pot_revenues_r = lag(pot_revenues)) %>%
  ungroup() # lag needs the grouping; nothing downstream should inherit it
rmod_2 <- feols(
  pulp_forest_ha ~ post_2015:pot_revenues_r | pixel_id + year,
  data = defor_df,
  vcov = ~kec_code
)
summary(rmod_2)

# Price deviation
defor_df <- defor_df %>%
  mutate(pot_revenues_r = pot_revenues_dev)
rmod_3 <- feols(
  pulp_forest_ha ~ post_2015:pot_revenues_r | pixel_id + year,
  data = defor_df,
  vcov = ~kec_code
)
summary(rmod_3)


# Use Indonesian pulpwood price series instead of SA
defor_df <- defor_df %>%
  mutate(pot_revenues_r = pot_revenues_indo)
rmod_4 <- feols(
  pulp_forest_ha ~ post_2015:pot_revenues_r | pixel_id + year,
  data = defor_df,
  vcov = ~kec_code
)
summary(rmod_4)


# Custom summary table
# Column titles follow the published SI Table 10. Each model sits in its own
# named group so the table carries both the descriptive header and the column
# number, matching the published layout.
rtbl_args <- list(
  list(
    "Primary spec." = list("(1)" = rmod_0),
    "Control for suitability time-trend" = list("(2)" = rmod_1),
    "Lagged returns" = list("(3)" = rmod_2),
    "Price shocks" = list("(4)" = rmod_3),
    "Indonesian price series" = list("(5)" = rmod_4)
  ),
  stars = c('*' = .1, '**' = .05, '***' = .01),
  coef_omit = "^(?!.*revenues)",
  coef_rename = c(
    "post_2015FALSE:pot_revenues_r" = "Potential revenues (y<=2015)",
    "post_2015TRUE:pot_revenues_r" = "Potential revenues (y>2015)"
  ),
  gof_map = gof_map_robust,
  notes = paste(
    "All models include grid cell and year fixed effects.",
    "Standard errors clustered by district (kecamatan) in parentheses.",
    "* p < 0.1, ** p < 0.05, *** p < 0.01"
  ),
  shape = "cbind"
)

do.call(msummary, rtbl_args) # display
# Writing .docx requires pandoc. Guard the call so a missing pandoc cannot halt
# the script: the plots and SI statistics below sit downstream of this call.
defor_elast_robust_docx <- paste0(
  wdir,
  data_dir,
  "/04_results/tables/defor_elast_robust.docx"
)
tryCatch(
  do.call(msummary, c(rtbl_args, list(output = defor_elast_robust_docx))),
  error = function(e) {
    warning(
      "Could not write ",
      defor_elast_robust_docx,
      ": ",
      conditionMessage(e),
      "\n  .docx output needs pandoc on the PATH; other outputs are unaffected.",
      call. = FALSE
    )
  }
)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# plot basic trends --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# plot sa_prices variable in risi_prices_annual
# (built for inspection; not printed or saved below)
price_plot <- ggplot(
  risi_prices_annual %>% filter(year > 2000, year < 2023),
  aes(x = year, y = sa_prices_real)
) +
  geom_line() +
  labs(
    title = "RISI South America Pulp Prices",
    x = "Year",
    y = "Real price (million IDR per m3, 2015 base)"
  ) +
  theme_minimal()

# plot total deforestation across pixel_id for each year
# Note: pulp_forest_ha is rescaled to thousand ha for plotting below; the other
# two area columns are left in hectares and are not currently plotted.
total_pulp_exp <- defor_df %>%
  group_by(year) %>%
  summarize(
    pulp_forest_ha = sum(pulp_forest_ha, na.rm = TRUE) / 1000,
    pulp_non_forest_ha = sum(pulp_non_forest_ha, na.rm = TRUE),
    pulp_exp_ha = sum(pulp_exp_ha, na.rm = TRUE),
    pot_revenues = mean(pot_revenues, na.rm = TRUE)
  )
rev_plot <- ggplot(
  total_pulp_exp %>% filter(year > 2000, year < 2023),
  aes(x = year, y = pot_revenues)
) +
  geom_line() +
  labs(
    title = "Potential returns to pulpwood production",
    x = "Year",
    y = "Value (Million IDR)"
  ) +
  theme_minimal() +
  ylim(0, 12)


defor_plot <- ggplot(
  total_pulp_exp %>% filter(year > 2000, year < 2023),
  aes(x = year, y = pulp_forest_ha)
) +
  geom_line() +
  labs(
    title = "Total Pulp Deforestation by Year",
    x = "Year",
    y = "Pulp-driven deforestation (thousand ha)"
  ) +
  theme_minimal()
rev_plot / defor_plot


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Interpretation - text in SI --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# How big of an impact does an increase in prices have?
# every 1000 IDR increase in potential returns (XX% increase relative to mean)
# leads to an XX hectare increase in pulp-driven deforestation (XX% increase relative to mean)
1 / (defor_df$pot_revenues %>% mean())
mod_1$coefficients[1]
mod_1$coefficients[1] / (defor_df$pulp_forest_ha %>% mean())


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


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Interpretation - SI Figure 3 --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
defor_cf <- defor_df %>%
  mutate(defor_price_partial = pot_revenues * mod_1$coefficients[1])

total_pulp_defor <- defor_cf %>%
  group_by(year) %>%
  summarize(
    pot_revenues = mean(pot_revenues, na.rm = TRUE),
    pulp_forest_ha_true = sum(pulp_forest_ha, na.rm = TRUE) / 1000,
    pulp_forest_ha_cf = sum(defor_price_partial, na.rm = TRUE) / 1000
  ) %>%
  print()

defor_plot <- ggplot(
  total_pulp_defor %>% filter(year > 2000, year < 2023),
  aes(x = year)
) +
  geom_line(aes(y = pulp_forest_ha_true)) +
  geom_line(aes(y = pulp_forest_ha_cf), linetype = 2) +
  labs(x = "Year", y = "Pulp-driven deforestation (thousand ha)") +
  theme_minimal(base_size = 12) +
  annotate(
    "text",
    x = 2020,
    y = 107,
    label = "Deforestation as predicted\nby price variation",
    color = "black"
  ) +
  annotate(
    "text",
    x = 2018,
    y = 20,
    label = "Observed deforestation",
    color = "black"
  )
defor_plot
ggsave(
  paste0(wdir, data_dir, "/04_results/figures/SI_f3_elasticity.png"),
  width = 7,
  height = 5
)


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Illustrate that mill capacity utilization is inelastic
## (respond to review round 2, reviewer 2, comment 7)
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
mill_prod <- mill_prod %>%
  select(MILL_ID, YEAR, TOTAL_PROD_KG_NET) %>%
  group_by(MILL_ID, YEAR) %>%
  summarize(prod_mtpy = sum(TOTAL_PROD_KG_NET) / 1000000000) %>%
  left_join(cap_df %>% select(MILL_ID, PULP_CAP_MTPY), by = "MILL_ID")

mill_prod <- mill_prod %>%
  mutate(
    PULP_CAP_MTPY = if_else(
      MILL_ID == "M-0004" & YEAR < 2023,
      2.9,
      PULP_CAP_MTPY
    )
  ) %>% # Adjusting RAPP capacity - pre-dates capacity expansion
  mutate(cap_usage = prod_mtpy / PULP_CAP_MTPY)

cap_usage_trend <- mill_prod %>%
  filter(!(MILL_ID == "M-0003" & YEAR < 2019), MILL_ID != "M-0007") %>%
  group_by(YEAR) %>%
  summarize(cap = sum(PULP_CAP_MTPY), prod = sum(prod_mtpy)) %>%
  mutate(cap_usage = prod / cap) %>%
  rename(year = YEAR)

mill_prod %>%
  filter(!(MILL_ID == "M-0003" & YEAR < 2019), MILL_ID != "M-0007") %>%
  mutate(all = 1) %>%
  group_by(all) %>%
  summarize(cap = sum(PULP_CAP_MTPY), prod = sum(prod_mtpy)) %>%
  mutate(cap_usage = prod / cap)

cap_usage_trend <- cap_usage_trend %>%
  left_join(
    risi_prices_annual %>% select(year, indo_prices_real_idr),
    by = 'year'
  )

cap_usage_plot <- cap_usage_trend %>%
  ggplot(aes(x = year, y = cap_usage)) +
  geom_line() +
  scale_x_continuous(breaks = scales::breaks_width(1), minor_breaks = NULL) +
  ylim(0, 1.2) +
  theme_bw() +
  xlab("Year") +
  ylab("Capacity utilization rate (percent)")

price_trend_plot <- cap_usage_trend %>%
  ggplot(aes(x = year, y = indo_prices_real_idr)) +
  geom_line() +
  scale_x_continuous(breaks = scales::breaks_width(1), minor_breaks = NULL) +
  ylim(0, NA) +
  theme_bw() +
  xlab("Year") +
  ylab("Indonesian pulp prices\n(constant 2015 million IDR per tonne)")

cap_usage_plot / price_trend_plot

# Statistics reported in SI Section 4.2
cat(
  "\nSI Section 4.2 -- years",
  min(cap_usage_trend$year),
  "to",
  max(cap_usage_trend$year),
  "\n"
)
cat(sprintf(
  "  Capacity utilization: mean=%.0f%%  min=%.0f%%  max=%.0f%%  sd=%.0f%%\n",
  100 * mean(cap_usage_trend$cap_usage),
  100 * min(cap_usage_trend$cap_usage),
  100 * max(cap_usage_trend$cap_usage),
  100 * sd(cap_usage_trend$cap_usage)
))
cat(sprintf(
  "  Real pulp price (constant 2015 million IDR/tonne): mean=%.2f  min=%.2f  max=%.2f  sd=%.2f\n",
  mean(cap_usage_trend$indo_prices_real_idr),
  min(cap_usage_trend$indo_prices_real_idr),
  max(cap_usage_trend$indo_prices_real_idr),
  sd(cap_usage_trend$indo_prices_real_idr)
))
cat(sprintf(
  "  Coefficient of variation: utilization=%.3f  price=%.3f\n",
  sd(cap_usage_trend$cap_usage) / mean(cap_usage_trend$cap_usage),
  sd(cap_usage_trend$indo_prices_real_idr) /
    mean(cap_usage_trend$indo_prices_real_idr)
))
