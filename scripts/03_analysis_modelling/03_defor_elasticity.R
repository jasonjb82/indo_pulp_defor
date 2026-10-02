#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Author: Robert Heilmayr
# Project: Indonesia pulp deforestation
# Date: 6-2-2025
# Purpose: Estimate the deforestation elasticity. Largely the foundation
#   for SI section 5, but also includes some stats on pulp price trends
#   reported in a single sentence in SI Section 4.2.
#
# Input datasets (paths relative to remote/01_data/)
#        1) 02_out/tables/tbl_long_pulp_clearing_gfc_forest.csv: Annual
#               pulp-driven deforestation and other pulp expansion by 10 km
#               grid cell, 2001-2022. The estimation panel.
#               Produced by scripts/02_data_preparation/16_create_long_data_10km_gc.R
#        2) 02_out/tables/grid_10km_adm_prov_kab_kec.csv: Province, kabupaten
#               and kecamatan labels per grid cell. Supplies the kecamatan
#               used for clustering. Note the grid spans Sumatra and
#               Kalimantan only, not all of Indonesia.
#               Produced by scripts/02_data_preparation/16_create_long_data_10km_gc.R
#        3) 02_out/tables/pulp_prices_annual_2001_2024.csv: Annual pulp and
#               pulpwood prices, expressed in constant 2015 IDR and rescaled
#               into pulpwood-equivalent units. Derived from commercially
#               licensed Fastmarkets and WRQ price data, which are not
#               redistributable and so are read only by the prep script.
#               Produced by scripts/02_data_preparation/19_prep_pulp_prices.R
#        4) 02_out/tables/gaez_hti_areas.csv and 02_out/tables/gaez_grid_share.csv:
#               Agro-ecological zone composition of concessions (areas, ha) and
#               of grid cells (percentages).
#               Produced by scripts/02_data_preparation/18_gaez_classes_hti_centroids.R
#        5) 02_out/tables/hti_mai.csv: Concession-level delivered mean annual
#               increment, used with the AEZ shares to predict potential
#               productivity per grid cell.
#               Produced by scripts/03_analysis_modelling/02_calc_mai.R
#        6) 01_in/wwi/MILLS_EXPORTERS_20200405.xlsx and
#               01_in/wwi/MILL_PRODUCTION_2015_2024.xlsx: Mill pulp capacity
#               and annual production, used for the capacity-utilisation
#               statistics reported in SI Section 4.2.
#
# Outputs:
#        1) SI Table 8: Equation 8 coefficients relating delivered mean annual
#               increment to agro-ecological zone shares. Written to
#               04_results/tables/si_table8_aez_productivity.csv
#        2) SI Table 9: Responsiveness of pulp-driven deforestation to
#               potential returns. Printed to the console and written to
#               04_results/tables/defor_elast_main.docx
#        3) SI Table 10: Robustness tests of that responsiveness. Printed to
#               the console and written to
#               04_results/tables/defor_elast_robust.docx
#               Both .docx writes need pandoc; the calls are guarded so a
#               missing pandoc only warns.
#        4) SI Figure 3: Observed pulp-driven deforestation against the
#               deforestation predicted by price variation alone. Written to
#               04_results/figures/SI_f3_elasticity.png
#        5) SI Sections 4.2, 5.2 and 5.3 text statements: The numeric claims
#               made in those sections, reproduced in their sentence context
#               with values interpolated from this run. Printed to the console
#               and written to 04_results/si_sections4_5_statements.txt
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# load packages --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(tidyverse)
library(fixest)
library(modelsummary)


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

# Annual pulp and pulpwood prices, in constant 2015 IDR.
# Derived from licensed Fastmarkets and WRQ price data, which cannot be
# redistributed; the conversions live in the prep script instead.
pulp_prices_annual <- read_csv(paste0(
  wdir,
  data_dir,
  "/02_out/tables/pulp_prices_annual_2001_2024.csv"
))

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
  left_join(pulp_prices_annual, by = "year")


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

# Report out proportions in grouped classes (quoted in SI Section 5.2)
aez_shares <- hti_gaez %>%
  group_by(class) %>%
  summarize(area_ha = sum(area_ha, na.rm = TRUE)) %>%
  mutate(prop_area = area_ha / sum(area_ha, na.rm = TRUE))
print(aez_shares)

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
# the script.
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
# Interpretation - text in SI --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# How big of an impact does an increase in prices have?
# SI 5.3: "every 1,000,000 IDR increase in potential returns (12% increase
# relative to mean) leads to an 0.89 hectare increase in pulp-driven
# deforestation in a grid cell." pot_revenues is in million IDR, so the first
# figure is 1 / mean(pot_revenues) and the second is the Column 1 coefficient.
1 / (defor_df$pot_revenues %>% mean())
mod_1$coefficients[1]


# SI 5.3 notes that pulp-driven deforestation peaked in 2011 and bottomed in
# 2017, yet potential returns were slightly HIGHER in 2017 than in 2011.
# (This previously compared 2011 with 2016, which does not support that claim:
# returns in 2016 were below those in 2011.)
pot_returns_2011 <- defor_df %>%
  filter(year == 2011) %>%
  pull(pot_revenues) %>%
  mean() %>%
  print()
pot_returns_2017 <- defor_df %>%
  filter(year == 2017) %>%
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

cap_usage_trend <- cap_usage_trend %>%
  left_join(
    pulp_prices_annual %>% select(year, indo_prices_real_idr),
    by = 'year'
  )

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Reproduce the numeric claims made in the SI -----------------------------
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Prints each statement from the SI that depends on this script, with its
## numbers interpolated live, and writes the same text to 04_results.
## Reproducing the sentences in context makes it straightforward to check the
## manuscript against the analysis, and any change in the underlying data
## surfaces directly in the wording below.

aez_pct <- function(cls) {
  100 * aez_shares$prop_area[aez_shares$class == cls]
}
# SI 5.3 describes the peak and trough "over the past 15 years", i.e. the last
# 15 years of the panel. The window matters: 2004 is the all-time peak of the
# 2001-2022 series (137.1 kha), so over the full panel the peak is 2004 rather
# than the 2011 the SI reports. From 2005 onward the peak is 2011 (105.7 kha).
recent_window_start <- max(total_pulp_defor$year) - 14
defor_by_year <- total_pulp_defor %>%
  filter(year >= recent_window_start)
peak_year <- defor_by_year$year[which.max(defor_by_year$pulp_forest_ha_true)]
trough_year <- defor_by_year$year[which.min(defor_by_year$pulp_forest_ha_true)]
returns_in <- function(y) {
  total_pulp_defor$pot_revenues[total_pulp_defor$year == y]
}

si_para <- function(...) c(strwrap(sprintf(...), width = 78), "")

si_text <- c(
  "SI SECTIONS 4.2 AND 5: DEFORESTATION ELASTICITY",
  strrep("=", 78),
  paste(
    "Generated by scripts/03_analysis_modelling/03_defor_elasticity.R on",
    Sys.Date()
  ),
  "",
  "4.2 Projected increases in pulpwood demand",
  strrep("-", 78),
  si_para(
    paste(
      "Between %d and %d, sector-wide capacity utilization rates remained",
      "stable and high (mean = %.0f percent; minimum = %.0f percent; maximum =",
      "%.0f percent; standard deviation = %.0f percent), despite much larger",
      "fluctuations in real pulp prices (mean = %.2f million IDR/tonne;",
      "minimum = %.2f million IDR/tonne; maximum = %.2f million IDR/tonne;",
      "standard deviation = %.2f million IDR/tonne). Prices are expressed in",
      "constant 2015 Indonesian rupiah, converting the reported USD price",
      "series at the annual average market exchange rate and deflating by the",
      "Indonesian consumer price index, so that they reflect the domestic",
      "purchasing power of mill revenue. Relative to their means, prices varied",
      "more than three times as much as capacity utilization (coefficients of",
      "variation of %.2f and %.2f, respectively)."
    ),
    min(cap_usage_trend$year),
    max(cap_usage_trend$year),
    100 * mean(cap_usage_trend$cap_usage),
    100 * min(cap_usage_trend$cap_usage),
    100 * max(cap_usage_trend$cap_usage),
    100 * sd(cap_usage_trend$cap_usage),
    mean(cap_usage_trend$indo_prices_real_idr),
    min(cap_usage_trend$indo_prices_real_idr),
    max(cap_usage_trend$indo_prices_real_idr),
    sd(cap_usage_trend$indo_prices_real_idr),
    sd(cap_usage_trend$indo_prices_real_idr) /
      mean(cap_usage_trend$indo_prices_real_idr),
    sd(cap_usage_trend$cap_usage) / mean(cap_usage_trend$cap_usage)
  ),
  "5.2 Data sources: agro-ecological zone composition",
  strrep("-", 78),
  si_para(
    paste(
      "Areas with few limitations for agricultural production (AFL). This class",
      "combines areas categorized as \"humid tropic lowlands\", \"humid tropic",
      "highlands\" and \"sub-humid tropic lowlands,\" and represents %.1f%% of",
      "concession area."
    ),
    aez_pct("noLimitations")
  ),
  si_para(
    paste(
      "Areas with hydromorphic soils (AH). This class combines areas",
      "categorized as \"land with ample irrigated soils\" and \"dominantly",
      "hydromorphic soils\" and represents %.1f%% of concession area."
    ),
    aez_pct("hydromorphic")
  ),
  si_para(
    paste(
      "Areas with topographic limitations (AT). This class combines areas",
      "categorized as \"very steep terrain\" and \"land with severe soil or",
      "terrain limitations\" and represents %.1f%% of concession area."
    ),
    aez_pct("terrain")
  ),
  si_para(
    paste(
      "Areas that can't be used for pulpwood production due to other land cover",
      "(AO). This class combines areas categorized as \"water\" and \"developed\"",
      "classes, and represents %.2f%% of concession area. We remove this final",
      "class from all analyses."
    ),
    aez_pct("other")
  ),
  si_para(
    paste(
      "Equation 8 coefficients (SI Table 8) are written to",
      "04_results/tables/si_table8_aez_productivity.csv."
    )
  ),
  "5.3 Results",
  strrep("-", 78),
  si_para(
    paste(
      "We find that increases in potential returns to pulpwood production do",
      "lead to a statistically significant increase in pulp-driven deforestation",
      "(Table 9, Column 1). However, this effect is small - every 1,000,000 IDR",
      "increase in potential returns (%.0f%% increase relative to mean) leads to",
      "a %.2f hectare increase in pulp-driven deforestation in a grid cell."
    ),
    100 / mean(defor_df$pot_revenues, na.rm = TRUE),
    mod_1$coefficients[["pot_revenues"]]
  ),
  si_para(
    paste(
      "For example, over the past 15 years the highest level of pulp-driven",
      "deforestation occurred in %d, and the lowest level occurred in %d.",
      "However, producers faced slightly higher real potential returns in %d",
      "than in %d."
    ),
    peak_year,
    trough_year,
    trough_year,
    peak_year
  ),
  si_para(
    paste(
      "Regression results (SI Tables 9 and 10) are written to",
      "04_results/tables/defor_elast_main.docx and defor_elast_robust.docx;",
      "SI Figure 3 is written to 04_results/figures/SI_f3_elasticity.png."
    )
  )
)

cat(si_text, sep = "\n")

si_text_path <- paste0(
  wdir,
  data_dir,
  "/04_results/si_sections4_5_statements.txt"
)
writeLines(si_text, si_text_path)
cat("\nSI statements written to", si_text_path, "\n")
