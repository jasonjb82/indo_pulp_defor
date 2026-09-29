## ---------------------------------------------------------
##
## Project: Indonesia pulp deforestation
##
## Purpose of script:  SI section 3 - Calculate MAI for HTI concessions
##    and analyze trends in productivity.
##
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Date Created: 2022-02-13
##
## ---------------------------------------------------------
##
## Input datasets (paths relative to remote/01_data/)
##        1) 02_out/tables/hti_harvest_yr.csv: Concession-year harvest record --
##               hectare-years harvested, area-weighted rotation length,
##               harvest-year precipitation and PET, and hectare-years on peat.
##               Also carries the alternate hectare-year columns used by the
##               robustness specifications (ha_y_rw for truncated rotations;
##               ha_y_if / ha_y_mf / ha_y_hf for alternate fire treatments).
##               Produced by scripts/02_data_preparation/06_gaveau_harvests.R
##        2) 02_out/tables/ws_merge_clean_2015_2022.csv: Pulpwood volumes
##               delivered to mills by concession and year, from the RPBBI
##               sourcing reports. Subset here to 2015-2021.
##               Produced by scripts/02_data_preparation/04_merge_ws_data.R
##
## Outputs:
##        1) SI Table 6: Robustness table of DMAI trend regressions. Printed to
##               the console and written to
##               04_results/tables/yield_growth_table.docx (the .docx needs
##               pandoc; the call is guarded so a missing pandoc only warns).
##        2) SI Section 3 text statements: The numeric claims made in that
##               section, reproduced in their sentence context with values
##               interpolated from this run. Printed to the console and written
##               to 04_results/si_section3_statements.txt
##        3) Concession-level DMAI: Delivered mean annual increment per
##               concession, raw and Winsorized. Written to
##               02_out/tables/hti_mai.csv
##               Read by 03_analysis_modelling/03_defor_elasticity.R
##        4) Key parameters: Sectoral DMAI, 2021 DMAI, yield growth rate and CI
##               half-width, share of production with matched harvest data,
##               median observations per concession, and the Hardiyanto et al.
##               comparison CAGR. Written to 04_results/key_parameters.csv
##               Read by 03_analysis_modelling/05_pulp_expansion_scenarios.R
##               and 04_figures_and_outputs/05_paper_stats.R
##
## ---------------------------------------------------------

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Load packages
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(tidyverse)
library(fixest)
library(marginaleffects)
library(modelsummary)
library(readxl)
library(janitor)
library(tidylog)

options(scipen = 6, digits = 4) # I prefer to view outputs in non-scientific notation


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## load data -------------------------------------
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

wdir <- "remote"
data_dir <- "/01_data"

# Harvest data
harvest_csv <- paste0(wdir, data_dir, "/02_out/tables/hti_harvest_yr.csv")
harvest_df <- read_csv(harvest_csv)

# Calculate peat percentages
harvest_df <- harvest_df %>%
  mutate(peat_pct = ha_y_peat / ha_y)

# wood production
ws_df <- read_csv(paste0(
  wdir,
  data_dir,
  "/02_out/tables/ws_merge_clean_2015_2022.csv"
)) %>%
  clean_names() %>%
  filter(year < 2022) %>%
  group_by(year, supplier_id) %>%
  summarize(volume_m3 = sum(volume_m3)) %>%
  rename(harvest_year = year)

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## clean data -------------------------------------
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Join datasets
harvest_df <- harvest_df %>%
  filter(harvest_year >= 2015)

mai_df <- ws_df %>%
  full_join(harvest_df, by = c("supplier_id", "harvest_year"))
## NOTE: We are missing production reports for some harvested concessions, and are missing harvests for some concessions with production data. We dig into the scale of this below

# Clean new merged data
mai_df <- mai_df %>%
  mutate(
    dmai = volume_m3 / ha_y,
    dmai_rw = volume_m3 / ha_y_rw,
    dmai_if = volume_m3 / ha_y_if,
    dmai_mf = volume_m3 / ha_y_mf,
    dmai_hf = volume_m3 / ha_y_hf
  )

mai_df <- mai_df %>%
  arrange(supplier_id, harvest_year)

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Explore missing data -------------------------------------
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Confirm that concessions with missing weather data aren't actually harvesting
missing_weather <- mai_df %>%
  group_by(supplier_id) %>%
  summarise(missing_weather = all(is.na(pr_harvest))) %>%
  filter(missing_weather) %>%
  pull(supplier_id)

mai_df %>%
  filter(supplier_id %in% missing_weather) %>%
  summarise(sum(ha_y, na.rm = TRUE))


# Confirm that large majority of reported production has associated harvest data
prod_coverage <- mai_df %>%
  mutate(missing_harvests = is.na(ha_y)) %>%
  group_by(missing_harvests) %>%
  summarise(volume_m3 = sum(volume_m3, na.rm = TRUE)) %>%
  mutate(prop = prop.table(volume_m3)) %>%
  print()
prod_coverage <- prod_coverage %>%
  filter(missing_harvests == FALSE) %>%
  pull(prop)


# Confirm that large majority of reported harvesting has associated production data
mai_df %>%
  mutate(missing_prod = is.na(volume_m3)) %>%
  group_by(missing_prod) %>%
  summarise(ha_y = sum(ha_y, na.rm = TRUE)) %>%
  mutate(prop = prop.table(ha_y))

# Two possibilities for missing harvest / production data. Show these yield similar estimates of DMAI
# a) if they're both accurate, but assigned to different concessions. Sector MAI should just include them both in the numerator and denominator:
sector_mai <- (sum(mai_df$volume_m3, na.rm = TRUE) /
  sum(mai_df$ha_y, na.rm = TRUE)) %>%
  print()

# b) if they're invalid, all should be dropped from sectoral calculations
# Restrict to rows with both volume and harvest area (needed to compute DMAI); weather NAs handled within regressions
nona_mai_df <- mai_df %>%
  drop_na(volume_m3, ha_y)
(sum(nona_mai_df$volume_m3) / sum(nona_mai_df$ha_y)) %>% print()


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Winsorize individual MAIs -------------------------------------
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Winsorize MAI
# From Hardiyanto et al., 2024: The best treatment (comprising low impact harvesting, removal of only merchantable stem wood, conservation of organic matter, planting with improved germplasm, weed control and application of P at planting), yielded an MAI of 52.5 m3 ha-1 y-1, one of the highest growth rates reported for 230 tropical plantations (Nambiar, 2008).
mai_limit <- 52.5
winsorize_mai <- function(mai) {
  max_mai <- mai_limit
  if (mai > max_mai) {
    mai <- max_mai
  }
  return(mai)
}

nona_mai_df <- nona_mai_df %>%
  mutate(
    mai_winsorized = map_dbl(dmai, winsorize_mai),
    volume_winsorized = mai_winsorized * ha_y,
    dmai_rw = map_dbl(dmai_rw, winsorize_mai),
    dmai_if = map_dbl(dmai_if, winsorize_mai),
    dmai_hf = map_dbl(dmai_hf, winsorize_mai)
  )


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Calculate sectoral MAI over time ----------------------------
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Calculate annual MAI in the sector
year_mai <- mai_df %>%
  group_by(harvest_year) %>%
  summarise(
    ha_y = sum(ha_y, na.rm = TRUE),
    volume_m3 = sum(volume_m3, na.rm = TRUE)
  ) %>%
  mutate(year_mai = volume_m3 / ha_y, ln_mai = log(year_mai)) %>%
  print()

mai_2021 <- year_mai %>%
  filter(harvest_year == 2021) %>%
  pull(year_mai) %>%
  print()

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Calculate hti-level average MAI -------------------------------------
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hti_mai <- mai_df %>%
  group_by(supplier_id) %>%
  summarise(
    volume_m3 = sum(volume_m3, na.rm = TRUE),
    ha_y = sum(ha_y, na.rm = TRUE)
  ) %>%
  filter(volume_m3 > 0, ha_y > 0) %>%
  mutate(
    dmai = volume_m3 / ha_y,
    dmai_winsorized = map_dbl(dmai, winsorize_mai)
  )

hti_mai %>% write_csv(paste0(wdir, data_dir, "/02_out/tables/hti_mai.csv"))


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Regressions to describe trends in MAI -------------------------------------
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nona_mai_df <- nona_mai_df %>%
  mutate(
    outlier = dmai != mai_winsorized,
    ln_mai = log(dmai),
    ln_mai_w = log(mai_winsorized),
    ln_rw = log(dmai_rw),
    ln_if = log(dmai_if),
    ln_hf = log(dmai_hf),
    Supplier = supplier_id
  )

# Result to report in appendix
nona_mai_df %>%
  group_by(outlier) %>%
  summarise(volume_m3 = sum(volume_m3)) %>%
  mutate(shr = prop.table(volume_m3))


# Controls include rotation characteristics and harvest-year precipitation/PET. Temperature and
# rotation-period weather are excluded; their effects on MAI are analyzed separately.
controls <- "rotation_length + peat_pct + pr_harvest + pet_harvest"

ols_mod <- feols(
  as.formula(paste0("ln_mai_w ~", controls, " + harvest_year")),
  cluster = ~Supplier,
  data = nona_mai_df
)
summary(ols_mod)

nocntrl_mod <- feols(
  as.formula(paste("ln_mai_w ~ harvest_year | Supplier")),
  cluster = ~Supplier,
  data = nona_mai_df
)
summary(nocntrl_mod)

base_mod <- feols(
  as.formula(paste0("ln_mai_w ~", controls, " + harvest_year | Supplier")),
  cluster = ~Supplier,
  data = nona_mai_df
)
summary(base_mod)


trim_mod <- feols(
  as.formula(paste("ln_mai_w ~", controls, "+ harvest_year | Supplier")),
  cluster = ~Supplier,
  data = nona_mai_df %>% filter(outlier == 0)
)
summary(trim_mod)
grow_yield(sector_mai, coef(trim_mod)["harvest_year"], nyears)

nowin_mod <- feols(
  as.formula(paste("ln_mai ~", controls, "+ harvest_year | Supplier")),
  cluster = ~Supplier,
  data = nona_mai_df
)
summary(nowin_mod)

# # Show these coefficient estimates fall within 95% confidence interval of base model
# confint(base_mod, "harvest_year", level = 0.95)

rw_mod <- feols(
  as.formula(paste("ln_rw ~", controls, "+ harvest_year | Supplier")),
  cluster = ~Supplier,
  data = nona_mai_df
)
summary(rw_mod)

if_mod <- feols(
  as.formula(paste("ln_if ~", controls, "+ harvest_year | Supplier")),
  cluster = ~Supplier,
  data = nona_mai_df
)
summary(if_mod)

hf_mod <- feols(
  as.formula(paste("ln_hf ~", controls, "+ harvest_year | Supplier")),
  cluster = ~Supplier,
  data = nona_mai_df
)
summary(hf_mod)

models <- list(
  "(1)" = ols_mod,
  "(2)" = nocntrl_mod,
  "(3)" = base_mod,
  "(4)" = trim_mod,
  "(5)" = nowin_mod,
  "(6)" = rw_mod,
  "(7)" = if_mod,
  "(8)" = hf_mod
)

# gof_map: show only Num.Obs. and FE: Supplier (Concessions added via add_rows below)
gof_map_custom <- tribble(
  ~raw           , ~clean         , ~fmt ,
  "nobs"         , "Num.Obs."     ,    0 ,
  "FE: Supplier" , "FE: Supplier" , NA
)

# Compute concession count per model.
# FE models: use fixef() which counts exactly the suppliers included in the fixed effects.
# OLS (no FE, fit on full nona_mai_df): use obs() to index into nona_mai_df directly.
get_n_concessions <- function(m) {
  fe <- tryCatch(fixef(m), error = function(e) NULL)
  if (!is.null(fe) && "Supplier" %in% names(fe)) {
    as.character(length(fe$Supplier))
  } else {
    as.character(n_distinct(nona_mai_df$Supplier[obs(m)]))
  }
}
n_conc <- sapply(models, get_n_concessions)

rows <- tribble(
  ~term                    , ~OLS        , ~NoCntrls   , ~Base       , ~Trimmed , ~NoWins  , ~ShortenRot , ~IgFire     , ~DropFire   ,
  'Treatment of outliers'  , 'Winsorize' , 'Winsorize' , 'Winsorize' , 'Drop'   , 'Keep'   , 'Winsorize' , 'Winsorize' , 'Winsorize' ,
  'Shorten long rotations' , 'False'     , 'False'     , 'False'     , 'False'  , 'False'  , 'True'      , 'False'     , 'False'     ,
  'Treatment of fires'     , 'Impute'    , 'Impute'    , 'Impute'    , 'Impute' , 'Impute' , 'Impute'    , 'Keep'      , 'Drop'      ,
  'Controls'               , 'X'         , ''          , 'X'         , 'X'      , 'X'      , 'X'         , 'X'         , 'X'
)

# Concessions is listed first so the add_rows positions below run in ascending
# order. Out-of-order positions make modelsummary pad the table with blank "NA"
# rows, which then have to be deleted by hand from the .docx.
rows <- bind_rows(
  tibble(
    term = "Concessions",
    !!!setNames(as.list(n_conc), names(rows)[-1])
  ),
  rows
)
# Final layout: 1=Year, 2=(SE), 3=Num.Obs., 4=Concessions, 5-8=descriptors,
# 9=FE: Supplier (the last row comes from gof_map).
attr(rows, 'position') <- c(4, 5, 6, 7, 8)


modelsummary(
  models,
  fmt = 3,
  coef_map = c("harvest_year" = "Year"),
  stars = c('*' = .1, '**' = .05, '***' = 0.01),
  gof_map = gof_map_custom,
  add_rows = rows
)

# Writing .docx requires pandoc. Guard the call so a missing pandoc cannot halt
# the script before key_parameters.csv is written below -- that file is a
# required input for the downstream paper-statistics and scenario scripts.
yield_growth_docx <- paste0(
  wdir,
  data_dir,
  "/04_results/tables/yield_growth_table.docx"
)
tryCatch(
  modelsummary(
    models,
    fmt = 3,
    coef_map = c("harvest_year" = "Year"),
    stars = c('*' = .1, '**' = .05, '***' = 0.01),
    gof_map = gof_map_custom,
    stars_note = FALSE,
    add_rows = rows,
    notes = "Standard errors clustered by concession. * p < 0.1, ** p < 0.05, *** p < 0.01",
    output = yield_growth_docx
  ),
  error = function(e) {
    warning(
      "Could not write ",
      yield_growth_docx,
      ": ",
      conditionMessage(e),
      "\n  .docx output needs pandoc on the PATH; other outputs are unaffected.",
      call. = FALSE
    )
  }
)


yield_growth <- base_mod$coefficients['harvest_year']
yield_growth_confint <- yield_growth -
  confint(base_mod, "harvest_year", level = 0.95)[1]

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Contrast against prior estimates -------------------------------------
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Comparing against Section 7 of hardiyanto et al., 2024.
# Productivity increased 15% between R-4 (2013) and R-5 (2017).
# Under compound growth, this implies ~3.6% growth per year
hardiyanto_cagr <- (1.15)^(1 / (2017 - 2013)) - 1
hardiyanto_cagr


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Export key model parameters -------------------------------------
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
median_obs <- nona_mai_df %>%
  group_by(supplier_id) %>%
  tally() %>%
  pull(n) %>%
  median()

output <- list(
  "dmai" = sector_mai,
  "dmai_2021" = mai_2021,
  "yield_growth" = yield_growth[1],
  "yield_growth_ci" = yield_growth_confint[1, 1],
  "production_coverage" = prod_coverage,
  "median_obs" = median_obs,
  "hardiyanto_cagr" = hardiyanto_cagr
) %>%
  as_tibble()

write_csv(output, paste0(wdir, data_dir, "/04_results/key_parameters.csv"))


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Reproduce the numeric claims made in SI Section 3 -------------------------
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Prints each statement from SI Section 3 that depends on this script, with
## its numbers interpolated live from the objects above, and writes the same
## text to 04_results.

n_concessions <- n_distinct(nona_mai_df$supplier_id)
n_concessions_panel <- nona_mai_df %>%
  ungroup() %>%
  count(supplier_id) %>%
  filter(n >= 2) %>%
  nrow()
outlier_volume_shr <- nona_mai_df %>%
  ungroup() %>%
  group_by(outlier) %>%
  summarise(volume_m3 = sum(volume_m3), .groups = "drop") %>%
  mutate(shr = prop.table(volume_m3)) %>%
  filter(outlier) %>%
  pull(shr)

# Wrap a sprintf-formatted paragraph to a fixed width for legible output
si_para <- function(...) c(strwrap(sprintf(...), width = 78), "")

si_text <- c(
  "SI SECTION 3: ESTIMATING PRODUCTIVITY TRENDS IN PULPWOOD PLANTATIONS",
  strrep("=", 78),
  paste(
    "Generated by scripts/03_analysis_modelling/02_calc_mai.R on",
    Sys.Date()
  ),
  "",
  "3.2 Estimating delivered mean annual increment",
  strrep("-", 78),
  si_para(
    paste(
      "Based on these calculations, we estimate that, for timber blocks",
      "harvested between 2015 and 2021, each hectare of plantation in Indonesia",
      "yielded approximately %.1f m3 of delivered pulpwood per year, reaching",
      "%.1f m3 in blocks harvested in 2021. Our estimate of sectoral DMAI is",
      "derived from %d concessions that collectively delivered %.0f%% of all",
      "domestically produced pulpwood supplies detailed in RPBBI sourcing",
      "reports."
    ),
    sector_mai,
    mai_2021,
    n_concessions,
    100 * prod_coverage
  ),
  "3.3 Trends in DMAI",
  strrep("-", 78),
  si_para(
    paste(
      "Of the %d plantation concessions observed in our data, %d have at least",
      "two observations during our study period, and the median concession has",
      "%d distinct years of data."
    ),
    n_concessions,
    n_concessions_panel,
    median_obs
  ),
  si_para(
    paste(
      "We find that concessions experienced a %.1f%% (+/- %.1f) increase in",
      "productivity per year between 2015 and 2021. Reassuringly, this aligns",
      "with prior estimates that productivity increased ~%.1f%% per year between",
      "two successive eucalyptus rotations harvested in 2013 and 2017",
      "(Hardiyanto et al. 2024)."
    ),
    100 * yield_growth,
    100 * yield_growth_confint,
    100 * hardiyanto_cagr
  ),
  si_para(
    paste(
      "Treatment of outliers: We found that a relatively small proportion of",
      "production (%.1f%% of delivered volume) came from concessions with",
      "unreasonably large DMAI estimates, which we define as exceeding %.1f",
      "m3/ha/y, the highest MAI observed within experimental plantations in",
      "Indonesia (Hardiyanto et al. 2024)."
    ),
    100 * outlier_volume_shr,
    mai_limit
  ),
  "Table 6 (regression results) is written separately to",
  paste0("  ", yield_growth_docx),
  ""
)

cat(si_text, sep = "\n")

si_text_path <- paste0(wdir, data_dir, "/04_results/si_section3_statements.txt")
writeLines(si_text, si_text_path)
cat("\nSI Section 3 statements written to", si_text_path, "\n")
