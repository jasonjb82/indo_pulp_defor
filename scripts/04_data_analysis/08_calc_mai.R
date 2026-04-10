## ---------------------------------------------------------
## 
## Project: Indonesia pulp deforestation
##
## Purpose of script: Calculate MAI for HTI concessions
##
## Author: Robert Heilmayr and Jason Jon Benedict
##
## Date Created: 2022-02-13
## 
## ---------------------------------------------------------
##
## Notes: Input datasets
##        1) 
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

# Harvest data
harvest_csv <- paste0(wdir, "/01_data/02_out/tables/hti_harvest_yr.csv")
harvest_df <- read_csv(harvest_csv)

# Calculate peat percentages
harvest_df <- harvest_df %>%
  mutate(peat_pct = ha_y_peat / ha_y)

# wood production
ws_2015_2019 <- read_excel(paste0(wdir,"/01_data/01_in/wwi/RPBBI_2015_2019_compiled.xlsx")) %>%
  select(YEAR,SUPPLIER_ID,VOLUME_M3) %>%
  group_by(YEAR,SUPPLIER_ID) %>%
  summarize(VOLUME_M3 = sum(VOLUME_M3))

ws_2020 <- read_excel(paste0(wdir,"/01_data/01_in/wwi/RPBBI_2020_compiled.xlsx")) %>%
  select(YEAR,SUPPLIER_ID,VOLUME_M3) %>%
  group_by(YEAR,SUPPLIER_ID) %>%
  summarize(VOLUME_M3 = sum(VOLUME_M3))

ws_2021 <- read_excel(paste0(wdir,"/01_data/01_in/wwi/RPBBI_2021_compiled.xlsx")) %>%
  select(YEAR,SUPPLIER_ID,VOLUME_M3) %>%
  group_by(YEAR,SUPPLIER_ID) %>%
  summarize(VOLUME_M3 = sum(VOLUME_M3))

ws_df <- rbind(ws_2015_2019, ws_2020, ws_2021) %>% 
  clean_names() %>% 
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
  mutate(dmai = volume_m3 / ha_y,
         dmai_rw = volume_m3 / ha_y_rw,
         dmai_if = volume_m3 / ha_y_if,
         dmai_mf = volume_m3 / ha_y_mf,
         dmai_hf = volume_m3 / ha_y_hf)

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
mai_df %>% 
  mutate(missing_harvests = is.na(ha_y)) %>% 
  group_by(missing_harvests) %>% 
  summarise(volume_m3 = sum(volume_m3, na.rm = TRUE)) %>% 
  mutate(prop = prop.table(volume_m3))

# Confirm that large majority of reported harvesting has associated production data
mai_df %>% 
  mutate(missing_prod = is.na(volume_m3)) %>% 
  group_by(missing_prod) %>% 
  summarise(ha_y = sum(ha_y, na.rm = TRUE)) %>% 
  mutate(prop = prop.table(ha_y))

# Two possibilities for missing harvest / production data. Show these yield similar estimates of DMAI
# a) if they're both accurate, but assigned to different concessions. Sector MAI should just include them both in the numerator and denominator:
sector_mai <- (sum(mai_df$volume_m3, na.rm = TRUE) / sum(mai_df$ha_y, na.rm = TRUE)) %>% print()

# b) if they're invalid, all should be dropped from sectoral calculations
nona_mai_df <- mai_df %>% 
  drop_na()
(sum(nona_mai_df$volume_m3) / sum(nona_mai_df$ha_y)) %>% print()


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Winsorize individual MAIs -------------------------------------
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Winsorize MAI
# From Hardiyanto et al., 2023: The best treatment (comprising low impact harvesting, removal of only merchantable stem wood, conservation of organic matter, planting with improved germplasm, weed control and application of P at planting), yielded an MAI of 52.5 m3 ha-1 y-1, one of the highest growth rates reported for 230 tropical plantations (Nambiar, 2008).
mai_limit <- 52.5
winsorize_mai <- function(mai){
  max_mai <- mai_limit
  if(mai > max_mai){
    mai <- max_mai
  }
  return(mai)
}

nona_mai_df <- nona_mai_df %>% 
  mutate(mai_winsorized  = map_dbl(dmai, winsorize_mai),
         volume_winsorized = mai_winsorized * ha_y,
         dmai_rw = map_dbl(dmai_rw, winsorize_mai),
         dmai_if = map_dbl(dmai_if, winsorize_mai),
         dmai_mf = map_dbl(dmai_mf, winsorize_mai),
         dmai_hf = map_dbl(dmai_hf, winsorize_mai))

outlier_ids <- mai_df %>% 
  group_by(supplier_id) %>% 
  summarize(max_mai = max(dmai)) %>% 
  filter(max_mai > mai_limit) %>% 
  pull(supplier_id)


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Calculate sectoral MAI -------------------------------------
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Calculate baseline MAI
sector_mai <- (sum(mai_df$volume_m3, na.rm = TRUE) / sum(mai_df$ha_y, na.rm = TRUE)) %>% print()

## Compare to alternate specifications with different treatments of burned areas:
## a) Ignore all fire labels (keep all harvest blocks in dataset)
if_mai <- (sum(mai_df$volume_m3, na.rm = TRUE) / sum(mai_df$ha_y_if, na.rm = TRUE)) %>% print()

## b) (1) Use corrected fire data when year is labeled; (2) keep harvests in blocks with unspecified fires
mf_mai <- (sum(mai_df$volume_m3, na.rm = TRUE) / sum(mai_df$ha_y_mf, na.rm = TRUE)) %>% print()

## c) Drop all blocks with any recorded fires
hf_mai <- (sum(mai_df$volume_m3, na.rm = TRUE) / sum(mai_df$ha_y_hf, na.rm = TRUE)) %>% print()

## Compare to alternate specifications with winsorization of outliers
## a) Winsorize long rotations
rw_mai <- (sum(mai_df$volume_m3, na.rm = TRUE) / sum(mai_df$ha_y_rw, na.rm = TRUE)) %>% print()

## b) Use adjusted volumes based on Winsorized versions of excessively large DMAIs
(sum(nona_mai_df$volume_winsorized, na.rm = TRUE) / sum(nona_mai_df$ha_y, na.rm = TRUE)) %>% print()


## Calculate annual MAI in the sector
year_mai <- mai_df %>% 
  group_by(harvest_year) %>% 
  summarise(ha_y = sum(ha_y, na.rm = TRUE),
            volume_m3 = sum(volume_m3, na.rm = TRUE)) %>% 
  mutate(year_mai = volume_m3 / ha_y,
         ln_mai = log(year_mai)) %>% 
  print()


## MAI plots
nona_mai_df %>% 
  ggplot(aes(x = mai_winsorized)) +
  geom_histogram(bins = 20) +
  geom_vline(xintercept = sector_mai, linetype = "longdash") +
  theme_bw() +
  xlab("Mean annual increment (m3 / ha / y)") +
  ylab("Frequency")

year_mai %>% 
  ggplot(aes(x = harvest_year, y = ha_y)) +
  geom_line() +
  theme_bw(base_size = 16) + 
  xlab("Harvest year") +
  ylab("Total area harvested (ha)") +
  ylim(c(0, 2700000))

year_mai %>% 
  ggplot(aes(x = harvest_year, y = volume_m3)) +
  geom_line() +
  theme_bw(base_size = 16) + 
  xlab("Harvest year") +
  ylab("Total volume produced (m3)") +
  ylim(c(0, 45000000))

year_mai %>% 
  ggplot(aes(x = harvest_year, y = year_mai)) +
  geom_line() +
  theme_bw() + 
  xlab("Harvest year") +
  ylab("Mean annual increment (m3/ha/y)") +
  ylim(c(0, 32)) +
  geom_smooth(method = "lm")

mai_2021 <- year_mai %>% filter(harvest_year == 2021) %>% pull(year_mai) %>% print()

##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Calculate hti-level average MAI -------------------------------------
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
hti_mai <- mai_df %>% 
  group_by(supplier_id) %>% 
  summarise(volume_m3 = sum(volume_m3, na.rm = TRUE),
            ha_y = sum(ha_y, na.rm = TRUE)) %>% 
  filter(volume_m3 > 0,
         ha_y > 0) %>% 
  mutate(dmai = volume_m3 / ha_y,
         dmai_winsorized  = map_dbl(dmai, winsorize_mai))

hti_mai %>% write_csv(paste0(wdir, "/01_data/02_out/tables/hti_mai.csv"))


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Regressions to describe trends in MAI -------------------------------------
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
grow_yield <- function(current_mai, growth_rate, nyears){
  yield_growth <- growth_rate + 1
  future_mai <- (yield_growth^nyears) * current_mai
  return(future_mai)
}

nyears <- 10

nona_mai_df <- nona_mai_df %>% 
  mutate(outlier = dmai != mai_winsorized, 
         ln_mai = log(dmai),
         ln_mai_w = log(mai_winsorized),
         ln_rw = log(dmai_rw),
         ln_if = log(dmai_if),
         ln_mf = log(dmai_mf),
         ln_hf = log(dmai_hf),
         Supplier = supplier_id)

# Result to report in appendix
nona_mai_df %>% group_by(outlier) %>% summarise(volume_m3 = sum(volume_m3)) %>% mutate(shr = prop.table(volume_m3))


controls <- "rotation_length + peat_pct + pr_harvest + pet_harvest"

ols_mod <- feols(as.formula(paste0("ln_mai_w ~", controls, " + harvest_year")), cluster = ~Supplier, data = nona_mai_df)
summary(ols_mod)

nocntrl_mod <- feols(as.formula(paste("ln_mai_w ~ harvest_year | Supplier")), cluster = ~Supplier, data = nona_mai_df)
summary(nocntrl_mod)
grow_yield(sector_mai, coef(nocntrl_mod)["harvest_year"], nyears)

base_mod <- feols(as.formula(paste0("ln_mai_w ~", controls, " + harvest_year | Supplier")), cluster = ~Supplier, data = nona_mai_df)
summary(base_mod)
ci <- confint(base_mod, "harvest_year", level = 0.95) %>% print()


trim_mod <- feols(as.formula(paste("ln_mai_w ~", controls, "+ harvest_year | Supplier")), cluster = ~Supplier, data = nona_mai_df %>% filter(outlier == 0))
summary(trim_mod)
grow_yield(sector_mai, coef(trim_mod)["harvest_year"], nyears)

nowin_mod <- feols(as.formula(paste("ln_mai ~", controls, "+ harvest_year | Supplier")), cluster = ~Supplier, data = nona_mai_df)
summary(nowin_mod)

# # Show these coefficient estimates fall within 95% confidence interval of base model
# confint(base_mod, "harvest_year", level = 0.95)

rw_mod <- feols(as.formula(paste("ln_rw ~", controls, "+ harvest_year | Supplier")), cluster = ~Supplier, data = nona_mai_df)
summary(rw_mod)
rw_mai
grow_yield(rw_mai, coef(rw_mod)["harvest_year"], nyears)

if_mod <- feols(as.formula(paste("ln_if ~", controls, "+ harvest_year | Supplier")), cluster = ~Supplier, data = nona_mai_df)
summary(if_mod)
if_mai
grow_yield(if_mai, coef(if_mod)["harvest_year"], nyears)

hf_mod <- feols(as.formula(paste("ln_hf ~", controls, "+ harvest_year | Supplier")), cluster = ~Supplier, data = nona_mai_df)
summary(hf_mod)
hf_mai
grow_yield(hf_mai, coef(hf_mod)["harvest_year"], nyears)

models <- list(ols_mod, nocntrl_mod, base_mod, trim_mod, nowin_mod, rw_mod, if_mod, hf_mod)

rows <- tribble(~term, ~OLS,  ~NoCntrls, ~F.E., ~Trimmed, ~NoWins, ~ShortenRot, ~IgFire, ~DropFire,
                'Treatment of outliers', 'Winsorize',  'Winsorize', 'Winsorize', 'Drop', 'Keep', 'Winsorize', 'Winsorize', 'Winsorize',
                'Shorten long rotations', 'False', 'False', 'False', 'False', 'False', 'True', 'False', 'False',
                'Treatment of fires', 'Impute', 'Impute', 'Impute', 'Impute', 'Impute', 'Impute', 'Keep', 'Drop',
                'Controls', 'X', '', 'X', 'X', 'X', 'X', 'X', 'X')
attr(rows, 'position') <- c(4, 5, 6, 7)


modelsummary(models, 
             fmt = 3, 
             coef_map = c("harvest_year" = "Year"),
             stars = c('*' = .1, '**' = .05, '***' = 0.01),
             gof_omit = 'DF|Deviance|R2|AIC|BIC|RMSE|Log|Std',
             add_rows = rows)

modelsummary(models, 
             fmt = 3, 
             coef_omit =  c(1, 2, 3, 4), 
             stars = c('*' = .1, '**' = .05, '***' = 0.01),
             coef_rename = c("harvest_year" = "Year"),
             gof_omit = 'DF|Deviance|R2|AIC|BIC|RMSE|Log|Std',
             add_rows = rows,
             output =  paste0(wdir, "/01_data/04_results/yield_growth_table.docx"))


yield_growth <- base_mod$coefficients['harvest_year']
yield_growth_confint <- yield_growth - confint(base_mod, "harvest_year", level = 0.95)[1]


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Export key model parameters -------------------------------------
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
output <- list("dmai" = sector_mai,
               "dmai_2021" = mai_2021,
               "yield_growth" = yield_growth[1],
               "yield_gowth_ci" = yield_growth_confint[1,1]) %>% 
  as_tibble()

write_csv(output, paste0(wdir, "/01_data/04_results/key_parameters.csv"))


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Adding reviewer TFP check -------------------------------------
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Reviewer 2 asked for a series of diagnostic plots to illustrate DMAI trends
# --- Step i: Raw annual DMAI across the sector
p1 = year_mai %>% 
  ggplot(aes(x = harvest_year, y = year_mai)) +
  geom_line() + geom_point() +
  theme_bw() + 
  xlab("Harvest year") +
  ylab("DMAI (m3/ha/y)") +
  # ylim(c(0, 32)) +
  geom_smooth(method = "lm", se = FALSE) +
  ggtitle("(i): Sector-wide DMAI trend")

# --- Step ii: Raw yearly averages of DMAI ---
p2 = nona_mai_df %>%
  group_by(harvest_year) %>%
  summarise(mean_dmai = mean(mai_winsorized),
            se_ln   = sd(mai_winsorized) / sqrt(n()),
            ci_lo   = mean_dmai - 1.96 * se_ln,
            ci_hi   = mean_dmai + 1.96 * se_ln) %>%
  ggplot(aes(x = harvest_year, y = mean_dmai)) +
  # geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.2) +
  geom_line() + geom_point() +
  theme_bw() +
  labs(x = "Harvest year", y = "Mean DMAI (m3/ha/y)",
       title = "(ii): Average DMAI trend") +
  geom_smooth(method = "lm", se = FALSE)

# --- Step iii: Supplier FE only; plot year-averaged residuals ---
eq3_mod <- feols(mai_winsorized ~ 1 | Supplier, data = nona_mai_df, fixef.rm = "none")

resid_df <- nona_mai_df %>%
  ungroup() %>%
  mutate(resid = residuals(eq3_mod)) %>%
  group_by(harvest_year) %>%
  summarise(mean_r = mean(resid),
            se_r   = sd(resid) / sqrt(n()),
            ci_lo  = mean_r - 1.96 * se_r,
            ci_hi  = mean_r + 1.96 * se_r)

p3 = ggplot(resid_df, aes(x = harvest_year, y = mean_r)) +
  # geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.2) +
  geom_line() + geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  labs(x = "Harvest year", y = "Year-avg residual",
       title = "(iii): Year-averaged residuals after supplier FE") + 
  geom_smooth(method = "lm", se = FALSE)

# --- Step iv: year FEs + supplier FE; plot year FEs with CIs ---
ref_year <- 2018
eq4_mod <- feols(mai_winsorized ~ i(harvest_year, ref = ref_year) | Supplier,
                 data = nona_mai_df)
summary(eq4_mod)

eq4_fe_df <- tibble(
  harvest_year = as.numeric(gsub("harvest_year::", "", names(coef(eq4_mod)))),
  estimate     = coef(eq4_mod),
  conf.low     = confint(eq4_mod)[, 1],
  conf.high    = confint(eq4_mod)[, 2]
) %>%
  add_row(harvest_year = ref_year, estimate = 0, conf.low = NA, conf.high = NA)

p4 = ggplot(eq4_fe_df, aes(x = harvest_year, y = estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line() + geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  labs(x = "Harvest year", y = "Year FE",
       title = "(iv): Year FEs (controlling for supplier FE)") +
  geom_smooth(method = "lm", se = FALSE)

# --- Step v: year FEs + supplier FE + weather controls ---
eq5_mod <- feols(mai_winsorized ~ pr_harvest + pet_harvest + i(harvest_year, ref = ref_year) | Supplier,
                       data = nona_mai_df)

eq5_fe_df <- tibble(
  harvest_year = as.numeric(gsub("harvest_year::", "", names(coef(eq5_mod)))),
  estimate     = coef(eq5_mod),
  conf.low     = confint(eq5_mod)[, 1],
  conf.high    = confint(eq5_mod)[, 2]
) %>%
  filter(grepl("harvest_year::", names(coef(eq5_mod)))) %>%
  add_row(harvest_year = ref_year, estimate = 0, conf.low = NA, conf.high = NA)

p5 <- ggplot(eq5_fe_df, aes(x = harvest_year, y = estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line() + geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  labs(x = "Harvest year", y = "Year FE",
       title = "(iv): Year FEs (controlling for supplier\nFE, harvest-year weather)") +
  geom_smooth(method = "lm", se = FALSE)


# --- Step vi: year FEs + supplier FE + weather controls + additional controls ---
eq6_mod <- feols(mai_winsorized ~ rotation_length + peat_pct + pr_harvest + pet_harvest + 
  i(harvest_year, ref = ref_year) | Supplier,
                       data = nona_mai_df)

eq6_fe_df <- tibble(
  harvest_year = as.numeric(gsub("harvest_year::", "", names(coef(eq6_mod)))),
  estimate     = coef(eq6_mod),
  conf.low     = confint(eq6_mod)[, 1],
  conf.high    = confint(eq6_mod)[, 2]
) %>%
  filter(grepl("harvest_year::", names(coef(eq6_mod)))) %>%
  add_row(harvest_year = ref_year, estimate = 0, conf.low = NA, conf.high = NA)

p6 <- ggplot(eq6_fe_df, aes(x = harvest_year, y = estimate)) +
  geom_ribbon(aes(ymin = conf.low, ymax = conf.high), alpha = 0.2) +
  geom_line() + geom_point() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  theme_bw() +
  labs(x = "Harvest year", y = "Year FE",
       title = "(iv): Year FEs (controlling for supplier\nFE and all controls)") +
  geom_smooth(method = "lm", se = FALSE)

# Create combined diagnostic plot
library(patchwork)
combined_plot <- (p1 + p2) / (p3 + p4) / (p5 + p6)
# combined_plot & theme(plot.margin = margin(4, 4, 4, 4))
ggsave(paste0(wdir, "/01_data/04_results/figures/mai_diagnostic_plots.png"),
       plot = combined_plot, height = 10, width = 7, units = "in")

combined_plot
