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


# wood production
ws_2015_2019 <- read_excel(paste0(wdir,"\\01_data\\01_in\\wwi\\RPBBI_2015_2019_compiled.xlsx")) %>%
  select(YEAR,SUPPLIER_ID,VOLUME_M3) %>%
  group_by(YEAR,SUPPLIER_ID) %>%
  summarize(VOLUME_M3 = sum(VOLUME_M3))

ws_2020 <- read_excel(paste0(wdir,"\\01_data\\01_in\\wwi\\RPBBI_2020_compiled.xlsx")) %>%
  select(YEAR,SUPPLIER_ID,VOLUME_M3) %>%
  group_by(YEAR,SUPPLIER_ID) %>%
  summarize(VOLUME_M3 = sum(VOLUME_M3))

ws_2021 <- read_excel(paste0(wdir,"\\01_data\\01_in\\wwi\\RPBBI_2021_compiled.xlsx")) %>%
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
  rename(ha_y_rw = ha_y_w) %>% 
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
winsorize_mai <- function(mai){
  max_mai <- 52.5
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

## b) Winsorize excessively large DMAIs
(sum(nona_mai_df$volume_winsorized, na.rm = TRUE) / sum(nona_mai_df$ha_y, na.rm = TRUE)) %>% print()


## Calculate annual MAI in the sector
year_mai <- nona_mai_df %>% 
  group_by(harvest_year) %>% 
  summarise(ha_y = sum(ha_y),
            volume_m3 = sum(volume_m3)) %>% 
  mutate(year_mai = volume_m3 / ha_y) %>% 
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
  theme_bw(base_size = 15) + 
  xlab("Harvest year") +
  ylab("Mean annual increment (m3/ha/y)") +
  ylim(c(0, 32)) +
  geom_smooth(method = "lm")



##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Regressions to describe trends in MAI -------------------------------------
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nona_mai_df <- nona_mai_df %>% 
  mutate(outlier = dmai != mai_winsorized, 
         ln_mai = log(dmai),
         ln_mai_w = log(mai_winsorized),
         ln_rw = log(dmai_rw),
         ln_if = log(dmai_if),
         ln_mf = log(dmai_mf),
         ln_hf = log(dmai_hf),
         Supplier = supplier_id)

ols_mod <- feols(ln_mai_w ~ harvest_year, cluster = mai_df$Supplier, data = nona_mai_df)
summary(ols_mod)


# multi_harv <- mai_df %>% 
#   select(harvest_year, supplier_id, dmai) %>% 
#   group_by(supplier_id) %>% 
#   tally() %>% 
#   mutate(multi_years = n>1) %>% 
#   select(supplier_id, multi_years)
# 
# mai_df <- mai_df %>% 
#   left_join(multi_harv, by = "supplier_id")
# 
# base_mod <- feols(ln_mai_w ~ harvest_year | supplier_id, data = mai_df %>% filter(multi_years == 1))
# summary(base_mod)
grow_yield <- function(current_mai, growth_mod){
  yield_growth <- hf_mod$coefficients + 1
  future_mai <- (yield_growth^10) * current_mai
  return(future_mai)
}

base_mod <- feols(ln_mai_w ~ harvest_year | Supplier, data = nona_mai_df)
summary(base_mod)
grow_yield(sector_mai, base_mod)

trim_mod <- feols(ln_mai_w ~ harvest_year | Supplier, data = nona_mai_df %>% filter(outlier == 0))
summary(trim_mod)
grow_yield(sector_mai, trim_mod)

nowin_mod <- feols(ln_mai ~ harvest_year | Supplier, data = nona_mai_df)
summary(nowin_mod)


# # Show these coefficient estimates fall within 95% confidence interval of base model
# confint(base_mod, "harvest_year", level = 0.95)

rw_mod <- feols(ln_rw ~ harvest_year | Supplier, data = nona_mai_df)
summary(rw_mod)
grow_yield(rw_mai, rw_mod)

if_mod <- feols(ln_if ~ harvest_year | Supplier, data = nona_mai_df)
summary(if_mod)
grow_yield(if_mai, if_mod)

mf_mod <- feols(ln_mf ~ harvest_year | Supplier, data = nona_mai_df)
summary(mf_mod)
grow_yield(mf_mai, mf_mod)

hf_mod <- feols(ln_hf ~ harvest_year | Supplier, data = nona_mai_df)
summary(hf_mod)
grow_yield(hf_mai, hf_mod)



models <- list(ols_mod, base_mod, trim_mod, nowin_mod, noimpute_mod, noburn_mod)
# models <- list(ols_mod, base_mod, noimpute_mod, noburn_mod)
# models <- list(ols_mod, base_mod)

rows <- tribble(~term,          ~OLS,  ~F.E., ~Trimmed, ~Full, ~NoImpute, ~NoBurn,
                'Treatment of outliers', 'Winsorize',   'Winsorize', 'Drop', 'Keep', 'Winsorize', 'Winsorize',
                'Drop imputed',   'No', 'No', 'No', 'No', 'Yes', 'No',
                'Drop burned', 'No', 'No', 'No', 'No', 'No', 'Yes',)
attr(rows, 'position') <- c(4, 5, 6)


modelsummary(models, 
             fmt = 3, 
             coef_omit = 1, 
             stars = c('*' = .1, '**' = .05, '***' = 0.01),
             coef_rename = c("harvest_year" = "Year"),
             gof_omit = 'DF|Deviance|R2|AIC|BIC|RMSE|Log|Std',
             add_rows = rows,
             output =  paste0(wdir, "/01_data/04_results/yield_growth_table.docx"))



modelsummary(models, stars = TRUE)


modelsummary(models, output =  paste0(wdir, "/01_data/04_results/yield_growth_table.docx"))


yield_growth <- base_mod$coefficients
yield_growth_confint <- yield_growth - confint(base_mod, "harvest_year", level = 0.95)[1]


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Export key model parameters -------------------------------------
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


output <- list("dmai" = sector_mai,
               "yield_growth" = yield_growth[1],
               "yield_gowth_ci" = yield_growth_confint[1,1]) %>% 
  as_tibble()

write_csv(output, paste0(wdir, "/01_data/04_results/key_parameters.csv"))

 
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ## clean data -------------------------------------
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# harvest_df <- harvest_df %>% 
#   select(order(colnames(.)))
# 
# wood_df <- harvest_df %>% 
#   select(HARVEST_YEAR, SUPPLIER_ID, VOLUME_M3) %>% 
#   mutate(VOLUME_M3 = as.numeric(VOLUME_M3))
# 
# harvest_df <- harvest_df %>% 
#   select(-VOLUME_M3)
# 
# harvest_df <- harvest_df %>% 
#   mutate_at(vars(starts_with("X")), ~as.numeric(.))
# 
# harvest_df <- harvest_df %>% 
#   pivot_longer(cols = starts_with("X"), 
#                names_to = "years_since_clear",
#                names_prefix = "X",
#                values_to = "area")
# 
# harvest_df <- harvest_df %>% 
#   mutate(years_since_clear = as.integer(years_since_clear)) %>% 
#   arrange(SUPPLIER_ID, HARVEST_YEAR, years_since_clear) %>% 
#   mutate(last_clear = HARVEST_YEAR - years_since_clear)
# 
# 
# 
# 
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ## explore rotation lengths -------------------------------------
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# # Check David's raw data to see proportion of harvests occurring without prior harvest data
# 
# # Load davids data
# itp_hv <- read_sf(paste0(wdir,"\\01_data\\01_in\\gaveau\\IDN_ITPHarvesting_V20220208\\IDN_ITPHarvesting_V20220208.shp"))
# hti <- read_sf(paste0(wdir,"\\01_data\\01_in\\klhk\\IUPHHK_HT_proj.shp"))
# itp_hv <- st_make_valid(itp_hv) 
# itp_hv_proj <- st_transform(itp_hv, crs = st_crs(hti)) 
# ## Question for David - why do some plantations have no Harvest1, but do have a Harvest2?
# 
# itp_hv_long <- itp_hv_proj %>% pivot_longer(cols = starts_with("Harvest"),names_to = "harvest",
#                         names_prefix = "Harvest",
#                         values_to = "harvest_year")
# itp_hv_long <- itp_hv_long %>% 
#   filter(harvest_year > 0)
# no_prior_harvest <- itp_hv_long %>% filter(harvest ==1, harvest_year >= 2015) %>% pull(Shape_Area) %>% sum()
# total_harvest <- itp_hv %>% pull(Shape_Area) %>% sum()
# prop_harvest_data <- (1 - (no_prior_harvest / total_harvest)) %>% print()
# 
# # Compute rotation lengths for second and third harvests
# itp_rot <- itp_hv_proj %>% 
#   mutate(Harvest1 = replace(Harvest1, Harvest1==0, NA),
#          Harvest2 = replace(Harvest2, Harvest2==0, NA),
#          Harvest3 = replace(Harvest3, Harvest3==0, NA),
#          rot1 = Harvest2 - Harvest1,
#          rot2 = Harvest3 - Harvest1) %>% 
#   pivot_longer(cols = starts_with("rot"), names_to = "rotation", values_to = "rotation_length") %>% 
#   st_drop_geometry()
# 
# itp_rot <- itp_rot %>% 
#   select(OBJECTID, Shape_Area, rotation_length) %>% 
#   drop_na()
# 
# harvest_year_summary <- itp_rot %>% 
#   # filter(HARVEST_YEAR==2020) %>%
#   group_by(rotation_length) %>% 
#   summarise(area_sum = sum(Shape_Area))
#   
# harvest_year_summary %>% 
#   ggplot(aes(x = rotation_length, y = area_sum)) +
#   geom_line() +
#   theme_bw(base_size = 16) +
#   xlab("Years since last clearing when harvested") +
#   ylab("Total area harvested over 2015-2020 (ha)")
# 
# 
# # Proportion of harvests occurring in 4-6 years
# total_harvests <- harvest_year_summary %>% pull(area_sum) %>% sum()
# prop_4_6 <- ((harvest_year_summary %>% filter(rotation_length >= 4, rotation_length <= 6) %>% pull(area_sum) %>% sum()) / total_harvests) %>% print()
# 
# # Proportion of harvests occurring within 7 years
# prop_5 <- ((harvest_year_summary %>% filter(rotation_length == 5) %>% pull(area_sum) %>% sum()) / total_harvests) %>% print()
# prop_6 <- ((harvest_year_summary %>% filter(rotation_length <= 6) %>% pull(area_sum) %>% sum()) / total_harvests) %>% print()
# prop_7 <- ((harvest_year_summary %>% filter(rotation_length <= 7) %>% pull(area_sum) %>% sum()) / total_harvests) %>% print()
# 
# 
# 
# #
# harvest_year_summary <- harvest_df %>% 
#   # filter(HARVEST_YEAR==2020) %>%
#   filter(last_clear > 2010) %>% 
#   group_by(years_since_clear) %>% 
#   summarise(area_sum = sum(area))
# 
# # Proportion of harvests occurring in 4-6 years
# total_harvests <- harvest_year_summary %>% pull(area_sum) %>% sum()
# prop_4_6 <- ((harvest_year_summary %>% filter(years_since_clear >= 4, years_since_clear <= 6) %>% pull(area_sum) %>% sum()) / total_harvests) %>% print()
# 
# # Proportion of harvests occurring within 7 years
# prop_5 <- ((harvest_year_summary %>% filter(years_since_clear == 5) %>% pull(area_sum) %>% sum()) / total_harvests) %>% print()
# prop_6 <- ((harvest_year_summary %>% filter(years_since_clear <= 6) %>% pull(area_sum) %>% sum()) / total_harvests) %>% print()
# prop_7 <- ((harvest_year_summary %>% filter(years_since_clear <= 7) %>% pull(area_sum) %>% sum()) / total_harvests) %>% print()
# 
# 
# harvest_year_summary %>% 
#   ggplot(aes(x = years_since_clear, y = area_sum)) +
#   geom_line() +
#   theme_bw(base_size = 16) +
#   xlab("Years since last clearing when harvested") +
#   # ylab("Total area harvested in 2019 (ha)")
#   ylab("Total area harvested over 2015-2020 (ha)")
#   
# # Explore what proportion of harvests include years before David started mapping 
# test <- harvest_df %>% 
#   filter(HARVEST_YEAR >= 2015) %>% 
#   mutate(pre_gaveau_harvest = last_clear < 2010) %>% 
#   group_by(pre_gaveau_harvest, HARVEST_YEAR) %>% 
#   summarise(area = sum(area)) %>% 
#   # mutate(freq = area / sum(area)) %>% 
#   print()
# 
# # test <- harvest_df %>% 
# #   filter(HARVEST_YEAR==2020) %>%
# #   group_by(last_clear) %>% 
# #   summarise(area = sum(area)) %>% 
# #   mutate(freq = area / sum(area)) %>% 
# #   print()
# 
# 
# 
# ## NOTE: based on rotation length plot, set max length for rotation; 
# ## If rotations were actually longer, MAI would be even smaller, so this is a conservative estimate
# max_rotation <- 7
# 
# 
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ## Calculate MAI -------------------------------------
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ## Filter to acacia?
# 
# ## Adjust long rotations
# harvest_df <- harvest_df %>% 
#   mutate(grow_y = ifelse((last_clear<2010) & (years_since_clear>=max_rotation), max_rotation, years_since_clear),
#          grow_ha_y = area * grow_y)
# 
# harvest_df <- harvest_df %>% 
#   group_by(SUPPLIER_ID, HARVEST_YEAR) %>% 
#   summarise(grow_ha_y = sum(grow_ha_y),
#             area = sum(area)) %>% 
#   left_join(wood_df, by = c("SUPPLIER_ID", "HARVEST_YEAR")) %>% 
#   ungroup()
# 
# ## Calculate concession by year MAI
# harvest_df <- harvest_df %>% 
#   mutate(mai = VOLUME_M3 / grow_ha_y)
# 
# ## Calculate MAI for entire sector: Note, it's reassuring that this is virtually unchanged if run before or after winsorizing
# (sum(harvest_df$VOLUME_M3) / sum(harvest_df$grow_ha_y)) %>% print() # m3 / ha / y
# 
# 
# ## Filter unreasonable MAIs
# # From Hardiyanto et al., 2023: The best treatment (comprising low impact harvesting, removal of only merchantable stem wood, conservation of organic matter, planting with improved germplasm, weed control and application of P at planting), yielded an MAI of 52.5 m3 ha-1 y-1, one of the highest growth rates reported for 230 tropical plantations (Nambiar, 2008).
# max_mai <- 52.5
# harvest_df <- harvest_df %>% 
#   mutate(outlier = mai > max_mai, 
#          mai_winsorized  = ifelse(outlier, max_mai, mai),
#          volume_winsorized = mai_winsorized * grow_ha_y) # winsorize outliers
# sector_mai <- (sum(harvest_df$volume_winsorized) / sum(harvest_df$grow_ha_y)) %>%  print()  # m3 / ha / y
# 
# ## Calculate annual MAI across sector
# year_mai <- harvest_df %>% 
#   group_by(harvest_year) %>% 
#   summarise(grow_ha_y = sum(ha_y),
#             area = sum(area),
#             VOLUME_M3 = sum(volume_winsorized)) %>% 
#   mutate(year_mai = VOLUME_M3 / grow_ha_y)
# 
# 
# ## MAI plots
# harvest_df %>% 
#   ggplot(aes(x = mai)) +
#   geom_histogram(bins = 20) +
#   geom_vline(xintercept = sector_mai, linetype = "longdash") +
#   theme_bw() +
#   xlab("Mean annual increment (m3 / ha / y") +
#   ylab("Frequency")
# 
# year_mai %>% 
#   ggplot(aes(x = HARVEST_YEAR, y = area)) +
#   geom_line() +
#   theme_bw(base_size = 16) + 
#   xlab("Harvest year") +
#   ylab("Total area harvested (ha)") +
#   ylim(c(0, 380000))
# 
# year_mai %>% 
#   ggplot(aes(x = HARVEST_YEAR, y = VOLUME_M3)) +
#   geom_line() +
#   theme_bw(base_size = 16) + 
#   xlab("Harvest year") +
#   ylab("Total volume produced (m3)") +
#   ylim(c(0, 45000000))
# 
# year_mai %>% 
#   ggplot(aes(x = HARVEST_YEAR, y = year_mai)) +
#   geom_line() +
#   theme_bw(base_size = 15) + 
#   xlab("Harvest year") +
#   ylab("Mean annual increment (m3/ha/y)") +
#   ylim(c(0, 32)) +
#   geom_smooth(method = "lm")
# 
# # Improvement in MAI over the 6 years observed
# mod <- lm(year_mai ~ HARVEST_YEAR, data = year_mai %>% filter(outlier==0))
# summary(mod)
# 
# harvest_df <- harvest_df %>% 
#   mutate(ln_mai = log(mai),
#          ln_mai_w = log(mai_winsorized))
# 
# mod <- lm(ln_mai ~ HARVEST_YEAR, data = harvest_df %>% filter(outlier==0))
# summary(mod)
# 
# mod <- feols(ln_mai ~ HARVEST_YEAR | SUPPLIER_ID, data = harvest_df %>% filter(outlier == 0))
# summary(mod)
# 
# # NEED TO ADD ONE MORE REGRESSION SHOWING SENSITIVITY TO ROTATION IMPUTATION
# 
# 
# mod <- feols(ln_mai_w ~ HARVEST_YEAR | SUPPLIER_ID, data = harvest_df)
# summary(mod)
# 
# 
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ## Calculate MAI -------------------------------------
# ##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# ## Summary of proposed expansions: remote/01_data/01_in/new_capacity/planned_expansions.xlsx"
# oki_exp_mt <- 4.2
# rapp_exp_mt <- 2
# phoenix_exp_mt <- 1.7
# total_exp_mt <- oki_exp_mt + rapp_exp_mt + phoenix_exp_mt
# baseline_cap_mt <- 9.3 ## TODO: Check this with Brian. Doesn't match (mills$PULP_CAP_2019_MTPY %>% sum())
# 
# # Line 102: Together, these three projects would increase the country’s pulp capacity by 91% and, once fully operational, would lead to a concomitant XX m3 increase in the country’s annual demand for pulpwood. 
# total_exp_mt
# cap_change <- (total_exp_mt / baseline_cap_mt) %>% print()
# 
# # Estimate of land demand from capacity expansions
# current_wood_demand <- pw_supply_2022 %>% pull(VOLUME_M3) %>%  sum()
# new_wood_demand <- (current_wood_demand * cap_change) %>% print()
# 
# # new_wood_demand <- 30600000 # m3 / y - taken from Brian's calculations in paper draft. Was for original expansion estimates without PT phoenix
# (area_demand <- new_wood_demand / sector_mai) # ha
# 
# harvest_df %>% write_csv(paste0(wdir, "/01_data/02_out/tables/hti_mai.csv"))
