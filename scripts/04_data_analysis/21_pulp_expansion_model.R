#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Author: Robert Heilmayr
# Project: Indonesia pulp deforestation
# Date: 2-25-26
# Purpose: Build spatial model of pulp expansion locations
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# load packages --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(tidyverse)
library(tidylog)
library(tidymodels)
library(ranger)
library(themis)   # step_downsample
library(vip)        # variable importance plots
library(probably)   # calibration plots
library(future)     # parallel backend for tune_grid
library(sf)
library(terra)
library(tmap)
library(cols4all)
library(gt)
library(pdp)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# load data --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
wdir <- "remote/"
p1_df <- read_csv(paste0(wdir, "01_data/02_out/tables/pulp_exp_model_var_1km_2017.csv")) %>%
  rename_with(tolower) %>%
  rename(pulp_start = pulp_2017, palm_start = palm_2017, forest_start = forest_2017,
         hti_start = hti_risk_2017, dist_mill = dist_mill_2017) %>%
  rename_with(~ str_replace(., "^y2017_a", "ya_"), starts_with("y2017_a")) %>%
  mutate(across(c(tmmx, tmmn, pr, pet, def, clay_content, soil_ph, gaez_cat), ~ na_if(., -9999)))

p2_df <- read_csv(paste0(wdir, "01_data/02_out/tables/pulp_exp_model_var_1km_2022.csv")) %>%
  rename_with(tolower) %>%
  rename(pulp_start = pulp_2022, palm_start = palm_2022, forest_start = forest_2022,
         hti_start = hti_risk_2022, dist_mill = dist_mill_2022) %>%
  rename_with(~ str_replace(., "^y2022_a", "ya_"), starts_with("y2022_a")) %>%
  mutate(across(c(tmmx, tmmn, pr, pet, def, clay_content, soil_ph, gaez_cat), ~ na_if(., -9999)))
# admin_df <- read_csv(paste0(wdir, "01_data/02_out/tables/grid_10km_adm_prov_kab.csv"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# set up estimation dataframe --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
est_df <- p1_df %>%
  left_join(p2_df %>% select(pixel_id, pulp_end = pulp_start), by = "pixel_id")

est_df <- est_df %>%
  filter(pulp_start == 0)

# Encode gaez and kh (kawasan hutan) as a labelled factor — codes are arbitrary KLHK IDs,
# not a continuous or ordinal scale
gaez_levels <- c(
  "1" = "No limitations",
  "2" = "Hydromorphic",
  "3" = "Terrain",
  "4" = "Other"
)

kh_levels <- c(
  "0"      = "Not yet defined",
  "1"      = "Nature sanctuary / conservation area",
  "1001"   = "Protected forest",
  "1002"   = "Nature sanctuary and recreation forest",
  "1003"   = "Production forest",
  "1004"   = "Limited production forest",
  "1005"   = "Convertible production forest",
  "1007"   = "Other land use area",
  "5001"   = "Lake / river",
  "5003"   = "Sea / water",
  "10021"  = "Nature reserve",
  "10022"  = "Wildlife sanctuary",
  "10023"  = "Hunting park",
  "10024"  = "National park",
  "10025"  = "Nature recreation park",
  "10026"  = "Community forest park",
  "100201" = "Terrestrial nature sanctuary",
  "100211" = "Marine nature reserve",
  "100221" = "Marine wildlife sanctuary",
  "100241" = "Marine national park",
  "100251" = "Marine nature recreation park"
)
est_df <- est_df %>%
  mutate(kh       = factor(kh,       levels = as.integer(names(kh_levels)),   labels = kh_levels),
         gaez_cat = factor(gaez_cat, levels = as.integer(names(gaez_levels)), labels = gaez_levels))


# est_df <- est_df %>%
#   sample_n(10000)

glimpse(est_df)
count(est_df, pulp_end)  # check class balance
count(est_df, kh)        # verify kh encoding


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# build predictive model of deforestation --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# --- 1. Select features (start-of-period baseline only; exclude post-period vars) ---
model_df <- est_df %>%
  select(
    pixel_id, pulp_end, kab_code,     # pixel ID + outcome + spatial grouping variable
    dist_mill, dist_water_m,          # mill access
    hti_start,                        # industrial concession at baseline
    forest_start, palm_start,         # forest / op cover at baseline
    peat, op_conc, wdpa,              # land type indicators
    elevation, slope, gaez_cat,       # physical geography
    tmmx, tmmn, pr, pet, def,         # climate
    clay_content, soil_ph, kh,        # soil properties
    starts_with("ya_")                # 64 spectral anomaly indices
  ) %>%
  mutate(
    pulp_end = factor(pulp_end, levels = c(1, 0), labels = c("pulp", "no_pulp")),
    kab_code = factor(kab_code)
  )

# Drop NA pixels; verify loss is < 2% of sample
n_before  <- nrow(model_df)
model_df  <- drop_na(model_df)
pct_dropped <- (n_before - nrow(model_df)) / n_before
message(sprintf("Dropped %d pixels with NAs (%.1f%% of sample)", n_before - nrow(model_df), pct_dropped * 100))
stopifnot("More than 2% of pixels dropped — check NA sources" = pct_dropped < 0.02)

# Class weights to compensate for class imbalance (replaces step_downsample)
prevalence <- mean(model_df$pulp_end == "pulp")
class_wts  <- c(pulp = 1 - prevalence, no_pulp = prevalence)

# Cap majority class at 10:1 ratio for memory/speed
n_pulp   <- sum(model_df$pulp_end == "pulp")
model_df <- bind_rows(
  model_df %>% filter(pulp_end == "pulp"),
  model_df %>% filter(pulp_end == "no_pulp") %>% sample_n(min(n_pulp * 10, n()))
)

# --- 2. Spatial train/test split + CV on training data only ---
set.seed(42)
data_split <- group_initial_split(model_df, group = kab_code, prop = 0.8)
train_df   <- training(data_split)
test_df    <- testing(data_split)
cv_folds   <- group_vfold_cv(train_df, group = kab_code, v = 5)

# --- 3. Recipe ---
rf_recipe <- recipe(pulp_end ~ ., data = model_df) %>%
  update_role(kab_code,  new_role = "ID") %>%      # keep for grouping, exclude from model
  update_role(pixel_id,  new_role = "ID")           # carry through for evaluation joins

# --- 4. Model specification ---
rf_spec <- rand_forest(
  trees = 500,
  mtry  = tune(),
  min_n = tune()
) %>%
  set_engine("ranger", importance = "permutation", class.weights = !!class_wts) %>%
  set_mode("classification")

# --- 5. Workflow ---
rf_workflow <- workflow() %>%
  add_recipe(rf_recipe) %>%
  add_model(rf_spec)

# --- 6. Hyperparameter tuning over spatial CV folds ---
rf_grid <- grid_regular(
  mtry(range = c(5, 30)),
  min_n(range = c(5, 30)),
  levels = 4  # 4x4 = 16 combinations
)

plan(multisession, workers = parallel::detectCores() - 1)
rf_tune <- tune_grid(
  rf_workflow,
  resamples = cv_folds,
  grid      = rf_grid,
  metrics   = metric_set(roc_auc, pr_auc, sensitivity, specificity),
  control   = control_grid(save_pred = TRUE, verbose = TRUE)
)

# --- 7. Review CV results ---
collect_metrics(rf_tune) %>%
  filter(.metric == "roc_auc") %>%
  arrange(desc(mean)) %>%
  print(n = 16)

autoplot(rf_tune)

# --- 8. Select best hyperparameters, evaluate on held-out test, fit final model on all data ---
best_params    <- select_best(rf_tune, metric = "roc_auc")
final_workflow <- finalize_workflow(rf_workflow, best_params)

# Fit on train + evaluate on held-out test set (unbiased performance estimate)
last_fit_result <- last_fit(final_workflow, data_split,
                            metrics = metric_set(roc_auc, pr_auc))

# Fit on all data for spatial prediction maps
set.seed(42)
final_fit <- fit(final_workflow, data = model_df)

# Save / reload final model (skip re-tuning in future runs)
saveRDS(final_fit, paste0(wdir, "01_data/02_out/models/rf_final_fit.rds"))
# final_fit <- readRDS(paste0(wdir, "01_data/02_out/models/rf_final_fit.rds"))

# --- 9. Predicted probabilities for all pixels ---
predictions_df <- augment(final_fit, new_data = model_df)


predictions_df %>%
  select(.pred_pulp) %>%
  summary()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# evaluate model performance --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_preds <- collect_predictions(last_fit_result) %>%
  mutate(pixel_id = test_df$pixel_id[.row])

# --- 1. Discrimination metrics ---
collect_metrics(last_fit_result)                             # roc_auc, pr_auc summary

roc_curve(test_preds, truth = pulp_end, .pred_pulp) %>% autoplot()
pr_curve(test_preds,  truth = pulp_end, .pred_pulp) %>% autoplot()

# --- 2. Brier score (combines discrimination + calibration) ---
brier_class(test_preds, truth = pulp_end, .pred_pulp)

# --- 3. Calibration plot (predicted probability vs. observed conversion rate) ---
cal_plot_breaks(test_preds, truth = pulp_end, estimate = .pred_pulp, num_breaks = 10)

# --- 4. Confusion matrix at 0.5 threshold ---
test_preds %>%
  mutate(.pred_class = if_else(.pred_pulp >= 0.5, "pulp", "no_pulp"),
         .pred_class = factor(.pred_class, levels = c("pulp", "no_pulp"))) %>%
  conf_mat(truth = pulp_end, estimate = .pred_class) %>%
  tidy() %>%
  mutate(
    actual    = if_else(str_detect(name, "^cell_1_"), "Actual: pulp", "Actual: no_pulp"),
    predicted = if_else(str_detect(name, "_1$"),      "Predicted: pulp", "Predicted: no_pulp")
  ) %>%
  select(actual, predicted, value) %>%
  pivot_wider(names_from = predicted, values_from = value)

# --- 5. Variable importance (top 20 features) ---
final_fit %>%
  extract_fit_parsnip() %>%
  vip(num_features = 20)

# --- 6. Partial dependence plots for top 10 variables ---
priority_vars <- c("hti_start", "dist_mill", "forest_start", "dist_water_m", "palm_start")

top10_vars <- final_fit %>%
  extract_fit_parsnip() %>%
  vi() %>%
  slice_max(Importance, n = 10) %>%
  pull(Variable)

pdp_vars <- union(priority_vars, top10_vars)

# Bake recipe to get predictor matrix in the form ranger expects
train_baked <- prep(rf_recipe) %>%
  bake(new_data = model_df) %>%
  select(-pulp_end, -pixel_id, -kab_code) %>%
  slice_sample(n = 2000)  # subsample for speed; PDPs are averaged anyway

rf_engine <- extract_fit_parsnip(final_fit)$fit  # underlying ranger object

# Split vars: kh is categorical; all others are continuous
numeric_pdp_vars <- pdp_vars[pdp_vars != "kh"]
has_kh <- "kh" %in% pdp_vars

# Continuous PDPs: line plots
pdp_df <- map_dfr(numeric_pdp_vars, \(var)
  pdp::partial(rf_engine, pred.var = var, train = train_baked,
               which.class = 1, prob = TRUE) %>%
    as_tibble() %>%
    rename(x_val = 1) %>%
    mutate(variable = var)
)

ggplot(pdp_df, aes(x = x_val, y = yhat)) +
  geom_line() +
  geom_rug(data = map_dfr(numeric_pdp_vars, \(var)
    tibble(x_val = train_baked[[var]], variable = var)),
    aes(x = x_val, y = NULL), sides = "b", alpha = 0.1, length = unit(0.03, "npc")) +
  facet_wrap(~ variable, scales = "free_x", ncol = 5) +
  labs(x = NULL, y = "P(pulp expansion)",
       title = "Partial dependence: continuous predictors")

# kh PDP: bar chart (only rendered if kh is among the plotted variables)
if (has_kh) {
  pdp::partial(rf_engine, pred.var = "kh", train = train_baked,
               which.class = 1, prob = TRUE) %>%
    as_tibble() %>%
    ggplot(aes(x = yhat, y = reorder(kh, yhat))) +
    geom_col() +
    labs(x = "P(pulp expansion)", y = "Forest estate class (kh)",
         title = "Partial dependence: forest estate class")
}


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# apply model to 2022 baseline for 2022-2027 predictions --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pred2027_input <- p2_df %>%
  filter(pulp_start == 0) %>%
  select(
    pixel_id, kab_code, dist_mill, dist_water_m,
    hti_start, forest_start, palm_start,
    peat, op_conc, wdpa,
    elevation, slope,
    tmmx, tmmn, pr, pet, def,
    clay_content, soil_ph, kh, gaez_cat,
    starts_with("ya_")
  ) %>%
  mutate(
    kab_code = factor(kab_code),
    kh       = factor(kh,       levels = as.integer(names(kh_levels)),   labels = kh_levels),
    gaez_cat = factor(gaez_cat, levels = as.integer(names(gaez_levels)), labels = gaez_levels)
  )

n_before2027 <- nrow(pred2027_input)
pred2027_input <- drop_na(pred2027_input)
pct_dropped2027 <- (n_before2027 - nrow(pred2027_input)) / n_before2027
message(sprintf("Dropped %d pixels with NAs (%.1f%% of sample)", n_before2027 - nrow(pred2027_input), pct_dropped2027 * 100))
stopifnot("More than 2% of pixels dropped — check NA sources" = pct_dropped2027 < 0.02)

predictions2027_df <- augment(final_fit, new_data = pred2027_input)

predictions2027_df %>%
  select(.pred_pulp) %>%
  summary()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# map predicted pulp expansion probabilities --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# --- 1. Load pixel coordinates (panel file; deduplicate to one row per pixel) ---
coords_df <- p1_df %>%
  select(pixel_id, lat, lon)

# --- 2. Build sf points layer ---
pred_sf <- predictions_df %>%
  left_join(coords_df, by = "pixel_id") %>%
  drop_na(lon, lat) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)

nrow(pred_sf) == nrow(predictions_df)  # sanity check: no pixels lost

# --- 3. Aggregate 1km predictions to 10km raster (~0.1 degree resolution) ---
pred_vect <- pred_sf %>%
  mutate(converted = as.integer(pulp_end == "pulp")) %>%
  vect()
rast_template <- rast(pred_vect, resolution = 0.1)
pred_rast <- rasterize(pred_vect, rast_template, field = ".pred_pulp", fun = mean)
names(pred_rast) <- "pred_pulp"
obs_rast <- rasterize(pred_vect, rast_template, field = "converted", fun = mean)
names(obs_rast) <- "obs_conversion"

# --- 4. Spatial calibration: predicted vs. observed at 10km grid (held-out test set) ---
test_spatial_sf <- test_preds %>%
  mutate(converted = as.integer(pulp_end == "pulp")) %>%
  left_join(coords_df, by = "pixel_id") %>%
  drop_na(lon, lat) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)
test_vect      <- vect(test_spatial_sf)
test_pred_rast <- rasterize(test_vect, rast_template, field = ".pred_pulp", fun = mean)
test_obs_rast  <- rasterize(test_vect, rast_template, field = "converted",  fun = mean)

spatial_cal_df <- tibble(
  pred = values(test_pred_rast)[, 1],
  obs  = values(test_obs_rast)[, 1]
) %>% drop_na()

cor(spatial_cal_df$pred, spatial_cal_df$obs, method = "spearman")

ggplot(spatial_cal_df, aes(x = obs, y = pred)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_smooth(method = "lm", se = FALSE, colour = "steelblue") +
  geom_abline(linetype = "dashed", colour = "grey50") +
  labs(x = "Observed conversion rate (10km grid, test set)",
       y = "Mean predicted P(conversion) (10km grid, test set)",
       title = "Spatial calibration: held-out test set, 10km grid cells")

# --- 5. Aggregate 2022-2027 predictions to 10km raster ---
coords2027_df <- p2_df %>% select(pixel_id, lat, lon)
pred2027_sf <- predictions2027_df %>%
  left_join(coords2027_df, by = "pixel_id") %>%
  drop_na(lon, lat) %>%
  st_as_sf(coords = c("lon", "lat"), crs = 4326)
pred2027_vect <- vect(pred2027_sf)
pred2027_rast <- rasterize(pred2027_vect, rast_template, field = ".pred_pulp", fun = mean)
names(pred2027_rast) <- "pred_pulp_2027"

# --- 6. Build province boundary layer (dissolve kabupaten shapefile) ---
kab_sf  <- read_sf(paste0(wdir, "01_data/01_in/big/idn_kabupaten_big.shp"))
prov_sf <- kab_sf %>%
  group_by(prov, prov_code) %>%
  summarise(.groups = "drop")

# --- 7. Interactive tmap ---
tmap_mode("view")

pulp_map <- tm_shape(pred_rast) +
  tm_raster(
    col     = "pred_pulp",
    palette = "brewer.yl_or_rd",
    col_alpha   = 0.8,
    title   = "Predicted pulp expansion, 2017-2022)"
  ) +
tm_shape(obs_rast, group = "Observed conversion rate (2017-2022)") +
  tm_raster(
    col     = "obs_conversion",
    palette = "brewer.blues",
    col_alpha   = 0.8,
    title   = "Observed pulp expansion (2017-2022)"
  ) +
tm_shape(pred2027_rast, group = "Predicted P(pulp expansion, 2022-2027)") +
  tm_raster(
    col     = "pred_pulp_2027",
    palette = "brewer.yl_or_rd",
    col_alpha   = 0.8,
    title   = "Predicted pulp expansion, 2022-2027)"
  ) +
tm_shape(prov_sf) +
  tm_borders(col = "grey40", lwd = 1) +
tm_title("Predicted probability of pulp expansion")

pulp_map
htmlwidgets::saveWidget(
  tmap_leaflet(pulp_map),
  file          = paste0(wdir, "01_data/04_results/figures/pulp_expansion_map.html"),
  selfcontained = TRUE
)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# pulp expansion scenarios --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
exp_area_1 <- 1523000  # Predicted area of pulp expansion in hectares
exp_area_2 <- 668000  # Predicted area of pulp expansion in hectares
exp_area_3 <- 12000  # Predicted area of pulp expansion in hectares


# All pixels in predictions2027_df have pulp_start == 0 (enforced by filter in
# pred2027_input), so the full set is eligible for expansion

# --- 1. Derive island and land type for each candidate pixel ---
scenario_df <- predictions2027_df %>%
  mutate(
    island = case_when(
      str_sub(as.character(kab_code), 1, 1) == "1" ~ "Sumatera",
      str_sub(as.character(kab_code), 1, 1) == "6" ~ "Kalimantan"
    ),
    land_type = case_when(
      forest_start == 1 & peat == 1 ~ "Forest on peat",
      forest_start == 1 & peat == 0 ~ "Forest off peat",
      forest_start == 0 & peat == 1 ~ "Non-forest on peat",
      TRUE                           ~ "Other"
    )
  ) %>%
  filter(!is.na(island))  # keep Sumatera + Kalimantan only

# --- 2. Helper functions ---
# Select top-probability pixels up to the target expansion area
select_scenario <- function(df, exp_area_ha) {
  n_px <- exp_area_ha / 100  # 1 pixel = 1 km² = 100 ha
  df %>%
    slice_max(.pred_pulp, n = n_px, with_ties = FALSE) %>%
    mutate(area_ha = 100)
}

# Summarise selected pixels by island x land type, with island subtotals
summarise_scenario <- function(scenario_df) {
  by_type <- scenario_df %>%
    group_by(island, land_type) %>%
    summarise(area_ha = sum(area_ha), .groups = "drop")

  bind_rows(
    by_type,
    by_type %>%
      group_by(island) %>%
      summarise(area_ha = sum(area_ha), .groups = "drop") %>%
      mutate(land_type = "Island total")
  ) %>%
    arrange(island, land_type == "Island total", land_type)
}

# --- 3. Run all three scenarios ---
scenario1_df <- select_scenario(scenario_df, exp_area_1)
scenario2_df <- select_scenario(scenario_df, exp_area_2)
scenario3_df <- select_scenario(scenario_df, exp_area_3)

# --- 4. Build combined table with one column per scenario ---
expansion_table <- summarise_scenario(scenario1_df) %>%
  rename(scenario_1_ha = area_ha) %>%
  left_join(summarise_scenario(scenario2_df) %>% rename(scenario_2_ha = area_ha),
            by = c("island", "land_type")) %>%
  left_join(summarise_scenario(scenario3_df) %>% rename(scenario_3_ha = area_ha),
            by = c("island", "land_type")) %>%
  mutate(across(c(scenario_1_ha, scenario_2_ha, scenario_3_ha), ~ replace_na(.x, 0)))

expansion_table %>%
  gt(groupname_col = "island", rowname_col = "land_type") %>%
  cols_label(
    scenario_1_ha = "S1: No productivity growth",
    scenario_2_ha = "S2: 3.0% annual growth",
    scenario_3_ha = "S3: 5.9% annual growth"
  ) %>%
  fmt_number(columns = c(scenario_1_ha, scenario_2_ha, scenario_3_ha), decimals = 0)

# --- Reorganised table: land type as rows, islands as columns, S2 (S3–S1) cells ---
fmt_cell <- function(s2, s3, s1) {
  paste0(formatC(s2, format = "d", big.mark = ","),
         " (", formatC(s3, format = "d", big.mark = ","),
         "–", formatC(s1, format = "d", big.mark = ","), ")")
}

reorg_base <- expansion_table %>%
  filter(land_type != "Island total") %>%
  mutate(across(c(scenario_1_ha, scenario_2_ha, scenario_3_ha), ~ round(.x / 1000)))

# Total column: sum across islands for each land type
reorg_totals_col <- reorg_base %>%
  group_by(land_type) %>%
  summarise(across(c(scenario_1_ha, scenario_2_ha, scenario_3_ha), sum), .groups = "drop") %>%
  mutate(Total = fmt_cell(scenario_2_ha, scenario_3_ha, scenario_1_ha)) %>%
  select(land_type, Total)

# Total row: sum across land types for each island + overall total
reorg_totals_row <- reorg_base %>%
  group_by(island) %>%
  summarise(across(c(scenario_1_ha, scenario_2_ha, scenario_3_ha), sum), .groups = "drop") %>%
  mutate(cell = fmt_cell(scenario_2_ha, scenario_3_ha, scenario_1_ha),
         land_type = "Total") %>%
  select(land_type, island, cell) %>%
  pivot_wider(names_from = island, values_from = cell, values_fill = "0 (0–0)") %>%
  left_join(
    reorg_base %>%
      summarise(across(c(scenario_1_ha, scenario_2_ha, scenario_3_ha), sum)) %>%
      mutate(Total = fmt_cell(scenario_2_ha, scenario_3_ha, scenario_1_ha)) %>%
      select(Total),
    by = character()
  )

reorg_base %>%
  mutate(cell = fmt_cell(scenario_2_ha, scenario_3_ha, scenario_1_ha)) %>%
  select(land_type, island, cell) %>%
  pivot_wider(names_from = island, values_from = cell, values_fill = "0 (0–0)") %>%
  left_join(reorg_totals_col, by = "land_type") %>%
  bind_rows(reorg_totals_row) %>%
  gt(rowname_col = "land_type") %>%
  tab_header(
    title    = "Projected pulp expansion area by land type and island",
    subtitle = "Central estimate (low–high), thousand ha; S2 = 3.0% growth, S3 = 5.9% growth, S1 = no growth"
  ) %>%
  cols_label(
    Kalimantan = "Kalimantan",
    Sumatera   = "Sumatera",
    Total      = "Total"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = list(cells_body(rows = "Total"), cells_column_labels(columns = "Total"))
  )

# --- 5. Rasterise scenario expansion pixels and add to map ---
rasterise_scenario <- function(scenario_df, rast_template, coords_df) {
  scenario_df %>%
    left_join(coords_df, by = "pixel_id") %>%
    drop_na(lon, lat) %>%
    st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
    vect() %>%
    rasterize(rast_template, field = "area_ha", fun = sum)
}

scenario1_rast <- rasterise_scenario(scenario1_df, rast_template, coords2027_df)
scenario2_rast <- rasterise_scenario(scenario2_df, rast_template, coords2027_df)
scenario3_rast <- rasterise_scenario(scenario3_df, rast_template, coords2027_df)
names(scenario1_rast) <- "scenario1_expansion"
names(scenario2_rast) <- "scenario2_expansion"
names(scenario3_rast) <- "scenario3_expansion"

pulp_map <- pulp_map +
  tm_shape(scenario1_rast, group = "Scenario 1: No productivity growth") +
  tm_raster(
    col       = "scenario1_expansion",
    palette   = "brewer.greens",
    col_alpha = 0.8,
    title     = "Low productivity-growth expansion (ha)"
  ) +
  tm_shape(scenario2_rast, group = "Scenario 2: 3.0% annual growth") +
  tm_raster(
    col       = "scenario2_expansion",
    palette   = "brewer.greens",
    col_alpha = 0.8,
    title     = "Moderate productivity-growth expansion (ha)"
  ) +
  tm_shape(scenario3_rast, group = "Scenario 3: 5.9% annual growth") +
  tm_raster(
    col       = "scenario3_expansion",
    palette   = "brewer.greens",
    col_alpha = 0.8,
    title     = "High productivity-growth expansion (ha)"
  )

pulp_map
htmlwidgets::saveWidget(
  tmap_leaflet(pulp_map),
  file          = paste0(wdir, "01_data/04_results/figures/pulp_expansion_map.html"),
  selfcontained = TRUE
)
