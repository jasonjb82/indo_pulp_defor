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


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# load data --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
wdir <- "remote/"
p1_df <- read_csv(paste0(wdir, "01_data/02_out/tables/pulp_exp_model_var_1km_2017.csv")) %>%
  rename_with(tolower) %>%
  rename(pulp_start = pulp_2017, palm_start = palm_2017, forest_start = forest_2017,
         hti_start = hti_2016, dist_mill = dist_mill_2017) %>%
  rename_with(~ str_replace(., "^y2017_a", "ya_"), starts_with("y2017_a"))

p2_df <- read_csv(paste0(wdir, "01_data/02_out/tables/pulp_exp_model_var_1km_2022.csv")) %>%
  rename_with(tolower) %>%
  rename(pulp_start = pulp_2022, palm_start = palm_2022, forest_start = forest_2022,
         hti_start = hti_2022, dist_mill = dist_mill_2022) %>%
  rename_with(~ str_replace(., "^y2022_a", "ya_"), starts_with("y2022_a"))
# admin_df <- read_csv(paste0(wdir, "01_data/02_out/tables/grid_10km_adm_prov_kab.csv"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# set up estimation dataframe --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
est_df <- p1_df %>%
  left_join(p2_df %>% select(pixel_id, pulp_end = pulp_start), by = "pixel_id")

est_df <- est_df %>%
  filter(pulp_start == 0)

# est_df <- est_df %>%
#   sample_n(10000)

glimpse(est_df)
count(est_df, pulp_end)  # check class balance


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# build predictive model of deforestation --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# --- 1. Select features (start-of-period baseline only; exclude post-period vars) ---
model_df <- est_df %>%
  select(
    pixel_id, pulp_end, kab_code,     # pixel ID + outcome + spatial grouping variable
    dist_mill, dist_water_m,          # mill access
    hti_start,                        # industrial concession at baseline
    forest_start,                     # forest cover at baseline
    peat, op_conc, wdpa,              # land type indicators
    elevation, slope,                 # physical geography
    tmmx, tmmn, pr, pet, def,         # climate
    clay_content, soil_ph, kh,        # soil properties
    starts_with("ya_")                # 64 spectral anomaly indices
  ) %>%
  mutate(
    pulp_end = factor(pulp_end, levels = c(1, 0), labels = c("pulp", "no_pulp")),
    kab_code = factor(kab_code)
  )

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
  update_role(pixel_id,  new_role = "ID") %>%      # carry through for evaluation joins
  step_impute_median(all_numeric_predictors())      # handle any NAs

# --- 4. Model specification ---
rf_spec <- rand_forest(
  trees = 500,
  mtry  = tune(),
  min_n = tune()
) %>%
  set_engine("ranger", importance = "permutation", class.weights = class_wts) %>%
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

# plan(multisession, workers = parallel::detectCores() - 1)
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

# --- 9. Predicted probabilities for all pixels ---
predictions_df <- augment(final_fit, new_data = model_df)


predictions_df %>%
  select(.pred_pulp) %>%
  summary()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# evaluate model performance --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

test_preds <- collect_predictions(last_fit_result)

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


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# apply model to 2022 baseline for 2022-2027 predictions --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pred2027_input <- p2_df %>%
  filter(pulp_start == 0) %>%
  select(
    pixel_id, kab_code, dist_mill, dist_water_m,
    hti_start, forest_start,
    peat, op_conc, wdpa,
    elevation, slope,
    tmmx, tmmn, pr, pet, def,
    clay_content, soil_ph, kh,
    starts_with("ya_")
  ) %>%
  mutate(kab_code = factor(kab_code))

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
