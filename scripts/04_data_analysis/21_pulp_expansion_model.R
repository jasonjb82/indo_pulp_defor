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
library(vip)      # variable importance plots
library(sf)
library(tmap)


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# load data --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
wdir <- "remote/"

p1_df <- read_csv(paste0(wdir, "01_data/02_out/tables/pulp_exp_model_var_2017.csv"))
p2_df <- read_csv(paste0(wdir, "01_data/02_out/tables/pulp_exp_model_var_2022.csv"))
admin_df <- read_csv(paste0(wdir, "01_data/02_out/tables/grid_10km_adm_prov_kab.csv"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# set up estimation dataframe --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
est_df <- p1_df %>%
  left_join(p2_df %>% select(pixel_id, pulp_2022), by = "pixel_id") %>%
  left_join(admin_df %>% select(pixel_id, kab_code), by = "pixel_id")

est_df <- est_df %>%
  filter(pulp_2017 == 0)

glimpse(est_df)
count(est_df, pulp_2022)  # check class balance


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# build predictive model of deforestation --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# --- 1. Select features (2017-era baseline only; exclude post-period vars) ---
model_df <- est_df %>%
  select(
    pulp_2022, kab_code,               # outcome + spatial grouping variable
    mill_dist_km_2017,                # mill access
    hti_2016,                         # industrial concession at baseline
    forest_2017,                      # forest cover at baseline
    peat, op, wdpa,                   # land type indicators
    elevation, slope, dist_water_m,   # physical geography
    tmmx, tmmn, pr, pet, def,         # climate
    clay_content, soil_ph, kh,        # soil properties
    starts_with("y2017_a")            # 64 spectral anomaly indices
  ) %>%
  mutate(
    pulp_2022 = factor(pulp_2022, levels = c(1, 0), labels = c("pulp", "no_pulp")),
    kab_code = factor(kab_code)
  )

# --- 2. Spatial cross-validation (province-level grouped folds) ---
set.seed(42)
cv_folds <- group_vfold_cv(model_df, group = kab_code, v = 5)

# --- 3. Recipe ---
rf_recipe <- recipe(pulp_2022 ~ ., data = model_df) %>%
  update_role(kab_code, new_role = "ID") %>%      # keep for grouping, exclude from model
  step_impute_median(all_numeric_predictors()) %>% # handle any NAs
  step_downsample(pulp_2022, under_ratio = 1)      # balance classes in each training fold

# --- 4. Model specification ---
rf_spec <- rand_forest(
  trees = 500,
  mtry  = tune(),
  min_n = tune()
) %>%
  set_engine("ranger", importance = "impurity") %>%
  set_mode("classification")

# --- 5. Workflow ---
rf_workflow <- workflow() %>%
  add_recipe(rf_recipe) %>%
  add_model(rf_spec)

# --- 6. Hyperparameter tuning over spatial CV folds ---
rf_grid <- grid_regular(
  mtry(range = c(5, 20)),
  min_n(range = c(5, 30)),
  levels = 4  # 4x4 = 16 combinations
)

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

# --- 8. Select best hyperparameters and fit final model ---
best_params   <- select_best(rf_tune, metric = "roc_auc")
final_workflow <- finalize_workflow(rf_workflow, best_params)
final_fit      <- fit(final_workflow, data = model_df)

# --- 9. Evaluation outputs ---

# ROC curve from CV held-out predictions
cv_preds <- collect_predictions(rf_tune, parameters = best_params)
roc_curve(cv_preds, truth = pulp_2022, .pred_pulp) %>% autoplot()
roc_auc(cv_preds, truth = pulp_2022, .pred_pulp)

# Precision-recall curve
pr_curve(cv_preds, truth = pulp_2022, .pred_pulp) %>% autoplot()

# Confusion matrix at 0.5 threshold
cv_preds %>%
  mutate(.pred_class = if_else(.pred_pulp >= 0.5, "pulp", "no_pulp"),
         .pred_class = factor(.pred_class, levels = c("pulp", "no_pulp"))) %>%
  conf_mat(truth = pulp_2022, estimate = .pred_class) %>%
  tidy() %>%
  mutate(
    actual    = if_else(str_detect(name, "^cell_1_"), "Actual: pulp", "Actual: no_pulp"),
    predicted = if_else(str_detect(name, "_1$"),      "Predicted: pulp", "Predicted: no_pulp")
  ) %>%
  select(actual, predicted, value) %>%
  pivot_wider(names_from = predicted, values_from = value)

# Variable importance (top 20 features)
final_fit %>%
  extract_fit_parsnip() %>%
  vip(num_features = 20)

# --- 10. Predicted probabilities for all pixels ---
predictions_df <- augment(final_fit, new_data = model_df) %>%
  bind_cols(est_df %>% select(pixel_id))


predictions_df %>%
  select(.pred_pulp) %>%
  summary()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# map predicted pulp expansion probabilities --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# --- 1. Load pixel coordinates (panel file; deduplicate to one row per pixel) ---
coords_df <- read_csv(
  paste0(wdir, "01_data/02_out/gee/explore_deforestation_Indonesia.csv"),
  col_select = c(pixel_id, lon_upper_left, lat_upper_left)
) %>%
  distinct(pixel_id, .keep_all = TRUE) %>%
  semi_join(predictions_df, by = "pixel_id")

# --- 2. Build sf points layer ---
pred_sf <- predictions_df %>%
  left_join(coords_df, by = "pixel_id") %>%
  drop_na(lon_upper_left, lat_upper_left) %>%
  st_as_sf(coords = c("lon_upper_left", "lat_upper_left"), crs = 4326)

nrow(pred_sf) == nrow(predictions_df)  # sanity check: no pixels lost

# --- 3. Build province boundary layer (dissolve kabupaten shapefile) ---
kab_sf  <- read_sf(paste0(wdir, "01_data/01_in/big/idn_kabupaten_big.shp"))
prov_sf <- kab_sf %>%
  group_by(prov, prov_code) %>%
  summarise(.groups = "drop")

# --- 4. Interactive tmap ---
tmap_mode("view")

tm_shape(prov_sf) +
  tm_borders(col = "grey40", lwd = 1) +
tm_shape(pred_sf) +
  tm_symbols(
    col        = ".pred_pulp",
    palette    = "YlOrRd",
    size       = 0.4,
    border.lwd = NA,
    title.col  = "P(pulp expansion)",
    popup.vars = c("pixel_id", ".pred_pulp")
  ) +
tm_layout(title = "Predicted probability of pulp expansion (2017\u20132022)")
