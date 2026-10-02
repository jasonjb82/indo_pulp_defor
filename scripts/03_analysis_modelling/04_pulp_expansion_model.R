#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# Author: Robert Heilmayr
# Project: Indonesia pulp deforestation
# Date: 2-25-26
# Purpose: Build spatial model of pulp expansion locations. The foundation for
#   SI Section 8, and produces the pixel-level expansion probabilities that the
#   scenario analysis in script 05 allocates.
#
# Input datasets (paths relative to remote/01_data/)
#        1) 02_out/tables/pulp_exp_model_var_1km_2017.csv: Predictors and
#               pulpwood plantation extent for each 1 km grid point, measured at
#               the start of the estimation period (2017). Supplies the
#               estimation sample.
#               Produced by scripts/02_data_preparation/11_pulp_expansion_model_variables_1km.R
#        2) 02_out/tables/pulp_exp_model_var_1km_2022.csv: The same predictors
#               measured in 2022. Supplies the 2022 outcome used for estimation
#               and the baseline from which future expansion is predicted.
#               Produced by scripts/02_data_preparation/11_pulp_expansion_model_variables_1km.R
#
# Outputs:
#        1) SI Figure 4: Receiver operating characteristic and precision-recall
#               curves for the final model, computed on the held-out spatial
#               test set. Written to 04_results/figures/SI_f4_auc.png
#        2) Fitted random forest workflow, retained so the model can be reloaded
#               without re-tuning. Written to 02_out/models/rf_final_fit.rds
#        3) Predicted probability of pulpwood plantation expansion for every
#               1 km point not yet converted as of 2022, with the covariates the
#               scenarios tabulate by (starting forest cover, peat, coordinates).
#               Written to 02_out/tables/pulp_predictions.csv
#               Read by scripts/03_analysis_modelling/05_pulp_expansion_scenarios.R
#        4) SI Section 8 text statements: The numeric claims made in that
#               section, reproduced in their sentence context with values
#               interpolated from this run. Printed to the console and written
#               to 04_results/si_section8_statements.txt
#
# Note: hyperparameter tuning runs in parallel and takes several minutes. Run the
#   script top to bottom; a single seed at the head governs every stochastic step.
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# load packages --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
library(tidyverse)
library(tidylog)
library(tidymodels)
library(ranger)
library(vip) # variable importance plots
library(future) # parallel backend for tune_grid


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# set random seed --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# One seed for the whole script. Every stochastic step below draws from this
# stream in sequence: the majority-class downsample, the kabupaten train/test
# split, the CV folds, hyperparameter tuning, the final fit, and the PDP
# subsample. Because they share one stream, the script must be run top to bottom
# for results to reproduce, and editing an earlier step shifts every later draw.
# L'Ecuyer-CMRG is required so that tune_grid's parallel workers receive
# reproducible, non-overlapping RNG streams.
set.seed(42, kind = "L'Ecuyer-CMRG")


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# load data --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
wdir <- "remote"
data_dir <- "/01_data/"

p1_df <- read_csv(paste0(
  wdir,
  data_dir,
  "/02_out/tables/pulp_exp_model_var_1km_2017.csv"
)) %>%
  rename_with(tolower) %>%
  rename(
    pulp_start = pulp_2017,
    palm_start = palm_2017,
    forest_start = forest_2017,
    hti_start = hti_risk_2017,
    dist_mill = dist_mill_2017
  ) %>%
  rename_with(~ str_replace(., "^y2017_a", "ya_"), starts_with("y2017_a")) %>%
  mutate(across(
    c(tmmx, tmmn, pr, pet, def, clay_content, soil_ph, gaez_cat),
    ~ na_if(., -9999)
  ))

p2_df <- read_csv(paste0(
  wdir,
  data_dir,
  "/02_out/tables/pulp_exp_model_var_1km_2022.csv"
)) %>%
  rename_with(tolower) %>%
  rename(
    pulp_start = pulp_2022,
    palm_start = palm_2022,
    forest_start = forest_2022,
    hti_start = hti_risk_2022,
    dist_mill = dist_mill_2022
  ) %>%
  rename_with(~ str_replace(., "^y2022_a", "ya_"), starts_with("y2022_a")) %>%
  mutate(across(
    c(tmmx, tmmn, pr, pet, def, clay_content, soil_ph, gaez_cat),
    ~ na_if(., -9999)
  ))

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
  "0" = "Not yet defined",
  "1" = "Nature sanctuary / conservation area",
  "1001" = "Protected forest",
  "1002" = "Nature sanctuary and recreation forest",
  "1003" = "Production forest",
  "1004" = "Limited production forest",
  "1005" = "Convertible production forest",
  "1007" = "Other land use area",
  "5001" = "Lake / river",
  "5003" = "Sea / water",
  "10021" = "Nature reserve",
  "10022" = "Wildlife sanctuary",
  "10023" = "Hunting park",
  "10024" = "National park",
  "10025" = "Nature recreation park",
  "10026" = "Community forest park",
  "100201" = "Terrestrial nature sanctuary",
  "100211" = "Marine nature reserve",
  "100221" = "Marine wildlife sanctuary",
  "100241" = "Marine national park",
  "100251" = "Marine nature recreation park"
)
est_df <- est_df %>%
  mutate(
    kh = factor(kh, levels = as.integer(names(kh_levels)), labels = kh_levels),
    gaez_cat = factor(
      gaez_cat,
      levels = as.integer(names(gaez_levels)),
      labels = gaez_levels
    )
  )


glimpse(est_df)
count(est_df, pulp_end) # check class balance
count(est_df, kh) # verify kh encoding


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# build predictive model of deforestation --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# --- 1. Select features (start-of-period baseline only; exclude post-period vars) ---
model_df <- est_df %>%
  select(
    pixel_id,
    pulp_end,
    kab_code, # pixel ID + outcome + spatial grouping variable
    dist_mill,
    dist_water_m, # mill access
    hti_start, # industrial concession at baseline
    forest_start,
    palm_start, # forest / op cover at baseline
    peat,
    op_conc,
    wdpa, # land type indicators
    elevation,
    slope,
    gaez_cat, # physical geography
    tmmx,
    tmmn,
    pr,
    pet,
    def, # climate
    clay_content,
    soil_ph,
    kh, # soil properties
    starts_with("ya_") # 64 spectral anomaly indices
  ) %>%
  mutate(
    pulp_end = factor(
      pulp_end,
      levels = c(1, 0),
      labels = c("pulp", "no_pulp")
    ),
    kab_code = factor(kab_code)
  )

# Drop NA pixels; verify loss is < 2% of sample
n_before <- nrow(model_df)
model_df <- drop_na(model_df)
pct_dropped <- (n_before - nrow(model_df)) / n_before
message(sprintf(
  "Dropped %d pixels with NAs (%.1f%% of sample)",
  n_before - nrow(model_df),
  pct_dropped * 100
))
stopifnot(
  "More than 2% of pixels dropped — check NA sources" = pct_dropped < 0.02
)

# Two-pronged class-imbalance strategy:
#   1. Downsample the majority class to 10:1 for memory and compute efficiency.
#      This retains enough no_pulp variation to characterise the decision boundary
#      while keeping the estimation sample to roughly 37,000 rows.
#   2. Apply class weights derived from the *original* prevalence (~0.35% pulp),
#      which makes tree construction cost-sensitive: the splitting rule favours
#      splits that separate the rare class.
#      Note that ranger applies class.weights in the splitting rule only. Leaf
#      probabilities remain in-bag class fractions, so predicted probabilities are
#      NOT calibrated to the landscape base rate and should not be read as absolute
#      risks. This is immaterial downstream because the scenarios use the
#      predictions solely to rank locations.
prevalence <- mean(model_df$pulp_end == "pulp")
class_wts <- c(pulp = 1 - prevalence, no_pulp = prevalence)

n_pulp <- sum(model_df$pulp_end == "pulp")
# Keep the full eligible population before downsampling. Reassigning model_df below
# leaves this binding pointing at the original frame, so this costs no extra memory.
# Used by the SI statements block to report the size of the eligible population.
model_pop_df <- model_df
model_df <- bind_rows(
  model_df %>% filter(pulp_end == "pulp"),
  model_df %>% filter(pulp_end == "no_pulp") %>% sample_n(min(n_pulp * 10, n()))
)

# --- 2. Spatial train/test split + CV on training data only ---
data_split <- group_initial_split(model_df, group = kab_code, prop = 0.8)
train_df <- training(data_split)
test_df <- testing(data_split)
# Spatial CV: hold out entire kabupaten (districts) per fold to prevent
# geographic data leakage between training and validation pixels.
cv_folds <- group_vfold_cv(train_df, group = kab_code, v = 5)

# --- 3. Recipe ---
rf_recipe <- recipe(pulp_end ~ ., data = model_df) %>%
  update_role(kab_code, new_role = "ID") %>% # keep for grouping, exclude from model
  update_role(pixel_id, new_role = "ID") # carry through for evaluation joins

# --- 4. Model specification ---
# Permutation importance is deliberately not requested here. It is computed from the
# fitted forest and affects neither splits nor predictions, so asking for it during
# tuning would repeat the calculation across every resample fit for no gain. It is
# switched on for the final fit only (step 8), which is the model vip() interrogates.
# min_n is fixed, not tuned: cross-validated ROC-AUC varies by less than 0.004
# across min_n from 5 to 400 (0.9499-0.9536 at mtry = 13), well inside one CV
# standard error (~0.008) and far below the fold-to-fold spread (0.924-0.974).
# Tuning it only let select_best() chase whichever grid edge was offered. 100 sits
# mid-plateau. mtry, by contrast, is genuinely identified and is still tuned.
rf_spec <- rand_forest(
  trees = 500,
  mtry = tune(),
  min_n = 100
) %>%
  set_engine(
    "ranger",
    class.weights = !!class_wts
  ) %>%
  set_mode("classification")

# --- 5. Workflow ---
rf_workflow <- workflow() %>%
  add_recipe(rf_recipe) %>%
  add_model(rf_spec)

# --- 6. Hyperparameter tuning over spatial CV folds ---
# Grid: mtry {5, 13, 22, 31, 40}. The feature set has 83 predictors, so sqrt(83)
# ~ 9 is the usual default, but the wider range captures the potential benefit of
# larger subsets for spatial data. The range brackets the optimum on both sides:
# ROC-AUC peaks at mtry = 13 and falls away consistently toward 40, so the
# selection below should be interior to the grid -- widen the range if it is not.
rf_grid <- grid_regular(
  mtry(range = c(5, 40)),
  levels = 5
)

n_workers <- max(1, parallel::detectCores() - 1)
plan(multisession, workers = n_workers)
rf_tune <- tune_grid(
  rf_workflow,
  resamples = cv_folds,
  grid = rf_grid,
  metrics = metric_set(roc_auc, pr_auc, sensitivity, specificity),
  control = control_grid(save_pred = TRUE, verbose = TRUE)
)
plan(sequential) # shut the workers down; they otherwise persist for the session

# --- 7. Review CV results ---
collect_metrics(rf_tune) %>%
  filter(.metric == "roc_auc") %>%
  arrange(desc(mean)) %>%
  print(n = nrow(rf_grid))

autoplot(rf_tune)

# --- 8. Select best hyperparameters, evaluate on held-out test, fit final model on all data ---
best_params <- select_best(rf_tune, metric = "roc_auc")
final_workflow <- finalize_workflow(rf_workflow, best_params)

# Fit on train + evaluate on held-out test set (unbiased performance estimate)
last_fit_result <- last_fit(
  final_workflow,
  data_split,
  metrics = metric_set(roc_auc, pr_auc)
)

# Fit on all data for spatial prediction maps, now with permutation importance
# enabled for the variable-importance plots below.
final_fit <- final_workflow %>%
  update_model(
    extract_spec_parsnip(final_workflow) %>%
      set_engine(
        "ranger",
        class.weights = !!class_wts,
        importance = "permutation"
      )
  ) %>%
  fit(data = model_df)

# Save / reload final model (skip re-tuning in future runs)
saveRDS(final_fit, paste0(wdir, data_dir, "/02_out/models/rf_final_fit.rds"))
# final_fit <- readRDS(paste0(wdir, "01_data/02_out/models/rf_final_fit.rds"))

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# evaluate model performance --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

# collect_predictions() returns .row as the row index in the full data frame given
# to group_initial_split(), not a position within the test set, so pixel_id must be
# recovered from model_df.
test_preds <- collect_predictions(last_fit_result) %>%
  mutate(pixel_id = model_df$pixel_id[.row])

# --- 1. Discrimination metrics ---
collect_metrics(last_fit_result) # roc_auc, pr_auc summary

p1 <- roc_curve(test_preds, truth = pulp_end, .pred_pulp) %>%
  autoplot() +
  ggtitle("Receiver operating characteristic")
p2 <- pr_curve(test_preds, truth = pulp_end, .pred_pulp) %>%
  autoplot() +
  ggtitle("Precision-recall") +
  geom_hline(
    yintercept = mean(test_df$pulp_end == "pulp"),
    linetype = "dotted",
    colour = "black"
  )
combined_plot <- p1 | p2
combined_plot
ggsave(
  paste0(wdir, data_dir, "/04_results/figures/SI_f4_auc.png"),
  combined_plot,
  width = 7,
  height = 4
)

# --- 2. Variable importance (top 20 features) ---
final_fit %>%
  extract_fit_parsnip() %>%
  vip(num_features = 20)

#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# apply model to 2022 baseline for 2022-2027 predictions --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

pred2027_input <- p2_df %>%
  filter(pulp_start == 0) %>%
  select(
    pixel_id,
    kab_code,
    dist_mill,
    dist_water_m,
    hti_start,
    forest_start,
    palm_start,
    peat,
    op_conc,
    wdpa,
    elevation,
    slope,
    tmmx,
    tmmn,
    pr,
    pet,
    def,
    clay_content,
    soil_ph,
    kh,
    gaez_cat,
    starts_with("ya_")
  ) %>%
  mutate(
    kab_code = factor(kab_code),
    kh = factor(kh, levels = as.integer(names(kh_levels)), labels = kh_levels),
    gaez_cat = factor(
      gaez_cat,
      levels = as.integer(names(gaez_levels)),
      labels = gaez_levels
    )
  )

n_before2027 <- nrow(pred2027_input)
pred2027_input <- drop_na(pred2027_input)
pct_dropped2027 <- (n_before2027 - nrow(pred2027_input)) / n_before2027
message(sprintf(
  "Dropped %d pixels with NAs (%.1f%% of sample)",
  n_before2027 - nrow(pred2027_input),
  pct_dropped2027 * 100
))
stopifnot(
  "More than 2% of pixels dropped — check NA sources" = pct_dropped2027 < 0.02
)

predictions2027_df <- augment(final_fit, new_data = pred2027_input)

predictions2027_df %>%
  select(.pred_pulp) %>%
  summary()


#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# save predictions for downstream scenario analysis --------------
#%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
# lat/lon pre-joined so 05_pulp_expansion_scenarios.R doesn't need to re-read p2_df
pulp_predictions <- predictions2027_df %>%
  left_join(p2_df %>% select(pixel_id, lat, lon), by = "pixel_id") %>%
  select(pixel_id, kab_code, forest_start, peat, lat, lon, .pred_pulp)

stopifnot(
  "Rows gained in left_join — check for duplicate pixel_ids" = nrow(
    pulp_predictions
  ) ==
    nrow(predictions2027_df),
  "Missing coordinates in prediction output" = !anyNA(
    pulp_predictions$lat
  ) &&
    !anyNA(pulp_predictions$lon)
)

write_csv(
  pulp_predictions,
  paste0(wdir, data_dir, "/02_out/tables/pulp_predictions.csv")
)


##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Reproduce the numeric claims made in the SI -----------------------------
##%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
## Prints each statement from SI Section 8 that depends on this script, with
## its numbers interpolated live, and writes the same text to 04_results.
## Reproducing the sentences in context makes it straightforward to check the
## manuscript against the analysis, and any change in the underlying data
## surfaces directly in the wording below.

si_para <- function(...) c(strwrap(sprintf(...), width = 78), "")

# Held-out test performance (SI Section 8.3)
test_metrics <- collect_metrics(last_fit_result)
metric_val <- function(m) test_metrics$.estimate[test_metrics$.metric == m]

# Cross-validated performance at the selected hyperparameters
cv_best <- collect_metrics(rf_tune) %>%
  inner_join(best_params %>% select(mtry), by = "mtry") %>%
  filter(.metric %in% c("roc_auc", "pr_auc"))
cv_val <- function(m, col) cv_best[[col]][cv_best$.metric == m]

si_text <- c(
  "SI SECTION 8: SPATIAL MODEL OF PULP EXPANSION",
  strrep("=", 78),
  paste(
    "Generated by scripts/03_analysis_modelling/04_pulp_expansion_model.R on",
    Sys.Date()
  ),
  "",
  "8.1 Sample construction and partitioning",
  strrep("-", 78),
  si_para(
    paste(
      "Dropping points already converted to pulpwood plantations by 2017, and",
      "points with missing covariates (%.1f%% of the sample), leaves %s 1 km",
      "points. Of these, %s (%.3f%% of the sample) were converted to pulpwood",
      "plantations between 2017 and 2022."
    ),
    100 * pct_dropped,
    format(nrow(model_pop_df), big.mark = ","),
    format(n_pulp, big.mark = ","),
    100 * prevalence
  ),
  si_para(
    paste(
      "Downsampling the majority class to 10 times the number of minority-class",
      "observations yields an estimation sample of %s points. This was",
      "partitioned into a training set of %s points (%.0f%%) across %d",
      "regencies and a held-out test set of %s points (%.0f%%) across %d",
      "regencies, blocking at the regency (kabupaten) level so that all points",
      "in a regency fall in the same set. Hyperparameters were tuned using",
      "%d-fold spatial cross-validation within the training set."
    ),
    format(nrow(model_df), big.mark = ","),
    format(nrow(train_df), big.mark = ","),
    100 * nrow(train_df) / nrow(model_df),
    n_distinct(train_df$kab_code),
    format(nrow(test_df), big.mark = ","),
    100 * nrow(test_df) / nrow(model_df),
    n_distinct(test_df$kab_code),
    nrow(cv_folds)
  ),
  "8.3 Model estimation and validation",
  strrep("-", 78),
  si_para(
    paste(
      "Inverse-prevalence class weights were derived from the prevalence of",
      "expansion in the full population prior to downsampling (%.5f), giving",
      "weights of %.4f for the expansion class and %.5f for the non-expansion",
      "class, a ratio of %.0f to 1. ranger applies these in the splitting rule",
      "only, so predicted probabilities are not calibrated to the landscape",
      "base rate and are used solely to rank locations."
    ),
    prevalence,
    class_wts[["pulp"]],
    class_wts[["no_pulp"]],
    class_wts[["pulp"]] / class_wts[["no_pulp"]]
  ),
  si_para(
    paste(
      "The number of trees was fixed at 500 and the minimum number of",
      "observations required to split a terminal node at %d. The number of",
      "candidate features considered at each split was tuned over %d values",
      "spanning %d to %d; the value maximising mean cross-validated ROC-AUC",
      "was %d (cross-validated ROC-AUC %.3f, standard error %.3f). The model",
      "uses %d predictors in total."
    ),
    extract_fit_parsnip(final_fit)$fit$min.node.size,
    nrow(rf_grid),
    min(rf_grid$mtry),
    max(rf_grid$mtry),
    best_params$mtry,
    cv_val("roc_auc", "mean"),
    cv_val("roc_auc", "std_err"),
    extract_fit_parsnip(final_fit)$fit$num.independent.variables
  ),
  si_para(
    paste(
      "The final model achieved a ROC-AUC of %.3f and a PR-AUC of %.3f on the",
      "held-out spatial test set. Both are computed on the downsampled test",
      "set, which retains the 10:1 class ratio of the estimation sample",
      "(prevalence %.1f%%, against %.3f%% across the full landscape). ROC-AUC",
      "is invariant to class prevalence; precision is not, and is",
      "correspondingly lower at the landscape base rate. SI Figure 4 is",
      "written to 04_results/figures/SI_f4_auc.png."
    ),
    metric_val("roc_auc"),
    metric_val("pr_auc"),
    100 * mean(test_df$pulp_end == "pulp"),
    100 * prevalence
  ),
  "8.4 Predicted pulpwood plantation expansion",
  strrep("-", 78),
  si_para(
    paste(
      "The final model was refit on the complete downsampled dataset (%s",
      "points, training and test partitions combined) and used to predict",
      "expansion probabilities for all %s points not yet converted to",
      "pulpwood plantations as of 2022 (%.1f%% of candidate points dropped for",
      "missing covariates). Predictions are written to",
      "02_out/tables/pulp_predictions.csv and allocated across space by",
      "scripts/03_analysis_modelling/05_pulp_expansion_scenarios.R."
    ),
    format(nrow(model_df), big.mark = ","),
    format(nrow(predictions2027_df), big.mark = ","),
    100 * pct_dropped2027
  )
)

cat(si_text, sep = "\n")

si_text_path <- paste0(
  wdir,
  data_dir,
  "/04_results/si_section8_statements.txt"
)
writeLines(si_text, si_text_path)
cat("\nSI statements written to", si_text_path, "\n")
