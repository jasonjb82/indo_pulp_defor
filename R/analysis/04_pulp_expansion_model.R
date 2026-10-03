## ---------------------------------------------------------
## Project: Indonesia pulp deforestation
## Purpose of script: Build the spatial model of pulp expansion locations. The
##   foundation for SI Section 8, and produces the pixel-level expansion
##   probabilities that the scenario analysis (script 05) allocates.
## Author: Robert Heilmayr
## Notes: Refactored for the targets pipeline from
##   scripts/03_analysis_modelling/04_pulp_expansion_model.R. The modelling
##   steps run in the same order with the same single seed, so every random
##   draw (downsampling, the kabupaten train/test split, CV folds, tuning, the
##   final fit) happens in the same sequence. The seed is set inside
##   run_pulp_expansion_model() and the previous RNG kind is restored on exit,
##   so other targets are unaffected. tidymodels functions are namespaced
##   rather than attached, to avoid masking dplyr across the pipeline.
##   Display-only steps (autoplot of the tuning results, the vip plot) are
##   replaced by tables in the diagnostics.
##
## Saved model vs re-estimation: hyperparameter tuning is a near-tie
##   (cross-validated ROC-AUC 0.956-0.957 for mtry 13 and 22) and its parallel
##   random streams depend on package versions (tune 2.x changed them), so a
##   re-estimation on a different setup can select a different mtry and give
##   different predictions. The published predictions therefore come from the
##   fitted model saved by the authors (02_out/models/rf_final_fit.rds, mtry =
##   22), which reproduces them exactly; the full estimation remains in the
##   pipeline as separate targets.
##
## Pipeline inputs (targets in _targets.R; paths relative to
##   data/01_data_replication/)
##        1) rf_vars_2017_file -> 02_out/tables/pulp_exp_model_var_1km_2017.csv:
##               Predictors and pulpwood plantation extent for each 1 km grid
##               point at the start of the estimation period (2017).
##               Produced by scripts/02_data_preparation/11_pulp_expansion_model_variables_1km.R
##        2) rf_vars_2022_file -> 02_out/tables/pulp_exp_model_var_1km_2022.csv:
##               The same predictors in 2022: the 2022 outcome for estimation
##               and the baseline from which future expansion is predicted.
##               Produced by scripts/02_data_preparation/11_pulp_expansion_model_variables_1km.R
##        3) rf_final_fit_file -> 02_out/models/rf_final_fit.rds: The fitted
##               model saved by scripts/03_analysis_modelling/04_pulp_expansion_model.R
##               (run of 2 Oct 2026).
##
## Pipeline outputs
##        1) pulp_predictions (predict_pulp_expansion() on the saved model):
##               Predicted probability of pulpwood plantation expansion for
##               every 1 km point not yet converted as of 2022, with starting
##               forest cover, peat and coordinates.
##               Read by the scenario analysis (script 05)
##        2) pulp_predictions_csv -> outputs/tables/pulp_predictions.csv
##        From re-estimation (run_pulp_expansion_model(); version-sensitive):
##        3) si_fig4_png -> outputs/figures/SI_f4_auc.png: SI Figure 4, ROC and
##               precision-recall curves on the held-out spatial test set.
##        4) si_section8_txt -> outputs/text/si_section8_statements.txt: SI
##               Section 8 statements with values from the re-estimation.
##        5) rf_diagnostics_txt -> outputs/text/rf_model_diagnostics.txt
## ---------------------------------------------------------

#' Read a 1 km predictor table and standardise its column names
#' @param csv_path Path to pulp_exp_model_var_1km_<year>.csv
#' @param year 2017 or 2022
#' @return The table with year-specific names replaced by *_start / ya_*
read_rf_vars <- function(csv_path, year) {
  sfx <- paste0("_", year)
  read_csv(csv_path, show_col_types = FALSE) %>%
    rename_with(tolower) %>%
    rename(
      pulp_start = !!paste0("pulp", sfx),
      palm_start = !!paste0("palm", sfx),
      forest_start = !!paste0("forest", sfx),
      hti_start = !!paste0("hti_risk", sfx),
      dist_mill = !!paste0("dist_mill", sfx)
    ) %>%
    rename_with(
      ~ str_replace(., paste0("^y", year, "_a"), "ya_"),
      starts_with(paste0("y", year, "_a"))
    ) %>%
    mutate(across(
      c(tmmx, tmmn, pr, pet, def, clay_content, soil_ph, gaez_cat),
      ~ na_if(., -9999)
    ))
}

#' Labels for the gaez and kh (kawasan hutan) codes
#'
#' The codes are arbitrary KLHK IDs, not a continuous or ordinal scale, so
#' they enter the model as labelled factors.
rf_factor_levels <- function() {
  list(
    gaez = c(
      "1" = "No limitations",
      "2" = "Hydromorphic",
      "3" = "Terrain",
      "4" = "Other"
    ),
    kh = c(
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
  )
}

#' Build the 2022 baseline that the model scores for 2022-2027
#' @param p2_df Output of read_rf_vars(<2022 csv>, 2022)
#' @return A list: input (complete-case predictors for points not yet
#'   converted in 2022), n_before and pct_dropped
prep_rf_prediction_input <- function(p2_df) {
  lv <- rf_factor_levels()
  input <- p2_df %>%
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
      kh = factor(kh, levels = as.integer(names(lv$kh)), labels = lv$kh),
      gaez_cat = factor(
        gaez_cat,
        levels = as.integer(names(lv$gaez)),
        labels = lv$gaez
      )
    )

  n_before <- nrow(input)
  input <- drop_na(input)
  pct_dropped <- (n_before - nrow(input)) / n_before
  stopifnot(
    "More than 2% of pixels dropped - check NA sources" = pct_dropped < 0.02
  )
  list(input = input, n_before = n_before, pct_dropped = pct_dropped)
}

#' Score the 2022 baseline with a fitted model and attach coordinates
#' @param final_fit A fitted tidymodels workflow
#' @param pred_input Output of prep_rf_prediction_input()$input
#' @param p2_df Output of read_rf_vars(<2022 csv>, 2022), for lat/lon
#' @return pixel_id, kab_code, forest_start, peat, lat, lon, .pred_pulp
score_rf_baseline <- function(final_fit, pred_input, p2_df) {
  # A workflow read back from disk does not load the packages that provide
  # its methods (augment, the recipe, the ranger engine), so load them here
  for (pkg in c("workflows", "parsnip", "recipes", "ranger")) {
    loadNamespace(pkg)
  }
  predictions2027_df <- generics::augment(final_fit, new_data = pred_input)

  # lat/lon pre-joined so the scenario analysis doesn't need to re-read p2_df
  pulp_predictions <- predictions2027_df %>%
    left_join(p2_df %>% select(pixel_id, lat, lon), by = "pixel_id") %>%
    select(pixel_id, kab_code, forest_start, peat, lat, lon, .pred_pulp)

  stopifnot(
    "Rows gained in left_join - check for duplicate pixel_ids" = nrow(
      pulp_predictions
    ) ==
      nrow(predictions2027_df),
    "Missing coordinates in prediction output" = !anyNA(
      pulp_predictions$lat
    ) &&
      !anyNA(pulp_predictions$lon)
  )
  pulp_predictions
}

#' Predict 2022-2027 pulp expansion from the authors' saved model
#' @param rf_fit_rds Path to 02_out/models/rf_final_fit.rds
#' @param p2022_csv Path to the 2022 1 km predictor table
#' @return Output of score_rf_baseline()
predict_pulp_expansion <- function(rf_fit_rds, p2022_csv) {
  final_fit <- readRDS(rf_fit_rds)
  p2_df <- read_rf_vars(p2022_csv, 2022)
  score_rf_baseline(final_fit, prep_rf_prediction_input(p2_df)$input, p2_df)
}

#' Fit the random forest model of pulp expansion and predict 2022-2027
#'
#' Hyperparameter tuning runs in parallel and takes several minutes.
#' @param p2017_csv,p2022_csv Paths to the 2017 and 2022 1 km predictor tables.
#' @param n_workers Parallel workers for tuning. The default matches the
#'   standalone script; tune assigns each resample its own RNG stream, so the
#'   number of workers does not change the results.
#' @param results_paths Output paths named in the SI statements (text only).
#' @return A named list: pulp_predictions, final_fit, auc_plot, si_text and
#'   diagnostics.
run_pulp_expansion_model <- function(
  p2017_csv,
  p2022_csv,
  n_workers = max(1, parallel::detectCores() - 1),
  results_paths = c(
    si_fig4 = "outputs/figures/SI_f4_auc.png",
    predictions = "outputs/tables/pulp_predictions.csv"
  )
) {
  # =========================================================================
  # Random seed
  # =========================================================================
  # One seed for the whole analysis. Every stochastic step below draws from
  # this stream in sequence, so the steps must stay in this order for results
  # to reproduce. L'Ecuyer-CMRG is required so that tune_grid's parallel
  # workers receive reproducible, non-overlapping RNG streams.
  old_rng <- RNGkind()
  on.exit(
    RNGkind(kind = old_rng[1], normal.kind = old_rng[2], sample.kind = old_rng[3]),
    add = TRUE
  )
  set.seed(42, kind = "L'Ecuyer-CMRG")

  # =========================================================================
  # Load data
  # =========================================================================

  p1_df <- read_rf_vars(p2017_csv, 2017)
  p2_df <- read_rf_vars(p2022_csv, 2022)

  # =========================================================================
  # Set up estimation dataframe
  # =========================================================================

  est_df <- p1_df %>%
    left_join(
      p2_df %>% select(pixel_id, pulp_end = pulp_start),
      by = "pixel_id"
    )

  est_df <- est_df %>%
    filter(pulp_start == 0)

  # Encode gaez and kh (kawasan hutan) as labelled factors
  lv <- rf_factor_levels()
  gaez_levels <- lv$gaez
  kh_levels <- lv$kh
  est_df <- est_df %>%
    mutate(
      kh = factor(kh, levels = as.integer(names(kh_levels)), labels = kh_levels),
      gaez_cat = factor(
        gaez_cat,
        levels = as.integer(names(gaez_levels)),
        labels = gaez_levels
      )
    )

  # =========================================================================
  # Build predictive model of deforestation
  # =========================================================================

  # --- 1. Select features (start-of-period baseline only) ---
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
  stopifnot(
    "More than 2% of pixels dropped - check NA sources" = pct_dropped < 0.02
  )

  # Two-pronged class-imbalance strategy:
  #   1. Downsample the majority class to 10:1 for memory and compute
  #      efficiency.
  #   2. Apply class weights derived from the *original* prevalence, which
  #      makes tree construction cost-sensitive. ranger applies class.weights
  #      in the splitting rule only, so predicted probabilities are not
  #      calibrated to the landscape base rate; the scenarios use them solely
  #      to rank locations.
  prevalence <- mean(model_df$pulp_end == "pulp")
  class_wts <- c(pulp = 1 - prevalence, no_pulp = prevalence)

  n_pulp <- sum(model_df$pulp_end == "pulp")
  # Full eligible population before downsampling, for the SI statements
  model_pop_df <- model_df
  model_df <- bind_rows(
    model_df %>% filter(pulp_end == "pulp"),
    model_df %>%
      filter(pulp_end == "no_pulp") %>%
      sample_n(min(n_pulp * 10, n()))
  )

  # --- 2. Spatial train/test split + CV on training data only ---
  data_split <- rsample::group_initial_split(
    model_df,
    group = kab_code,
    prop = 0.8
  )
  train_df <- rsample::training(data_split)
  test_df <- rsample::testing(data_split)
  # Spatial CV: hold out entire kabupaten (districts) per fold to prevent
  # geographic data leakage between training and validation pixels.
  cv_folds <- rsample::group_vfold_cv(train_df, group = kab_code, v = 5)

  # --- 3. Recipe ---
  rf_recipe <- recipes::recipe(pulp_end ~ ., data = model_df) %>%
    recipes::update_role(kab_code, new_role = "ID") %>%
    recipes::update_role(pixel_id, new_role = "ID")

  # --- 4. Model specification ---
  # Permutation importance is computed from the fitted forest and affects
  # neither splits nor predictions, so it is requested for the final fit only.
  # min_n is fixed at 100 (mid-plateau of cross-validated ROC-AUC); mtry is
  # tuned.
  rf_spec <- parsnip::rand_forest(
    trees = 500,
    mtry = tune::tune(),
    min_n = 100
  ) %>%
    parsnip::set_engine(
      "ranger",
      class.weights = !!class_wts
    ) %>%
    parsnip::set_mode("classification")

  # --- 5. Workflow ---
  rf_workflow <- workflows::workflow() %>%
    workflows::add_recipe(rf_recipe) %>%
    workflows::add_model(rf_spec)

  # --- 6. Hyperparameter tuning over spatial CV folds ---
  # Grid: mtry {5, 13, 22, 31, 40}; the optimum should be interior to the grid.
  rf_grid <- dials::grid_regular(
    dials::mtry(range = c(5, 40)),
    levels = 5
  )

  old_plan <- future::plan(future::multisession, workers = n_workers)
  on.exit(future::plan(old_plan), add = TRUE)
  rf_tune <- tune::tune_grid(
    rf_workflow,
    resamples = cv_folds,
    grid = rf_grid,
    metrics = yardstick::metric_set(
      yardstick::roc_auc,
      yardstick::pr_auc,
      yardstick::sensitivity,
      yardstick::specificity
    ),
    control = tune::control_grid(save_pred = TRUE)
  )
  future::plan(future::sequential) # shut the workers down

  # --- 7./8. Select best hyperparameters, evaluate on held-out test, fit ---
  best_params <- tune::select_best(rf_tune, metric = "roc_auc")
  final_workflow <- tune::finalize_workflow(rf_workflow, best_params)

  # Fit on train + evaluate on held-out test set (unbiased performance)
  last_fit_result <- tune::last_fit(
    final_workflow,
    data_split,
    metrics = yardstick::metric_set(yardstick::roc_auc, yardstick::pr_auc)
  )

  # Fit on all data for spatial prediction, with permutation importance
  final_fit <- final_workflow %>%
    workflows::update_model(
      workflows::extract_spec_parsnip(final_workflow) %>%
        parsnip::set_engine(
          "ranger",
          class.weights = !!class_wts,
          importance = "permutation"
        )
    ) %>%
    generics::fit(data = model_df)

  # =========================================================================
  # Evaluate model performance (SI Figure 4)
  # =========================================================================

  # collect_predictions() returns .row as the row index in the full data frame
  # given to group_initial_split(), so pixel_id is recovered from model_df.
  test_preds <- tune::collect_predictions(last_fit_result) %>%
    mutate(pixel_id = model_df$pixel_id[.row])

  auc_plot <- plot_rf_auc(
    test_preds %>% select(pulp_end, .pred_pulp),
    test_prevalence = mean(test_df$pulp_end == "pulp")
  )

  # =========================================================================
  # Apply model to 2022 baseline for 2022-2027 predictions
  # =========================================================================

  pred_prep <- prep_rf_prediction_input(p2_df)
  pred2027_input <- pred_prep$input
  n_before2027 <- pred_prep$n_before
  pct_dropped2027 <- pred_prep$pct_dropped

  pulp_predictions <- score_rf_baseline(final_fit, pred2027_input, p2_df)

  # =========================================================================
  # SI Section 8 statements
  # =========================================================================

  si_para <- function(...) c(strwrap(sprintf(...), width = 78), "")

  # Held-out test performance (SI Section 8.3)
  test_metrics <- tune::collect_metrics(last_fit_result)
  metric_val <- function(m) test_metrics$.estimate[test_metrics$.metric == m]

  # Cross-validated performance at the selected hyperparameters
  cv_best <- tune::collect_metrics(rf_tune) %>%
    inner_join(best_params %>% select(mtry), by = "mtry") %>%
    filter(.metric %in% c("roc_auc", "pr_auc"))
  cv_val <- function(m, col) cv_best[[col]][cv_best$.metric == m]

  ranger_fit <- workflows::extract_fit_parsnip(final_fit)$fit
  ratio <- round(
    sum(model_df$pulp_end == "no_pulp") / sum(model_df$pulp_end == "pulp")
  )

  si_text <- c(
    "SI SECTION 8: SPATIAL MODEL OF PULP EXPANSION",
    strrep("=", 78),
    "Generated by R/analysis/04_pulp_expansion_model.R (targets pipeline)",
    "",
    "8.1 Sample construction and partitioning",
    strrep("-", 78),
    si_para(
      paste(
        "To address this, we randomly downsampled the number of observations from",
        "the majority class (non-converting points) to be equal to %d times the",
        "number of observations in the minority class (pulpwood plantation",
        "expansion points)."
      ),
      ratio
    ),
    si_para(
      paste(
        "We then partitioned the downsampled dataset into a training set (%.0f%%)",
        "and a held-out test set (%.0f%%) using spatial blocking at the regency",
        "(kabupaten) level, such that all grid cells within a given regency were",
        "assigned entirely to one set. We used %d-fold spatial cross-validation on",
        "the training set for hyperparameter tuning and model selection."
      ),
      100 * nrow(train_df) / nrow(model_df),
      100 * nrow(test_df) / nrow(model_df),
      nrow(cv_folds)
    ),
    "8.3 Model estimation and validation",
    strrep("-", 78),
    si_para(
      paste(
        "When defining our model structure, we fixed the number of trees at %d and",
        "the minimum number of observations required to split a terminal node at",
        "%d, and tuned the number of candidate features considered at each split."
      ),
      ranger_fit$num.trees,
      ranger_fit$min.node.size
    ),
    si_para(
      paste(
        "The final model achieved a ROC-AUC of %.3f and a PR-AUC of %.3f on the",
        "held-out spatial test set, indicating a strong ability to discriminate",
        "pixels that underwent pulpwood plantation expansion from those that did",
        "not (Figure 4)."
      ),
      metric_val("roc_auc"),
      metric_val("pr_auc")
    ),
    si_para(
      paste(
        "Figure 4 caption: Both curves are computed on the held-out spatial test",
        "set, which retains the %d:1 majority-to-minority class ratio of the",
        "downsampled estimation sample."
      ),
      ratio
    ),
    "SUPPORTING VALUES (not reported in the manuscript)",
    strrep("-", 78),
    si_para(
      paste(
        "Sample: %s eligible 1 km points after dropping those already converted by",
        "2017 and those with missing covariates (%.1f%% of the sample), of which",
        "%s (%.3f%%) converted between 2017 and 2022. Downsampling leaves an",
        "estimation sample of %s points: %s training points across %d regencies",
        "and %s test points across %d regencies."
      ),
      format(nrow(model_pop_df), big.mark = ","),
      100 * pct_dropped,
      format(n_pulp, big.mark = ","),
      100 * prevalence,
      format(nrow(model_df), big.mark = ","),
      format(nrow(train_df), big.mark = ","),
      n_distinct(train_df$kab_code),
      format(nrow(test_df), big.mark = ","),
      n_distinct(test_df$kab_code)
    ),
    si_para(
      paste(
        "Class weights derived from the pre-downsampling prevalence (%.5f):",
        "%.4f for the expansion class and %.5f for the non-expansion class, a",
        "ratio of %.0f to 1. ranger applies these in the splitting rule only, so",
        "predicted probabilities are not calibrated to the landscape base rate and",
        "are used solely to rank locations."
      ),
      prevalence,
      class_wts[["pulp"]],
      class_wts[["no_pulp"]],
      class_wts[["pulp"]] / class_wts[["no_pulp"]]
    ),
    si_para(
      paste(
        "Tuning: %d candidate values of mtry spanning %d to %d; the value",
        "maximising mean cross-validated ROC-AUC was %d (cross-validated ROC-AUC",
        "%.3f, standard error %.3f). The model uses %d predictors in total."
      ),
      nrow(rf_grid),
      min(rf_grid$mtry),
      max(rf_grid$mtry),
      best_params$mtry,
      cv_val("roc_auc", "mean"),
      cv_val("roc_auc", "std_err"),
      ranger_fit$num.independent.variables
    ),
    si_para(
      paste(
        "Test-set prevalence is %.1f%%, against %.3f%% across the full landscape.",
        "ROC-AUC is invariant to class prevalence; precision is not, and is",
        "correspondingly lower at the landscape base rate."
      ),
      100 * mean(test_df$pulp_end == "pulp"),
      100 * prevalence
    ),
    si_para(
      paste(
        "Prediction: the final model was refit on the complete downsampled dataset",
        "and scored all %s points not yet converted as of 2022 (%.1f%% of",
        "candidate points dropped for missing covariates). SI Figure 4 is written",
        "to %s and the predictions to %s."
      ),
      format(nrow(pulp_predictions), big.mark = ","),
      100 * pct_dropped2027,
      results_paths[["si_fig4"]],
      results_paths[["predictions"]]
    )
  )

  # =========================================================================
  # Console diagnostics (captured, not printed)
  # =========================================================================

  importance <- sort(ranger_fit$variable.importance, decreasing = TRUE)

  diagnostics <- utils::capture.output({
    cat(sprintf(
      "Estimation sample: dropped %d pixels with NAs (%.1f%%); prediction input: dropped %d (%.1f%%)\n",
      n_before - nrow(model_pop_df),
      100 * pct_dropped,
      n_before2027 - nrow(pred2027_input),
      100 * pct_dropped2027
    ))
    cat("\nClass balance before downsampling:\n")
    print(count(model_pop_df, pulp_end))
    cat("\nCross-validated ROC-AUC by mtry:\n")
    print(
      tune::collect_metrics(rf_tune) %>%
        filter(.metric == "roc_auc") %>%
        arrange(desc(mean)),
      n = Inf
    )
    cat("\nHeld-out test metrics:\n")
    print(test_metrics)
    cat("\nPredicted probability of expansion, 2022-2027:\n")
    print(summary(pulp_predictions$.pred_pulp))
    cat("\nPermutation importance, top 20:\n")
    print(utils::head(round(importance, 6), 20))
  })

  list(
    pulp_predictions = pulp_predictions,
    final_fit = final_fit,
    auc_plot = auc_plot,
    si_text = si_text,
    diagnostics = diagnostics
  )
}

#' Build SI Figure 4 (ROC and precision-recall curves)
#'
#' Built in its own function because a ggplot keeps the environment it was
#' created in; inside run_pulp_expansion_model() that would carry the full
#' input tables into the stored result.
#' @param test_preds Held-out predictions with pulp_end and .pred_pulp
#' @param test_prevalence Share of expansion points in the test set
plot_rf_auc <- function(test_preds, test_prevalence) {
  p1 <- yardstick::roc_curve(test_preds, truth = pulp_end, .pred_pulp) %>%
    ggplot2::autoplot() +
    ggtitle("Receiver operating characteristic")
  p2 <- yardstick::pr_curve(test_preds, truth = pulp_end, .pred_pulp) %>%
    ggplot2::autoplot() +
    ggtitle("Precision-recall") +
    geom_hline(
      yintercept = test_prevalence,
      linetype = "dotted",
      colour = "black"
    )
  patchwork::wrap_plots(p1, p2, nrow = 1)
}

#' Save SI Figure 4 (ROC and precision-recall curves)
#' @param rf_results Output of run_pulp_expansion_model()
#' @param output_path Destination .png path
#' @return output_path, as required by format = "file"
save_si_fig4 <- function(rf_results, output_path) {
  dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
  ggsave_without_showtext(output_path, rf_results$auc_plot, width = 7, height = 4)
  output_path
}
