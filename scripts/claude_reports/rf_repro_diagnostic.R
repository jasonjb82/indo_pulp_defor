# -----------------------------------------------------------------------------
# Where does the random forest stop being reproducible across machines?
#
# Two runs of the pipeline on identical input data, identical package versions
# (renv.lock) and identical R (4.5.2) produced different results:
#
#                      macOS aarch64        Windows x86_64
#   mtry                      22                    22
#   ROC-AUC                0.964                 0.966
#   PR-AUC                 0.613                 0.611
#   new deforestation   132,600 ha            139,600 ha
#
# The engine seed is pinned (seed = 42), so the forest is determined by the
# dataset handed to it. This script hashes the dataset at every stage of its
# construction, then fits one forest, so the first stage that disagrees between
# two machines tells us what to fix:
#
#   * a STAGE 1-6 mismatch means the dataset itself differs, i.e. R's RNG
#     stream reached sample_n()/the split in a different state. That is fixable
#     by re-seeding immediately before each sampling call.
#   * stages 1-6 matching but STAGE 7 differing means the dataset is identical
#     and the divergence is in ranger's compiled code -- floating-point
#     behaviour differing between CPU architectures and compilers. No amount of
#     seeding fixes that; it needs a pinned platform (Docker).
#
# Run from the repository root on each machine and compare the output:
#   Rscript scripts/claude_reports/rf_repro_diagnostic.R
#
# Point it at a data directory other than the default with:
#   INDO_PULP_DATA=remote/01_data Rscript scripts/claude_reports/rf_repro_diagnostic.R
# -----------------------------------------------------------------------------

suppressMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(digest)
  library(ranger)
})

# The loading helpers come from the pipeline itself rather than being
# re-implemented here, so this cannot drift from what the pipeline does.
source("R/analysis/04_pulp_expansion_model.R")

data_dir <- Sys.getenv("INDO_PULP_DATA", unset = "data/01_data_replication")
p2017_csv <- file.path(data_dir, "02_out/tables/pulp_exp_model_var_1km_2017.csv")
p2022_csv <- file.path(data_dir, "02_out/tables/pulp_exp_model_var_1km_2022.csv")
stopifnot(file.exists(p2017_csv), file.exists(p2022_csv))

h <- function(x) substr(digest(x, algo = "md5"), 1, 12)
say <- function(stage, label, value) {
  cat(sprintf("  [%s] %-34s %s\n", stage, label, value))
}

cat("\n=== environment ===\n")
say("", "R", R.version.string)
say("", "platform", R.version$platform)
say("", "cores detected", parallel::detectCores())
for (p in c("dplyr", "rsample", "ranger", "tune", "parsnip", "recipes")) {
  say("", p, as.character(packageVersion(p)))
}

cat("\n=== stage 0: inputs ===\n")
say("0", "data dir", data_dir)
say("0", "md5 pulp_exp..._2017.csv", unname(tools::md5sum(p2017_csv)))
say("0", "md5 pulp_exp..._2022.csv", unname(tools::md5sum(p2022_csv)))

# ---- reproduce the pipeline's dataset construction -------------------------
# Mirrors run_pulp_expansion_model() in R/analysis/04_pulp_expansion_model.R;
# the RNG kind and seed are set exactly as it does.
old_rng <- RNGkind()
on.exit(
  RNGkind(kind = old_rng[1], normal.kind = old_rng[2], sample.kind = old_rng[3]),
  add = TRUE
)
set.seed(42, kind = "L'Ecuyer-CMRG")

p1_df <- read_rf_vars(p2017_csv, 2017)
p2_df <- read_rf_vars(p2022_csv, 2022)

est_df <- p1_df %>%
  left_join(p2_df %>% select(pixel_id, pulp_end = pulp_start), by = "pixel_id") %>%
  filter(pulp_start == 0)

lv <- rf_factor_levels()
est_df <- est_df %>%
  mutate(
    kh = factor(kh, levels = as.integer(names(lv$kh)), labels = lv$kh),
    gaez_cat = factor(
      gaez_cat,
      levels = as.integer(names(lv$gaez)),
      labels = lv$gaez
    )
  )

model_df <- est_df %>%
  select(
    pixel_id, pulp_end, kab_code, dist_mill, dist_water_m, hti_start,
    forest_start, palm_start, peat, op_conc, wdpa, elevation, slope,
    gaez_cat, tmmx, tmmn, pr, pet, def, clay_content, soil_ph, kh,
    starts_with("ya_")
  ) %>%
  mutate(
    pulp_end = factor(pulp_end, levels = c(1, 0), labels = c("pulp", "no_pulp")),
    kab_code = factor(kab_code)
  )

cat("\n=== stage 1: assembled frame (pre-NA-drop) ===\n")
say("1", "rows", format(nrow(model_df), big.mark = ","))
say("1", "cols", ncol(model_df))
say("1", "hash of column names", h(names(model_df)))

model_df <- drop_na(model_df)
cat("\n=== stage 2: after drop_na ===\n")
say("2", "rows", format(nrow(model_df), big.mark = ","))
say("2", "hash of sorted pixel_id", h(sort(model_df$pixel_id)))
# Row ORDER matters: sample_n() draws by position, so two frames holding the
# same pixels in a different order give different downsamples.
say("2", "hash of pixel_id IN ORDER", h(model_df$pixel_id))

prevalence <- mean(model_df$pulp_end == "pulp")
n_pulp <- sum(model_df$pulp_end == "pulp")
cat("\n=== stage 3: class balance ===\n")
say("3", "prevalence", sprintf("%.10f", prevalence))
say("3", "n_pulp", n_pulp)

# Stage 4 is the first point where R's RNG stream is consumed.
rng_before <- h(.Random.seed)
model_df <- bind_rows(
  model_df %>% filter(pulp_end == "pulp"),
  model_df %>% filter(pulp_end == "no_pulp") %>% sample_n(min(n_pulp * 10, n()))
)
cat("\n=== stage 4: downsample (first RNG use) ===\n")
say("4", "RNG state BEFORE sample_n", rng_before)
say("4", "RNG state AFTER sample_n", h(.Random.seed))
say("4", "rows", format(nrow(model_df), big.mark = ","))
say("4", "hash of sorted pixel_id", h(sort(model_df$pixel_id)))

data_split <- rsample::group_initial_split(model_df, group = kab_code, prop = 0.8)
train_df <- rsample::training(data_split)
test_df <- rsample::testing(data_split)
cat("\n=== stage 5: spatial train/test split ===\n")
say("5", "train rows", format(nrow(train_df), big.mark = ","))
say("5", "test rows", format(nrow(test_df), big.mark = ","))
say("5", "hash of sorted train pixel_id", h(sort(train_df$pixel_id)))
say("5", "hash of sorted test pixel_id", h(sort(test_df$pixel_id)))

cv_folds <- rsample::group_vfold_cv(train_df, group = kab_code, v = 5)
fold_sig <- vapply(
  cv_folds$splits,
  function(s) h(sort(rsample::assessment(s)$pixel_id)),
  character(1)
)
cat("\n=== stage 6: 5-fold spatial CV ===\n")
for (i in seq_along(fold_sig)) say("6", paste0("fold ", i, " assessment hash"), fold_sig[i])

# ---- stage 7: one forest, fixed hyperparameters ----------------------------
# If stages 1-6 agree and this disagrees, the dataset is identical and the
# difference lives in ranger's compiled code. num.threads = 1 removes thread
# count as a variable; ranger seeds each tree as (i + 1) * seed, so this should
# match the multi-threaded result on the same machine.
class_wts <- c(pulp = 1 - prevalence, no_pulp = prevalence)
feat <- setdiff(names(train_df), c("pixel_id", "kab_code"))
fit <- ranger(
  dependent.variable.name = "pulp_end",
  data = train_df[, feat],
  num.trees = 500,
  mtry = 22,
  min.node.size = 100,
  class.weights = class_wts,
  probability = TRUE,
  seed = 42,
  num.threads = 1
)
pred <- predict(fit, data = test_df[, feat], num.threads = 1)$predictions[, "pulp"]

cat("\n=== stage 7: single ranger fit (mtry 22, seed 42, 1 thread) ===\n")
say("7", "OOB prediction error", sprintf("%.12f", fit$prediction.error))
say("7", "hash of test predictions", h(round(pred, 12)))
say("7", "mean predicted probability", sprintf("%.12f", mean(pred)))
say("7", "sum predicted probability", sprintf("%.10f", sum(pred)))

cat("\nCompare these blocks between machines. The FIRST stage that differs\n")
cat("is where reproducibility breaks.\n\n")
