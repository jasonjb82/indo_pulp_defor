# =============================================================================
# Land Cover Change Map – Accuracy Assessment & Area Estimation
# =============================================================================
#
# Project: Indonesia pulp deforestation
#
# Purpose of script: Implement stratified estimator for remote sensing accuracy
# and area estimates following Olofsson et al. (2013, 2014).
#
# Author: Robert Heilmayr and Adria Descals
#
# Date Created: May 2026
# 
#
# References:
#   Olofsson P., Foody G.M., Stehman S.V., Woodcock C.E. (2013) Making better
#     use of accuracy data in land change studies. RSE 129:122-131.
#     [Equations cited as O2013 Eq. N]
#   Olofsson P. et al. (2014) Good practices for estimating area and assessing
#     accuracy of land change. RSE 148:42-57.
#   Stehman S.V. (2014) Estimating area and map accuracy for stratified random
#     sampling when the strata are different from the map classes. IJRS 35:4923.
#   Cochran W.G. (1977) Sampling Techniques, 3rd ed. Wiley.
#
# INPUT:  "Points" sheet of the validation spreadsheet.
#         Required columns:
#           Map   – map class label (integer 0–10) for each sample point
#           Truth – reference class label (integer 0–10) for each sample point
#
# OUTPUT: Sections 1–10 printed to the console.
#
# =============================================================================

library(readxl)

# =============================================================================
# 0.  USER-SUPPLIED PARAMETERS
# =============================================================================

wdir <- "remote"
data_dir <- "/01_data/"

CLASS_NAMES <- c(
  "0"  = "Stable other",
  "1"  = "Stable pulpwood",
  "2"  = "Other to pulpwood 2001-2011",
  "3"  = "Other to pulpwood 2012-2017",
  "4"  = "Other to pulpwood 2018-2024",
  "5"  = "Forest to pulpwood 2001-2011",
  "6"  = "Forest to pulpwood 2012-2017",
  "7"  = "Forest to pulpwood 2018-2024",
  "8"  = "Peat to pulpwood 2001-2011",
  "9"  = "Peat to pulpwood 2012-2017",
  "10" = "Peat to pulpwood 2018-2024"
)

# Combined class sets for aggregate area reporting.
# Each entry is a named character vector of class codes to aggregate.
COMBINED_CLASSES <- list(
  "PP expansion 2001-2011    [2+5+8]"         = c("2", "5", "8"),
  "PP expansion 2012-2017    [3+6+9]"         = c("3", "6", "9"),
  "PP expansion 2018-2024    [4+7+10]"        = c("4", "7", "10"),
  "Deforestation 2001-2011   [5+8]"           = c("5", "8"),
  "Deforestation 2012-2017   [6+9]"           = c("6", "9"),
  "Deforestation 2018-2024   [7+10]"          = c("7", "10"),
  "All deforestation         [5+6+7+8+9+10]"  = c("5","6","7","8","9","10")
)

Z95         <- 1.96            # z-multiplier for 95% confidence intervals
ALL_CLASSES <- as.character(0:10)

# =============================================================================
# 1.  LOAD DATA - VALIDATION SAMPLE AND MAPPED AREAS 
# =============================================================================

# Path to the validation spreadsheet
XLSX_PATH <- paste0(wdir, data_dir,"/01_in/gaveau/Validation_11classes_land-cover-change-map_v1-2.xlsx")

# Load validation sample
pts <- read_excel(XLSX_PATH, sheet = "Points")
stopifnot("Map"   %in% names(pts),
          "Truth" %in% names(pts))

# Load mapped (pixel-counting) areas per stratum class (ha).
# Read directly from the Inputs tab of the validation spreadsheet (A6:C17).
inputs_raw <- read_excel(XLSX_PATH, sheet = "Inputs", range = "A6:C17")
MAPPED_AREA_HA <- setNames(
  as.numeric(inputs_raw[[3]]),
  as.character(as.integer(inputs_raw[[1]]))
)

# =============================================================================
# 2.  COUNT-BASED CONFUSION MATRIX
#
#   n_mat[i, j] = number of sample units with Map == i AND Truth == j
#   Rows = map class / sampling stratum h
#   Columns = reference class (ground truth)
# =============================================================================

n_mat <- table(
  Map   = factor(pts$Map,   levels = 0:10, labels = ALL_CLASSES),
  Truth = factor(pts$Truth, levels = 0:10, labels = ALL_CLASSES)
)

# =============================================================================
# 3.  STRATUM WEIGHTS AND SAMPLE SIZES
# =============================================================================

A_total <- sum(MAPPED_AREA_HA)          # Total study-region area (ha)
W       <- MAPPED_AREA_HA / A_total     # Stratum weight W_h = A_h / A_total
n_i     <- rowSums(n_mat)               # Sample size per stratum
names(n_i) <- ALL_CLASSES

# =============================================================================
# 4.  USER'S ACCURACY
#   U_i = n_{ii} / n_i   [O2013 Eq. 4]
#   Proportion of map-class-i area correctly labelled in the reference data.
# =============================================================================

U <- diag(n_mat) / n_i

# =============================================================================
# 5.  AREA-WEIGHTED CONFUSION MATRIX
#   p_hat_ij = W_i * (n_{ij} / n_i)   [O2013 Eq. 1]
#
#   Converts sample counts to estimated area proportions, correcting for the
#   unequal sampling intensities across strata.  p_hat_ij is the estimated
#   fraction of the total study region that was mapped as class i but belongs
#   to reference class j.
# =============================================================================

p_mat <- sweep(n_mat / n_i, 1, W, "*")
# Row sums of p_mat equal stratum weights W_i.

# =============================================================================
# 6.  AREA ESTIMATES AND ACCURACY MEASURES
# =============================================================================

# Estimated reference-class area proportions (column sums)
# p_hat_j = sum_i p_hat_ij   [O2013 Eq. 3]
p_j <- colSums(p_mat)

# Area-adjusted overall accuracy
# OA = sum_j p_hat_jj   [O2013 Eq. 6]
OA <- sum(diag(p_mat))

# Area-adjusted producer's accuracy
# P_hat_j = p_hat_jj / p_hat_j   [O2013 Eq. 8]
P_j <- diag(p_mat) / p_j

# Area estimates (ha)
A_j <- p_j * A_total

# =============================================================================
# 7.  VARIANCE ESTIMATION
# =============================================================================

# --- Core helper -----------------------------------------------------------
# Variance of a stratified area proportion estimate.
# For any class-k column of the weighted confusion matrix:
#
#   V_hat(p_hat_k) = sum_i  W_i^2 * (n_ik/n_i) * (1 - n_ik/n_i) / (n_i - 1)
#
# [Cochran 1977, Eq. 5.7;  O2013 Eq. 10]
var_strat <- function(n_col, n_row, W_vec) {
  q <- as.numeric(n_col) / as.numeric(n_row)
  sum(W_vec^2 * q * (1 - q) / (as.numeric(n_row) - 1))
}

# --- V_hat(OA) ---
# V_hat(OA) = sum_i  W_i^2 * U_i * (1 - U_i) / (n_i - 1)   [O2013 Eq. 25]
V_OA <- sum(W^2 * U * (1 - U) / (n_i - 1))

# --- V_hat(U_i): user's accuracy variance per stratum ---
# V_hat(U_i) = U_i * (1 - U_i) / (n_i - 1)   [O2013 Eq. 6]
V_U <- U * (1 - U) / (n_i - 1)

# --- V_hat(p_hat_j): reference-class area proportion variance ---
V_p_j <- sapply(ALL_CLASSES, function(j) var_strat(n_mat[, j], n_i, W))

# --- V_hat(p_hat_jj): variance of a weighted diagonal element ---
# p_hat_jj = W_j * (n_jj/n_j) is a single-stratum estimate:
# V_hat(p_hat_jj) = W_j^2 * (n_jj/n_j) * (1 - n_jj/n_j) / (n_j - 1)
V_p_jj <- sapply(ALL_CLASSES, function(j)
  var_strat(n_mat[j, j], n_i[j], W[j]))

# --- V_hat(P_hat_j): producer's accuracy variance ---
# Delta method for the ratio P_hat_j = p_hat_jj / p_hat_j.
#
# Because p_hat_jj is itself a term of p_hat_j, and strata are independent,
# the only non-zero covariance term is from stratum j:
#   Cov(p_hat_jj, p_hat_j) = Var(p_hat_jj)
#
# Substituting into the standard ratio-variance formula:
#   V_hat(P_hat_j) = (1/p_hat_j^2) *
#                    [(1 - 2*P_hat_j) * V_hat(p_hat_jj) + P_hat_j^2 * V_hat(p_hat_j)]
# [O2013 Eq. 7]
V_P_j <- (1 / p_j^2) * ((1 - 2 * P_j) * V_p_jj + P_j^2 * V_p_j)
V_P_j <- pmax(V_P_j, 0)   # floor at 0: floating-point guard when P_hat_j = 1

# --- V_hat(A_hat_j): area estimate variance ---
V_A_j <- A_total^2 * V_p_j

# =============================================================================
# 8.  COMBINED-CLASS AREA ESTIMATES
#
# Treat the combined set S as a single new class via a binary indicator.
# Apply the standard stratified variance formula directly:
#
#   q_i        = (sum_{k in S} n_ik) / n_i   [combined proportion per stratum]
#   V_hat(p_S) = sum_i  W_i^2 * q_i * (1 - q_i) / (n_i - 1)
#
# This correctly accounts for the negative within-stratum covariances between
# mutually exclusive reference classes.
# [Cochran 1977, Eq. 5.7;  Olofsson 2013, Eq. 10]
# =============================================================================

calc_combined <- function(class_set) {
  p_combined <- sum(p_j[class_set])
  area_ha    <- p_combined * A_total
  n_comb <- rowSums(n_mat[, class_set, drop = FALSE])
  q_i    <- n_comb / n_i
  V      <- sum(W^2 * q_i * (1 - q_i) / (n_i - 1))
  list(
    area_ha  = area_ha,
    area_Mha = area_ha / 1e6,
    se       = sqrt(V) * A_total,
    ci       = Z95 * sqrt(V) * A_total
  )
}

combined <- lapply(COMBINED_CLASSES, calc_combined)

# =============================================================================
# 9.  COVARIANCE MATRIX OF CLASS AREA ESTIMATORS  (diagnostic)
#
#   Cov_hat(p_hat_k, p_hat_l) =
#       sum_i  W_i^2 * (-n_ik * n_il) / (n_i^2 * (n_i - 1))
#   Diagonal = V_hat(p_hat_k)
#   Off-diagonal entries are <= 0 (mutual exclusivity of reference classes).
# =============================================================================

cov_p <- matrix(0, 11, 11, dimnames = list(ALL_CLASSES, ALL_CLASSES))
for (k in ALL_CLASSES) {
  for (l in ALL_CLASSES) {
    if (k == l) {
      cov_p[k, l] <- V_p_j[k]
    } else {
      cov_p[k, l] <- sum(
        W^2 * (-as.numeric(n_mat[, k]) * as.numeric(n_mat[, l])) /
          (n_i^2 * (n_i - 1))
      )
    }
  }
}

# =============================================================================
# 10.  STATIC MAP ACCURACY ANALYSIS
# =============================================================================

# At each snapshot year, the 11 change classes map to pulp (PP) or non-pulp (Other)
# based on whether planting occurred by that year.
pulp_by_year <- list(
  "2000" = c("1"),                                   # stable pulpwood only
  "2011" = c("1", "2", "5", "8"),                    # + 2001-2011 conversions
  "2017" = c("1", "2", "3", "5", "6", "8", "9"),    # + 2012-2017 conversions
  "2022" = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")  # all conversions
)

# Binary accuracy estimator using the original 11-class stratification.
# Applies the standard stratified estimator (Olofsson 2013) to the binary PP/Other
# classification derived from the change-class labels.
static_map_stats <- function(year_label, pulp_classes) {
  other_classes <- setdiff(ALL_CLASSES, pulp_classes)

  # Per-stratum reference-class counts for the binary classification
  n_truth_pp    <- setNames(
    sapply(ALL_CLASSES, function(i) sum(n_mat[i, pulp_classes])), ALL_CLASSES)
  n_truth_other <- n_i - n_truth_pp
  W_pp    <- sum(W[pulp_classes])
  W_other <- sum(W[other_classes])

  # Estimated PP area
  p_truth_pp    <- sum(W * n_truth_pp / n_i)
  p_truth_other <- 1 - p_truth_pp
  V_p_truth_pp  <- sum(W^2 * (n_truth_pp / n_i) * (1 - n_truth_pp / n_i) / (n_i - 1))

  # Overall accuracy: map and reference agree on binary class
  n_correct <- setNames(sapply(ALL_CLASSES, function(i) {
    if (i %in% pulp_classes) sum(n_mat[i, pulp_classes]) else sum(n_mat[i, other_classes])
  }), ALL_CLASSES)
  OA_s   <- sum(W * n_correct / n_i)
  V_OA_s <- sum(W^2 * (n_correct / n_i) * (1 - n_correct / n_i) / (n_i - 1))

  # User's accuracy (PP): fraction of mapped-PP area correctly identified as PP
  # p_map_pp_truth_pp = sum_{i in pulp} W_i * (n_truth_pp_i / n_i)
  p_mpp_tpp  <- sum(W[pulp_classes] * n_truth_pp[pulp_classes] / n_i[pulp_classes])
  V_p_mpp    <- sum(W[pulp_classes]^2 *
                      (n_truth_pp[pulp_classes] / n_i[pulp_classes]) *
                      (1 - n_truth_pp[pulp_classes] / n_i[pulp_classes]) /
                      (n_i[pulp_classes] - 1))
  U_pp   <- p_mpp_tpp / W_pp
  V_U_pp <- V_p_mpp / W_pp^2

  # User's accuracy (Other): fraction of mapped-Other area correctly identified as Other
  p_mot_tot  <- sum(W[other_classes] * n_truth_other[other_classes] / n_i[other_classes])
  V_p_mot    <- sum(W[other_classes]^2 *
                      (n_truth_other[other_classes] / n_i[other_classes]) *
                      (1 - n_truth_other[other_classes] / n_i[other_classes]) /
                      (n_i[other_classes] - 1))
  U_other   <- p_mot_tot / W_other
  V_U_other <- V_p_mot / W_other^2

  # Producer's accuracy (PP): P_PP = p_mpp_tpp / p_truth_pp
  # Cov(p_mpp_tpp, p_truth_pp) = V(p_mpp_tpp): pulp strata contribute to both terms.
  P_pp   <- p_mpp_tpp / p_truth_pp
  V_P_pp <- max(0, (1 / p_truth_pp^2) *
    ((1 - 2 * P_pp) * V_p_mpp + P_pp^2 * V_p_truth_pp))

  # Producer's accuracy (Other): P_Other = p_mot_tot / p_truth_other
  V_p_truth_other <- sum(W^2 * (n_truth_other / n_i) * (1 - n_truth_other / n_i) / (n_i - 1))
  P_other   <- p_mot_tot / p_truth_other
  V_P_other <- max(0, (1 / p_truth_other^2) *
    ((1 - 2 * P_other) * V_p_mot + P_other^2 * V_p_truth_other))

  data.frame(
    year                     = year_label,
    mapped_pp_ha             = sum(MAPPED_AREA_HA[pulp_classes]),
    estimated_pp_ha          = p_truth_pp * A_total,
    se_pp_ha                 = sqrt(V_p_truth_pp) * A_total,
    ci95_pp_ha               = Z95 * sqrt(V_p_truth_pp) * A_total,
    overall_accuracy         = OA_s,
    oa_se                    = sqrt(V_OA_s),
    oa_ci95                  = Z95 * sqrt(V_OA_s),
    users_acc_pp             = U_pp,
    users_acc_pp_se          = sqrt(V_U_pp),
    users_acc_pp_ci95        = Z95 * sqrt(V_U_pp),
    users_acc_other          = U_other,
    users_acc_other_se       = sqrt(V_U_other),
    users_acc_other_ci95     = Z95 * sqrt(V_U_other),
    producers_acc_pp         = P_pp,
    producers_acc_pp_se      = sqrt(V_P_pp),
    producers_acc_pp_ci95    = Z95 * sqrt(V_P_pp),
    producers_acc_other      = P_other,
    producers_acc_other_se   = sqrt(V_P_other),
    producers_acc_other_ci95 = Z95 * sqrt(V_P_other)
  )
}

# Compute per-year accuracy stats
static_results <- do.call(rbind, lapply(names(pulp_by_year), function(yr) {
  static_map_stats(yr, pulp_by_year[[yr]])
}))

# =============================================================================
# 11.  FORMATTED OUTPUT
# =============================================================================

rule  <- paste0(strrep("=", 74), "\n")
hrule <- paste0(strrep("-", 74), "\n")

H <- function(title, sub = NULL) {
  cat("\n", hrule, sep = "")
  cat(title, "\n")
  if (!is.null(sub)) cat(sub, "\n")
  cat(hrule)
}

fmt_ha  <- function(x) formatC(round(x), format = "d", big.mark = ",")
fmt_Mha <- function(x) sprintf("%.3f", x / 1e6)
pct     <- function(x, d = 1) sprintf(paste0("%.", d, "f%%"), 100 * x)
ci_str  <- function(x, se) sprintf("(%s, %s)",
                                   pct(max(0, x - Z95 * se)),
                                   pct(min(1, x + Z95 * se)))

cat(rule)
cat("  LAND COVER CHANGE MAP - ACCURACY ASSESSMENT\n")
cat("  Stratified estimator  (Olofsson et al. 2013, 2014)\n")
cat(rule)
cat(sprintf("  Total mapped area  : %s ha\n", fmt_ha(A_total)))
cat(sprintf("  Total sample size  : %d points\n", nrow(pts)))
cat(sprintf("  Map strata         : %d classes (0-10)\n", length(ALL_CLASSES)))
cat(sprintf("  95%% CI z-value     : %.2f\n", Z95))


# Section 1: Count-based confusion matrix
H("SECTION 1 - COUNT-BASED CONFUSION MATRIX  (n_ij)",
  "  Rows = map class / stratum i,  Columns = reference class j")
print(addmargins(n_mat))


# Section 2: Weights and sample sizes
H("SECTION 2 - STRATUM WEIGHTS & SAMPLE SIZES")
cat(sprintf("  %-3s  %-30s  %13s  %10s  %5s  %6s\n",
            "Cls", "Name", "Mapped area (ha)", "Weight W_h", "n_i", "U_i"))
for (j in ALL_CLASSES) {
  cat(sprintf("  %-3s  %-30s  %13s  %10.8f  %5d  %5.3f\n",
              j, CLASS_NAMES[j], fmt_ha(MAPPED_AREA_HA[j]),
              W[j], n_i[j], U[j]))
}


# Section 3: Weighted confusion matrix
H("SECTION 3 - AREA-WEIGHTED CONFUSION MATRIX  (p_hat_ij = W_i * n_ij / n_i)",
  "  Row sums = W_i;  column sums = p_hat_j  (estimated reference-class proportions)")
print(round(p_mat, 8))
cat(sprintf("\n  Sum of diagonal (= OA): %.10f\n", OA))


# Section 4: Overall accuracy
H("SECTION 4 - OVERALL ACCURACY")
cat(sprintf("  OA      = %s\n",          pct(OA, 4)))
cat(sprintf("  SE(OA)  = %.6f\n",        sqrt(V_OA)))
cat(sprintf("  95%% CI = %s +/- %s  %s\n",
            pct(OA, 4), pct(Z95 * sqrt(V_OA), 4),
            ci_str(OA, sqrt(V_OA))))


# Section 5: User's accuracy
H("SECTION 5 - USER'S ACCURACY  (U_i = n_ii / n_i)",
  "  Proportion of map-class area correctly identified in reference data")
cat(sprintf("  %-3s  %-30s  %7s  %8s  %22s\n",
            "Cls", "Name", "U_i", "+/-SE", "95% CI"))
for (j in ALL_CLASSES) {
  cat(sprintf("  %-3s  %-30s  %7s  +/-%5s  %22s\n",
              j, CLASS_NAMES[j],
              pct(U[j]), pct(Z95 * sqrt(V_U[j])),
              ci_str(U[j], sqrt(V_U[j]))))
}


# Section 6: Producer's accuracy
H("SECTION 6 - PRODUCER'S ACCURACY  (P_hat_j = p_hat_jj / p_hat_j, area-adjusted)",
  "  Fraction of ground-truth class j area correctly mapped")
cat(sprintf("  %-3s  %-30s  %7s  %8s  %22s\n",
            "Cls", "Name", "P_hat_j", "+/-SE", "95% CI"))
for (j in ALL_CLASSES) {
  cat(sprintf("  %-3s  %-30s  %7s  +/-%5s  %22s\n",
              j, CLASS_NAMES[j],
              pct(P_j[j]), pct(Z95 * sqrt(V_P_j[j])),
              ci_str(P_j[j], sqrt(V_P_j[j]))))
}


# Section 7: Area estimates
H("SECTION 7 - AREA ESTIMATES BY CLASS")
cat(sprintf("  %-3s  %-30s  %12s  %12s  %10s  %s\n",
            "Cls", "Name", "Mapped (Mha)", "Estim. (Mha)", "SE (ha)", "95% CI (Mha)"))
for (j in ALL_CLASSES) {
  lo <- (A_j[j] - Z95 * sqrt(V_A_j[j])) / 1e6
  hi <- (A_j[j] + Z95 * sqrt(V_A_j[j])) / 1e6
  cat(sprintf("  %-3s  %-30s  %12s  %12s  %10s  (%.3f, %.3f)\n",
              j, CLASS_NAMES[j],
              fmt_Mha(MAPPED_AREA_HA[j]),
              fmt_Mha(A_j[j]),
              fmt_ha(Z95 * sqrt(V_A_j[j])),
              lo, hi))
}


# Section 8: Combined-class estimates
cat("\n", rule, sep = "")
cat("  SECTION 8 - COMBINED-CLASS AREA ESTIMATES\n")
cat(rule)

cat(sprintf("  %-44s  %9s  %12s\n", "Combination", "Area (Mha)", "SE (ha)"))
cat(strrep("-", 70), "\n")
for (nm in names(combined)) {
  cr <- combined[[nm]]
  cat(sprintf("  %-44s  %9.4f  %12s\n", nm, cr$area_Mha, fmt_ha(cr$se)))
}

cat("\n  Full 95% confidence intervals:\n\n")
for (nm in names(combined)) {
  cr <- combined[[nm]]
  cat(sprintf("  %s\n", nm))
  cat(sprintf("    Area    : %s ha  (%.4f Mha)\n", fmt_ha(cr$area_ha), cr$area_Mha))
  cat(sprintf("    95%% CI : %.4f +/- %.4f Mha\n\n", cr$area_Mha, cr$ci / 1e6))
}


# Section 9: Summary table
H("SECTION 9 - SUMMARY TABLE  (replicates spreadsheet 'Summary' tab)")

periods <- list(
  list(label = "2001-2011", pp = c("2","5","8"), def = c("5","8"), peat = "8"),
  list(label = "2012-2017", pp = c("3","6","9"), def = c("6","9"), peat = "9"),
  list(label = "2018-2024", pp = c("4","7","10"),def = c("7","10"),peat = "10")
)

cat("\n  Pulpwood plantation (PP) expansion [all sources]:\n")
cat(sprintf("  %-10s  %14s  %s\n", "Period", "Mapped (Mha)",
            "Estimated (Mha) [95% CI]"))
for (p in periods) {
  cr <- calc_combined(p$pp)
  cat(sprintf("  %-10s  %14.4f  %.2f  (%.2f, %.2f)\n", p$label,
              sum(MAPPED_AREA_HA[p$pp]) / 1e6, cr$area_Mha,
              (cr$area_ha - cr$ci) / 1e6,
              (cr$area_ha + cr$ci) / 1e6))
}

cat("\n  Pulp-driven deforestation (forest+peat to pulpwood):\n")
cat(sprintf("  %-10s  %14s  %s\n", "Period", "Mapped (Mha)",
            "Estimated (Mha) [95% CI]"))
for (p in periods) {
  cr <- calc_combined(p$def)
  cat(sprintf("  %-10s  %14.4f  %.2f  (%.2f, %.2f)\n", p$label,
              sum(MAPPED_AREA_HA[p$def]) / 1e6, cr$area_Mha,
              (cr$area_ha - cr$ci) / 1e6,
              (cr$area_ha + cr$ci) / 1e6))
}

cat("\n  Peat deforestation (peat to pulpwood):\n")
cat(sprintf("  %-10s  %14s  %s\n", "Period", "Mapped (Mha)",
            "Estimated (Mha) [95% CI]"))
for (p in periods) {
  j <- p$peat
  cat(sprintf("  %-10s  %14.4f  %.2f  (%.2f, %.2f)\n", p$label,
              MAPPED_AREA_HA[j] / 1e6, A_j[j] / 1e6,
              (A_j[j] - Z95 * sqrt(V_A_j[j])) / 1e6,
              (A_j[j] + Z95 * sqrt(V_A_j[j])) / 1e6))
}


# Section 10: Covariance matrix
H("SECTION 10 - COVARIANCE MATRIX OF CLASS AREA ESTIMATORS  (x10^-12)",
  "  Off-diagonal entries <= 0 (mutual exclusivity of reference classes).")
print(round(cov_p * 1e12, 2))


# =============================================================================
# 12.  SAVE OUTPUTS
# =============================================================================

out_dir <- paste0(wdir, data_dir, "04_results/")
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

# --- SI Table 3: Land cover class descriptions and sample sizes ---
si_table3 <- data.frame(
  class          = as.integer(ALL_CLASSES),
  name           = unname(CLASS_NAMES[ALL_CLASSES]),
  mapped_area_ha = as.numeric(MAPPED_AREA_HA[ALL_CLASSES]),
  sample_size    = as.integer(n_i[ALL_CLASSES]),
  row.names      = NULL
)
write.csv(si_table3, paste0(out_dir, "si_table3_class_descriptions.csv"), row.names = FALSE)

# --- SI Table 4: Accuracy metrics for the 11-class land cover change map ---
si_table4 <- data.frame(
  class                 = as.integer(ALL_CLASSES),
  name                  = unname(CLASS_NAMES[ALL_CLASSES]),
  mapped_area_ha        = as.numeric(MAPPED_AREA_HA[ALL_CLASSES]),
  estimated_area_ha     = as.numeric(A_j[ALL_CLASSES]),
  se_area_ha            = as.numeric(sqrt(V_A_j[ALL_CLASSES])),
  ci95_area_ha          = as.numeric(Z95 * sqrt(V_A_j[ALL_CLASSES])),
  users_accuracy        = as.numeric(U[ALL_CLASSES]),
  users_accuracy_se     = as.numeric(sqrt(V_U[ALL_CLASSES])),
  producers_accuracy    = as.numeric(P_j[ALL_CLASSES]),
  producers_accuracy_se = as.numeric(sqrt(V_P_j[ALL_CLASSES])),
  row.names             = NULL
)
write.csv(si_table4, paste0(out_dir, "si_table4_change_map_accuracy.csv"), row.names = FALSE)

# --- SI Table 5: Binary PP/Other accuracy for static snapshot maps ---
fmt_pct_ci <- function(est, ci95) {
  lo <- pmax(0,   est * 100 - ci95 * 100)
  hi <- pmin(100, est * 100 + ci95 * 100)
  sprintf("%.2f (%.2f, %.2f)", est * 100, lo, hi)
}
fmt_mha_ci <- function(est_ha, ci95_ha) {
  sprintf("%.2f (%.2f, %.2f)",
          est_ha / 1e6,
          (est_ha - ci95_ha) / 1e6,
          (est_ha + ci95_ha) / 1e6)
}

static_rows <- lapply(seq_len(nrow(static_results)), function(i) {
  s <- static_results[i, ]
  list(
    year             = s$year,
    oa               = fmt_pct_ci(s$overall_accuracy,    s$oa_ci95),
    u_other          = fmt_pct_ci(s$users_acc_other,     s$users_acc_other_ci95),
    u_pp             = fmt_pct_ci(s$users_acc_pp,        s$users_acc_pp_ci95),
    p_other          = fmt_pct_ci(s$producers_acc_other, s$producers_acc_other_ci95),
    p_pp             = fmt_pct_ci(s$producers_acc_pp,    s$producers_acc_pp_ci95),
    mapped_pp_mha    = sprintf("%.2f", s$mapped_pp_ha / 1e6),
    estimated_pp_mha = fmt_mha_ci(s$estimated_pp_ha, s$ci95_pp_ha)
  )
})

# Wide format: metrics as rows, years as columns
metric_labels <- c(
  "Overall accuracy (%)",
  "User's accuracy (%) - Other",
  "User's accuracy (%) - PP",
  "Producer's accuracy (%) - Other",
  "Producer's accuracy (%) - PP",
  "Mapped PP area (Mha)",
  "Estimated PP area (Mha)"
)
field_names <- c("oa", "u_other", "u_pp", "p_other", "p_pp",
                 "mapped_pp_mha", "estimated_pp_mha")

si_table5 <- data.frame(
  Metric = metric_labels,
  setNames(
    lapply(static_rows, function(r) sapply(field_names, function(f) r[[f]])),
    sapply(static_rows, `[[`, "year")
  ),
  check.names = FALSE,
  row.names   = NULL
)
write.csv(si_table5, paste0(out_dir, "si_table5_static_map_accuracy.csv"), row.names = FALSE)

# --- Paper statistics: key area estimates with 95% CIs ---
# Maps combined-class estimates to specific paper citations.
ps_keys <- c("Deforestation 2001-2011   [5+8]",
             "PP expansion 2001-2011    [2+5+8]",
             "All deforestation         [5+6+7+8+9+10]")
paper_stats <- data.frame(
  stat_name            = c("defor_2001_2011", "pulp_expansion_2001_2011", "total_defor_all_periods"),
  paper_location       = c("Main text lines 8, 24", "Main text line 82", "SI line 31"),
  estimated_area_kha   = sapply(ps_keys, function(k) combined[[k]]$area_ha  / 1e3),
  se_kha               = sapply(ps_keys, function(k) combined[[k]]$se       / 1e3),
  ci95_halfwidth_kha   = sapply(ps_keys, function(k) combined[[k]]$ci       / 1e3),
  ci95_lower_kha       = sapply(ps_keys, function(k) (combined[[k]]$area_ha - combined[[k]]$ci) / 1e3),
  ci95_upper_kha       = sapply(ps_keys, function(k) (combined[[k]]$area_ha + combined[[k]]$ci) / 1e3),
  row.names = NULL
)
write.csv(paper_stats, paste0(out_dir, "rs_accuracy_paper_stats.csv"), row.names = FALSE)

cat(sprintf("\n  Outputs saved to %s:\n", out_dir))
cat("    si_table3_class_descriptions.csv\n")
cat("    si_table4_change_map_accuracy.csv\n")
cat("    si_table5_static_map_accuracy.csv\n")
cat("    rs_accuracy_paper_stats.csv\n")

cat("\nScript complete.\n")

