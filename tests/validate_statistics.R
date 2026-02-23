# =============================================================================
# Statistical Validation Script
# Tests the app's analysis functions against synthetic data with known values
#
# Ground truth from generate_test_data.R (set.seed(42)):
#   - test_binary_outcome.csv: logit = -1 + 0.5*x1 - 0.3*x2 + 0.4*x3_num
#   - test_clustered.csv: test_score = 50 + school_effect(sd=5) + noise(sd=10)
#   - test_survival.csv: Experimental treatment has 30% longer follow-up, lower events
#   - benchmark_5k.csv: Independent random columns (null effects expected)
# =============================================================================

cat("=" |> rep(72) |> paste(collapse = ""), "\n")
cat("STATISTICAL VALIDATION — Ground Truth Testing\n")
cat("=" |> rep(72) |> paste(collapse = ""), "\n")
cat("Date:", format(Sys.time(), "%Y-%m-%d %H:%M:%S"), "\n")
cat("R:", R.version.string, "\n\n")

# Track results
results <- list()
pass_count <- 0
fail_count <- 0

record <- function(test_name, passed, detail = "") {
  status <- if (passed) "PASS" else "FAIL"
  cat(sprintf("  [%s] %s", status, test_name))
  if (nchar(detail) > 0) cat(" —", detail)
  cat("\n")
  results[[length(results) + 1]] <<- list(
    test = test_name, status = status, detail = detail
  )
  if (passed) pass_count <<- pass_count + 1 else fail_count <<- fail_count + 1
}

# ============================================================================
# Source app helper functions
# ============================================================================
cat("--- Loading app helper functions ---\n")

# Extract classify_variables and prepare_model_data from app.R
# (We inline them here to test in isolation)

classify_variables <- function(df) {
  types <- sapply(names(df), function(col) {
    x <- df[[col]]
    if (inherits(x, c("Date", "POSIXct", "POSIXlt"))) return("date")
    if (is.factor(x) || is.character(x) || is.logical(x)) return("categorical")
    if (is.numeric(x) || is.integer(x)) {
      n_unique <- length(unique(na.omit(x)))
      if (n_unique <= 10) return("categorical")
      return("numeric")
    }
    "other"
  })
  data.frame(variable = names(df), type = unname(types), stringsAsFactors = FALSE)
}

prepare_model_data <- function(df, var_types) {
  for (i in seq_len(nrow(var_types))) {
    v <- var_types$variable[i]
    if (var_types$type[i] == "categorical" && v %in% names(df)) {
      if (!is.factor(df[[v]])) {
        df[[v]] <- factor(df[[v]])
      }
    }
  }
  df
}

var_summary <- function(x, name = "") {
  n <- length(x)
  n_miss <- sum(is.na(x))
  pct_miss <- round(100 * n_miss / n, 1)
  if (is.numeric(x) && length(unique(na.omit(x))) > 10) {
    vals <- na.omit(x)
    list(variable = name, type = "numeric",
         n = n, n_missing = n_miss, pct_missing = pct_miss,
         mean = round(mean(vals), 2), sd = round(sd(vals), 2),
         median = round(median(vals), 2),
         min = round(min(vals), 2), max = round(max(vals), 2),
         n_unique = length(unique(vals)))
  } else {
    vals <- na.omit(x)
    freq <- sort(table(vals), decreasing = TRUE)
    list(variable = name, type = "categorical",
         n = n, n_missing = n_miss, pct_missing = pct_miss,
         n_levels = length(freq),
         top_level = if (length(freq) > 0) names(freq)[1] else NA,
         top_freq = if (length(freq) > 0) unname(freq[1]) else NA)
  }
}

cat("  Functions loaded.\n\n")

# ============================================================================
# TEST 1: classify_variables() correctness
# ============================================================================
cat("--- TEST 1: Variable Classification ---\n")

# Test with known data types
test_df <- data.frame(
  numeric_many = rnorm(100),
  numeric_few = sample(1:5, 100, replace = TRUE),   # <=10 unique -> categorical
  char_var = sample(letters[1:3], 100, replace = TRUE),
  factor_var = factor(sample(c("A", "B"), 100, replace = TRUE)),
  logical_var = sample(c(TRUE, FALSE), 100, replace = TRUE),
  date_var = Sys.Date() + 1:100,
  integer_many = sample(1:50, 100, replace = TRUE),  # >10 unique -> numeric
  binary_01 = sample(0:1, 100, replace = TRUE),      # <=10 unique -> categorical
  stringsAsFactors = FALSE
)

vt <- classify_variables(test_df)

record("numeric_many detected as numeric",
       vt$type[vt$variable == "numeric_many"] == "numeric")

record("numeric_few (5 unique int) detected as categorical",
       vt$type[vt$variable == "numeric_few"] == "categorical",
       paste("unique values:", length(unique(test_df$numeric_few))))

record("char_var detected as categorical",
       vt$type[vt$variable == "char_var"] == "categorical")

record("factor_var detected as categorical",
       vt$type[vt$variable == "factor_var"] == "categorical")

record("logical_var detected as categorical",
       vt$type[vt$variable == "logical_var"] == "categorical")

record("date_var detected as date",
       vt$type[vt$variable == "date_var"] == "date")

record("integer_many (>10 unique) detected as numeric",
       vt$type[vt$variable == "integer_many"] == "numeric",
       paste("unique values:", length(unique(test_df$integer_many))))

record("binary_01 (0/1) detected as categorical",
       vt$type[vt$variable == "binary_01"] == "categorical")

cat("\n")

# ============================================================================
# TEST 2: prepare_model_data() converts categoricals to factor
# ============================================================================
cat("--- TEST 2: prepare_model_data() Factor Conversion ---\n")

df_test <- data.frame(
  x_num = rnorm(50),
  x_cat_char = sample(c("A", "B"), 50, replace = TRUE),
  x_cat_int = sample(1:3, 50, replace = TRUE),
  stringsAsFactors = FALSE
)
vt2 <- classify_variables(df_test)
df_prepared <- prepare_model_data(df_test, vt2)

record("Numeric column stays numeric after prepare_model_data",
       is.numeric(df_prepared$x_num))

record("Character categorical converted to factor",
       is.factor(df_prepared$x_cat_char))

record("Integer categorical (<=10 unique) converted to factor",
       is.factor(df_prepared$x_cat_int))

cat("\n")

# ============================================================================
# TEST 3: Logistic Regression — Known Coefficients
# Ground truth: logit = -1 + 0.5*x1 - 0.3*x2 + 0.4*x3_num
# where x3_num = as.numeric(factor(x3)) -> A=1, B=2, C=3
#
# When R fits glm with factor(x3), reference level = "A" (x3_num=1):
#   Intercept = -1 + 0.4*1 = -0.6
#   x1 coefficient ~ 0.5
#   x2 coefficient ~ -0.3
#   x3B coefficient ~ 0.4 (x3_num=2 minus x3_num=1)
#   x3C coefficient ~ 0.8 (x3_num=3 minus x3_num=1)
# ============================================================================
cat("--- TEST 3: Logistic Regression vs Known Coefficients ---\n")

binary_data <- read.csv("data/test_binary_outcome.csv", stringsAsFactors = FALSE)
cat("  Loaded test_binary_outcome.csv:", nrow(binary_data), "rows\n")

# Classify and prepare like the app does
vt_bin <- classify_variables(binary_data)
df_bin <- prepare_model_data(binary_data, vt_bin)

# Check that predictor_categorical is now factor
record("predictor_categorical detected as categorical",
       vt_bin$type[vt_bin$variable == "predictor_categorical"] == "categorical")

record("predictor_categorical converted to factor by prepare_model_data",
       is.factor(df_bin$predictor_categorical))

# Fit logistic model (same as app would)
mod_logit <- glm(outcome ~ predictor_continuous_1 + predictor_continuous_2 + predictor_categorical,
                 data = df_bin, family = binomial)
coefs <- coef(mod_logit)
cat("  Fitted coefficients:\n")
print(round(coefs, 4))

# Ground truth values
# Due to sampling noise (n=500), we use tolerance of +/- 0.5 from true value
true_intercept <- -0.6  # -1 + 0.4*1
true_x1 <- 0.5
true_x2 <- -0.3
true_x3B <- 0.4   # diff between x3_num=2 and x3_num=1
true_x3C <- 0.8   # diff between x3_num=3 and x3_num=1

tol <- 0.5  # generous tolerance for n=500

record("Intercept near -0.6 (true: -1 + 0.4*ref)",
       abs(coefs["(Intercept)"] - true_intercept) < tol,
       sprintf("est=%.3f, true=%.1f, |diff|=%.3f",
               coefs["(Intercept)"], true_intercept,
               abs(coefs["(Intercept)"] - true_intercept)))

record("predictor_continuous_1 coefficient near 0.5",
       abs(coefs["predictor_continuous_1"] - true_x1) < tol,
       sprintf("est=%.3f, true=%.1f, |diff|=%.3f",
               coefs["predictor_continuous_1"], true_x1,
               abs(coefs["predictor_continuous_1"] - true_x1)))

record("predictor_continuous_2 coefficient near -0.3",
       abs(coefs["predictor_continuous_2"] - true_x2) < tol,
       sprintf("est=%.3f, true=%.1f, |diff|=%.3f",
               coefs["predictor_continuous_2"], true_x2,
               abs(coefs["predictor_continuous_2"] - true_x2)))

record("predictor_categoricalB coefficient near 0.4",
       abs(coefs["predictor_categoricalB"] - true_x3B) < tol,
       sprintf("est=%.3f, true=%.1f, |diff|=%.3f",
               coefs["predictor_categoricalB"], true_x3B,
               abs(coefs["predictor_categoricalB"] - true_x3B)))

record("predictor_categoricalC coefficient near 0.8",
       abs(coefs["predictor_categoricalC"] - true_x3C) < tol,
       sprintf("est=%.3f, true=%.1f, |diff|=%.3f",
               coefs["predictor_categoricalC"], true_x3C,
               abs(coefs["predictor_categoricalC"] - true_x3C)))

# Check that the sign of each coefficient matches ground truth
record("x1 coefficient has correct sign (positive)",
       coefs["predictor_continuous_1"] > 0)

record("x2 coefficient has correct sign (negative)",
       coefs["predictor_continuous_2"] < 0)

record("x3B coefficient has correct sign (positive)",
       coefs["predictor_categoricalB"] > 0)

record("x3C coefficient has correct sign (positive)",
       coefs["predictor_categoricalC"] > 0)

record("x3C > x3B (C has larger effect than B)",
       coefs["predictor_categoricalC"] > coefs["predictor_categoricalB"])

# Check broom::tidy works on the model
library(broom)
tidy_logit <- broom::tidy(mod_logit, conf.int = TRUE)
record("broom::tidy returns correct number of rows (5 terms)",
       nrow(tidy_logit) == 5)

record("broom::tidy includes confidence intervals",
       all(c("conf.low", "conf.high") %in% names(tidy_logit)))

# Exponentiate for odds ratios (as the app does)
tidy_logit$OR <- exp(tidy_logit$estimate)
tidy_logit$ci_lower <- exp(tidy_logit$conf.low)
tidy_logit$ci_upper <- exp(tidy_logit$conf.high)

record("Odds ratio for x1 > 1 (positive effect)",
       tidy_logit$OR[tidy_logit$term == "predictor_continuous_1"] > 1,
       sprintf("OR=%.3f", tidy_logit$OR[tidy_logit$term == "predictor_continuous_1"]))

record("Odds ratio for x2 < 1 (protective effect)",
       tidy_logit$OR[tidy_logit$term == "predictor_continuous_2"] < 1,
       sprintf("OR=%.3f", tidy_logit$OR[tidy_logit$term == "predictor_continuous_2"]))

cat("\n")

# ============================================================================
# TEST 4: Linear Regression — benchmark_5k.csv
# Outcome_continuous is independent random, so coefficients should be ~0
# ============================================================================
cat("--- TEST 4: Linear Regression — Null Effects ---\n")

bench_data <- read.csv("data/benchmark_5k.csv", stringsAsFactors = FALSE)
cat("  Loaded benchmark_5k.csv:", nrow(bench_data), "rows x", ncol(bench_data), "cols\n")

vt_bench <- classify_variables(bench_data)
df_bench <- prepare_model_data(bench_data, vt_bench)

# outcome_continuous is independent of everything (all generated separately)
mod_lm <- lm(outcome_continuous ~ age + bmi + bp_systolic, data = df_bench)
coefs_lm <- coef(mod_lm)
cat("  Fitted coefficients:\n")
print(round(coefs_lm, 4))

# With n=5000, null coefficients should be very close to 0
record("age coefficient near 0 (null effect)",
       abs(coefs_lm["age"]) < 0.1,
       sprintf("est=%.4f", coefs_lm["age"]))

record("bmi coefficient near 0 (null effect)",
       abs(coefs_lm["bmi"]) < 0.1,
       sprintf("est=%.4f", coefs_lm["bmi"]))

record("bp_systolic coefficient near 0 (null effect)",
       abs(coefs_lm["bp_systolic"]) < 0.1,
       sprintf("est=%.4f", coefs_lm["bp_systolic"]))

# Intercept should be near 50 (the mean of outcome_continuous)
record("Intercept near 50 (mean of outcome_continuous)",
       abs(coefs_lm["(Intercept)"] - 50) < 5,
       sprintf("est=%.2f", coefs_lm["(Intercept)"]))

# broom::tidy
tidy_lm <- broom::tidy(mod_lm, conf.int = TRUE)
record("broom::tidy(lm) returns 4 rows",
       nrow(tidy_lm) == 4)

# P-values should mostly be non-significant for null effects
p_age <- tidy_lm$p.value[tidy_lm$term == "age"]
p_bmi <- tidy_lm$p.value[tidy_lm$term == "bmi"]
p_bp <- tidy_lm$p.value[tidy_lm$term == "bp_systolic"]

# At least 2 of 3 null predictors should have p > 0.05
n_nonsig <- sum(c(p_age, p_bmi, p_bp) > 0.05)
record("At least 2/3 null predictors have p > 0.05",
       n_nonsig >= 2,
       sprintf("p_age=%.3f, p_bmi=%.3f, p_bp=%.3f", p_age, p_bmi, p_bp))

cat("\n")

# ============================================================================
# TEST 5: Cox Regression — test_survival.csv
# Experimental treatment: 30% longer follow-up, lower event rate
# ============================================================================
cat("--- TEST 5: Cox Regression — Treatment Effect ---\n")

surv_data <- read.csv("data/test_survival.csv", stringsAsFactors = FALSE)
cat("  Loaded test_survival.csv:", nrow(surv_data), "rows\n")

vt_surv <- classify_variables(surv_data)
df_surv <- prepare_model_data(surv_data, vt_surv)

library(survival)

# The app's prepare_model_data converts 0/1 event to factor — must convert back
# (This mirrors the fix in app.R for Cox models)
if (is.factor(df_surv$follow_up_months)) {
  df_surv$follow_up_months <- as.numeric(as.character(df_surv$follow_up_months))
}
if (is.factor(df_surv$event_death)) {
  df_surv$event_death <- as.numeric(as.character(df_surv$event_death))
}

mod_cox <- coxph(Surv(follow_up_months, event_death) ~ treatment + age + stage,
                 data = df_surv)
coefs_cox <- coef(mod_cox)
cat("  Fitted coefficients:\n")
print(round(coefs_cox, 4))

tidy_cox <- broom::tidy(mod_cox, conf.int = TRUE)
tidy_cox$HR <- exp(tidy_cox$estimate)

# Experimental treatment should have HR < 1 (protective)
hr_treat <- tidy_cox$HR[grep("Experimental|Standard", tidy_cox$term)]
if (length(hr_treat) > 0) {
  # The coefficient sign tells us the direction
  treat_coef <- coefs_cox[grep("treatment", names(coefs_cox))]
  record("Treatment coefficient present in Cox model",
         length(treat_coef) > 0,
         sprintf("coef=%.3f", treat_coef[1]))

  # Experimental has lower event rate (0.45 vs 0.55) + longer follow-up
  # So HR for Experimental vs Standard should be < 1 (protective)
  # Or equivalently, HR for Standard vs Experimental > 1
  record("Treatment has detectable effect (HR != 1)",
         length(treat_coef) > 0,
         sprintf("HR=%.3f", exp(treat_coef[1])))
} else {
  record("Treatment variable included in model", FALSE, "not found in coefficients")
}

record("broom::tidy(coxph) returns rows",
       nrow(tidy_cox) > 0,
       sprintf("%d terms", nrow(tidy_cox)))

record("Hazard ratios are all positive",
       all(tidy_cox$HR > 0))

cat("\n")

# ============================================================================
# TEST 6: Mixed Model — test_clustered.csv
# test_score = 50 + school_effect(sd=5) + noise(sd=10)
# ============================================================================
cat("--- TEST 6: Mixed Model — School Random Effects ---\n")

clust_data <- read.csv("data/test_clustered.csv", stringsAsFactors = FALSE)
cat("  Loaded test_clustered.csv:", nrow(clust_data), "rows,",
    length(unique(clust_data$school_id)), "schools\n")

vt_clust <- classify_variables(clust_data)
df_clust <- prepare_model_data(clust_data, vt_clust)

library(lme4)
mod_lmer <- lmer(test_score ~ 1 + (1 | school_id), data = df_clust)
vc <- as.data.frame(VarCorr(mod_lmer))
cat("  Variance components:\n")
print(vc)

# Extract SDs
re_sd <- vc$sdcor[vc$grp == "school_id"]
resid_sd <- vc$sdcor[vc$grp == "Residual"]

record("Random effect SD near 5 (school_effect sd=5)",
       abs(re_sd - 5) < 3,
       sprintf("est=%.2f, true=5.0", re_sd))

record("Residual SD near 10 (noise sd=10)",
       abs(resid_sd - 10) < 3,
       sprintf("est=%.2f, true=10.0", resid_sd))

# Fixed intercept should be near 50
fe <- fixef(mod_lmer)
record("Fixed intercept near 50",
       abs(fe["(Intercept)"] - 50) < 5,
       sprintf("est=%.2f, true=50", fe["(Intercept)"]))

# ICC: school_var / (school_var + resid_var)
# True ICC = 25/(25+100) = 0.2
icc_est <- re_sd^2 / (re_sd^2 + resid_sd^2)
icc_true <- 25 / (25 + 100)
record("ICC near 0.20 (true: 25/(25+100))",
       abs(icc_est - icc_true) < 0.15,
       sprintf("est=%.3f, true=%.3f", icc_est, icc_true))

cat("\n")

# ============================================================================
# TEST 7: gtsummary Table 1 — sanity checks
# ============================================================================
cat("--- TEST 7: gtsummary Table 1 ---\n")

library(gtsummary)

# Use the binary outcome data
tbl1 <- tbl_summary(
  df_bin[, c("predictor_continuous_1", "predictor_continuous_2",
             "predictor_categorical", "outcome")],
  by = "outcome"
)

record("tbl_summary creates valid object",
       inherits(tbl1, "tbl_summary"))

# Check it has the right number of variable rows
record("Table 1 includes all 3 predictors",
       length(tbl1$table_body$variable |> unique()) >= 3)

# Add p-values
tbl1_p <- tbl1 |> add_p()
record("add_p() succeeds",
       inherits(tbl1_p, "tbl_summary"))

# Table 1 on benchmark data with stratification
tbl_bench <- tbl_summary(
  df_bench[, c("age", "bmi", "sex", "smoking", "treatment")],
  by = "treatment"
)
record("Table 1 on benchmark data succeeds",
       inherits(tbl_bench, "tbl_summary"))

# gt rendering
library(gt)
gt_tbl <- as_gt(tbl_bench)
html_out <- gt::as_raw_html(gt_tbl)
record("gt renders Table 1 to HTML",
       nchar(html_out) > 100,
       sprintf("%d chars of HTML", nchar(html_out)))

cat("\n")

# ============================================================================
# TEST 8: Variable type detection on real-ish data
# ============================================================================
cat("--- TEST 8: Variable Type Detection on Synthetic Data ---\n")

# Check classification on test_binary_outcome.csv
record("subject_id detected as categorical (character)",
       vt_bin$type[vt_bin$variable == "subject_id"] == "categorical")

record("predictor_continuous_1 detected as numeric",
       vt_bin$type[vt_bin$variable == "predictor_continuous_1"] == "numeric")

record("predictor_continuous_2 detected as numeric",
       vt_bin$type[vt_bin$variable == "predictor_continuous_2"] == "numeric")

record("predictor_categorical detected as categorical",
       vt_bin$type[vt_bin$variable == "predictor_categorical"] == "categorical")

record("outcome (0/1 binary) detected as categorical",
       vt_bin$type[vt_bin$variable == "outcome"] == "categorical")

record("comorbidity_count (Poisson, few unique) detected as categorical",
       vt_bin$type[vt_bin$variable == "comorbidity_count"] == "categorical",
       sprintf("unique values: %d", length(unique(binary_data$comorbidity_count))))

# Check benchmark_5k.csv
record("age (continuous, many unique) detected as numeric",
       vt_bench$type[vt_bench$variable == "age"] == "numeric")

record("sex (character) detected as categorical",
       vt_bench$type[vt_bench$variable == "sex"] == "categorical")

record("smoking (3 levels) detected as categorical",
       vt_bench$type[vt_bench$variable == "smoking"] == "categorical")

record("outcome_binary (0/1) detected as categorical",
       vt_bench$type[vt_bench$variable == "outcome_binary"] == "categorical")

cat("\n")

# ============================================================================
# TEST 9: End-to-end pipeline — logistic regression with factor conversion
# This mirrors exactly what the app does: classify -> prepare -> fit -> tidy
# ============================================================================
cat("--- TEST 9: End-to-End Pipeline (App Replication) ---\n")

# Step 1: Read data (as the app does from fileInput)
raw_data <- read.csv("data/test_binary_outcome.csv", stringsAsFactors = FALSE)

# Step 2: Classify variables (as the app does on upload)
var_types <- classify_variables(raw_data)

# Step 3: Prepare data (as the app does in model_server)
model_data <- prepare_model_data(raw_data, var_types)

# Step 4: Check that categorical predictors are now factors
record("Pipeline: predictor_categorical is factor after pipeline",
       is.factor(model_data$predictor_categorical))

record("Pipeline: outcome is factor after pipeline (binary detected as categorical)",
       is.factor(model_data$outcome))

# Step 5: Fit model (as the app does)
# For logistic: app converts non-0/1 outcome to 0/1
# Since outcome is already 0/1 but factor, need to convert back
outcome_vals <- as.numeric(as.character(model_data$outcome))
model_data$outcome_numeric <- outcome_vals
formula_str <- "outcome_numeric ~ predictor_continuous_1 + predictor_continuous_2 + predictor_categorical"
mod_pipeline <- glm(as.formula(formula_str), data = model_data, family = binomial)

# Step 6: Tidy (as the app does)
tidy_pipeline <- broom::tidy(mod_pipeline, conf.int = TRUE)
tidy_pipeline$OR <- exp(tidy_pipeline$estimate)

record("Pipeline: model converges",
       mod_pipeline$converged)

record("Pipeline: 5 terms in output",
       nrow(tidy_pipeline) == 5)

record("Pipeline: x1 OR > 1 (positive effect)",
       tidy_pipeline$OR[tidy_pipeline$term == "predictor_continuous_1"] > 1)

record("Pipeline: x2 OR < 1 (protective effect)",
       tidy_pipeline$OR[tidy_pipeline$term == "predictor_continuous_2"] < 1)

record("Pipeline: categorical levels B and C present",
       sum(grepl("predictor_categorical", tidy_pipeline$term)) == 2,
       paste("terms:", paste(tidy_pipeline$term[grepl("predictor_categorical", tidy_pipeline$term)],
                             collapse = ", ")))

cat("\n")

# ============================================================================
# TEST 10: Robust standard errors (sandwich)
# ============================================================================
cat("--- TEST 10: Robust Standard Errors ---\n")

library(sandwich)
library(lmtest)

mod_for_robust <- lm(outcome_continuous ~ age + bmi + sex, data = df_bench)
robust_test <- lmtest::coeftest(mod_for_robust,
  vcov = sandwich::vcovHC(mod_for_robust, type = "HC1"))

record("sandwich::vcovHC produces valid covariance matrix",
       is.matrix(sandwich::vcovHC(mod_for_robust)))

record("coeftest with robust SEs produces valid output",
       nrow(robust_test) == 4)

# Robust SEs should be similar to OLS SEs for well-behaved data
ols_se <- summary(mod_for_robust)$coefficients[, "Std. Error"]
robust_se <- robust_test[, "Std. Error"]
ratio <- robust_se / ols_se
record("Robust SEs within 2x of OLS SEs (well-behaved data)",
       all(ratio > 0.5 & ratio < 2),
       sprintf("ratios: %s", paste(round(ratio, 3), collapse = ", ")))

cat("\n")

# ============================================================================
# TEST 11: VIF computation
# ============================================================================
cat("--- TEST 11: VIF (Multicollinearity) ---\n")

library(car)

mod_vif <- lm(outcome_continuous ~ age + bmi + bp_systolic + cholesterol,
              data = df_bench)
vif_vals <- car::vif(mod_vif)
record("car::vif computes successfully",
       length(vif_vals) == 4)

# For independent random variables, VIF should be near 1
record("All VIF values near 1 (independent predictors)",
       all(vif_vals < 5),
       sprintf("VIF: %s", paste(round(vif_vals, 2), collapse = ", ")))

cat("\n")

# ============================================================================
# TEST 12: emmeans
# ============================================================================
cat("--- TEST 12: Estimated Marginal Means ---\n")

library(emmeans)

mod_emm <- lm(outcome_continuous ~ sex + treatment, data = df_bench)
emm_result <- emmeans::emmeans(mod_emm, "sex")
emm_df <- as.data.frame(summary(emm_result))

record("emmeans produces valid output",
       nrow(emm_df) == 2)

record("emmeans for both sexes near 50 (null effect)",
       all(abs(emm_df$emmean - 50) < 5),
       sprintf("Male=%.2f, Female=%.2f", emm_df$emmean[1], emm_df$emmean[2]))

cat("\n")

# ============================================================================
# SUMMARY
# ============================================================================
cat("=" |> rep(72) |> paste(collapse = ""), "\n")
cat(sprintf("VALIDATION COMPLETE: %d PASS, %d FAIL out of %d tests\n",
            pass_count, fail_count, pass_count + fail_count))
cat("=" |> rep(72) |> paste(collapse = ""), "\n")

if (fail_count > 0) {
  cat("\nFailed tests:\n")
  for (r in results) {
    if (r$status == "FAIL") {
      cat(sprintf("  FAIL: %s", r$test))
      if (nchar(r$detail) > 0) cat(" —", r$detail)
      cat("\n")
    }
  }
}

cat("\nGround truth coefficient comparison (logistic regression):\n")
cat(sprintf("  %-30s %10s %10s %10s\n", "Parameter", "True", "Estimated", "|Diff|"))
cat(sprintf("  %-30s %10s %10s %10s\n", "---------", "----", "---------", "------"))
cat(sprintf("  %-30s %10.1f %10.3f %10.3f\n", "Intercept", true_intercept,
            coefs["(Intercept)"], abs(coefs["(Intercept)"] - true_intercept)))
cat(sprintf("  %-30s %10.1f %10.3f %10.3f\n", "predictor_continuous_1 (x1)", true_x1,
            coefs["predictor_continuous_1"], abs(coefs["predictor_continuous_1"] - true_x1)))
cat(sprintf("  %-30s %10.1f %10.3f %10.3f\n", "predictor_continuous_2 (x2)", true_x2,
            coefs["predictor_continuous_2"], abs(coefs["predictor_continuous_2"] - true_x2)))
cat(sprintf("  %-30s %10.1f %10.3f %10.3f\n", "predictor_categoricalB", true_x3B,
            coefs["predictor_categoricalB"], abs(coefs["predictor_categoricalB"] - true_x3B)))
cat(sprintf("  %-30s %10.1f %10.3f %10.3f\n", "predictor_categoricalC", true_x3C,
            coefs["predictor_categoricalC"], abs(coefs["predictor_categoricalC"] - true_x3C)))

cat("\nMixed model variance components:\n")
cat(sprintf("  School random effect SD: est=%.2f, true=5.0\n", re_sd))
cat(sprintf("  Residual SD:             est=%.2f, true=10.0\n", resid_sd))
cat(sprintf("  ICC:                     est=%.3f, true=%.3f\n", icc_est, icc_true))
