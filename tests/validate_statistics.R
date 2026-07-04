# =============================================================================
# Statistical Validation — tests the REAL app functions against ground truth
#
# Unlike earlier versions, nothing is copy-pasted from app.R: the script parses
# shinylive-app/app.R and evaluates only its top-level function definitions, so
# any drift in the app's statistical logic is caught here directly.
#
# Ground truth (from tests/generate_test_data.R, set.seed(42)):
#   - test_binary_outcome.csv: logit = -1 + 0.5*x1 - 0.3*x2 + 0.4*x3_num
#   - test_clustered.csv:      test_score clustered by school_id
#   - test_survival.csv:       Experimental arm has lower event hazard
#
# Exit code: 0 if all tests pass, 1 otherwise (CI-gating).
# =============================================================================

repo_root <- normalizePath(file.path(dirname(sub("--file=", "", grep("--file=",
  commandArgs(trailingOnly = FALSE), value = TRUE)[1])), ".."))
setwd(repo_root)

cat(strrep("=", 72), "\n")
cat("STATISTICAL VALIDATION - real app functions vs ground truth\n")
cat(strrep("=", 72), "\n")
cat("R:", R.version.string, "\n\n")

pass_count <- 0; fail_count <- 0
record <- function(test_name, passed, detail = "") {
  status <- if (isTRUE(passed)) "PASS" else "FAIL"
  cat(sprintf("  [%s] %s", status, test_name))
  if (nchar(detail) > 0) cat(" -", detail)
  cat("\n")
  if (isTRUE(passed)) pass_count <<- pass_count + 1 else fail_count <<- fail_count + 1
}

# ---------------------------------------------------------------------------
# Load the app's function definitions (and nothing else) from app.R
# ---------------------------------------------------------------------------
cat("--- Loading function definitions from shinylive-app/app.R ---\n")
app_env <- new.env(parent = globalenv())
exprs <- parse("shinylive-app/app.R")
n_loaded <- 0
for (e in exprs) {
  if (is.call(e) && identical(as.character(e[[1]]), "<-") &&
      is.call(e[[3]]) && identical(as.character(e[[3]][[1]]), "function")) {
    eval(e, envir = app_env)
    n_loaded <- n_loaded + 1
  }
}
cat("Loaded", n_loaded, "function definitions\n\n")
attach(app_env, name = "app_functions", warn.conflicts = FALSE)

# ---------------------------------------------------------------------------
# 1. Variable classification
# ---------------------------------------------------------------------------
cat("--- 1. classify_variables ---\n")
df <- data.frame(
  cont = rnorm(100), few = sample(1:3, 100, TRUE),
  chr = sample(letters[1:5], 100, TRUE), lgl = sample(c(TRUE, FALSE), 100, TRUE),
  dt = as.Date("2020-01-01") + 1:100
)
vt <- classify_variables(df)
record("continuous numeric -> numeric", vt$type[vt$variable == "cont"] == "numeric")
record("numeric <=10 uniques -> categorical", vt$type[vt$variable == "few"] == "categorical")
record("character -> categorical", vt$type[vt$variable == "chr"] == "categorical")
record("logical -> categorical", vt$type[vt$variable == "lgl"] == "categorical")
record("Date -> date", vt$type[vt$variable == "dt"] == "date")

# ---------------------------------------------------------------------------
# 2. Binary outcome coercion (the Yes/No regression-crash fix)
# ---------------------------------------------------------------------------
cat("--- 2. to_binary01 ---\n")
b <- to_binary01(factor(c("No", "Yes", "Yes", "No")))
record("Yes/No factor -> 0/1, event=Yes",
       !is.null(b) && identical(b$values, c(0, 1, 1, 0)) && b$event_label == "Yes")
b <- to_binary01(c("Case", "Control", "Case"))
record("character 2-level -> 0/1, event=Control (alphabetical)",
       !is.null(b) && b$event_label == "Control" && identical(b$values, c(0, 1, 0)))
b <- to_binary01(c(0, 1, 1, 0, NA))
record("numeric 0/1 passes through", !is.null(b) && identical(b$values[1:4], c(0, 1, 1, 0)))
b <- to_binary01(c(1, 2, 2, 1))
record("numeric 1/2 -> 0/1, event=2", !is.null(b) && identical(b$values, c(0, 1, 1, 0)) && b$event_label == "2")
record("logical -> 0/1", identical(to_binary01(c(TRUE, FALSE))$values, c(1, 0)))
record("3-level variable rejected", is.null(to_binary01(c("a", "b", "c"))))

cat("--- 3. to_count ---\n")
record("integer counts accepted", identical(to_count(c(0, 3, 12, NA)), c(0, 3, 12, NA)))
record("factor counts converted", identical(to_count(factor(c(0, 2, 5))), c(0, 2, 5)))
record("negative values rejected", is.null(to_count(c(-1, 2, 3))))
record("non-integers rejected", is.null(to_count(c(1.5, 2))))

# ---------------------------------------------------------------------------
# 4. Labels and model-type helpers
# ---------------------------------------------------------------------------
cat("--- 4. model-type helpers ---\n")
record("exp_label mapping", exp_label("glm") == "OR" && exp_label("cox") == "HR" &&
       exp_label("poisson") == "IRR" && exp_label("negbin") == "IRR")
record("is_exp_model", is_exp_model("poisson") && is_exp_model("cox") &&
       !is_exp_model("lm") && is_exp_model("lmer", TRUE) && !is_exp_model("lmer", FALSE))
record("estimate_description covers count models",
       grepl("Poisson", estimate_description("poisson")) &&
       grepl("negative binomial", estimate_description("negbin")))

# ---------------------------------------------------------------------------
# 5. base64 encoder (report image embedding)
# ---------------------------------------------------------------------------
cat("--- 5. b64encode_raw ---\n")
record("RFC test vector 'Man' -> TWFu", b64encode_raw(charToRaw("Man")) == "TWFu")
record("padding 'Ma' -> TWE=", b64encode_raw(charToRaw("Ma")) == "TWE=")
record("padding 'M' -> TQ==", b64encode_raw(charToRaw("M")) == "TQ==")
if (requireNamespace("jsonlite", quietly = TRUE)) {
  set.seed(1); r <- as.raw(sample(0:255, 5000, replace = TRUE))
  record("5000 random bytes match jsonlite reference",
         b64encode_raw(r) == gsub("[\r\n]", "", jsonlite::base64_enc(r)))
}

# ---------------------------------------------------------------------------
# 6. AUC / ROC
# ---------------------------------------------------------------------------
cat("--- 6. calc_auc / roc_points ---\n")
a <- calc_auc(c(.9, .8, .3, .2), c(1, 1, 0, 0))
record("perfect separation AUC = 1", !is.null(a) && abs(a$auc - 1) < 1e-9)
a <- calc_auc(c(.2, .8, .3, .9), c(1, 0, 1, 0))
record("perfectly wrong AUC = 0", !is.null(a) && abs(a$auc - 0) < 1e-9)
set.seed(7)
p <- runif(400); y <- rbinom(400, 1, p)
a <- calc_auc(p, y)
rp <- roc_points(p, y)
trap <- sum(diff(rp$fpr) * (head(rp$tpr, -1) + tail(rp$tpr, -1)) / 2)
record("AUC matches ROC trapezoid integral", abs(a$auc - trap) < 0.005,
       sprintf("mann-whitney=%.4f trapezoid=%.4f", a$auc, trap))
record("AUC CI is a proper interval", a$lo <= a$auc && a$auc <= a$hi)
record("degenerate outcome returns NULL", is.null(calc_auc(runif(10), rep(1, 10))))

# ---------------------------------------------------------------------------
# 7. friendly_model_error
# ---------------------------------------------------------------------------
cat("--- 7. friendly_model_error ---\n")
record("0 non-NA cases mapped",
       grepl("no complete rows", friendly_model_error("0 (non-NA) cases")))
record("single-level factor mapped",
       grepl("single level", friendly_model_error("contrasts can be applied only to factors with 2 or more levels")))
record("unknown message passes through",
       friendly_model_error("some novel error") == "some novel error")

# ---------------------------------------------------------------------------
# 8. Ground-truth model recovery (as the app fits them)
# ---------------------------------------------------------------------------
cat("--- 8. logistic regression ground truth (test_binary_outcome.csv) ---\n")
suppressPackageStartupMessages({ library(broom); library(survival)
  library(sandwich); library(lmtest) })

dat <- read.csv("tests/data/test_binary_outcome.csv", stringsAsFactors = FALSE)
# Recode to Yes/No to exercise the exact path that used to crash the app
dat$outcome_yn <- ifelse(dat$outcome == 1, "Yes", "No")
vt <- classify_variables(dat)
dat2 <- prepare_model_data(dat, vt)
bin <- to_binary01(dat2$outcome_yn)
record("Yes/No outcome coerced with event=Yes", !is.null(bin) && bin$event_label == "Yes")
dat2$outcome_yn <- bin$values
fit <- glm(outcome_yn ~ predictor_continuous_1 + predictor_continuous_2,
           data = dat2, family = binomial)
td <- broom::tidy(fit, conf.int = TRUE)
ci1 <- td[td$term == "predictor_continuous_1", c("conf.low", "conf.high")]
ci2 <- td[td$term == "predictor_continuous_2", c("conf.low", "conf.high")]
record("beta(x1) 95% CI covers truth 0.5", ci1$conf.low <= 0.5 && 0.5 <= ci1$conf.high,
       sprintf("CI [%.3f, %.3f]", ci1$conf.low, ci1$conf.high))
record("beta(x2) 95% CI covers truth -0.3", ci2$conf.low <= -0.3 && -0.3 <= ci2$conf.high,
       sprintf("CI [%.3f, %.3f]", ci2$conf.low, ci2$conf.high))
a <- calc_auc(fitted(fit), fit$y)
record("model AUC in sane range", !is.null(a) && a$auc > 0.55 && a$auc < 0.95,
       sprintf("AUC=%.3f", a$auc))

cat("--- 9. cluster-robust SEs (test_clustered.csv) ---\n")
cl <- read.csv("tests/data/test_clustered.csv", stringsAsFactors = FALSE)
fit <- lm(test_score ~ study_hours + prior_gpa, data = cl)
# Align cluster vector to the model frame, exactly as the app does
used_rows <- match(rownames(model.frame(fit)), rownames(cl))
cluster <- cl$school_id[used_rows]
rob <- lmtest::coeftest(fit, vcov = sandwich::vcovCL(fit, cluster = cluster))
record("cluster-robust SEs computed after model-frame alignment",
       all(is.finite(rob[, 2])) && nrow(rob) == 3)
naive_se <- summary(fit)$coefficients[, 2]
record("robust SEs differ from naive (clustering has an effect)",
       any(abs(rob[, 2] - naive_se) / naive_se > 0.01))

cat("--- 10. Cox regression (test_survival.csv mechanics + trial ground truth) ---\n")
# test_survival.csv does not encode a controlled hazard ratio (events are
# assigned independently of time in the generator), so it only checks that
# the app's coercion + fit pipeline works on a factor event variable.
sv <- read.csv("tests/data/test_survival.csv", stringsAsFactors = FALSE)
vt <- classify_variables(sv)
sv2 <- prepare_model_data(sv, vt)
bin <- to_binary01(sv2$event_death)
sv2$event_death <- bin$values
fit <- survival::coxph(survival::Surv(follow_up_months, event_death) ~ treatment + age,
                       data = sv2)
hr_trt <- exp(coef(fit)[grep("treatment", names(coef(fit)))])
record("Cox fits with factor event variable, finite HR", is.finite(hr_trt),
       sprintf("HR=%.3f", hr_trt))

# Ground truth: make_trial_example simulates exponential survival with
# treatment log-hazard -0.4 (HR = 0.670)
trial_cox <- make_trial_example()
fit <- survival::coxph(survival::Surv(followup_months, cv_event) ~ arm + age,
                       data = trial_cox)
ci <- exp(confint(fit)["armTreatment", ])
record("trial-example treatment HR 95% CI covers truth 0.670",
       ci[1] <= exp(-0.4) && exp(-0.4) <= ci[2],
       sprintf("CI [%.3f, %.3f]", ci[1], ci[2]))

cat("--- 11. Poisson / negative binomial (synthetic trial example) ---\n")
trial <- make_trial_example()
record("trial example is deterministic",
       identical(trial$age[1:5], make_trial_example()$age[1:5]))
record("trial example has expected columns",
       all(c("arm", "bp_controlled", "admissions", "followup_months", "cv_event")
           %in% names(trial)))
cnt <- to_count(trial$admissions)
record("admissions is a valid count outcome", !is.null(cnt))
fitp <- glm(admissions ~ arm + age, data = trial, family = poisson)
irr <- exp(coef(fitp)[["armTreatment"]])
record("Poisson IRR for treatment < 1 (as simulated)", irr < 1,
       sprintf("IRR=%.3f", irr))
if (requireNamespace("MASS", quietly = TRUE)) {
  fitnb <- MASS::glm.nb(admissions ~ arm + age, data = trial)
  record("negative binomial fits and agrees in direction",
         exp(coef(fitnb)[["armTreatment"]]) < 1)
}

# ---------------------------------------------------------------------------
cat("\n", strrep("=", 72), "\n", sep = "")
cat(sprintf("RESULT: %d passed, %d failed\n", pass_count, fail_count))
cat(strrep("=", 72), "\n")
if (fail_count > 0) quit(status = 1)
