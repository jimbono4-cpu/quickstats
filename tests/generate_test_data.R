# Phase 0: Generate Synthetic Test Datasets
# Run this script in standard R (not WebR) to create test files
# These files will be bundled with the Shinylive app for benchmarking

set.seed(42)

output_dir <- "data"
if (!dir.exists(output_dir)) dir.create(output_dir)

# ============================================================================
# 1. Main benchmark dataset (~1 MB CSV, 5000 rows, 20 columns)
# ============================================================================
n <- 5000

benchmark_data <- data.frame(
  id = sprintf("P%04d", 1:n),
  age = round(rnorm(n, 50, 15)),
  sex = sample(c("Male", "Female"), n, replace = TRUE),
  bmi = round(rnorm(n, 27, 5), 1),
  smoking = sample(c("Never", "Former", "Current"), n, replace = TRUE),
  bp_systolic = round(rnorm(n, 130, 20)),
  bp_diastolic = round(rnorm(n, 80, 12)),
  cholesterol = round(rnorm(n, 200, 40)),
  glucose = round(rnorm(n, 100, 30)),
  creatinine = round(rnorm(n, 1.0, 0.3), 2),
  hemoglobin = round(rnorm(n, 14, 2), 1),
  treatment = sample(c("Control", "Treatment"), n, replace = TRUE),
  site = sample(paste0("Site_", sprintf("%02d", 1:10)), n, replace = TRUE),
  outcome_continuous = round(rnorm(n, 50, 10), 1),
  outcome_binary = sample(0:1, n, replace = TRUE, prob = c(0.7, 0.3)),
  time_to_event = pmax(1, round(rexp(n, 0.02))),
  event_status = sample(0:1, n, replace = TRUE, prob = c(0.4, 0.6)),
  score1 = round(rnorm(n, 75, 12)),
  score2 = round(rnorm(n, 80, 10)),
  category = sample(LETTERS[1:8], n, replace = TRUE),
  stringsAsFactors = FALSE
)

# Inject some missing values (~3-7% per variable)
inject_missing <- function(x, pct = 0.05) {
  idx <- sample(length(x), size = round(length(x) * pct))
  x[idx] <- NA
  x
}

benchmark_data$bmi <- inject_missing(benchmark_data$bmi, 0.04)
benchmark_data$bp_systolic <- inject_missing(benchmark_data$bp_systolic, 0.03)
benchmark_data$cholesterol <- inject_missing(benchmark_data$cholesterol, 0.05)
benchmark_data$glucose <- inject_missing(benchmark_data$glucose, 0.06)
benchmark_data$hemoglobin <- inject_missing(benchmark_data$hemoglobin, 0.03)
benchmark_data$score1 <- inject_missing(benchmark_data$score1, 0.07)

write.csv(benchmark_data, file.path(output_dir, "benchmark_5k.csv"), row.names = FALSE)
cat("benchmark_5k.csv:",
    sprintf("%.1f KB", file.size(file.path(output_dir, "benchmark_5k.csv")) / 1024),
    "\n")

# ============================================================================
# 2. test_csv.csv — 500 rows, mixed types, some missing
# ============================================================================
n2 <- 500
test_csv <- data.frame(
  participant_id = sprintf("S%03d", 1:n2),
  age = round(rnorm(n2, 45, 12)),
  sex = sample(c("Male", "Female"), n2, replace = TRUE),
  race = sample(c("White", "Black", "Hispanic", "Asian", "Other"), n2,
                replace = TRUE, prob = c(0.5, 0.2, 0.15, 0.1, 0.05)),
  education = sample(c("High School", "College", "Graduate"), n2,
                     replace = TRUE, prob = c(0.3, 0.4, 0.3)),
  income = round(rlnorm(n2, log(50000), 0.6)),
  bmi = round(rnorm(n2, 28, 6), 1),
  systolic_bp = round(rnorm(n2, 125, 18)),
  diastolic_bp = round(rnorm(n2, 78, 11)),
  heart_rate = round(rnorm(n2, 72, 12)),
  cholesterol_total = round(rnorm(n2, 210, 45)),
  hdl = round(rnorm(n2, 55, 15)),
  ldl = round(rnorm(n2, 130, 35)),
  triglycerides = round(rlnorm(n2, log(150), 0.5)),
  fasting_glucose = round(rnorm(n2, 95, 25)),
  hba1c = round(rnorm(n2, 5.7, 0.8), 1),
  smoking_status = sample(c("Never", "Former", "Current"), n2, replace = TRUE),
  alcohol_use = sample(c("None", "Moderate", "Heavy"), n2,
                       replace = TRUE, prob = c(0.3, 0.5, 0.2)),
  physical_activity = sample(c("Sedentary", "Low", "Moderate", "High"), n2,
                             replace = TRUE),
  diabetes_status = sample(0:1, n2, replace = TRUE, prob = c(0.85, 0.15)),
  stringsAsFactors = FALSE
)

# Inject missing values
test_csv$bmi <- inject_missing(test_csv$bmi, 0.05)
test_csv$cholesterol_total <- inject_missing(test_csv$cholesterol_total, 0.04)
test_csv$hba1c <- inject_missing(test_csv$hba1c, 0.06)
test_csv$income <- inject_missing(test_csv$income, 0.08)
test_csv$physical_activity <- inject_missing(test_csv$physical_activity, 0.03)

write.csv(test_csv, file.path(output_dir, "test_csv.csv"), row.names = FALSE)
cat("test_csv.csv:",
    sprintf("%.1f KB", file.size(file.path(output_dir, "test_csv.csv")) / 1024),
    "\n")

# ============================================================================
# 3. test_clustered.csv — 500 students in 25 schools
# ============================================================================
n_schools <- 25
students_per <- 20
n3 <- n_schools * students_per

school_id <- rep(1:n_schools, each = students_per)
school_effect <- rep(rnorm(n_schools, 0, 5), each = students_per)

test_clustered <- data.frame(
  student_id = sprintf("STU%04d", 1:n3),
  school_id = sprintf("SCH%02d", school_id),
  grade = sample(9:12, n3, replace = TRUE),
  age = round(rnorm(n3, 16, 1.5), 1),
  sex = sample(c("Male", "Female"), n3, replace = TRUE),
  ses = sample(c("Low", "Middle", "High"), n3, replace = TRUE,
               prob = c(0.3, 0.5, 0.2)),
  prior_gpa = round(pmin(4.0, pmax(0, rnorm(n3, 2.8, 0.7))), 2),
  study_hours = round(pmax(0, rnorm(n3, 10, 5)), 1),
  attendance_pct = round(pmin(100, pmax(50, rnorm(n3, 92, 8))), 1),
  test_score = round(50 + school_effect + rnorm(n3, 0, 10), 1),
  passed = as.integer(50 + school_effect + rnorm(n3, 0, 10) > 50),
  stringsAsFactors = FALSE
)

write.csv(test_clustered, file.path(output_dir, "test_clustered.csv"), row.names = FALSE)
cat("test_clustered.csv:",
    sprintf("%.1f KB", file.size(file.path(output_dir, "test_clustered.csv")) / 1024),
    "\n")

# ============================================================================
# 4. test_survival.csv — time-to-event with censoring
# ============================================================================
n4 <- 500
test_survival <- data.frame(
  patient_id = sprintf("PT%04d", 1:n4),
  age = round(rnorm(n4, 60, 12)),
  sex = sample(c("Male", "Female"), n4, replace = TRUE),
  stage = sample(c("I", "II", "III", "IV"), n4, replace = TRUE,
                 prob = c(0.2, 0.35, 0.3, 0.15)),
  treatment = sample(c("Standard", "Experimental"), n4, replace = TRUE),
  ecog_score = sample(0:3, n4, replace = TRUE, prob = c(0.3, 0.4, 0.2, 0.1)),
  tumor_size = round(pmax(0.5, rnorm(n4, 3, 1.5)), 1),
  biomarker_a = round(rnorm(n4, 100, 30)),
  biomarker_b = round(rlnorm(n4, log(50), 0.5)),
  follow_up_months = round(pmax(1, rexp(n4, 1/24)), 1),
  event_death = sample(0:1, n4, replace = TRUE, prob = c(0.45, 0.55)),
  stringsAsFactors = FALSE
)

# Make censoring correlated with treatment (experimental has longer survival)
treat_idx <- test_survival$treatment == "Experimental"
test_survival$follow_up_months[treat_idx] <- round(
  pmax(1, test_survival$follow_up_months[treat_idx] * 1.3), 1)
test_survival$event_death[treat_idx] <- sample(
  0:1, sum(treat_idx), replace = TRUE, prob = c(0.55, 0.45))

write.csv(test_survival, file.path(output_dir, "test_survival.csv"), row.names = FALSE)
cat("test_survival.csv:",
    sprintf("%.1f KB", file.size(file.path(output_dir, "test_survival.csv")) / 1024),
    "\n")

# ============================================================================
# 5. test_binary_outcome.csv — logistic regression dataset
# ============================================================================
n5 <- 500
x1 <- rnorm(n5)
x2 <- rnorm(n5)
x3 <- sample(c("A", "B", "C"), n5, replace = TRUE)
x3_num <- as.numeric(factor(x3))
logit <- -1 + 0.5 * x1 - 0.3 * x2 + 0.4 * x3_num
prob <- 1 / (1 + exp(-logit))

test_binary <- data.frame(
  subject_id = sprintf("SBJ%04d", 1:n5),
  predictor_continuous_1 = round(x1, 3),
  predictor_continuous_2 = round(x2, 3),
  predictor_categorical = x3,
  age = round(rnorm(n5, 50, 15)),
  sex = sample(c("Male", "Female"), n5, replace = TRUE),
  comorbidity_count = rpois(n5, 1.5),
  outcome = rbinom(n5, 1, prob),
  stringsAsFactors = FALSE
)

write.csv(test_binary, file.path(output_dir, "test_binary_outcome.csv"), row.names = FALSE)
cat("test_binary_outcome.csv:",
    sprintf("%.1f KB", file.size(file.path(output_dir, "test_binary_outcome.csv")) / 1024),
    "\n")

# ============================================================================
# 6. High-cardinality stress test
# ============================================================================
n6 <- 5000
test_highcard <- data.frame(
  id = 1:n6,
  value = rnorm(n6),
  group = sample(c("A", "B"), n6, replace = TRUE),
  high_card_var = sample(paste0("Category_", sprintf("%02d", 1:30)), n6, replace = TRUE),
  stringsAsFactors = FALSE
)

write.csv(test_highcard, file.path(output_dir, "test_highcard.csv"), row.names = FALSE)
cat("test_highcard.csv:",
    sprintf("%.1f KB", file.size(file.path(output_dir, "test_highcard.csv")) / 1024),
    "\n")

cat("\nAll test datasets generated successfully.\n")
cat("Files in", output_dir, ":\n")
for (f in list.files(output_dir)) {
  cat(sprintf("  %s: %.1f KB\n", f, file.size(file.path(output_dir, f)) / 1024))
}
