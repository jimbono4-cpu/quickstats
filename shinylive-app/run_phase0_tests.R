#!/usr/bin/env Rscript
# Phase 0: WebR Package Validation — Standalone Test Runner
# ============================================================================
# This script tests all planned packages in standard R and outputs a
# structured validation report. Run this BEFORE building the Shinylive app
# to confirm packages work. Then re-run inside WebR via test_app.R.
#
# Usage: Rscript run_phase0_tests.R [--output PHASE0_VALIDATION.md]
# ============================================================================

args <- commandArgs(trailingOnly = TRUE)
output_file <- if ("--output" %in% args) {
  args[which(args == "--output") + 1]
} else {
  "PHASE0_VALIDATION.md"
}

cat("=" |> rep(70) |> paste(collapse = ""), "\n")
cat("  Phase 0: WebR Package Validation — Standard R Baseline\n")
cat("=" |> rep(70) |> paste(collapse = ""), "\n\n")

# ============================================================================
# Helpers
# ============================================================================

try_load <- function(pkg) {
  start <- proc.time()["elapsed"]
  result <- tryCatch({
    suppressPackageStartupMessages(library(pkg, character.only = TRUE))
    list(status = "loaded",
         time = round(proc.time()["elapsed"] - start, 3),
         error = NA_character_,
         version = as.character(packageVersion(pkg)))
  }, error = function(e) {
    list(status = "failed",
         time = round(proc.time()["elapsed"] - start, 3),
         error = conditionMessage(e),
         version = NA_character_)
  })
  result$package <- pkg
  result
}

try_test <- function(name, expr) {
  start <- proc.time()["elapsed"]
  result <- tryCatch({
    res <- eval(expr)
    list(test = name, status = "pass",
         time = round(proc.time()["elapsed"] - start, 3),
         error = NA_character_)
  }, error = function(e) {
    list(test = name, status = "fail",
         time = round(proc.time()["elapsed"] - start, 3),
         error = conditionMessage(e))
  })
  result
}

status_icon <- function(s) {
  switch(s, "loaded" = "\u2705", "pass" = "\u2705", "failed" = "\u274C", "fail" = "\u274C", "\u26A0\uFE0F")
}

pad_right <- function(s, w) {
  formatC(s, width = w, flag = "-")
}

# ============================================================================
# 1. PACKAGE LOAD TESTS
# ============================================================================

cat("\n--- 1. Package Load Tests ---\n\n")

tiers <- list(
  "Tier 1: Core" = c("gt", "gtsummary", "ggplot2", "broom", "labelled"),
  "Tier 2: Analysis" = c("survival", "sandwich", "lmtest", "car", "emmeans"),
  "Tier 3: File I/O" = c("haven", "readxl"),
  "Tier 4: Advanced" = c("lme4", "ggdag", "writexl")
)

load_results <- list()
for (tier in names(tiers)) {
  cat(sprintf("\n[%s]\n", tier))
  for (pkg in tiers[[tier]]) {
    res <- try_load(pkg)
    res$tier <- tier
    load_results[[pkg]] <- res
    cat(sprintf("  %s %-12s %s  (%.3fs)%s\n",
                status_icon(res$status), pkg,
                if (!is.na(res$version)) paste0("v", res$version) else "      ",
                res$time,
                if (!is.na(res$error)) paste0("  ERROR: ", res$error) else ""))
  }
}

loaded_pkgs <- names(Filter(function(x) x$status == "loaded", load_results))
failed_pkgs <- names(Filter(function(x) x$status == "failed", load_results))
cat(sprintf("\nSummary: %d loaded, %d failed\n", length(loaded_pkgs), length(failed_pkgs)))

# ============================================================================
# 2. FUNCTIONAL SMOKE TESTS
# ============================================================================

cat("\n--- 2. Functional Smoke Tests ---\n\n")

smoke_tests <- list()

# gt
if ("gt" %in% loaded_pkgs) {
  smoke_tests[["gt_render"]] <- try_test(
    "gt: Render mtcars[1:5,] as HTML",
    quote({ gt::gt(mtcars[1:5, ]) |> gt::as_raw_html() })
  )
}

# gtsummary
if ("gtsummary" %in% loaded_pkgs) {
  smoke_tests[["gtsummary_tbl"]] <- try_test(
    "gtsummary: tbl_summary(mtcars[,1:4])",
    quote({ gtsummary::tbl_summary(mtcars[, 1:4]) })
  )
}

# ggplot2
if ("ggplot2" %in% loaded_pkgs) {
  smoke_tests[["ggplot2_scatter"]] <- try_test(
    "ggplot2: Scatter plot mpg vs hp",
    quote({
      p <- ggplot2::ggplot(mtcars, ggplot2::aes(mpg, hp)) + ggplot2::geom_point()
      grDevices::png(tf <- tempfile(fileext = ".png"), width = 600, height = 400)
      print(p)
      grDevices::dev.off()
      stopifnot(file.exists(tf) && file.size(tf) > 0)
    })
  )
}

# broom
if ("broom" %in% loaded_pkgs) {
  smoke_tests[["broom_tidy"]] <- try_test(
    "broom: tidy(lm(mpg ~ hp, mtcars))",
    quote({
      res <- broom::tidy(lm(mpg ~ hp, data = mtcars))
      stopifnot(nrow(res) == 2)
      res
    })
  )
}

# labelled
if ("labelled" %in% loaded_pkgs) {
  smoke_tests[["labelled_labels"]] <- try_test(
    "labelled: Set/get variable labels",
    quote({
      df <- mtcars[, 1:3]
      labelled::var_label(df) <- list(mpg = "Miles per Gallon", cyl = "Cylinders", disp = "Displacement")
      labs <- labelled::var_label(df)
      stopifnot(labs$mpg == "Miles per Gallon")
    })
  )
}

# survival
if ("survival" %in% loaded_pkgs) {
  smoke_tests[["survival_km"]] <- try_test(
    "survival: survfit(Surv(time, status) ~ x, aml)",
    quote({
      data(aml, package = "survival")
      survival::survfit(survival::Surv(time, status) ~ x, data = aml)
    })
  )
}

# sandwich
if ("sandwich" %in% loaded_pkgs) {
  smoke_tests[["sandwich_vcov"]] <- try_test(
    "sandwich: vcovCL(lm, cluster=mtcars$cyl)",
    quote({
      mod <- lm(mpg ~ hp, data = mtcars)
      sandwich::vcovCL(mod, cluster = mtcars$cyl)
    })
  )
}

# lmtest
if (all(c("lmtest", "sandwich") %in% loaded_pkgs)) {
  smoke_tests[["lmtest_coeftest"]] <- try_test(
    "lmtest: coeftest with sandwich vcov",
    quote({
      mod <- lm(mpg ~ hp, data = mtcars)
      lmtest::coeftest(mod, vcov = sandwich::vcovCL(mod, cluster = mtcars$cyl))
    })
  )
}

# car
if ("car" %in% loaded_pkgs) {
  smoke_tests[["car_vif"]] <- try_test(
    "car: VIF for lm(mpg ~ hp + wt + disp)",
    quote({
      mod <- lm(mpg ~ hp + wt + disp, data = mtcars)
      car::vif(mod)
    })
  )
}

# emmeans
if ("emmeans" %in% loaded_pkgs) {
  smoke_tests[["emmeans_test"]] <- try_test(
    "emmeans: Marginal means from ANOVA",
    quote({
      mt2 <- mtcars; mt2$cyl <- factor(mt2$cyl)
      mod <- lm(mpg ~ cyl, data = mt2)
      emmeans::emmeans(mod, "cyl")
    })
  )
}

# haven
if ("haven" %in% loaded_pkgs) {
  smoke_tests[["haven_rw"]] <- try_test(
    "haven: Write/read Stata .dta roundtrip",
    quote({
      tf <- tempfile(fileext = ".dta")
      haven::write_dta(mtcars[1:10, ], tf)
      df <- haven::read_dta(tf)
      stopifnot(nrow(df) == 10)
    })
  )
}

# readxl
if ("readxl" %in% loaded_pkgs) {
  smoke_tests[["readxl_read"]] <- try_test(
    "readxl: Read built-in example xlsx",
    quote({
      path <- readxl::readxl_example("datasets.xlsx")
      df <- readxl::read_xlsx(path)
      stopifnot(nrow(df) > 0)
    })
  )
}

# lme4
if ("lme4" %in% loaded_pkgs) {
  smoke_tests[["lme4_model"]] <- try_test(
    "lme4: lmer(Reaction ~ Days + (1|Subject), sleepstudy)",
    quote({
      data(sleepstudy, package = "lme4")
      lme4::lmer(Reaction ~ Days + (1 | Subject), data = sleepstudy)
    })
  )
}

# ggdag
if ("ggdag" %in% loaded_pkgs) {
  smoke_tests[["ggdag_dag"]] <- try_test(
    "ggdag: dagify(y ~ x + z, x ~ z) + ggdag()",
    quote({
      dag <- ggdag::dagify(y ~ x + z, x ~ z)
      p <- ggdag::ggdag(dag)
      grDevices::png(tf <- tempfile(fileext = ".png"), width = 600, height = 400)
      print(p)
      grDevices::dev.off()
      stopifnot(file.exists(tf))
    })
  )
}

# writexl
if ("writexl" %in% loaded_pkgs) {
  smoke_tests[["writexl_write"]] <- try_test(
    "writexl: write_xlsx(mtcars, ...)",
    quote({
      tf <- tempfile(fileext = ".xlsx")
      writexl::write_xlsx(mtcars, tf)
      stopifnot(file.exists(tf) && file.size(tf) > 0)
    })
  )
}

for (name in names(smoke_tests)) {
  res <- smoke_tests[[name]]
  cat(sprintf("  %s %-50s (%.3fs)%s\n",
              status_icon(res$status), res$test, res$time,
              if (!is.na(res$error)) paste0("  ERROR: ", res$error) else ""))
}

pass_count <- sum(sapply(smoke_tests, function(x) x$status == "pass"))
fail_count <- sum(sapply(smoke_tests, function(x) x$status == "fail"))
cat(sprintf("\nSummary: %d pass, %d fail\n", pass_count, fail_count))

# ============================================================================
# 3. PERFORMANCE BENCHMARKS
# ============================================================================

cat("\n--- 3. Performance Benchmarks ---\n\n")

# Load benchmark data
bench_file <- "data/benchmark_5k.csv"
if (!file.exists(bench_file)) {
  cat("Generating benchmark dataset...\n")
  source("generate_test_data.R")
}

benchmarks <- list()

# CSV parse
benchmarks[["csv_parse"]] <- try_test(
  "CSV parse (~500KB, 5000 rows)",
  quote({
    d <- read.csv(bench_file)
    stopifnot(nrow(d) >= 5000)
    d
  })
)
# Make data available for subsequent benchmarks
d <- tryCatch(read.csv(bench_file), error = function(e) NULL)

if (!is.null(d)) {

  # Table 1 basic
  if ("gtsummary" %in% loaded_pkgs) {
    benchmarks[["table1_10var"]] <- try_test(
      "Table 1: gtsummary (10 vars, stratified)",
      quote({
        gtsummary::tbl_summary(
          d[, c("age", "sex", "bmi", "smoking", "bp_systolic",
                 "cholesterol", "glucose", "treatment", "outcome_binary", "category")],
          by = "treatment"
        )
      })
    )
  }

  # Table 1 stress test (all columns)
  if ("gtsummary" %in% loaded_pkgs) {
    benchmarks[["table1_all_cols"]] <- try_test(
      "Table 1: gtsummary (all 18 vars, stress test)",
      quote({
        cols <- setdiff(names(d), c("id", "treatment"))
        gtsummary::tbl_summary(d[, c(cols, "treatment")], by = "treatment")
      })
    )
  }

  # High-cardinality categorical
  if ("gtsummary" %in% loaded_pkgs) {
    benchmarks[["table1_highcard"]] <- try_test(
      "Table 1: High-cardinality categorical (25 levels)",
      quote({
        d2 <- d
        d2$high_card <- sample(paste0("Level_", sprintf("%02d", 1:25)), nrow(d2), replace = TRUE)
        gtsummary::tbl_summary(d2[, c("high_card", "treatment")], by = "treatment")
      })
    )
  }

  # Linear regression
  benchmarks[["lm_5pred"]] <- try_test(
    "Linear regression: 5 predictors (n=5000)",
    quote({
      mod <- lm(outcome_continuous ~ age + bmi + bp_systolic + cholesterol + glucose, data = d)
      summary(mod)
    })
  )

  # Logistic regression
  benchmarks[["glm_5pred"]] <- try_test(
    "Logistic regression: 5 predictors (n=5000)",
    quote({
      mod <- glm(outcome_binary ~ age + bmi + bp_systolic + cholesterol + glucose,
                  data = d, family = binomial)
      summary(mod)
    })
  )

  # Survival analysis
  if ("survival" %in% loaded_pkgs) {
    benchmarks[["cox_5pred"]] <- try_test(
      "Cox regression: 5 predictors (n=5000)",
      quote({
        survival::coxph(
          survival::Surv(time_to_event, event_status) ~ age + bmi + bp_systolic + treatment + sex,
          data = d)
      })
    )
  }

  # Mixed model
  if ("lme4" %in% loaded_pkgs) {
    benchmarks[["lmer_model"]] <- try_test(
      "Mixed model: Random intercept (n=5000, 10 sites)",
      quote({
        lme4::lmer(outcome_continuous ~ age + bmi + treatment + (1 | site), data = d)
      })
    )
  }

  # Cluster-robust SEs
  if (all(c("sandwich", "lmtest") %in% loaded_pkgs)) {
    benchmarks[["cluster_se"]] <- try_test(
      "Cluster-robust SEs: sandwich vcovCL (10 clusters)",
      quote({
        mod <- lm(outcome_continuous ~ age + bmi + treatment, data = d)
        lmtest::coeftest(mod, vcov = sandwich::vcovCL(mod, cluster = d$site))
      })
    )
  }

  # gt render
  if (all(c("gt", "gtsummary") %in% loaded_pkgs)) {
    benchmarks[["gt_render_html"]] <- try_test(
      "gt: Render Table 1 as HTML",
      quote({
        tbl <- gtsummary::tbl_summary(
          d[, c("age", "sex", "bmi", "smoking", "treatment")], by = "treatment")
        gt_tbl <- gtsummary::as_gt(tbl)
        gt::as_raw_html(gt_tbl)
      })
    )
  }

  # ggplot2 with large dataset
  if ("ggplot2" %in% loaded_pkgs) {
    benchmarks[["ggplot_5k"]] <- try_test(
      "ggplot2: Scatter 5000 pts + smoothing + 300 DPI",
      quote({
        p <- ggplot2::ggplot(d, ggplot2::aes(age, outcome_continuous, color = treatment)) +
          ggplot2::geom_point(alpha = 0.3) +
          ggplot2::geom_smooth(method = "lm") +
          ggplot2::theme_classic()
        grDevices::png(tf <- tempfile(fileext = ".png"), width = 8, height = 6, units = "in", res = 300)
        print(p)
        grDevices::dev.off()
        stopifnot(file.exists(tf))
      })
    )
  }

}

for (name in names(benchmarks)) {
  res <- benchmarks[[name]]
  time_flag <- if (res$time > 30) " [SLOW >30s]" else if (res$time > 10) " [WARN >10s]" else ""
  cat(sprintf("  %s %-50s %7.3fs%s%s\n",
              status_icon(res$status), res$test, res$time, time_flag,
              if (!is.na(res$error)) paste0("  ERROR: ", res$error) else ""))
}

# ============================================================================
# 4. GENERATE VALIDATION REPORT
# ============================================================================

cat("\n--- Generating Validation Report ---\n\n")

lines <- c(
  "# Phase 0: WebR Package Validation Report",
  "",
  "**Hard gate: No application code should be written until this document is complete.**",
  "",
  "## Environment",
  "",
  sprintf("- **Date:** %s", Sys.Date()),
  sprintf("- **R Version:** %s", R.version.string),
  sprintf("- **Platform:** %s (%s)", R.version$platform, .Platform$OS.type),
  sprintf("- **Test context:** Standard R (baseline — WebR browser tests to follow)"),
  ""
)

# --- Load Results Table ---
lines <- c(lines,
  "## 1. Package Load Results",
  "",
  "| Package | Tier | Status | Version | Load Time (s) | Error |",
  "|---------|------|--------|---------|---------------|-------|"
)
for (pkg in unlist(tiers)) {
  r <- load_results[[pkg]]
  lines <- c(lines, sprintf(
    "| %s | %s | %s %s | %s | %.3f | %s |",
    pkg, r$tier, status_icon(r$status), r$status,
    if (!is.na(r$version)) r$version else "—",
    r$time,
    if (!is.na(r$error)) r$error else "—"
  ))
}
lines <- c(lines, "")

# --- Smoke Test Results ---
lines <- c(lines,
  "## 2. Functional Smoke Test Results",
  "",
  "| Test | Status | Time (s) | Error |",
  "|------|--------|----------|-------|"
)
for (name in names(smoke_tests)) {
  r <- smoke_tests[[name]]
  lines <- c(lines, sprintf(
    "| %s | %s %s | %.3f | %s |",
    r$test, status_icon(r$status), r$status, r$time,
    if (!is.na(r$error)) r$error else "—"
  ))
}
lines <- c(lines, "")

# --- Benchmark Timings ---
lines <- c(lines,
  "## 3. Performance Benchmark Results",
  "",
  "| Benchmark | Status | Time (s) | Performance Flag |",
  "|-----------|--------|----------|------------------|"
)
for (name in names(benchmarks)) {
  r <- benchmarks[[name]]
  flag <- if (r$time > 30) "SLOW (>30s) — consider disabling" else
          if (r$time > 10) "WARNING (>10s) — add perf warning" else "OK"
  lines <- c(lines, sprintf(
    "| %s | %s %s | %.3f | %s |",
    r$test, status_icon(r$status), r$status, r$time, flag
  ))
}
lines <- c(lines,
  "",
  "**Note:** These are standard R timings. WebR (browser) timings are expected to be",
  "2-10x slower. Benchmarks must be re-run in the Shinylive test app.",
  ""
)

# --- Safari Memory Test ---
lines <- c(lines,
  "## 4. Safari Memory Test",
  "",
  "| Test | Result | Notes |",
  "|------|--------|-------|",
  "| Full workflow — first run | _pending browser test_ | — |",
  "| Full workflow — second run (no reload) | _pending browser test_ | — |",
  "| Memory usage after workflow 1 (Chrome DevTools) | _pending browser test_ | — |",
  "| Memory usage after workflow 2 (Chrome DevTools) | _pending browser test_ | — |",
  "| Safari crash or excessive slowdown? | _pending browser test_ | — |",
  ""
)

# --- Decision Log ---
lines <- c(lines,
  "## 5. Decision Log",
  "",
  "| Package | Decision | Rationale |",
  "|---------|----------|-----------|"
)

for (pkg in unlist(tiers)) {
  r <- load_results[[pkg]]

  # Find related benchmark if exists
  bench_time <- NA
  for (bname in names(benchmarks)) {
    if (grepl(tolower(pkg), tolower(bname), fixed = TRUE) ||
        grepl(tolower(pkg), tolower(benchmarks[[bname]]$test), fixed = TRUE)) {
      if (benchmarks[[bname]]$status == "pass") {
        bench_time <- benchmarks[[bname]]$time
      }
    }
  }

  if (r$status == "failed") {
    decision <- "EXCLUDE"
    rationale <- sprintf("Failed to load: %s. Use fallback.", r$error)
  } else {
    # Check smoke tests
    smoke_ok <- TRUE
    for (sname in names(smoke_tests)) {
      if (grepl(tolower(pkg), tolower(sname), fixed = TRUE) ||
          grepl(tolower(pkg), tolower(smoke_tests[[sname]]$test), fixed = TRUE)) {
        if (smoke_tests[[sname]]$status == "fail") smoke_ok <- FALSE
      }
    }

    if (!smoke_ok) {
      decision <- "PARTIAL"
      rationale <- "Loaded but smoke test(s) failed. Document limitations."
    } else if (pkg == "lme4") {
      # Special lme4 policy
      if (!is.na(bench_time) && bench_time > 30) {
        decision <- "EXCLUDE"
        rationale <- sprintf("Loaded but benchmark >30s (%.1fs). Disable per lme4 policy.", bench_time)
      } else {
        decision <- "EXPERIMENTAL"
        rationale <- sprintf("Loaded (%.3fs). Mark experimental per lme4 policy. Re-verify in WebR.", r$time)
      }
    } else if (!is.na(bench_time) && bench_time > 10) {
      decision <- "INCLUDE (with warning)"
      rationale <- sprintf("Loaded but benchmark >10s (%.1fs). Include with performance warning.", bench_time)
    } else {
      decision <- "INCLUDE"
      rationale <- sprintf("Loaded and passed smoke tests (%.3fs).", r$time)
    }
  }

  lines <- c(lines, sprintf("| %s | **%s** | %s |", pkg, decision, rationale))
}

lines <- c(lines, "")

# --- Confirmed v1 Package List ---
lines <- c(lines,
  "## 6. Confirmed v1 Package List",
  "",
  "### Confirmed (Include)",
  ""
)

for (pkg in unlist(tiers)) {
  r <- load_results[[pkg]]
  if (r$status == "loaded") {
    lines <- c(lines, sprintf("- `%s` v%s", pkg, r$version))
  }
}

if (length(failed_pkgs) > 0) {
  lines <- c(lines, "", "### Excluded (Use Fallbacks)", "")
  for (pkg in failed_pkgs) {
    r <- load_results[[pkg]]
    fallback <- switch(pkg,
      "haven" = "CSV-only input for v1",
      "readxl" = "CSV-only input for v1",
      "lme4" = "Drop mixed models from v1",
      "ggdag" = "Pure ggplot2 manual DAG rendering",
      "writexl" = "CSV + HTML export",
      "car" = "Manual VIF/Levene implementation in base R",
      "sandwich" = "Note clustering only, no SE adjustment",
      "lmtest" = "Note clustering only, no SE adjustment",
      "Use alternative approach"
    )
    lines <- c(lines, sprintf("- ~~`%s`~~ — Fallback: %s", pkg, fallback))
  }
}

lines <- c(lines,
  "",
  "### Experimental",
  ""
)

if ("lme4" %in% loaded_pkgs) {
  lines <- c(lines, "- `lme4` — Label as experimental in UI; disable if WebR benchmark >30s")
}

lines <- c(lines,
  "",
  "---",
  "",
  "## Next Steps",
  "",
  "1. [ ] Deploy test_app.R as Shinylive app",
  "2. [ ] Re-run all tests in Chrome",
  "3. [ ] Re-run all tests in Safari",
  "4. [ ] Complete Safari memory test (repeated workflow)",
  "5. [ ] Record browser-specific timings in this document",
  "6. [ ] Finalize decision log with WebR results",
  "7. [ ] Gate review: proceed to Phase 1 only after all checks pass",
  ""
)

writeLines(lines, output_file)
cat(sprintf("Report written to: %s\n", output_file))
cat(sprintf("File size: %.1f KB\n", file.size(output_file) / 1024))
cat("\nPhase 0 standard R validation complete.\n")
cat("Next: Deploy test_app.R as Shinylive and re-run in browser.\n")
