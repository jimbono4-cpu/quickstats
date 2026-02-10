# Phase 0: WebR Package Validation Results

**Date:** 2026-02-07
**R Version:** R version 4.5.2 (2025-10-31 ucrt)
**Platform:** windows

## 1. Package Load Results

| Package | Tier | Status | Version | Load Time (s) | Error |
|---------|------|--------|---------|---------------|-------|
| gt | Tier 1: Core (must pass) | ✅ loaded | 1.3.0 | 0.00 | — |
| gtsummary | Tier 1: Core (must pass) | ✅ loaded | 2.5.0 | 0.00 | — |
| ggplot2 | Tier 1: Core (must pass) | ✅ loaded | 4.0.1 | 0.00 | — |
| broom | Tier 1: Core (must pass) | ✅ loaded | 1.0.12 | 0.00 | — |
| labelled | Tier 1: Core (must pass) | ✅ loaded | 2.16.0 | 0.00 | — |
| survival | Tier 2: Analysis (must pass for MVP) | ✅ loaded | 3.8.3 | 0.00 | — |
| sandwich | Tier 2: Analysis (must pass for MVP) | ✅ loaded | 3.1.1 | 0.00 | — |
| lmtest | Tier 2: Analysis (must pass for MVP) | ✅ loaded | 0.9.40 | 0.00 | — |
| car | Tier 2: Analysis (must pass for MVP) | ✅ loaded | 3.1.5 | 0.00 | — |
| emmeans | Tier 2: Analysis (must pass for MVP) | ✅ loaded | 2.0.1 | 0.00 | — |
| haven | Tier 3: File I/O (have fallbacks) | ✅ loaded | 2.5.5 | 0.00 | — |
| readxl | Tier 3: File I/O (have fallbacks) | ✅ loaded | 1.4.5 | 0.00 | — |
| lme4 | Tier 4: Advanced (nice to have) | ✅ loaded | 1.1.38 | 0.00 | — |
| ggdag | Tier 4: Advanced (nice to have) | ✅ loaded | 0.2.13 | 0.00 | — |
| writexl | Tier 4: Advanced (nice to have) | ✅ loaded | 1.5.4 | 0.00 | — |

## 2. Functional Smoke Test Results

| Test | Status | Time (s) | Error |
|------|--------|----------|-------|
| gt: Render mtcars as HTML table | ✅ pass | 0.14 | — |
| gtsummary: tbl_summary on mtcars | ✅ pass | 0.68 | — |
| ggplot2: Scatter plot (mpg vs hp) | ✅ pass | 0.09 | — |
| broom: Tidy lm(mpg ~ hp, mtcars) | ✅ pass | 0.00 | — |
| labelled: Set/get variable labels | ✅ pass | 0.00 | — |
| survival: survfit(Surv(time, status) ~ x, aml) | ✅ pass | 0.02 | — |
| sandwich: vcovCL with clustering | ✅ pass | 0.00 | — |
| lmtest: coeftest with sandwich vcov | ✅ pass | 0.00 | — |
| car: VIF for multivariate model | ✅ pass | 0.00 | — |
| emmeans: Marginal means from ANOVA | ✅ pass | 0.00 | — |
| haven: Create and read Stata file | ✅ pass | 0.01 | — |
| readxl: Read built-in example xlsx | ✅ pass | 0.02 | — |
| lme4: lmer(Reaction ~ Days + (1|Subject), sleepstudy) | ✅ pass | 0.01 | — |
| ggdag: dagify(y ~ x) and ggdag() | ✅ pass | 0.16 | — |
| writexl: write_xlsx(mtcars, ...) | ✅ pass | 0.01 | — |

## 3. Performance Benchmark Results

| Benchmark | Status | Time (s) | Notes |
|-----------|--------|----------|-------|
| CSV parse (write then read ~1MB) | ❌ fail | 0.00 | object 'd' not found |
| Table 1: gtsummary tbl_summary (basic, ~10 vars) | ❌ fail | 0.00 | object 'd' not found |
| Table 1: gtsummary tbl_summary (all columns, stress test) | ❌ fail | 0.00 | object 'd' not found |
| Table 1: High-cardinality categorical (20+ levels) | ❌ fail | 0.00 | object 'd' not found |
| Linear regression: lm with 5 predictors (n=5000) | ❌ fail | 0.00 | object 'd' not found |
| Logistic regression: glm with 5 predictors (n=5000) | ❌ fail | 0.00 | object 'd' not found |
| Cox regression: survival with 5 predictors (n=5000) | ❌ fail | 0.01 | object 'd' not found |
| Mixed model: lmer with random intercept (n=5000, 10 sites) | ❌ fail | 0.00 | bad 'data': object 'd' not found |
| Cluster-robust SEs: sandwich vcovCL (n=5000, 10 clusters) | ❌ fail | 0.00 | object 'd' not found |
| gt: Render summary table as HTML (large) | ❌ fail | 0.00 | object 'd' not found |
| ggplot2: Scatter with 5000 points + smoothing | ❌ fail | 0.00 | object 'd' not found |

## 4. Decision Matrix

| Package | Decision | Rationale |
|---------|----------|-----------|
| gt | INCLUDE | Loaded and passed smoke tests |
| gtsummary | INCLUDE | Loaded and passed smoke tests |
| ggplot2 | INCLUDE | Loaded and passed smoke tests |
| broom | INCLUDE | Loaded and passed smoke tests |
| labelled | INCLUDE | Loaded and passed smoke tests |
| survival | INCLUDE | Loaded and passed smoke tests |
| sandwich | INCLUDE | Loaded and passed smoke tests |
| lmtest | INCLUDE | Loaded and passed smoke tests |
| car | INCLUDE | Loaded and passed smoke tests |
| emmeans | INCLUDE | Loaded and passed smoke tests |
| haven | INCLUDE | Loaded and passed smoke tests |
| readxl | INCLUDE | Loaded and passed smoke tests |
| lme4 | INCLUDE | Loaded and passed smoke tests |
| ggdag | INCLUDE | Loaded and passed smoke tests |
| writexl | INCLUDE | Loaded and passed smoke tests |

## 5. Safari Memory Test

_To be completed manually in Safari browser._

## 6. Confirmed v1 Package List

_To be finalized after browser testing._

