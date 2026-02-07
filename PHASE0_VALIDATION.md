# Phase 0: WebR Package Validation Report

**Hard gate: No application code should be written until this document is complete.**

## Environment

- **Date:** 2026-02-07
- **R Version:** R version 4.3.3 (2024-02-29)
- **Platform:** x86_64-pc-linux-gnu (unix)
- **Test context:** Standard R (baseline) + WebR/Shinylive (browser)

## 1. Package Load Results

| Package | Tier | Status | Version | Load Time (s) | Error |
|---------|------|--------|---------|---------------|-------|
| gt | Tier 1: Core | PASS | 0.11.1 | 0.600 | — |
| gtsummary | Tier 1: Core | PASS | 2.5.0.9001 | 0.028 | — |
| ggplot2 | Tier 1: Core | PASS | 3.4.4 | 0.278 | — |
| broom | Tier 1: Core | PASS | 1.0.5 | 0.140 | — |
| labelled | Tier 1: Core | PASS | 2.12.0 | 0.105 | — |
| survival | Tier 2: Analysis | PASS | 3.5.8 | 1.014 | — |
| sandwich | Tier 2: Analysis | PASS | 3.1.0 | 0.040 | — |
| lmtest | Tier 2: Analysis | PASS | 0.9.40 | 0.037 | — |
| car | Tier 2: Analysis | PASS | 3.1.2 | 0.051 | — |
| emmeans | Tier 2: Analysis | PASS | 1.10.0 | 0.175 | — |
| haven | Tier 3: File I/O | PASS | 2.5.4 | 0.009 | — |
| readxl | Tier 3: File I/O | PASS | 1.4.3 | 0.028 | — |
| lme4 | Tier 4: Advanced | PASS | 1.1.35.1 | 0.564 | — |
| ggdag | Tier 4: Advanced | FAIL | — | 0.001 | Not installable in test env; deep dependency chain (dagitty, ggraph, tidygraph). WebR availability TBD. |
| writexl | Tier 4: Advanced | PASS | 1.5.0 | 0.026 | — |

**Summary: 14/15 loaded, 1 failed (ggdag — Tier 4, has fallback)**

## 2. Functional Smoke Test Results

| Test | Status | Time (s) | Error |
|------|--------|----------|-------|
| gt: Render mtcars[1:5,] as raw HTML | PASS | 0.567 | — |
| gtsummary: tbl_summary(mtcars[,1:4]) | PASS | 0.915 | — |
| ggplot2: Scatter plot (mpg vs hp) + PNG export | PASS | 0.315 | — |
| broom: tidy(lm(mpg ~ hp, mtcars)) | PASS | 0.110 | — |
| labelled: Set/get variable labels roundtrip | PASS | 0.001 | — |
| survival: survfit(Surv(time, status) ~ x, aml) | PASS | 0.205 | — |
| sandwich: vcovCL(lm, cluster=mtcars$cyl) | PASS | 0.003 | — |
| lmtest: coeftest with sandwich robust vcov | PASS | 0.002 | — |
| car: VIF for lm(mpg ~ hp + wt + disp) | PASS | 0.005 | — |
| emmeans: Marginal means from one-way ANOVA | PASS | 0.028 | — |
| haven: Write/read Stata .dta roundtrip | PASS | 0.066 | — |
| readxl: Read built-in example .xlsx | PASS | 0.046 | — |
| lme4: lmer(Reaction ~ Days + (1\|Subject), sleepstudy) | PASS | 0.057 | — |
| writexl: write_xlsx(mtcars, tempfile) | PASS | 0.010 | — |

**Summary: 14/14 smoke tests pass (ggdag skipped — not installed)**

## 3. Performance Benchmark Results

Benchmark dataset: synthetic 5,000 rows x 20 columns (~486 KB CSV).

| Benchmark | Status | Time (s) | Performance Flag |
|-----------|--------|----------|------------------|
| CSV parse (~500KB, 5000 rows) | PASS | 0.030 | OK |
| Table 1: gtsummary (10 vars, stratified by treatment) | PASS | 1.650 | OK |
| Table 1: gtsummary (all 18 vars, stress test) | PASS | 2.471 | OK |
| Table 1: High-cardinality categorical (25 levels) | PASS | 0.854 | OK |
| Linear regression: 5 predictors (n=5000) | PASS | 0.003 | OK |
| Logistic regression: 5 predictors (n=5000) | PASS | 0.010 | OK |
| Cox regression: 5 predictors (n=5000) | PASS | 0.021 | OK |
| Mixed model: Random intercept (n=5000, 10 sites) | PASS | 0.046 | OK |
| Cluster-robust SEs: sandwich vcovCL (10 clusters) | PASS | 0.006 | OK |
| gt: Render Table 1 as HTML | PASS | 1.500 | OK |
| ggplot2: Scatter 5000 pts + lm smooth + 300 DPI PNG | PASS | 1.049 | OK |

**All benchmarks pass in standard R. No operation exceeds 3 seconds.**

**WebR multiplier note:** WebR (browser WASM) timings are expected to be 2-10x slower than standard R. The heaviest operations (gtsummary Table 1 at 2.5s) may take 5-25s in WebR. These must be re-validated in browser via the Shinylive test app (`test_app.R`). The 30s threshold for lme4 disablement applies to WebR timings, not standard R.

## 4. Safari Memory Test

| Test | Result | Notes |
|------|--------|-------|
| Full workflow — first run | _pending browser test_ | — |
| Full workflow — second run (no reload) | _pending browser test_ | — |
| Memory usage after workflow 1 (Chrome DevTools) | _pending browser test_ | — |
| Memory usage after workflow 2 (Chrome DevTools) | _pending browser test_ | — |
| Safari crash or excessive slowdown? | _pending browser test_ | — |

_These tests require deploying the Shinylive test app and running manually in each browser._

## 5. Decision Log

| Package | Decision | Rationale |
|---------|----------|-----------|
| gt | **INCLUDE** | Tier 1 core. Loaded (0.6s), smoke test pass, renders HTML tables correctly. |
| gtsummary | **INCLUDE** | Tier 1 core. Loaded (0.03s), Table 1 generation works, stress test passes at 2.5s. |
| ggplot2 | **INCLUDE** | Tier 1 core. Loaded (0.3s), scatter + PNG export verified, 300 DPI output works. |
| broom | **INCLUDE** | Tier 1 core. Loaded (0.1s), tidy() produces correct 2-row tibble for simple lm. |
| labelled | **INCLUDE** | Tier 1 core. Loaded (0.1s), set/get variable labels roundtrip verified. |
| survival | **INCLUDE** | Tier 2 MVP. Loaded (1.0s), survfit and Surv work correctly on aml dataset. |
| sandwich | **INCLUDE** | Tier 2 MVP. Loaded (0.04s), vcovCL cluster-robust SEs computed correctly. |
| lmtest | **INCLUDE** | Tier 2 MVP. Loaded (0.04s), coeftest with robust vcov works with sandwich. |
| car | **INCLUDE** | Tier 2 MVP. Loaded (0.05s), VIF computation verified on 3-predictor model. |
| emmeans | **INCLUDE** | Tier 2 MVP. Loaded (0.2s), marginal means from one-way ANOVA computed. |
| haven | **INCLUDE** | Tier 3 I/O. Loaded (0.01s), write_dta/read_dta roundtrip preserves data. |
| readxl | **INCLUDE** | Tier 3 I/O. Loaded (0.03s), reads built-in .xlsx example successfully. |
| lme4 | **EXPERIMENTAL** | Tier 4. Loaded (0.6s), simple random-intercept model fits in 0.05s. Mark as experimental in UI per lme4 policy. Disable if WebR benchmark exceeds 30s. |
| ggdag | **EXCLUDE** | Tier 4. Failed to install due to deep dependency chain (dagitty, ggraph, tidygraph, graphlayouts). **Fallback: Pure ggplot2 manual DAG rendering** with geom_segment() arrows and geom_point() nodes. Core DAG variable classification (confounder/mediator/collider) is unaffected — only the auto-rendered diagram changes. |
| writexl | **INCLUDE** | Tier 4. Loaded (0.03s), writes valid .xlsx file. |

## 6. Confirmed v1 Package List

### Confirmed (Include)

- `gt` v0.11.1 — Primary table rendering
- `gtsummary` v2.5.0.9001 — Table 1 and summary statistics
- `ggplot2` v3.4.4 — All plotting
- `broom` v1.0.5 — Model tidying
- `labelled` v2.12.0 — Variable label handling
- `survival` v3.5.8 — Kaplan-Meier and Cox regression
- `sandwich` v3.1.0 — Cluster-robust standard errors
- `lmtest` v0.9.40 — Coefficient testing with robust vcov
- `car` v3.1.2 — VIF and diagnostics
- `emmeans` v1.10.0 — Estimated marginal means
- `haven` v2.5.4 — Stata/SPSS file import
- `readxl` v1.4.3 — Excel file import
- `writexl` v1.5.0 — Excel file export

### Experimental

- `lme4` v1.1.35.1 — Mixed models. Label as "Experimental" in UI. Disable entirely if WebR benchmark >30s for simple random-intercept model.

### Excluded (Use Fallbacks)

- ~~`ggdag`~~ — **Fallback:** Pure ggplot2 manual DAG rendering using `geom_segment()` + `geom_point()` + `geom_text()`. The causal reasoning module (Step 4) is unaffected — users still classify variables as confounder/mediator/collider/etc. Only the auto-generated DAG diagram uses a simpler rendering approach.

---

## WebR/Shinylive Browser Validation (Run 1 — 2026-02-07)

**Browser:** Windows laptop, R 4.5.2 (ucrt), WebR/Shinylive
**Issue:** Initial test app did NOT auto-install packages via `webr::install()`. Packages that were not pre-bundled in the Shinylive runtime failed with "no package called X".

### Browser Package Load Results

| Package | Tier | Browser Status | Version | Notes |
|---------|------|---------------|---------|-------|
| gt | Tier 1: Core | FAIL | — | Not pre-installed in WebR runtime |
| gtsummary | Tier 1: Core | FAIL | — | Not pre-installed in WebR runtime |
| ggplot2 | Tier 1: Core | PASS | 4.0.1 | Pre-installed |
| broom | Tier 1: Core | PASS | 1.0.12 | Pre-installed |
| labelled | Tier 1: Core | FAIL | — | Not pre-installed in WebR runtime |
| survival | Tier 2: Analysis | PASS | 3.8.3 | Pre-installed |
| sandwich | Tier 2: Analysis | PASS | 3.1.1 | Pre-installed |
| lmtest | Tier 2: Analysis | PASS | 0.9.40 | Pre-installed |
| car | Tier 2: Analysis | FAIL | — | Not pre-installed in WebR runtime |
| emmeans | Tier 2: Analysis | FAIL | — | Not pre-installed in WebR runtime |
| haven | Tier 3: File I/O | PASS | 2.5.5 | Pre-installed |
| readxl | Tier 3: File I/O | PASS | 1.4.5 | Pre-installed |
| lme4 | Tier 4: Advanced | PASS | 1.1.38 | Pre-installed |
| ggdag | Tier 4: Advanced | FAIL | — | Expected — deep dependency chain |
| writexl | Tier 4: Advanced | PASS | 1.5.4 | Pre-installed |

**Browser result: 9/15 loaded, 6/15 failed**

### Root Cause Analysis

The Shinylive web component did NOT automatically detect and install packages from the WebR WASM repository. The initial `test_app.R` used `library()` directly without first calling `webr::install()`. Packages that happened to be bundled with the base Shinylive runtime loaded fine; others failed.

### Fix Applied (v2 of test app)

Updated `app.R` to:
1. Detect WebR environment via `is_webr()` helper
2. Call `webr::install(pkg, quiet = TRUE)` before `library()` for each package
3. Fixed benchmark scoping bug: changed `quote({...})` to `function() {...}` closures so benchmark data `d` is properly captured
4. Auto-generates 5000-row synthetic benchmark dataset (no file upload needed)

### Pending: Browser Re-validation (Run 2)

The updated test app must be re-run in the browser to determine which packages have WASM binaries in the WebR repo. Possible outcomes:

| Package | Expected after fix | If still fails |
|---------|-------------------|----------------|
| gt | Likely PASS — available in WebR repo | Use manual HTML tables via `htmltools` |
| gtsummary | Likely PASS — depends on gt, cards | Build Table 1 manually with base R |
| labelled | Likely PASS — pure R, no compiled code | Use base R `attr()` for variable labels |
| car | Likely PASS — available in WebR repo | Manual VIF implementation in base R |
| emmeans | Likely PASS — available in WebR repo | Manual marginal means via `predict()` |
| ggdag | Likely FAIL — deep dep chain | Use ggplot2 fallback (already decided) |

### Browser Test Checklist

- [x] Chrome (desktop): Run 1 complete (9/15 loaded, benchmarks blocked by bugs)
- [ ] Chrome (desktop): Run 2 with fixed test app (pending)
- [ ] Firefox (desktop): Run all 3 test suites
- [ ] Safari (Mac): Run all 3 test suites
- [ ] Safari memory test: Run full workflow twice without reload
- [ ] Edge (desktop): Run all 3 test suites
- [ ] Verify no network requests after initial load (DevTools Network tab)
- [ ] Record peak memory usage (Chrome DevTools Performance tab)

---

## Gate Decision

**Standard R baseline: PASS (14/15 packages, all smoke tests, all benchmarks <3s)**

**Browser Run 1: PARTIAL (9/15 loaded — test app bug, not WebR limitation)**

The initial browser failure was caused by missing `webr::install()` calls, not by WebR package unavailability. The test app has been fixed. A second browser run is needed to confirm the true WebR package availability.

**Action:** Re-run the updated test app in browser. If gt, gtsummary, labelled, car, and emmeans load after the fix, the gate is PASS and Phase 1 can proceed with the full confirmed package list. If any remain unavailable, activate their fallbacks per the table above.
