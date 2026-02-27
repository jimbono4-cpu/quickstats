# StatsBrowser — In-Browser Statistical Analysis

**A complete statistical analysis tool that runs entirely in your browser. No installation. No server. Your data never leaves your device.**

> **[Launch the app](https://jimbono4-cpu.github.io/Drafts-06.02.2026/)**

---

## Why StatsBrowser?

Most statistical tools require installing software, uploading data to a server, or writing code. StatsBrowser eliminates all three barriers — just open a link and start analysing.

- **100% private** — powered by [WebR](https://docs.r-wasm.org/webr/latest/), the R language compiled to WebAssembly. All computation happens on your machine; nothing is sent to a server.
- **No installation** — works in any modern browser (Chrome, Firefox, Edge, Safari). No R, no Python, no setup.
- **Publication-ready output** — generates APA-style tables and plots you can paste directly into Word, Google Docs, or LaTeX.

---

## Features

### Step 1 — Upload Data
Import your dataset in **CSV, Excel (.xlsx), Stata (.dta), SPSS (.sav), or SAS (.sas7bdat)** format. Variable labels from Stata/SPSS files are automatically preserved.

### Step 2 — Explore Data
- View variable distributions with histograms and bar charts
- Inspect missing data patterns
- Set variable types (continuous, categorical) and apply labels
- Normality testing (Shapiro-Wilk)

### Step 3 — Table 1 (Descriptive Statistics)
- Generate a publication-quality **Table 1** stratified by any grouping variable
- Continuous variables: mean (SD) or median (IQR) based on normality
- Categorical variables: n (%)
- Optional overall column with sample sizes — e.g. `Overall (N=500)`
- Chi-squared and t-test / Wilcoxon / ANOVA / Kruskal-Wallis p-values
- Copy to clipboard for direct pasting into Word

### Step 4 — Regression Models
| Model | Use case |
|-------|----------|
| **Linear regression** | Continuous outcomes |
| **Logistic regression** | Binary outcomes (odds ratios) |
| **Cox regression** | Time-to-event / survival data (hazard ratios) |
| **Mixed model** | Clustered / hierarchical data with random effects |

Additional features:
- **Cluster-robust standard errors** (sandwich estimator)
- **VIF** for multicollinearity diagnostics
- **Marginal means** (emmeans) for factor predictors
- **Forest plots** of coefficients, odds ratios, or hazard ratios
- **Diagnostic plots** — residuals vs fitted, Q-Q plots, Cook's distance
- **Exposure vs outcome** scatter plots with regression lines
- Automatic confidence intervals and exponentiation for logistic/Cox models
- Complete-case analysis with transparent missing data reporting

### Step 5 — Results & Export
- Review all tables and plots in one place
- **Copy to clipboard** — paste tables directly into Word or Google Docs as formatted HTML
- **Download as Word** (.doc) — complete report with tables and embedded plots
- **AI analysis prompt** — auto-generated prompt summarising your analysis for use with ChatGPT, Claude, or other LLMs

---

## Quick Start

1. **Open** the app: [jimbono4-cpu.github.io/Drafts-06.02.2026](https://jimbono4-cpu.github.io/Drafts-06.02.2026/)
2. **Wait** for R to load in your browser (~30–45 seconds on first visit, faster on reload)
3. **Upload** a CSV or Excel file
4. **Analyse** — follow the 5-step wizard from exploration to export

---

## Technical Details

| | |
|---|---|
| **Runtime** | [Shinylive](https://shinylive.io/) + [WebR](https://docs.r-wasm.org/webr/latest/) (R 4.x compiled to WebAssembly) |
| **Hosting** | GitHub Pages — static files only, no backend |
| **Privacy** | Zero data transmission. All processing is client-side. |
| **Browser support** | Chrome 89+, Firefox 89+, Edge 89+, Safari 16.4+ |
| **Best on** | Desktop or laptop (WebAssembly is memory-intensive) |

### R packages used

`ggplot2` · `broom` · `survival` · `sandwich` · `lmtest` · `car` · `emmeans` · `lme4` · `nlme` · `haven` · `readxl` · `writexl` · `gridExtra` · `base64enc`

---

## Privacy

**Your data stays on your device.** StatsBrowser uses WebAssembly to run R directly in your browser tab. There is no server, no database, no analytics, and no cookies. When you close the tab, everything is gone.

---

## License

This project is provided for academic and research use.
