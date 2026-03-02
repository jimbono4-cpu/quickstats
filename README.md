# StatsBrowser — In-Browser Statistical Analysis

**A statistical analysis tool that creates publication-ready tables and plots in seconds runs entirely in your browser. No installation. No server. Your data never leaves your device.**

> **[Launch the app](https://jimbono4-cpu.github.io/quickstats/)**

---

## Why StatsBrowser?

Most statistical tools require installing software, uploading data to a server, or writing code. StatsBrowser eliminates all three barriers — just open a link and start analysing.

- **100% private** — your data never leaves your computer. All computation happens on your machine. Analysis powered by [WebR](https://docs.r-wasm.org/webr/latest/), the R language compiled to WebAssembly. 
- **No installation** — works in any modern browser (Chrome, Firefox, Edge, Safari). No R, no Python, no setup. 
- **Publication-ready output** — generates publication level ready tables and plots in seconds you can paste directly into Word, Google Docs, Powerpoint, or LaTeX.
- **Run statistical analyses using R without writing R code** 
- **Initial load will be around 45 seconds but quicker on subsequent loads** 
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
- **All models automatically generate forest plots** of coefficients, odds ratios, or hazard ratios
- Automatic generation of plots depending on model chosen. For example, survival plots with option to stratify if you choose Cox model 
- **Diagnostic plots** — residuals vs fitted, Q-Q plots, Cook's distance
- **Exposure vs outcome** scatter plots with regression lines
- Automatic confidence intervals and exponentiation for logistic/Cox models
- Complete-case analysis with transparent missing data reporting

### Step 5 — Results & Export
- Review all tables and plots in one place
- **Download as PDF** — full report with tables and figures saved as a PDF file
- **Download as Text** (.txt) — plain text report for easy sharing or archiving
- **Download as Word** (.doc) — complete report with tables and embedded plots
- **Copy to clipboard** — paste tables directly into Word or Google Docs as formatted HTML
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
