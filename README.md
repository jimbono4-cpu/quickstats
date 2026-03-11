# Quickstats — In-Browser Statistical Analysis

**A statistical analysis tool that creates publication-ready tables and plots in seconds that runs entirely in your browser. No installation. No server. Your data never leaves your device.**

> **[Launch the app](https://quickstats.tools/)**

<p align="center">
<a href="https://quickstats.tools//">
  <img src="quickstats.gif" width="900">
</a>
</p>
---

## Why Quickstats?

Most statistical tools require installing software, uploading data to a server, or writing code. Quickstats eliminates all three barriers — just open a link and start analysing.

- **100% private** — your data never leaves your computer. All computation happens on your machine. Analysis powered by [WebR](https://docs.r-wasm.org/webr/latest/), the R language compiled to WebAssembly. This is unlike Jamovi cloud or RStudio cloud which need data to be uploaded to their servers.
- **No installation** — works in any modern browser (Chrome, Firefox, Edge, Safari). No R, no Python, no setup. 
- **Publication-ready output** — generates publication level ready tables and plots in seconds you can paste directly into Word, Google Docs, Powerpoint, or LaTeX.
- **Run statistical analyses using R without writing R code** 
- **The first load takes about 30–60 seconds while the analysis environment starts. After this, loading will be much faster.** 
---

## Features

### Step 1 — Upload Data
Import your dataset in **CSV, Excel (.xlsx), Stata (.dta), SPSS (.sav), SAS (.sas7bdat)**, or R (.rds or .rds) format. Variable labels from Stata/SPSS files are automatically preserved.

<img width="1887" height="827" alt="image" src="https://github.com/user-attachments/assets/19ce0b2e-d59c-4a75-a906-102308fb31ab" />


### Step 2 — Explore Data
- View variable distributions with histograms and bar charts
- Inspect missing data patterns
- Set variable types (continuous, categorical) and apply labels
- Normality testing (Shapiro-Wilk)

<img width="1554" height="887" alt="image" src="https://github.com/user-attachments/assets/b2d12b02-14b3-4893-a1ac-6b603cb04291" />
<img width="1885" height="739" alt="image" src="https://github.com/user-attachments/assets/0d0d0e3d-4bd1-470a-b326-85838dc67c9f" />
<img width="1887" height="859" alt="image" src="https://github.com/user-attachments/assets/cc2b3341-cb7d-4df3-a3de-ec18c3f36649" />

### Step 3 — Table 1 (Descriptive Statistics)
- Generate a publication-quality **Table 1** stratified by any grouping variable
- Continuous variables: mean (SD) or median (IQR) based on optional normality tests
- Categorical variables: n (%)
- Optional overall column with sample sizes — e.g. `Overall (N=500)`
- Chi-squared and t-test / Wilcoxon / ANOVA / Kruskal-Wallis p-values
- Copy to clipboard for direct pasting into Word

<img width="1880" height="878" alt="image" src="https://github.com/user-attachments/assets/5db965b2-a153-4ae4-8280-711a0e4d60d8" />

### Step 4 — Regression Models
| Model | Use case |
|-------|----------|
| **Linear regression** | Continuous outcomes |
| **Logistic regression** | Binary outcomes (odds ratios) |
| **Cox regression** | Time-to-event / survival data (hazard ratios) plus Schoenfeld residuals to test for proportional hazards|
| **Mixed model** | Clustered / hierarchical data with random effects |
**Plots** automatically generated depedent on model. Forest plots provided for all. Multiple plots chosen are faceted/ tiled.
<img width="1894" height="885" alt="image" src="https://github.com/user-attachments/assets/7a8b01b5-b398-46dc-b26b-910d0b384a8e" />
<img width="1887" height="883" alt="image" src="https://github.com/user-attachments/assets/7a58f2b7-9cc0-4bea-8be1-bb6a7c9103e7" />
<img width="1262" height="915" alt="image" src="https://github.com/user-attachments/assets/d682da2f-091f-4094-8981-c5a1c0acd891" />
<img width="1251" height="466" alt="image" src="https://github.com/user-attachments/assets/59cdcc75-54d2-4e30-a167-c7f0a0339400" />

### Step 5 — Results & Export
- Review all tables and plots in one place
- **Download as PDF** — full report with tables and figures saved as a PDF file
- **Download as Text** (.txt) — plain text report for easy sharing or archiving
- **Download as Word** (.doc) — complete report with tables and embedded plots
- **Copy to clipboard** — paste tables directly into Word or Google Docs as formatted HTML
- **AI analysis prompt** — auto-generated prompt summarising your analysis for use with ChatGPT, Claude, or other LLMs
<img width="1890" height="524" alt="image" src="https://github.com/user-attachments/assets/a67c1d1a-0e60-4f55-b8e6-f495f7896d07" />
<img width="1815" height="1021" alt="image" src="https://github.com/user-attachments/assets/5e5833d2-3151-407d-83a5-ed7f1d3d90cc" />

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

---

## Quick Start

1. **Open** the app: [https://quickstats.tools/](https://quickstats.tools/)
2. **Wait** for R to load in your browser (~30–45 seconds on first visit, faster on reload)
3. **Upload** a CSV, Excel, Stata, SAS, or SPSS file
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

**Your data stays on your device.** Quickstats uses WebAssembly to run R directly in your browser tab. There is no server, no database, and no cookies. We use privacy-friendly, cookie-free analytics ([GoatCounter](https://www.goatcounter.com/)) to count page visits — your uploaded data is never transmitted. When you close the tab, everything is gone.

---
⭐ If you find QuickStats useful, please star the project on GitHub.

## License

This project is provided for academic and research use.
