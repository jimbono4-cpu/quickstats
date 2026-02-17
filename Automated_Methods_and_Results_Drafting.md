# Optional Step: Automated Drafting of Methods and Results for Peer-Reviewed Publications

## Overview

This optional step allows users to generate a **draft Methods and Results section**, written in the style of a peer-reviewed journal article, based strictly on:

- Information provided by the user about the study context and design
- The statistical methods actually executed by the app
- The numerical results, tables, and plots produced by the app
- The exact R commands used to perform the analyses

The aim is **not** to replace scientific judgment or authorship, but to:
- Save time on first drafts
- Improve consistency and completeness
- Ensure alignment between analyses, code, and written reporting
- Support reproducible, transparent reporting

The generated text is intended as a **draft for review, editing, and approval by the user**.

---

## User-Provided Study Information

Before generating text, the user is prompted to complete a short structured form capturing information that **cannot be inferred from the data alone**.

### Required Fields

The user will be asked to provide:

#### 1. Study Design
Examples:
- Randomised controlled trial (individual or cluster)
- Cohort study
- Cross-sectional study
- Case–control study
- Interrupted time series
- Quasi-experimental study

#### 2. Study Setting
Examples:
- Country or region
- Healthcare, educational, community, or other setting
- Time period of data collection (if applicable)

#### 3. Participants
- Inclusion criteria
- Exclusion criteria
- Any relevant eligibility restrictions

#### 4. Timepoint Definitions
- Definition of baseline
- Follow-up timepoints
- Primary analysis timepoint
- Any secondary or sensitivity analysis timepoints

#### 5. Outcome Definitions
For each outcome:
- Outcome name (exact name to be used in the manuscript)
- Outcome type (continuous, binary, count, time-to-event)
- Measurement instrument or source (if relevant)
- Direction of effect (higher = better/worse, if relevant)
- Primary vs secondary outcome designation

All user inputs are treated as **authoritative** and are not modified or inferred by the system.

---

## Automatically Captured Analysis Information

The app automatically constructs a structured **analysis manifest** containing:

### Statistical Methods
- Descriptive statistics produced
- Statistical models fitted
- Model formulae
- Link functions and distributions
- Covariate adjustment sets
- Random effects or clustering specifications
- Robust standard errors or variance estimators
- Multiple imputation or missing data handling methods (if used)
- Sensitivity or secondary analyses run

### Software and Code
- R version
- Package names and versions
- Exact R commands executed (or a reproducible template populated with user-specific choices)

### Results
- Sample sizes and flow counts
- Summary statistics
- Model estimates, standard errors, confidence intervals, and p-values
- Generated tables
- Generated plots and figure captions

No raw individual-level data are sent to the language model.

---

## LLM Drafting Step

### Inputs to the LLM

The language model receives **only**:
- The user-provided study information
- The structured analysis manifest
- The generated tables and plots
- The executed R commands

### Outputs from the LLM

The model generates:
- A **Methods section**, including:
  - Study design and setting
  - Participants and eligibility criteria
  - Outcome definitions
  - Statistical analysis methods
  - Software and reproducibility statement
- A **Results section**, including:
  - Participant flow and sample characteristics
  - Primary and secondary outcome results
  - In-text references to tables and figures
  - Descriptions of plots consistent with journal conventions

The writing style is neutral, precise, and appropriate for submission to a peer-reviewed journal.

---

## Hallucination Prevention Instruction (Mandatory)

The following instruction is explicitly included in every LLM prompt:

> **Important instruction to the language model**
>
> - Use *only* the information provided in the structured inputs.
> - Do **not** invent, infer, assume, or embellish any methods, results, study characteristics, or interpretations.
> - Do **not** introduce statistical methods, analyses, covariates, or outcomes that are not explicitly listed.
> - Do **not** report values, sample sizes, p-values, confidence intervals, or effect estimates that are not present in the supplied results.
> - If required information is missing, state this explicitly (e.g. “The study setting was not specified”).
> - Do not add causal language unless explicitly appropriate for the stated study design.
> - Do not include a discussion or interpretation of findings beyond factual reporting.

The model’s role is strictly **translation from structured truth to narrative text**, not interpretation or inference.

---

## User Review and Export

After generation:
- The draft Methods and Results sections are displayed to the user
- All tables and plots are shown alongside the text
- Users can copy, download, or export the content (e.g. Markdown, Word, or Quarto)
- The user remains fully responsible for:
  - Accuracy
  - Interpretation
  - Journal-specific formatting
  - Final approval of the manuscript text

---

## Intended Use and Limitations

- The generated text is a **draft**, not a final manuscript
- It does not replace domain expertise, peer review, or editorial judgment
- It is designed to support reproducible reporting and reduce clerical burden
- It is especially suitable for:
  - Standard regression analyses
  - Pre-specified analysis plans
  - Grant, protocol, or manuscript drafting workflows

---

## Summary

This optional step provides a controlled, transparent way to transform:
**data → analysis → results → publication-style reporting**

By combining user-supplied study context with machine-captured analytical truth, the system enables fast, accurate, and reproducible drafting of Methods and Results sections while explicitly guarding against hallucination or over-interpretation.
