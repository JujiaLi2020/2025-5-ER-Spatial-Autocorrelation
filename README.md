# Opioid ER–Death Spatiotemporal Modeling

This repository provides a full modeling framework for analyzing the relationship between **opioid overdose ER visits**, **opioid overdose deaths**, **drug consumption**, and **socioeconomic factors** using:

- Generalized Linear Mixed Models (GLMM)
- Spatiotemporal models via `sdmTMB`
- ER × Drug and ER × SES interaction models
- PCA-based dimension reduction
- EM-based modeling for suppressed CDC death data
- A Shiny dashboard for model comparison

## Shiny Dashboard
Explore the interactive dashboard for model comparison, coefficient inspection, and diagnostics.

### Open Shiny App  
<a href="https://v6fozf-joshua-lee.shinyapps.io/shiny/" target="_blank">
  https://v6fozf-joshua-lee.shinyapps.io/shiny/
</a>

### Shiny App Button  
<a href="https://v6fozf-joshua-lee.shinyapps.io/shiny/" target="_blank">
  <img src="https://img.shields.io/badge/Open%20Shiny%20App-Click%20Here-blue?style=for-the-badge" />
</a>

### Data  
[Download full dataset (Data_all.csv)](/Data/Data_all.csv)

---

## What it does

The modeling system:

1. Takes county-level ER, death, drug, and SES inputs  
2. Standardizes variables and builds PCA indices  
3. Fits GLMM and spatiotemporal models  
4. Adds ER × Drug, ER × SES, and ER × Rural/Urban interactions  
5. Runs diagnostics (AIC, DHARMa, hotspot maps)  
6. Detects when the ER–death relationship becomes negative  
7. Provides an interactive Shiny dashboard for exploration  

---

## Repository Structure

```
/data/           Input ER, death, drug, SES datasets
/notebook/       R notebooks (.Rmd, .nb.html)
/scripts/        Core modeling scripts
/models/         Saved sdmTMB and GLMM model objects
/shiny/          Shiny dashboard
/static/         Images
README.md        Documentation
update_log.md    Full chronological update log
```

---

## Key Components

### 1. PCA (Drug & SES)
- `Opioid_Index_z` derived from OXY/HYDRO/BUPRE/METH
- `SES_Index_z` derived from poverty/unemployment/disability/etc.
- Reduces multicollinearity
- Provides loading tables for interpretation

### 2. GLMM Models
- Families: `poisson()`, `nbinom2()`, `tweedie()`
- Offsets: `offset(log(population))`
- Includes splines:  
  ```r
  s(time_index, k = 4)
  ```

### 3. Spatiotemporal Models (`sdmTMB`)
- Spatial = "on" | "off"
- Spatiotemporal = "off" | "iid" | "ar1" | "rw"
- 5 km mesh cutoff

### 4. Interaction Models
Examples:
```r
ER * (OXY + HYDRO + BUPRE + METH)
ER * (poverty + gini + unemployee)
ER * URcode
```

### 5. EM Algorithm for Suppressed Data
Handles CDC suppressed values (0–9):
- Two-hurdle Poisson
- Iterative imputation
- Refit GLMMs on completed dataset

---

## Getting Started

Clone the repository:

```bash
git clone https://github.com/<yourname>/<repo>.git
cd <repo>
```

Run the Shiny app:

```r
shiny::runApp("shiny")
```

Copy environment file if needed:

```bash
cp .env.example .env
```

---

## Requirements

```txt
R >= 4.2

tidyverse
sdmTMB
glmmTMB
DHARMa
mgcv
tigris
sf
FactoMineR
shiny
DT
```

---

## Update Log

A chronological summary of all modeling updates, decisions, and analysis progress.

---

### Table of Contents
- [2025-11-30 — ER Model Integration Update](#2025-11-30--er-model-integration-update)
- [2025-11-17 — Rural/Urban and Interaction Models](#2025-11-17--ruralurban-and-interaction-models)
- [2025-11-12 — Final Poisson Model Set](#2025-11-12--final-poisson-model-set)
- [2025-11-03 — Coefficients and Diagnostics](#2025-11-03--coefficients-and-diagnostics)
- [2025-10-20 — EM Algorithm and PCA](#2025-10-20--em-algorithm-and-pca)
- [2025-10-06 — Death Rate and Curvature Tests](#2025-10-06--death-rate-and-curvature-tests)
- [2025-09-22 — Illegal Drug Detection Discussion](#2025-09-22--illegal-drug-detection-discussion)
- [2025-09-08 — Spatiotemporal Modeling](#2025-09-08--spatiotemporal-modeling)
- [2025-08-25 — Table and Hotspot Revisions](#2025-08-25--table-and-hotspot-revisions)
- [2025-08-12 — Interaction Discussion](#2025-08-12--interaction-discussion)
- [2025-08-11 — Major Revision](#2025-08-11--major-revision)
- [2025-07-22 — Spatial and Temporal Modeling Guidance](#2025-07-22--spatial-and-temporal-modeling-guidance)
- [2025-07-21 — Mid-Summer Updates](#2025-07-21--mid-summer-updates)
- [2025-07-08 — Quarterly Update](#2025-07-08--quarterly-update)
- [2025-06-26 — Methods Planning](#2025-06-26--methods-planning)
- [2025-06-24 — County Trends](#2025-06-24--county-trends)
- [2025-06-10 — Project Kickoff](#2025-06-10--project-kickoff)

---

### 2025-11-30 — ER Model Integration Update
- Added support for a second model set (ER models) in the Shiny dashboard.
- Implemented a new “Model Set” selector to switch between Deaths and ER models.
- Loaded ER models safely using a separate environment (`new.env()`), preventing overwrites.
- Added reactive data/model switching (`current_df`, `current_fits`, `current_aic_table`).
- Updated all plots and outputs to work for both model sets automatically.
- Fixed missing-object errors (`df_ER`, `fits_ER`) and improved overall app stability.

---

### 2025-11-17 — Rural/Urban and Interaction Models
- Added URcode (rural–urban continuum classification).
- Integrated rural/urban grouping into ER, drug, and SES modeling.
- Built ER-only, ER+covariate, ER+drug, ER×drug, and ER×SES interaction models.
- Investigated conditions where ER–death correlation becomes negative.
- Prepared interaction structures for spline and EM-based models.

---

### 2025-11-12 — Final Poisson Model Set
**Models retained:**
- Model 1: `Poisson_spOFF_OFF_form.er`
- Model 10: `Poisson_spOFF_OFF_form.er.cov`
- Model 24: `Poisson_spOFF_OFF_form.inter.cov`
- Model 27: `Poisson_spON_OFF_form.inter.cov`

**Additional notes:**
- Identified time windows where the ER–death relationship reverses.
- Outlined additional models using spline(year) and interaction terms.
- Prepared AIC and coefficient comparison structure for EM algorithm integration.

---

### 2025-11-03 — Coefficients and Diagnostics
- Retained coefficient tables for Models 1, 10, 24, 27.
- Stored DHARMa residual diagnostics for key models.
- Added heatmaps for deaths, buprenorphine, and methadone consumption trends.

---

### 2025-10-20 — EM Algorithm and PCA
- Implemented EM-based two-hurdle Poisson models for ER, oxycodone, and buprenorphine.
- Constructed PCA predictors (retain 80–90% variance).
- Developed backward-stepwise comparison tables for EM-based models.
- Prepared full ER–drug–SES interaction models for imputed data.

---

### 2025-10-06 — Death Rate and Curvature Tests
- Switched main outcome to death rate (instead of raw counts).
- Tested curvature for:
  - ER rate
  - Four opioids
  - SES variables
- Prepared MICE imputation strategies for suppressed CDC death counts.

---

### 2025-09-22 — Illegal Drug Detection Discussion
- Explored whether illegal drug supply trends can be inferred from ER spikes.
- Tested annual-level ER × drug models.
- Investigated seasonality using Haar and Fourier transforms.
- Considered integrating ACS demographic variables.
- Began constructing spatiotemporal death models with MICE and EM imputations.

---

### 2025-09-08 — Spatiotemporal Modeling
- Evaluated NB1/AR1 structures with and without random effects.
- Compared ER-rate, ACS predictor sets, and offset-based models.
- Ran MICE imputation for death suppression scenarios.
- Evaluated delta and zero-inflated model options using sdmTMB.

---

### 2025-08-25 — Table and Hotspot Revisions
- Verified Table 1 (corrections to per-capita and per-million rates).
- Rechecked Figure 20 spatial hotspots.
- Updated regression model specifications for MME consumption.
- Compared Poisson, NB2, and Gamma models for consumption and rates.

---

### 2025-08-12 — Interaction Discussion
- Added GLMM-based interaction terms.
- Identified need for a small health dataset (~100 samples) for EM tutorial.
- Explored UCI datasets as potential demonstration datasets.

---

### 2025-08-11 — Major Revision
- Updated Table 1: mid-year rate per million population.
- Added drug consumption tables (Tables 2–5).
- Integrated `tigris` for obtaining county geometries.
- Compared GLMM vs spatial–spatiotemporal models.
- Extracted county-level random effects.
- Evaluated mesh with 5 km edge constraint.
- Compared best GLMM and best sdmTMB models (AIC, predictions, variance).

---

### 2025-07-22 — Spatial and Temporal Modeling Guidance
- Confirmed negative binomial (`nbinom2`) provides best ER fit.
- Best GLMM: random intercept + random slope (year).
- Verified offset(log(population)).
- Evaluated temporal structures:
  - `off`
  - `iid`
  - `ar1`
  - `rw`
- Suggested testing quadratic time term (`I(year_c^2)`).

---

### 2025-07-21 — Mid-Summer Updates
- Corrected Table 1 structural inconsistencies.
- Combined multi-drug figures for easier cross-comparison.
- Added MME (per capita) figures.
- Added spatiotemporal maps for ER and drug consumption patterns.

---

### 2025-07-08 — Quarterly Update
- Added quarterly ER visit analysis.
- Updated four-medication figures.
- Applied Moran’s I to ER and medication trends.
- Clarified methodology for medication comparison graphs.

---

### 2025-06-26 — Method


---

## Important Data Links

This project integrates ER visits, opioid consumption, socioeconomic, and geographic data from multiple public sources.  
Below is a comprehensive list of all key datasets used in the workflow.

---

### Emergency Department (ER) Data
- Alabama Department of Public Health: Opioid Overdose ER Visits  
  https://www.alabamapublichealth.gov/opioids/

- CDC Drug Overdose Surveillance and Epidemiology (DOSE)  
  https://www.cdc.gov/drugoverdose/datavault.html

---

### Mortality / Death Count Data
- CDC WONDER: Multiple Cause of Death  
  https://wonder.cdc.gov/mcd.html

- CDC Restricted Data / Suppressed Counts Documentation  
  https://www.cdc.gov/nchs/data_access/restrictions.htm

---

### Opioid Prescription / Consumption Data
- ARCOS (DEA):  
  https://www.deadiversion.usdoj.gov/arcos/

- DEA ARCOS Retail Drug Summary Reports  
  https://www.deadiversion.usdoj.gov/arcos/retail_drug_summary/

- MME Conversion Reference  
  https://www.cdc.gov/opioids/providers/prescribing/mmme.html

---

### Socioeconomic & Demographic Data (SES Variables)
- U.S. Census American Community Survey (ACS)  
  https://data.census.gov/

- ACS API Documentation  
  https://www.census.gov/data/developers/data-sets/acs-5year.html

- County-level Poverty, Unemployment, Insurance  
  https://www.ers.usda.gov/data-products/county-level-data-sets/

---

### Rural–Urban Classification
- USDA Rural–Urban Continuum Codes (RUCC)  
  https://www.ers.usda.gov/data-products/rural-urban-continuum-codes/

- NCHS Urban–Rural Classification Scheme  
  https://www.cdc.gov/nchs/data-analysis-tools/urban-rural.html

---

### Geographic Boundaries / Shapefiles
- TIGER/Line Shapefiles (Counties)  
  https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html

- `tigris` R package (recommended for automated AL county geometries)  
  https://github.com/walkerke/tigris

- Alabama State GIS Office  
  https://gis.alabama.gov/

---

### Substance Use & Public Health Background Reports
- JAMA Study Referenced (ER–Death correlation)  
  https://jamanetwork.com/journals/jamanetworkopen/fullarticle/2794462

- SAMHSA Opioid Overdose Resources  
  https://www.samhsa.gov/find-help/prevention/opioid

---

If additional datasets are added later, include them here for central reference.
