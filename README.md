🧮 Opioid ER–Death Spatiotemporal Modeling Project

Analysis of opioid overdose ER visits, deaths, drug consumption, SES, and urban–rural factors across Alabama (2016–2019)

📌 Overview

This project examines:

How ER overdose visits relate to overdose deaths

How the relationship varies by drug type, SES, urban–rural context, and time

County-level spatial and spatiotemporal dynamics

When and why the ER–death correlation becomes negative, as highlighted in prior research

Core methods include GLMM, sdmTMB spatiotemporal models, PCA indices, and interaction models.

📊 Data Sources
Source	Description
ARCOS (DEA)	Oxycodone, Hydrocodone, Buprenorphine, Methadone consumption
ADPH	ER overdose visits (monthly & annual)
CDC NCHS	Overdose death counts
ACS	Socioeconomic variables
USDA / CDC	Urban–Rural Continuum Code (URcode)
Census	Population denominators
🔧 Methods Summary
1. Descriptive Analysis

County/month/year summaries

Seasonal (quarterly) breakdown

Demographic trends

Outlier detection

Moran’s I for spatial autocorrelation

Temporal correlation

2. Statistical Modeling
GLMM (County-level)

Poisson, NB2, zero-inflated, hurdle models

Random intercepts + year slopes

Curvature tests for drugs & ER

EM algorithm for two-hurdle Poisson

Offset: log(pop)

Spatiotemporal (sdmTMB)

Families: Poisson, NB2, Tweedie

Spatial: "on" / "off"

Spatiotemporal: "off", "iid", "rw", "ar1"

Extract spatial random fields (hotspots/coldspots)

Mesh refinement (~5 km edge length)

3. Interaction Models

Used to explore when ER–death correlation becomes negative:

ER-only

ER + covariates

ER + drugs

ER × drug

ER × SES

ER × drug × URcode

ER × spline(time)

ER × PCA indices (Opioid_Index_z, SES_Index_z)

4. Dimensionality Reduction (PCA)

Created indices:

Opioid_Index_z (4 opioid drugs)

SES_Index_z (poverty, disability, unemployment, Gini, uninsurance)

Both reversed → higher = worse conditions.

📅 Major Progress Timeline
Phase 1 — Foundations (May 2025)

ANOVA-like tables

Monthly/annual/SES descriptive stats

Outlier detection

Moran’s I spatial analysis

Phase 2 — Modeling Start (Jun 2025)

GLMM & sdmTMB setup

Poisson vs NB2 vs Tweedie

Begin ER × Drug exploration

Seasonal summaries

Phase 3 — Spatial/Temporal Expansion (Jul 2025)

Quarterly ER & consumption figures

Spatiotemporal heatmaps

Combined medication figures

Beginning of hotspot analysis

Phase 4 — Model Comparison (Aug 2025)

GLMM vs S–S AIC comparison

County-specific estimates

Mesh tuning

Population-adjusted Table 1

Phase 5 — Advanced Models (Sep–Oct 2025)

Hurdle & zero-inflated models

MICE imputation for death suppression

ER–drug–death annual models

PCA-based reduction

EM (two-hurdle Poisson)

Curvature tests

Phase 6 — Interaction & URcode (Nov 2025)

Final models: 1, 10, 24, 27

ER × drug × SES interactions

Urban–rural moderation

Heatmaps for Death/Bupren/Methadone

Key result: conditions where ER–death turns negative

🧾 Update Log (Clean, Chronological)
2025-11-17

Added URcode (urban–rural) stratification

ER-only, ER+cov, ER+drug, ER×drug, ER×SES, spline-based models tested

Investigated when ER–death correlation becomes negative

2025-11-12

Finalized four Poisson comparison models

Identified negative ER–death correlation transitions

2025-11-03

Kept coefficient tables + DHARMa for Models 1, 10, 24, 27

Added heatmaps for Death, Buprenorphine, Methadone

2025-10-20

EM-based two-hurdle Poisson procedure

PCA predictors retaining 80–90% variance

Imputation for suppressed values (<1 or >9)

2025-10-06

Death outcome changed to rate

Curvature testing (ER & drugs)

MICE imputation evaluation

2025-09-22

Spatiotemporal models for deaths (annual)

Explored illegal drug signals

Fourier/Haar seasonal decomposition

2025-09-08

Hurdle modeling for ER

MICE imputation for deaths

ER–drug–death annual analysis

2025-08-12

Interaction plots added

GLMM “Top 5 county” summaries

2025-08-11

Table 1 revised with per-million rates

Tables 2–5 (drug consumption) added

tigris mapping

GLMM vs S–S model comparison

Spatial mesh refinement

2025-07-22

Added spatiotemporal model parameters

Offsets and family selection discussion

2025-07-21

Updated Table 1 for missing counties

Added quarterly ER & consumption figures

Combined medication figures; annual MME version

2025-07-08

Moran’s I for ER + drugs

Seasonal summaries

Moved insignificant figures

2025-06-26

Began using sdmTMB for GLMM-like models

2025-06-24

Fixed minor data issues

Four-drug graphs for northern counties

2025-06-10

Monthly increasing-trend detection

Buprenorphine & “third drug” displays

2025-05 (Initial)

ANOVA-like summaries

Outlier detection

Moran’s I

Temporal correlation diagnostics

📁 Planned Deliverables

Full spatiotemporal model output

Report-ready figures and tables

ER × Drug × SES × URcode models

Manuscript-ready results

Heatmaps for all outcomes

GLMM + S–S comparison

PCA interpretation plots

🔗 Useful Links

Urban–Rural Codes (USDA):
https://www.ers.usda.gov/data-products/rural-urban-continuum-codes

CDC Urban–Rural Classification:
https://www.cdc.gov/nchs/data-analysis-tools/urban-rural.html

JAMA 2022 ER–Death paper:
https://jamanetwork.com/journals/jamanetworkopen/fullarticle/2794462
