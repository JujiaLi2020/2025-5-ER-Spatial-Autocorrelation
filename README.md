🧮 Opioid ER–Death Spatiotemporal Modeling Project
Alabama Counties, 2016–2019
1. Overview

This project analyzes the relationship between:

Opioid overdose ER visits

Opioid overdose deaths

Four opioid medications (Oxycodone, Hydrocodone, Buprenorphine, Methadone)

Socioeconomic conditions (SES)

Urban–rural classifications

Spatial and temporal structure

It aims to identify when and why the ER–death relationship becomes negative, a key scientific finding and publication requirement.

2. Data Sources

Clinical & Public Health

ADPH — ER overdose visits (monthly/annual)

CDC NCHS — overdose deaths

Drug Consumption

DEA ARCOS — Oxy/Hydro/Bup/Methadone

Socioeconomic & Demographics

ACS — poverty, disability, uninsurance, Gini, unemployment

Census — population counts

Geographic

USDA RUCC — rural–urban codes

tigris — county shapefiles

3. Method Summary
3.1 Descriptive Analysis

Monthly / quarterly / annual summaries

Demographic breakdown

Outlier detection

Moran’s I spatial autocorrelation

Temporal correlation

3.2 Statistical Models
GLMM (County-level)

Poisson, NB2, zero-inflated, hurdle

Random intercepts + random year slopes

Curvature tests (ER², Drug²)

EM algorithm for hurdle regression

Offset = log(population)

Spatiotemporal Models (sdmTMB)

Families: Poisson, NB2, Tweedie

Spatial: "on" / "off"

Temporal: "off", "iid", "rw", "ar1"

Mesh refined to ~5 km edges

Extract spatial random fields (hotspots/coldspots)

3.3 Interaction Models

We test when the ER–death relationship flips:

ER-only

ER + covariates

ER + drug consumption

ER × drug

ER × SES

ER × Year

ER × URcode

ER × PCA indices (Opioid_Index, SES_Index)

3.4 PCA Indices (Dimensionality Reduction)

Opioid_Index_z

PC1 of Oxy/Hydro/Bup/Meth

Reflects general opioid environment

SES_Index_z

PC1 of poverty, disability, unemployment, uninsurance, Gini

Both reversed so higher = worse.

4. Timeline (Readable Phase Format)
Phase 1 — Foundations (May 2025)

ANOVA-style county comparison

Outlier & trend detection

Moran’s I spatial tests

Temporal autocorrelation

Phase 2 — Modeling Begins (Jun 2025)

GLMM and sdmTMB basic models

Poisson vs NB2 vs Tweedie family check

Seasonal/quarterly summaries

Initial ER × drug interaction tests

Phase 3 — Spatial/Temporal Expansion (Jul 2025)

Quarterly ER + drug rate figures

Spatiotemporal heatmaps for ER & consumption

Combined multi-drug figures

Phase 4 — Model Comparison (Aug 2025)

GLMM vs S–S model AIC comparison

Mesh refinement

County-specific parameter extraction

Updated Table 1 (per million population)

Phase 5 — Missing Data + Advanced Modeling (Sep–Oct 2025)

Zero-inflated + hurdle models

MICE imputation for suppressed CDC deaths

EM algorithm for two-hurdle Poisson

Curvature terms tested

PCA feature reduction added

Phase 6 — Interactions + URcode (Nov 2025)

Final comparison models:

Model 1

Model 10

Model 24

Model 27

Urban–rural interaction modeling (URcode)

Heatmaps for Death, Buprenorphine, Methadone

Identified windows where ER–death becomes negative

5. Update Log (Highly Readable Format)

Tip: This structure fits GitHub’s monospace blocks and indentation perfectly.

🗓️ 2025-11-17

Added URcode (Rural/Urban continuum)

Built ER-only, ER+cov, ER+drug, ER×drug, ER×SES models

Investigated negative ER–death correlation behavior

🗓️ 2025-11-12

Finalized 4 Poisson models

Established when ER–death flips sign

🗓️ 2025-11-03

Kept key coefficient tables + DHARMa results

Added heatmaps: Death, Buprenorphine, Methadone

🗓️ 2025-10-20

EM-based two-hurdle Poisson implemented

PCA 80–90% variance feature reduction

Handling suppressed values (<1 or >9)

🗓️ 2025-10-06

Switched deaths → rates

Added curvature tests (ER, drug)

Evaluated MICE imputation

🗓️ 2025-09-22

Annual spatiotemporal death models

Illegal drug signal exploration

Fourier/Haar seasonality decomposition

🗓️ 2025-09-08

Hurdle model for ER visits

Imputation for suppressed deaths

Annual ER–drug–death relationships

🗓️ 2025-08-12

GLMM interaction plots

Top-5 county comparison

🗓️ 2025-08-11

Revised Table 1

Added Tables 2–5 (drug consumption)

Added county mapping

GLMM vs S–S comparison

Mesh refinement completed

🗓️ 2025-07-21

Updated Table 1

Added quarterly ER & drug figures

Added annual medication consumption rates

🗓️ 2025-07-08

Moran’s I for ER & drugs

Seasonal analysis

Simplified figure set

🗓️ 2025-06-26

Began using sdmTMB for GLMM-like models

🗓️ 2025-06-24

Cleaned minor data issues

Built four-drug northern county graphs

🗓️ 2025-06-10

Trend detection in Cherokee/Etowah/Jackson/Lauderdale

Added Buprenorphine and other drug per capita trends

🗓️ May 2025

ANOVA-like summaries

Outlier detection

Moran’s I

Temporal correlation

6. Remaining Tasks

Finalize S–S outputs

Outlier fixes in S–S models

Add ACS variables

Expand ER × Drug × SES × URcode interactions

Produce manuscript-ready tables & figures

7. Helpful Links

USDA Rural–Urban Codes: https://www.ers.usda.gov/data-products/rural-urban-continuum-codes

CDC Urban–Rural Classification: https://www.cdc.gov/nchs/data-analysis-tools/urban-rural.html
