🧮 Opioid ER–Death Spatiotemporal Modeling Project



Comprehensive analysis of opioid overdose ER visits, deaths, drug consumption, SES, and urban–rural differences across Alabama (2016–2019)



📌 Overview



This project investigates how opioid overdose ER visits relate to overdose deaths at the county level, integrating:



Four opioid drug consumption rates (ARCOS)



ER overdose visits (ADPH)



Overdose deaths (CDC-NCHS)



Demographics \& SES (ACS)



Urban–Rural classification (USDA/CDC)



Spatial \& spatiotemporal dynamics (GLMM, sdmTMB)



Interaction effects (ER × Drug, ER × SES, ER × Year, ER × URcode)



The study aims to understand when and why the ER–death relationship becomes negative, which is highlighted in the literature and required for publication.



🔧 Methodology Overview

1\. Descriptive \& ANOVA-like Reports



Monthly, quarterly, and annual summaries



Per capita consumption and ER visit rates



Outlier detection



Temporal trends



Geographic comparisons and demographic stratification



2\. Spatial \& Spatiotemporal Modeling



Models built using sdmTMB, including:



Poisson, NB2, Tweedie families



Spatial on/off



Spatiotemporal: off, iid, rw, ar1



Offsets: log(pop) for count outcomes



Spatial random fields for hotspot detection



3\. GLMM (County-level)



Random intercepts and random slopes for year



NB2 preferred over Poisson



Tested zero-inflated + hurdle models



Year curvature (year^2) tests



EM algorithm for two-hurdle Poisson



4\. Interaction Models



Examined conditions where ER–death correlation changes sign:



ER-only



ER + covariates



ER + drugs



ER × drug



ER × SES



ER × drug × URcode



ER × spline(year\_index)



Including PCA-derived indices



5\. PCA Indices



To reduce multicollinearity:



Opioid\_Index\_z (PC1 of oxy/hydro/bupren/methadone)



SES\_Index\_z (PC1 of poverty, disability, uninsurance, unemployment, Gini)



Both reversed so higher index = worse conditions.



📊 Data Sources

Source	Description

ARCOS (DEA)	Opioid medication consumption

ADPH	ER overdose visits (monthly \& annual)

CDC NCHS	Overdose death counts

ACS	Socioeconomic variables

USDA / CDC	Urban–Rural classification (URcode)

Census	Population estimates

📅 Timeline \& Stages

Phase 1 — Descriptive (May 2025)



Reports for county/year/month



ANOVA-like tables



Moran’s I and temporal correlation



Outlier detection



Phase 2 — Modeling (June–July 2025)



GLMM and sdmTMB initiation



Negative Binomial vs Poisson



Quarterly/seasonal summaries



Spatiotemporal maps



Phase 3 — Comparison (August 2025)



AIC-based model comparison



County-specific parameters



Mesh refinement (5 km edges)



GLMM vs S–S model comparison



Phase 4 — Missing Data + Hurdle (September 2025)



Zero-inflation + hurdle models



MICE imputation for suppressed death cells



Annual-level spatiotemporal death models



Phase 5 — Advanced Modeling (Oct–Nov 2025)



EM algorithm for two-hurdle models



PCA-based dimensionality reduction



ER × Drug × SES interactions



Urban–Rural (URcode) moderation



Model sets finalized (Models 1, 10, 24, 27)



Coefficient tables + DHARMa residuals



Heatmaps for deaths and drugs



🧾 Update Log (Complete History)

2025-11-17



Added URcode (urban–rural continuum) to models.



Explored full ER-only, ER+covariates, ER+drug, ER×drug, ER×SES interaction models.



Investigated ER–death negative correlation transitions.



2025-11-12



Finalized four key Poisson models:



Poisson\_spOFF\_OFF\_form.er



Poisson\_spOFF\_OFF\_form.er.cov



Poisson\_spOFF\_OFF\_form.inter.cov



Poisson\_spON\_OFF\_form.inter.cov



Identified when ER–death correlation becomes negative.



2025-11-03



Kept coefficient tables and DHARMa for Model 1, 10, 24, 27.



Added heatmaps for Death, Buprenorphine, Methadone.



Drafted “BR–Methadone” relationship section.



2025-10-20



Implemented EM-based two-hurdle Poisson procedure.



Explored PCA features retaining 80–90% variance.



Reviewed suppressed-value imputation (values <1 or >9).



Added signal exploration in Walker and Jefferson counties.



2025-10-06



Shifted death modeling to rates.



Added curvature tests for ER and opioid covariates.



Evaluated MICE for imputation.



2025-09-22



Tested spatiotemporal models for deaths.



Compared NB1/AR1 models with and without random effects.



Added decomposition (Fourier/Haar) for seasonality.



Explored ACS variables integration.



2025-09-08



Implemented hurdle models for ER visits.



Evaluated MICE imputation for suppressed CDC counts.



Annual-level modeling of ER–drug–death relationships.



2025-08-12



Considered consumption vs per capita for tables.



Added GLMM interaction plots.



Highlighted top 5 counties in GLMM.



2025-08-11



Updated Table 1 (mid-year rate, per 1M population).



Added drug consumption tables (2–5).



Integrated tigris for mapping.



Compared GLMM vs sdmTMB models.



Extracted county-specific estimates.



Refined mesh (5 km).



Compared GLMM and spatial–S-S models.



2025-07-22



Added spatiotemporal model tables.



Discussed spatiotemporal parameter tuning.



Detailed offset handling and family choices.



2025-07-21



Updated Table 1 for missing counties.



Removed monthly ER figure for certain counties.



Added population averages.



Created quarterly figures for ER \& consumption.



Combined medication figures (4–7).



Added annual consumption rate visualization.



Added spatiotemporal figures (ER + drugs).



2025-07-08



Added quarterly/seasonal analysis.



Added Moran’s I for ER \& drugs.



Moved insignificant figures; kept Methadone.



Added classical sdmTMB results.



2025-06-26



Began using sdmTMB for GLMM-like models.



2025-06-24



Addressed dropped values and minor data issues.



Added 4-drug separate graphs for northern counties.



2025-06-10



Monthly increasing-trend detection (Cherokee, Etowah, Jackson, Lauderdale).



Added Brupenorphine and 3rd-drug per capita comparisons.



2025-05 (Initial)



ANOVA-like summaries.



Outlier detection by month/county.



Moran’s I spatial diagnostics.



Began temporal correlation analysis.



🗺️ Remaining Tasks



Finalize S–S model outputs.



Clean outlier handling in S–S models.



Test illegal-drug signal proxies.



Expand ACS integration.



Refine ER × Drug × SES × URcode models.



Prepare manuscript figures + tables.

