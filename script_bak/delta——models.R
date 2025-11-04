### Borrowing from https://pbs-assess.github.io/sdmTMB/articles/delta-models.html
library(sdmTMB)
library(sf)
library(dplyr)


# LOAD DATA ---
# Make sure the CSV file is in your R working directory.
file_path <- "Data/ER_opioid_2016_2019_cleaned.csv"
if (!file.exists(file_path)) {
  stop("Error: File not found. Please make sure 'ER_opioid_2016_2019_cleaned.csv' is in your working directory.")
}
er_data <- read.csv(file_path)

er_data <- er_data%>%
  filter(County %in% toupper(c(
    "Lauderdale", "Limestone", "Madison", "Jackson", "Colbert", "Franklin",
    "Lawrence", "Morgan", "Marshall", "Dekalb", "Marion", "Winston",
    "Cullman", "Blount", "Etowah", "Cherokee", "Lamar", "Fayette",
    "Walker", "Jefferson", "St. Clair", "Pickens", "Tuscaloosa", "Shelby"
  ))
  )


# Example: using annual aggregated data
# 1. Add presence indicator
er_data <- er_data %>%
  mutate(
    year = year(Date),
    presence = if_else(Count > 0, 1, 0)
  )

# 2. Aggregate to monthly (optional, reduces zeros)
er_month <- er_data %>%
  group_by(County, year, month = month(Date)) %>%
  summarise(
    Count = sum(Count),
    pop = mean(pop),  # assume stable pop
    presence = if_else(sum(Count) > 0, 1, 0),
    case_percapita = sum(Count) / mean(pop),
    .groups = "drop"
  )

# 1) Get county polygons and transform to a projected CRS in meters (UTM 16N works for most of AL)
counties_sf <- tigris::counties(state = "AL", cb = TRUE, year = 2023) |>
  st_transform(26916) |>
  mutate(County = toupper(NAME))

# 2) Centroids (geometry-only to avoid warnings)
cent_geom <- st_centroid(st_geometry(counties_sf))
coords_m  <- st_coordinates(cent_geom)

# 3) Attach numeric coords in **km**
county_xy <- counties_sf |>
  st_drop_geometry() |>
  transmute(County,
            X = coords_m[, "X"] / 1000,
            Y = coords_m[, "Y"] / 1000)

# 4) Merge coords into your data
df_month <- er_month |>
  mutate(County = toupper(County),
         year_c = as.numeric(scale(year, center = TRUE, scale = FALSE)),
         log_pop = log(pop),
         year_index = match(year, sort(unique(year)))) |>
  left_join(county_xy, by = "County")




## 2. Build mesh
mesh <- make_mesh(df_month, xy_cols = c("X","Y"), cutoff = diff(range(df_fit$X, na.rm=TRUE))/10)
# choose a cutoff ~ 5–20% of range; adjust if mesh too coarse/fine


# Basic 
fit_fix <- sdmTMB(
  Count ~ year_c,
  data = df_month,
  family = delta_truncated_nbinom2(link1 = "logit", link2 = "log"),
  spatial = "off",
  spatiotemporal = "off",
  offset = df_month$log_pop
)


fit_space <- sdmTMB(
  Count ~ year_c,
  data = df_month,
  mesh = mesh,
  family = delta_truncated_nbinom2(link1 = "logit", link2 = "log"),
  spatial = "on",
  spatiotemporal = "off",
  offset = df_month$log_pop
)


fit_st_iid <- update(fit_space, spatiotemporal = "iid", time = "year_index")

library(DHARMa)

# Best: predict with the same offset you fitted with
pp <- predict(fit_st_iid, newdata = df_month, offset = df_month$log_pop)

p_presence <- plogis(pp$est1)
mu_pos     <- exp(pp$est2)
fit_hat    <- p_presence * mu_pos  # overall expected count

dh <- createDHARMa(
  simulatedResponse = sim,            # from simulate(fit_st_iid, nsim=250)
  observedResponse  = df_month$Count,
  fittedPredicted   = fit_hat
)
plot(dh)
testUniformity(dh)
testDispersion(dh)
testZeroInflation(dh)



# Predicted means (overall expectation, including delta part + offset)
fit_hat <- predict(fit_st_iid)$est  

# Build DHARMa object
dh <- createDHARMa(
  simulatedResponse = sim,
  observedResponse  = df_month$Count,
  fittedPredicted   = fit_hat
)

plot(dh)   # produces QQ-plot, residuals vs. predictions, etc.









install.packages("broom.mixed")
library(broom.mixed)
tidy(fit_st_iid, conf.int = TRUE)


library(DHARMa)

# 1) Simulate from the fitted model
sim <- simulate(fit_st_iid, nsim = 250)  # matrix: n x nsim

# 2) Fitted values on the response scale
fit_hat <- predict(fit_st_iid)$est       # expected Count incl. delta

# 3) Build a DHARMa object and plot
dh <- DHARMa::createDHARMa(
  simulatedResponse = sim,
  observedResponse  = df_fit$Count,
  fittedPredicted   = fit_hat
)
plot(dh)  # QQ, residuals vs. pred, etc.

library(dplyr)
library(ggplot2)

pred_obs <- predict(fit_st_iid, newdata = df_fit)

pred_obs <- predict(
  fit_st_iid,
  newdata = df_fit,
  offset  = df_fit$log_pop   # <- REQUIRED when model used an offset
)



# 1) Probability of presence (delta part)
ggplot(pred_obs, aes(X, Y, color = plogis(est1))) +
  geom_point(size = 3) +
  coord_equal() +
  scale_color_viridis_c() +
  labs(color = "Pr(presence)", 
       title = "Delta part: presence probability")





# 2) Expected positive counts (given presence)
ggplot(pred_obs, aes(X, Y, color = exp(est2))) +
  geom_point(size = 3) +
  coord_equal() +
  scale_color_viridis_c() +
  labs(fill = "E[count | >0]", title = "Positive-count intensity")

# 3) Overall expected counts (per pop offset already applied)
ggplot(pred_obs, aes(X, Y, color = est)) +
  geom_point() +
  coord_equal() +
  scale_color_viridis_c() +
  labs(fill = "E[count]", title = "Expected count (delta × intensity)")













## ---- 0) Setup & data prep -----------------------------------------------
library(dplyr)
library(ggplot2)
library(DHARMa)
library(sdmTMB)

# Ensure finite offset & clean time index (consecutive 1..T)
df_use <- df_month %>%
  mutate(
    log_pop = log(pmax(pop, 1)),
    year_c  = scale(year, center = TRUE, scale = FALSE) |> as.numeric(),
    # cyclic seasonality (smoother than factor(month))
    month_sin = sin(2 * pi * month / 12),
    month_cos = cos(2 * pi * month / 12),
    # force time index to consecutive integers (1..T)
    year_index = match(year, sort(unique(year))),
    County_f = factor(County)
  ) %>%
  filter(is.finite(log_pop), is.finite(X), is.finite(Y), !is.na(Count))

# If you built `mesh` from a different frame, you can reuse it.
# Otherwise (re)build mesh from these coordinates:
# mesh <- make_mesh(df_use, xy_cols = c("X","Y"), cutoff = 30000)

## OPTIONAL: add county-level predictors by joining a table `county_covars`
## with columns: County, median_income, urban_rate (example names)
# df_use <- df_use %>%
#   left_join(county_covars, by = "County") %>%
#   mutate(
#     income_sc = scale(median_income),
#     urban_sc  = scale(urban_rate)
#   )

## ---- helper: DHARMa residual check --------------------------------------
check_dharma <- function(fit, df, offset_vec, nsim = 250) {
  sim <- simulate(fit, nsim = nsim)
  pp  <- predict(fit, newdata = df, offset = offset_vec)
  # Some sdmTMB builds don't return 'est'; compute overall mean manually:
  mu  <- plogis(pp$est1) * exp(pp$est2)
  dh  <- DHARMa::createDHARMa(simulatedResponse = sim,
                              observedResponse  = df$Count,
                              fittedPredicted   = mu)
  print(plot(dh))
  print(DHARMa::testUniformity(dh))
  print(DHARMa::testDispersion(dh))
  print(DHARMa::testOutliers(dh))
  invisible(dh)
}

## ---- 1) Space + seasonality (sin/cos), IID spatiotemporal ---------------
fit_season_nb <- sdmTMB(
  Count ~ year_c + month_sin + month_cos,
  data = df_use,
  mesh = mesh,
  family = delta_truncated_nbinom2(link1 = "logit", link2 = "log"),
  spatial = "on",
  spatiotemporal = "iid",
  time = "year_index",
  offset = df_use$log_pop
)

## ---- 2) Add county-level predictors (UNCOMMENT if you created them) -----
# fit_covars_nb <- sdmTMB(
#   Count ~ year_c + month_sin + month_cos + income_sc + urban_sc,
#   data = df_use,
#   mesh = mesh,
#   family = delta_truncated_nbinom2(link1 = "logit", link2 = "log"),
#   spatial = "on",
#   spatiotemporal = "iid",
#   time = "year_index",
#   offset = df_use$log_pop
# )

## ---- 3) Add a random intercept by County (iid RE) ------------------------
## sdmTMB supports lme4-style iid REs: (1 | County_f)
fit_re_nb <- sdmTMB(
  Count ~ year_c + month_sin + month_cos + (1 | County_f),
  data = df_use,
  mesh = mesh,
  family = delta_truncated_nbinom2(link1 = "logit", link2 = "log"),
  spatial = "on",
  spatiotemporal = "iid",
  time = "year_index",
  offset = df_use$log_pop
)

## If your sdmTMB version errors on `(1 | County_f)`,
## update sdmTMB to a newer release. (As a fallback just for diagnosis,
## you could fit a non-spatial GLMM with glmmTMB and compare dispersion.)

## ---- 4) Compare with a simpler delta Poisson -----------------------------
fit_re_pois <- sdmTMB(
  Count ~ year_c + month_sin + month_cos + (1 | County_f),
  data = df_use,
  mesh = mesh,
  family = delta_gamma(),  # delta + Poisson
  spatial = "on",
  spatiotemporal = "iid",
  time = "year_index",
  offset = df_use$log_pop
)


## ---- 5) Try AR1 spatiotemporal (only if >1 time slice) -------------------
if (dplyr::n_distinct(df_use$year_index) > 1) {
  fit_re_nb_ar1 <- try(
    update(fit_re_nb, spatiotemporal = "ar1", time = "year_index"),
    silent = TRUE
  )
  if (!inherits(fit_re_nb_ar1, "try-error")) {
    message("AR1 model attempted. Check pdHess: ",
            fit_re_nb_ar1$sd_report$pdHess)
  } else {
    message("AR1 attempt failed to converge; staying with IID.")
  }
} else {
  message("Only one unique time; AR1 is not identifiable. Skipping.")
}

## ---- 6) Model comparison (AIC) ------------------------------------------
# Collect what you fit
models_to_compare <- list(
  season_nb = fit_season_nb,
  re_nb     = fit_re_nb,
  re_pois   = fit_re_pois
  # , covars_nb = fit_covars_nb  # uncomment if you fit it
  # , re_nb_ar1 = fit_re_nb_ar1  # uncomment if it converged
)

# AIC side-by-side
AIC(fit_season_nb, fit_re_nb, fit_re_pois)  # add others if they exist


## ---- 7) Diagnostics with DHARMa -----------------------------------------
# Check overdispersion / outliers after each change
dh_season_nb <- check_dharma(fit_season_nb, df_use, df_use$log_pop)
dh_re_nb     <- check_dharma(fit_re_nb,     df_use, df_use$log_pop)
dh_re_pois   <- check_dharma(fit_re_pois,   df_use, df_use$log_pop)
# if exists:
# dh_covars_nb <- check_dharma(fit_covars_nb, df_use, df_use$log_pop)
# dh_re_nb_ar1 <- check_dharma(fit_re_nb_ar1, df_use, df_use$log_pop)

## ---- 8) Quick summaries you may want ------------------------------------
broom.mixed::tidy(fit_re_nb, conf.int = TRUE)
broom.mixed::tidy(fit_re_pois, conf.int = TRUE)








fit_month_nb <- sdmTMB(
  Count ~ year_c + factor(month),   # categorical seasonality
  data = df_use, mesh = mesh,
  family = delta_truncated_nbinom2(link1="logit", link2="log"),
  spatial = "on", spatiotemporal = "iid", time = "year_index",
  offset = df_use$log_pop
)

AIC(fit_season_nb, fit_month_nb)


fit_re_nb_nospace <- sdmTMB(
  Count ~ year_c + factor(month) + (1 | County_f),
  data = df_use,mesh = mesh,
  family = delta_truncated_nbinom2(link1="logit", link2="log"),
  spatial = "off", spatiotemporal = "iid", time = "year_index",
  offset = df_use$log_pop
)

AIC(fit_re_nb_nospace, fit_re_nb, fit_season_nb)



unique(diff(sort(unique(df_use$year_index))))   # should be all 1s
fit_season_nb_ar1 <- try(
  update(fit_season_nb, spatiotemporal = "ar1"),
  silent = TRUE
)

if (!inherits(fit_season_nb_ar1, "try-error")) {
  fit_season_nb_ar1$sd_report$pdHess  # TRUE is good
  AIC(fit_season_nb, fit_season_nb_ar1)
}


sim <- simulate(fit_season_nb, nsim = 250)
pp  <- predict(fit_season_nb, newdata = df_use, offset = df_use$log_pop)
mu  <- plogis(pp$est1) * exp(pp$est2)


dh <- createDHARMa(sim, observedResponse = df_use$Count, fittedPredicted = mu)
plot(dh); testUniformity(dh); testDispersion(dh); testOutliers(dh)


