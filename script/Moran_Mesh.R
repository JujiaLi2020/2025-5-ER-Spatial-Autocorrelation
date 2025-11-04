# --- Load packages ---
library(dplyr)
library(glmmTMB)
library(sdmTMB)
library(sf)

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
            X_km = coords_m[, "X"] / 1000,
            Y_km = coords_m[, "Y"] / 1000)

# 4) Merge coords into your data
df <- Data_ER_filted |>
  mutate(County = toupper(County),
         year_c = as.numeric(scale(year, center = TRUE, scale = FALSE)),
         log_pop = log(pop),
         year_index = match(year, sort(unique(year)))) |>
  left_join(county_xy, by = "County")

# --- 4. Fit best GLMM model (both_nb2) ---
fit_glmm <- glmmTMB(
  Count ~ year_c + (1 + year_c | County),
  data = df,
  family = nbinom2(),
  offset = log_pop
)

# --- 5. Fit sdmTMB mesh model ---
#mesh <- make_mesh(df, xy_cols = c("X", "Y"), cutoff = 50000) # ~50 km cutoff
mesh <- make_mesh(df, xy_cols = c("X_km", "Y_km"), cutoff = 5)  # ~5 km cutoff is a good start

# Spatial only (no ST), NB2
fit_sp <- sdmTMB(
  Count ~ year_c,
  data  = df,
  mesh  = mesh,
  family = nbinom2(),
  offset = df$log_pop,
  spatial = "on",
  spatiotemporal = "off",
  control = sdmTMBcontrol(multiphase = TRUE, nlminb_loops = 2, newton_loops = 0)
)

# If that’s OK, add a simple ST field (iid) with a clean integer time index
fit_st <- sdmTMB(
  Count ~ year_c,
  data  = df,
  mesh  = mesh,
  family = nbinom2(),
  offset = df$log_pop,
  spatial = "on",
  spatiotemporal = "iid",
  time = "year_index",
  control = sdmTMBcontrol(multiphase = TRUE, nlminb_loops = 2, newton_loops = 0)
)

AIC(fit_sp, fit_st)

# fit_sp  5 32051.24
# fit_st  6 31467.39



# --- 7. Residual checks ---
library(DHARMa)

# 1) Fitted predictions (response scale)
mu <- predict(fit_st, type = "response")$est

# 2) Simulate directly (returns matrix already)
set.seed(1)
sim_mat <- simulate(fit_st, nsim = 250, type = "mle-eb")

# 3) Create DHARMa object
res_dh <- createDHARMa(
  simulatedResponse = sim_mat,
  observedResponse  = df$Count,
  fittedPredicted   = mu,
  integerResponse   = TRUE
)

# 4) Plot diagnostics
plot(res_dh)
testDispersion(res_dh)
testZeroInflation(res_dh)









# Compare AIC
AIC_values <- data.frame(
  model = c("GLMM both_nb2", "Mesh sdmTMB"),
  AIC   = c(AIC(fit_glmm), AIC(fit_st))
)
print(AIC_values)





library(DHARMa)

# GLMM residuals
res_glmm <- simulateResiduals(fit_glmm, n = 1000)
plot(res_glmm)
testDispersion(res_glmm)
testZeroInflation(res_glmm)
testTemporalAutocorrelation(res_glmm, time = df$year)

# sdmTMB residuals
res_sdm <- residuals(fit_st, type = "response")  # or "pearson" if available
qqnorm(res_sdm); qqline(res_sdm)
hist(res_sdm, main = "sdmTMB residuals")





### Identity outliers in Spatail model
res <- DHARMa::simulateResiduals(fit_st)
out <- which(DHARMa::testOutliers(res)$outliers)
df[out, ]



#  Compare predictive accuracy
install.packages("MuMIn")
library(MuMIn)
r.squaredGLMM(fit_glmm)
r.squaredGLMM(fit_st)




fit_st
ranef_county <- ranef(fit_st)$cond$County
head(ranef_county)









