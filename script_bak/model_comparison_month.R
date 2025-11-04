### Borrowing from https://pbs-assess.github.io/sdmTMB/articles/delta-models.html
library(sdmTMB)
library(sf)
library(dplyr)
library(lubridate) ## year and month function


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
  mutate(year_month = paste(year, month(Date), sep = "-"))%>%
  group_by(year, County, year_month) %>%
  summarise(
    Count = sum(Count),
    pop = mean(pop),  # assume stable pop
    presence = if_else(sum(Count) > 0, 1, 0),
    case_percapita = sum(Count) / mean(pop),
    .groups = "drop"
  )

er_month <- er_month %>%
  mutate(
    year_month = factor(year_month, levels = unique(year_month)),
    time_index = as.integer(year_month)   # 1, 2, 3, ...
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









fit0 <- sdmTMB(
  Count ~ 1,
  data = df_month,
  family = delta_truncated_nbinom1(link1="logit", link2="log"),
  spatial = "off",
  spatiotemporal = "off",
  offset = df_month$log_pop
)

fit1 <- sdmTMB(
  Count ~ 1,
  data = df_month,
  mesh = mesh,
  family = delta_truncated_nbinom1(link1="logit", link2="log"),
  spatial = "on",
  spatiotemporal = "off",
  offset = df_month$log_pop
)

fit2 <- sdmTMB(
  Count ~ 1,
  data = df_month,
  mesh = mesh,
  family = delta_truncated_nbinom1(link1="logit", link2="log"),
  spatial = "on",
  spatiotemporal = "iid",
  time = "time_index",
  offset = df_month$log_pop
)


df_month <- df_month %>%
  mutate(t_c = as.numeric(scale(time_index, center = TRUE, scale = FALSE)))



# must be consecutive 1..T with NO gaps globally
sort(unique(df_month$time_index))
unique(diff(sort(unique(df_month$time_index))))  # should be all 1

# each time slice should have data rows
table(df_month$time_index)  # no zeros

# no NA / Inf in key columns
anyNA(df_month[c("Count","X","Y","time_index","log_pop")])
any(!is.finite(df_month$log_pop))





# A) spatial OFF, ST OFF: checks offset/family are fine
m0 <- sdmTMB(
  Count ~ 1, data = df_month,
  family = delta_truncated_nbinom1(link1="logit", link2="log"),
  spatial = "off", spatiotemporal = "off",
  offset = df_month$log_pop
)

# B) spatial ON, ST OFF: tests spatial field alone
m1 <- sdmTMB(
  Count ~ 1, data = df_month, mesh = mesh,
  family = delta_truncated_nbinom1(link1="logit", link2="log"),
  spatial = "on", spatiotemporal = "off",
  offset = df_month$log_pop
)

# C) spatial ON, ST = IID: your target structure
m2 <- sdmTMB(
  Count ~ 1, data = df_month, mesh = mesh,
  family = delta_truncated_nbinom1(link1="logit", link2="log"),
  spatial = "on", spatiotemporal = "iid", time = "time_index",
  offset = df_month$log_pop
)

# Check Hessians:
m0$sd_report$pdHess; m1$sd_report$pdHess; m2$sd_report$pdHess



df_month <- df_month |>
  mutate(t_c = as.numeric(scale(time_index, center=TRUE, scale=FALSE)))

fit_time_trend <- sdmTMB(
  Count ~ t_c, data=df_month, mesh=mesh,
  family=delta_truncated_nbinom1(link1="logit", link2="log"),
  spatial="on", spatiotemporal="iid", time="time_index",
  offset=df_month$log_pop
)





## 3. Define the model grid
seasonality_list <- list(
  sincos = Count ~ year_c + month_sin + month_cos,
  facmon = Count ~ year_c + factor(month)
)


spatiotemporal_opts <- list(
  off = list(spatial="off", spatiotemporal="off"),
  space = list(spatial="on", spatiotemporal="off"),
  iid = list(spatial="on", spatiotemporal="iid"),
  ar1 = list(spatial="on", spatiotemporal="ar1")
)

families <- list(
  NB2 = delta_truncated_nbinom2(link1="logit", link2="log"),
  NB1 = delta_truncated_nbinom1(link1="logit", link2="log"),
  Gamma = delta_gamma(link1="logit", link2="log"),
  Lognorm = delta_lognormal(link1="logit", link2="log")
)



## 4.Loop over combinations
model_results <- list()
i <- 1

for (sname in names(seasonality_list)) {
  form <- seasonality_list[[sname]]
  
  for (fname in names(families)) {
    fam <- families[[fname]]
    
    for (spname in names(spatiotemporal_opts)) {
      opts <- spatiotemporal_opts[[spname]]
      
      message("Fitting ", sname, "-", fname, "-", spname)
      
      fit <- try(sdmTMB(
        form,
        data = er_month,
        mesh = mesh,
        family = fam,
        spatial = opts$spatial,
        spatiotemporal = opts$spatiotemporal,
        time = "year_index",
        offset = er_month$log_pop
      ), silent=TRUE)
      
      if (!inherits(fit, "try-error")) {
        model_results[[i]] <- list(
          season = sname,
          family = fname,
          space  = spname,
          fit    = fit,
          AIC    = AIC(fit),
          logLik = as.numeric(logLik(fit)),
          npar   = attr(logLik(fit), "df")
        )
        i <- i+1
      }
    }
  }
}



## 5.Collect into a comparison table
aic_table <- do.call(rbind, lapply(model_results, function(x) {
  data.frame(
    season = x$season,
    family = x$family,
    space  = x$space,
    AIC    = x$AIC,
    logLik = x$logLik,
    npar   = x$npar
  )
})) %>%
  arrange(AIC) %>%
  mutate(DeltaAIC = AIC - min(AIC))

print(aic_table)



## 6. Inspect best models

#Start with lowest AIC.

#Check DHARMa residuals for dispersion/outliers.

#If multiple models within ΔAIC ≤ 2, prefer the more parsimonious (fewer parameters).
# Family: NB1 beats NB2 for your data.
# 
# Spatiotemporal: AR1 clearly improves fit.
# 
# Seasonality:
#   
#   Factor(month) slightly better AIC (but costs ~2× parameters).
# 
# Sin/cos gives nearly identical fit (ΔAIC = 0.3) with half the complexity.
# 
# 👉 Both are valid; if interpretability and parsimony matter, you could pick sincos + NB1 + AR1.
# 👉 If you want a purely descriptive seasonal pattern (each month free), go with facmon + NB1 + AR1.Family: NB1 beats NB2 for your data.
# 
# Spatiotemporal: AR1 clearly improves fit.
# 
# Seasonality:
#   
#   Factor(month) slightly better AIC (but costs ~2× parameters).
# 
# Sin/cos gives nearly identical fit (ΔAIC = 0.3) with half the complexity.
# 
# 👉 Both are valid; if interpretability and parsimony matter, you could pick sincos + NB1 + AR1.
# 👉 If you want a purely descriptive seasonal pattern (each month free), go with facmon + NB1 + AR1.


#DHARMa residual diagnostics

# Factor(month) + NB1 + AR1
fit_facmon_nb1_ar1 <- sdmTMB(
  Count ~ year_c + factor(month),
  data = er_month,
  mesh = mesh,
  family = delta_truncated_nbinom1(link1 = "logit", link2 = "log"),
  spatial = "on",
  spatiotemporal = "ar1",
  time = "year_index",
  offset = er_month$log_pop
)

# Sin/cos + NB1 + AR1
fit_sincos_nb1_ar1 <- sdmTMB(
  Count ~ year_c + month_sin + month_cos,
  data = er_month,
  mesh = mesh,
  family = delta_truncated_nbinom1(link1 = "logit", link2 = "log"),
  spatial = "on",
  spatiotemporal = "ar1",
  time = "year_index",
  offset = er_month$log_pop
)



# Helper (works with delta models returning est1/est2)
check_dharma <- function(fit, df, offset_vec, nsim = 250, title = NULL) {
  sim <- simulate(fit, nsim = nsim)
  pp  <- predict(fit, newdata = df, offset = offset_vec)
  mu  <- plogis(pp$est1) * exp(pp$est2)
  dh  <- DHARMa::createDHARMa(
    simulatedResponse = sim,
    observedResponse  = df$Count,
    fittedPredicted   = mu
  )
  if (!is.null(title)) message(title)
  plot(dh)
  print(testUniformity(dh))
  print(testDispersion(dh))
  print(testOutliers(dh))
  invisible(list(dh = dh, mu = mu))
}

# Run on the two best models
dh_facmon_ar1 <- check_dharma(
  fit_facmon_nb1_ar1, er_month, er_month$log_pop,
  title = "facmon + NB1 + AR1"
)

dh_sincos_ar1 <- check_dharma(
  fit_sincos_nb1_ar1, er_month, er_month$log_pop,
  title = "sincos + NB1 + AR1"
)













## 7. Visualization (optional)
library(ggplot2)
ggplot(aic_table, aes(family, season, fill = DeltaAIC)) +
  geom_tile(color="white") +
  facet_wrap(~ space) +
  scale_fill_viridis_c() +
  labs(title="ΔAIC across model specifications")






## 8.Seasonal effect plots (per 100k) + optional CIs, saved to PNG
library(ggplot2)
library(tidyr)
library(dplyr)

# Base covariates for marginal curve
newdat_base <- data.frame(
  year_c     = 0,                                  # centered year
  year_index = median(df_month$year_index),
  X          = mean(df_month$X, na.rm = TRUE),
  Y          = mean(df_month$Y, na.rm = TRUE)
)

# ----- factor(month) curve -----
new_fac <- newdat_base %>%
  tidyr::crossing(month = 1:12)

pp_fac <- predict(fit_facmon_nb1_ar1, newdata = new_fac, offset = log(1e5), se_fit = TRUE)
inc_100k_fac <- plogis(pp_fac$est1) * exp(pp_fac$est2)

dat_fac <- data.frame(
  model = "factor(month)",
  month = 1:12,
  inc_100k = inc_100k_fac
)

# ----- sin/cos curve -----
new_sincos <- newdat_base %>%
  tidyr::crossing(month = 1:12) %>%
  mutate(
    month_sin = sin(2*pi*month/12),
    month_cos = cos(2*pi*month/12)
  )

pp_sc <- predict(fit_sincos_nb1_ar1, newdata = new_sincos, offset = log(1e5), se_fit = TRUE)
inc_100k_sc <- plogis(pp_sc$est1) * exp(pp_sc$est2)

dat_sc <- data.frame(
  model = "sin+cos",
  month = 1:12,
  inc_100k = inc_100k_sc
)

# Combine (point estimates)
plot_dat <- bind_rows(dat_fac, dat_sc)

# ---------- OPTIONAL: 95% CI via simple Monte Carlo on link scales ----------
# Works if your predict() returned standard errors for both components.
add_ci <- function(pp_df, n_draws = 2000) {
  # Try to detect se columns:
  se1 <- pp_df$est1_se %||% pp_df$se_est1 %||% NULL
  se2 <- pp_df$est2_se %||% pp_df$se_est2 %||% NULL
  if (is.null(se1) || is.null(se2)) return(NULL)  # skip CI if SEs not available
  
  # Draw on link scales assuming independence between parts (approximation)
  draws1 <- matrix(rnorm(n_draws * nrow(pp_df), mean = pp_df$est1, sd = se1), nrow = nrow(pp_df))
  draws2 <- matrix(rnorm(n_draws * nrow(pp_df), mean = pp_df$est2, sd = se2), nrow = nrow(pp_df))
  # transform to response
  p     <- plogis(draws1)
  mupos <- exp(draws2)
  inc   <- p * mupos
  ci    <- t(apply(inc, 1, quantile, probs = c(0.025, 0.975), na.rm = TRUE))
  as.data.frame(ci) %>% setNames(c("lo","hi"))
}

`%||%` <- function(a,b) if (!is.null(a)) a else b

ci_fac <- add_ci(pp_fac)
ci_sc  <- add_ci(pp_sc)

if (!is.null(ci_fac)) {
  dat_fac$lo <- ci_fac$lo
  dat_fac$hi <- ci_fac$hi
}
if (!is.null(ci_sc)) {
  dat_sc$lo <- ci_sc$lo
  dat_sc$hi <- ci_sc$hi
}

plot_dat_ci <- bind_rows(dat_fac, dat_sc)

# ---- Plot with ribbons if CIs exist; otherwise lines/points only ----
p_season <- ggplot(plot_dat_ci, aes(month, inc_100k, color = model, group = model)) +
  { if (!any(is.na(plot_dat_ci$lo))) geom_ribbon(aes(ymin = lo, ymax = hi, fill = model), alpha = 0.2, color = NA) else NULL } +
  geom_line(size = 1) +
  geom_point() +
  scale_x_continuous(breaks = 1:12) +
  labs(x = "Month", y = "Predicted incidence per 100,000",
       title = "Seasonal effect: factor(month) vs sin+cos") +
  theme_minimal() +
  guides(fill = "none")

print(p_season)

ggsave("Seasonality_per100k_facmon_vs_sincos.png", p_season, width = 8, height = 5, dpi = 300)






## (Optional) Save the individual seasonal curves separately

p_fac <- ggplot(dat_fac, aes(month, inc_100k)) +
  { if (!any(is.na(dat_fac$lo))) geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.2) else NULL } +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = 1:12) +
  labs(x = "Month", y = "Incidence per 100k", title = "factor(month) – NB1 AR1") +
  theme_minimal()

p_sc <- ggplot(dat_sc, aes(month, inc_100k)) +
  { if (!any(is.na(dat_sc$lo))) geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.2) else NULL } +
  geom_line() + geom_point() +
  scale_x_continuous(breaks = 1:12) +
  labs(x = "Month", y = "Incidence per 100k", title = "sin+cos – NB1 AR1") +
  theme_minimal()

ggsave("Seasonality_facmon_NB1_AR1.png", p_fac, width = 7, height = 5, dpi = 300)
ggsave("Seasonality_sincos_NB1_AR1.png", p_sc, width = 7, height = 5, dpi = 300)






