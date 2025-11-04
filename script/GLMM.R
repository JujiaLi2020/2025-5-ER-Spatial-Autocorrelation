# ---- Packages ----
library(dplyr)
library(glmmTMB)
library(ggplot2)
library(DHARMa)

# ---- Data prep ----
df <- er_year_df %>%
  mutate(
    year_c  = as.numeric(scale(year, center = TRUE, scale = FALSE)),
    log_pop = log(pop)
  )

ctrl <- glmmTMBControl(optimizer = optim, optArgs = list(method = "BFGS"))

# Helper: safe fit returning NULL on failure
safe_fit <- function(expr) tryCatch(expr, error = function(e) NULL)

# ---- Model grid (no mesh; county = grouping unit) ----
# GLM (no county RE)
fit_glm_pois <- safe_fit(glmmTMB(Count ~ year_c, data = df, family = poisson(),
                                 offset = log_pop, control = ctrl))
fit_glm_nb2  <- safe_fit(glmmTMB(Count ~ year_c, data = df, family = nbinom2(),
                                 offset = log_pop, control = ctrl))

# ST-only: county-specific slope (no intercept RE)
fit_st_pois  <- safe_fit(glmmTMB(Count ~ year_c + (0 + year_c | County),
                                 data = df, family = poisson(),  offset = log_pop, control = ctrl))
fit_st_nb2   <- safe_fit(glmmTMB(Count ~ year_c + (0 + year_c | County),
                                 data = df, family = nbinom2(), offset = log_pop, control = ctrl))

# Spatial-only: county intercept RE (no slope RE)
fit_sp_pois  <- safe_fit(glmmTMB(Count ~ year_c + (1 | County),
                                 data = df, family = poisson(),  offset = log_pop, control = ctrl))
fit_sp_nb2   <- safe_fit(glmmTMB(Count ~ year_c + (1 | County),
                                 data = df, family = nbinom2(), offset = log_pop, control = ctrl))

# Both: county intercept + slope RE  *** (what AIC favored earlier)
fit_both_pois <- safe_fit(glmmTMB(Count ~ year_c + (1 + year_c | County),
                                  data = df, family = poisson(),  offset = log_pop, control = ctrl))
fit_both_nb2  <- safe_fit(glmmTMB(Count ~ year_c + (1 + year_c | County),
                                  data = df, family = nbinom2(), offset = log_pop, control = ctrl))

# Optional Tweedie with fixed p for stability
tweedie_fixed <- tweedie(link = "log", p = 1.5)
fit_both_twd  <- safe_fit(glmmTMB(Count ~ year_c + (1 + year_c | County),
                                  data = df, family = tweedie_fixed, offset = log_pop, control = ctrl))

# ---- AIC table ----
fits <- list(
  glm_pois  = fit_glm_pois,  glm_nb2  = fit_glm_nb2,
  st_pois   = fit_st_pois,   st_nb2   = fit_st_nb2,
  sp_pois   = fit_sp_pois,   sp_nb2   = fit_sp_nb2,
  both_pois = fit_both_pois, both_nb2 = fit_both_nb2,
  both_twd  = fit_both_twd
)

# keep successful fits and compute AIC safely
fits_ok <- Filter(function(x) inherits(x, "glmmTMB") && is.finite(suppressWarnings(as.numeric(logLik(x)))), fits)

aic_tab <- data.frame(
  model = names(fits_ok),
  AIC   = sapply(fits_ok, function(m) as.numeric(AIC(m))),
  pdHess = sapply(fits_ok, function(m) isTRUE(m$sdr$pdHess)),
  row.names = NULL
) |> arrange(AIC)

print(aic_tab)

# ---- Choose best model (NB2 + intercept & slope RE) ----
fit_best <- fit_both_nb2

# Summary + DHARMa diagnostics
summary(fit_best)
res <- simulateResiduals(fit_best); plot(res)

# ---- County-specific effects (intercept & slope totals) ----
fx <- fixef(fit_best)$cond
re <- ranef(fit_best)$cond$County
re$County <- rownames(re)

county_effects <- re |>
  mutate(
    intercept_total = fx["(Intercept)"] + `(Intercept)`,
    slope_total     = fx["year_c"]      + year_c
  ) |>
  arrange(desc(slope_total))

head(county_effects[, c("County", "intercept_total", "slope_total")], 10)


library(stringr)

# Fixed effects
fx  <- fixef(fit_best)$cond
# Random effects per county
re  <- ranef(fit_best)$cond$County
re$County <- rownames(re)

# Add parameters
library(dplyr)
library(stringr)

# Assuming `county_effects` already exists with:
# County, intercept_total_log, slope_total_log, baseline_rate_mil, annual_multiplier, annual_change_pct

county_table_paper <- county_effects %>%
  mutate(
    County = str_to_title(tolower(County))  # Capitalize only first letter of each word
  ) %>%
  rename(
    `Intercept (log)` = intercept_total_log,
    `Slope (log/year)` = slope_total_log,
    `Baseline rate (/1M pop)` = baseline_rate_mil,
    `Annual multiplier` = annual_multiplier,
    `Annual change (%)` = annual_change_pct
  )

# Export to CSV for paper
write.csv(county_table_paper, "county_model_parameters.csv", row.names = FALSE)

# Preview first 10 rows
head(county_table_paper, 10)





# ---- Optional curvature in time ----
fit_poly <- update(fit_best, . ~ year_c + I(year_c^2) + (1 + year_c | County))
AIC(fit_best, fit_poly)

# ---- Predicted trajectories by county ----
newdat <- df |>
  group_by(County) |>
  summarise(year = seq(min(year), max(year)), .groups = "drop") |>
  mutate(
    year_c  = as.numeric(scale(year, center = TRUE, scale = FALSE)),
    # Prefer county-year population if available; else mean pop as placeholder
    log_pop = ave(df$log_pop, df$County, FUN = mean)[match(County, df$County)]
  )

pred <- predict(fit_best, newdata = newdat, type = "response", se.fit = TRUE)
newdat$mu <- pred$fit; newdat$se <- pred$se.fit

ggplot(newdat, aes(year, mu)) +
  geom_ribbon(aes(ymin = mu - 1.96 * se, ymax = mu + 1.96 * se), alpha = .2) +
  geom_line() +
  facet_wrap(~ County, scales = "free_y") +
  labs(y = "Predicted count", x = "Year",
       title = "Predicted ER counts by county (NB2 with random intercept & slope)") +
  theme_minimal()

# Effect size per 1-year change (overall)
exp(fx["year_c"])
