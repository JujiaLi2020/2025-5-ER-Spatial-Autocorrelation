library(dplyr)
library(readr)
library(lubridate)
library(tidyr)
library(stringr)
library(janitor)
library(sdmTMB)
library(tigris)
library(sf)
###############################

#repos <- "/Users/yuihuiyao/Library/CloudStorage/Box-Box/2025-5-ER-Spatial-Autocorrelation"
#repos <- "C:/Users/bolus/Box/2025-5-ER-Spatial-Autocorrelation"
repos <- "/Users/julia/Box/Personal/Research/2025-5-ER-Spatial-Autocorrelation"

file_path <- paste(repos, "/Data/ER_opioid_2016_2019_cleaned.csv", sep = "")
if (!file.exists(file_path)) {
  stop("File not found: Data/ER_opioid_2016_2019_cleaned.csv")
}
er_data <- read_csv(file_path, show_col_types = FALSE)

# Keep a subset of North / Central AL (as in your script)
keep_cnty <- toupper(c(
  "Lauderdale","Limestone","Madison","Jackson","Colbert","Franklin",
  "Lawrence","Morgan","Marshall","Dekalb","Marion","Winston",
  "Cullman","Blount","Etowah","Cherokee","Lamar","Fayette",
  "Walker","Jefferson","St. Clair","Pickens","Tuscaloosa","Shelby"
))

er_data <- er_data |>
  mutate(County = toupper(County),
         Date = as.Date(Date)) |>
  filter(County %in% keep_cnty)


# Read Data_ARCOS and clean for next process
Data_ARCOS <- read_csv(paste(repos, "/BigData/ARCOS_AL_2016_2019_cleaned.csv", sep = ""))

Data_ARCOS_filted <- Data_ARCOS%>%
  filter(drug %in% c("OXYCODONE", "HYDROCODONE", "BUPRENORPHINE", "METHADONE"))%>%
  filter(county %in% toupper(c(
    "Lauderdale", "Limestone", "Madison", "Jackson", "Colbert", "Franklin",
    "Lawrence", "Morgan", "Marshall", "Dekalb", "Marion", "Winston",
    "Cullman", "Blount", "Etowah", "Cherokee", "Lamar", "Fayette",
    "Walker", "Jefferson", "St. Clair", "Pickens", "Tuscaloosa", "Shelby"
  ))
  )%>%
  #mutate(mme = mme/1000000)%>%
  mutate(Date = mdy(date))%>%
  mutate(County = toupper(county))

arcos_wide_mme <- Data_ARCOS_filted %>%
  pivot_wider(
    id_cols    = c(County, Date),
    names_from = drug,
    values_from= mme,
    values_fill = 0
  )

df_er_drug <- left_join(
  er_data,
  arcos_wide_mme,
  by = c("County", "Date")
)

###############################

# Presence at daily level
df_er_drug <- df_er_drug |>
  mutate(
    year = year(Date),
    presence = if_else(Count > 0, 1L, 0L)
  )

# Aggregate to county × year
df_er_drug_annual <- df_er_drug %>%
  group_by(County, year) %>%
  summarise(
    # population for the year (choose one: mean/first/median)
    pop_year = first(pop),

    # ER annual
    ER_annual = sum(Count, na.rm = TRUE),

    # Drug annual
    OXYCODONE_annual     = sum(OXYCODONE,     na.rm = TRUE),
    HYDROCODONE_annual   = sum(HYDROCODONE,   na.rm = TRUE),
    BUPRENORPHINE_annual = sum(BUPRENORPHINE, na.rm = TRUE),
    METHADONE_annual     = sum(METHADONE,     na.rm = TRUE),

    presence = as.integer(sum(Count, na.rm = TRUE) > 0),
    .groups = "drop"
  ) %>%
  mutate(
    # per-capita (proportion) and per 100k (common epidemiology scale)
    ER_annual_percapita           = ER_annual / pop_year,
    ER_annual_per100k             = ER_annual_percapita * 1e5,

    OXYCODONE_annual_percapita    = OXYCODONE_annual     / pop_year,
    OXYCODONE_annual_per100k      = OXYCODONE_annual_percapita * 1e5,

    HYDROCODONE_annual_percapita  = HYDROCODONE_annual   / pop_year,
    HYDROCODONE_annual_per100k    = HYDROCODONE_annual_percapita * 1e5,

    BUPRENORPHINE_annual_percapita = BUPRENORPHINE_annual / pop_year,
    BUPRENORPHINE_annual_per100k   = BUPRENORPHINE_annual_percapita * 1e5,

    METHADONE_annual_percapita    = METHADONE_annual     / pop_year,
    METHADONE_annual_per100k      = METHADONE_annual_percapita * 1e5
  )

###############################

mortality <- read_csv(paste(repos, "/Data/CDCWonder Opioid Related Mortality AL 2016-2019.csv", sep = ""))

mortality_filted <- mortality%>%
  mutate(County = toupper(gsub(" County, AL", "", County)))%>%
  filter(County %in% toupper(c(
    "Lauderdale", "Limestone", "Madison", "Jackson", "Colbert", "Franklin",
    "Lawrence", "Morgan", "Marshall", "Dekalb", "Marion", "Winston",
    "Cullman", "Blount", "Etowah", "Cherokee", "Lamar", "Fayette",
    "Walker", "Jefferson", "St. Clair", "Pickens", "Tuscaloosa", "Shelby"
  )))%>%
  mutate(
    Deaths = suppressWarnings(as.numeric(Deaths)),      # "Suppressed" → NA
    CrudeRate = suppressWarnings(as.numeric(CrudeRate)),
    deaths_per100k = Deaths / Population * 1e5
  )%>%
  rename(year = Year,
         pop_CDC=Population)


# Merge
df_er_drug_mortality <- left_join(
  df_er_drug_annual,
  mortality_filted,
  by = c("County", "year")
)

###############################

acs <- read_csv(paste(repos, "/Data/ACS 2019 Data Subset.csv", sep = ""))



# 1) Build a safe, readable column name from Concept + Label
clean_colname <- function(concept, label) {
  # drop leading "Estimate!!"
  label2 <- str_remove(label, "^Estimate!!")
  # collapse "!!" / ":" and other punctuation to separators
  label2 <- str_replace_all(label2, "!!", "_")
  label2 <- str_replace_all(label2, "[:;,-]+", "")
  # build combined name, then snake case + de-duplicate
  nm <- paste(concept, label2, sep = "__")
  # nm <- janitor::make_clean_names(nm)
  nm
}

acs2 <- acs %>%
  mutate(
    colname = clean_colname(Concept, Label)
  )

# 2) If there are duplicates per (State, County, Full_FIPS, colname), choose the first non-NA
acs_collapsed <- acs2 %>%
  group_by(State, County, Full_FIPS, colname) %>%
  summarise(Value = dplyr::first(na.omit(Value)), .groups = "drop")


# 3) Pivot to wide
acs_wide <- acs_collapsed %>%
  pivot_wider(
    id_cols    = c(State, County, Full_FIPS),
    names_from = colname,
    values_from = Value,
    values_fill = NA_real_
  ) %>%
  clean_names()%>%
  mutate(CountyCode = full_fips)# final pass to ensure valid names

# Merge three imputated mortality
df_er_drug_mortality_acs <- left_join(
  df_er_drug_mortality,
  acs_wide,
  by = c("CountyCode")
  )
#summary(mortality_filted)

###############################

df_er_drug_mortality_acs <- df_er_drug_mortality_acs%>%
  mutate(
    unemployee_rate = employment_status_for_the_population_16_years_and_over_total_in_labor_force_civilian_labor_force_unemployed/employment_status_for_the_population_16_years_and_over_total_in_labor_force_civilian_labor_force,

    gini_index = gini_index_of_income_inequality_gini_index,

    median_income = median_household_income_in_the_past_12_months_in_2021_inflation_adjusted_dollars_median_household_income_in_the_past_12_months_in_2021_inflationadjusted_dollars,

    poverty_rate = poverty_status_in_the_past_12_months_by_sex_by_age_total_income_in_the_past_12_months_below_poverty_level/poverty_status_in_the_past_12_months_by_sex_by_age_total,

    # sum all columns about disability
    disability = rowSums(
      across(starts_with("sex_by_age_by_disability"),
             ~ as.numeric(.x)),           # ensure numeric
      na.rm = TRUE),
    # (optional) per-capita / per-100k versions
    disability_rate      = disability / total_population_total,
    disability_per100k   = disability_rate * 1e5,

    # sum all columns about noinsurance
    noinsurance = rowSums(
      across(starts_with("types_of_health_insurance_coverage"),
             ~ replace_na(as.numeric(.x), 0)),
      na.rm = TRUE
    ),
    # Optional: per-capita or per-100k version
    noinsurance_rate    = noinsurance / total_population_total,
    noinsurance_per100k = noinsurance_rate * 1e5)

###############################

df_select <- df_er_drug_mortality_acs%>%
  select(
    County,
    year,
    pop_CDC,
    # Deaths
    Deaths,
    # ER visit
    ER_annual_percapita,
    # Drugs
    OXYCODONE_annual_percapita, HYDROCODONE_annual_percapita,   BUPRENORPHINE_annual_percapita, METHADONE_annual_percapita,

    # ACS
    unemployee_rate,    gini_index, poverty_rate,   disability_rate, noinsurance_rate
    )

###############################

scale_vars <-  c(
    # Deaths
    "Deaths",
    # ER visit
    "ER_annual_percapita",
    # Drugs
    "OXYCODONE_annual_percapita",   "HYDROCODONE_annual_percapita", "BUPRENORPHINE_annual_percapita", "METHADONE_annual_percapita",

    # ACS
    "unemployee_rate",  "gini_index",   "poverty_rate", "disability_rate", "noinsurance_rate"
    )

df_select_z <- df_select %>%
  mutate(Deaths_rate= Deaths / pop_CDC)%>%
  mutate(across(all_of(scale_vars), ~ as.numeric(scale(.x)), .names = "{.col}_z"))


###############################
# initialization
###############################

###############################

df_select_z$time_index <- df_select_z$year - 2018
df_select_z$intervention2017 <- as.numeric(df_select_z$year >= 2018)
df_select_z_int <- df_select_z
#df_select_z_int$Deaths[is.na(df_select_z_int$Deaths)] <- (9+1)/2

df_select_z_int$rural <- 0

df_select_z_int$rural[which(df_select_z_int$County == "BLOUNT")] <- 2
df_select_z_int$rural[which(df_select_z_int$County == "CHEROKEE")] <- 6
df_select_z_int$rural[which(df_select_z_int$County == "COLBERT")] <- 4
df_select_z_int$rural[which(df_select_z_int$County == "CULLMAN")] <- 5
df_select_z_int$rural[which(df_select_z_int$County == "ETOWAH")] <- 4
df_select_z_int$rural[which(df_select_z_int$County == "FAYETTE")] <- 6
df_select_z_int$rural[which(df_select_z_int$County == "FRANKLIN")] <- 5
df_select_z_int$rural[which(df_select_z_int$County == "JACKSON")] <- 5
df_select_z_int$rural[which(df_select_z_int$County == "JEFFERSON")] <- 2
df_select_z_int$rural[which(df_select_z_int$County == "LAMAR")] <- 6
df_select_z_int$rural[which(df_select_z_int$County == "LAUDERDALE")] <- 4
df_select_z_int$rural[which(df_select_z_int$County == "LAWRENCE")] <- 4
df_select_z_int$rural[which(df_select_z_int$County == "LIMESTONE")] <- 3
df_select_z_int$rural[which(df_select_z_int$County == "MADISON")] <- 3
df_select_z_int$rural[which(df_select_z_int$County == "MARION")] <- 6
df_select_z_int$rural[which(df_select_z_int$County == "MARSHALL")] <- 5
df_select_z_int$rural[which(df_select_z_int$County == "MORGAN")] <- 4
df_select_z_int$rural[which(df_select_z_int$County == "PICKENS")] <- 3
df_select_z_int$rural[which(df_select_z_int$County == "SHELBY")] <- 2
df_select_z_int$rural[which(df_select_z_int$County == "TUSCALOOSA")] <- 3
df_select_z_int$rural[which(df_select_z_int$County == "WALKER")] <- 2
df_select_z_int$rural[which(df_select_z_int$County == "WINSTON")] <- 6

df_select_z_int$rural_factor <- factor(df_select_z_int$rural)

###############################


counties_sf <- tigris::counties(state = "AL", cb = TRUE, year = 2023) |>
  st_transform(26916) |>
  mutate(County = toupper(NAME))

coords_m  <- st_coordinates(st_centroid(st_geometry(counties_sf)))
county_xy <- counties_sf |>
  st_drop_geometry() |>
  transmute(
    County,
    X = coords_m[, "X"] / 1000,  # km
    Y = coords_m[, "Y"] / 1000
  )

# Merge coords into model frame
df <- df_select_z_int |>
  left_join(county_xy, by = "County") |>
  filter(is.finite(X), is.finite(Y), is.finite(pop_CDC))%>%
  mutate(time_index = match(year, sort(unique(year))),
         log_pop    = log(pop_CDC))

mesh <- make_mesh(
  df,
  xy_cols = c("X","Y"),
  cutoff = diff(range(df$X, na.rm = TRUE)) / 10
)

###############################

df$time_index_factor <- factor(df$time_index)

###############################


getFit <- function(df, df_select_z_int, formula, family = poisson(link = "log"), spatial = "off", spatiotemporal = "off") {

  tr_ex <- function(mu, lower = 1, upper = 9) {
    mu_tr <- sum(((lower:upper) * dpois(lower:upper, mu))) / (ppois(upper, mu) - ppois(lower - 1, mu))
    mu_tr <- ifelse(mu_tr > 9, 9, mu_tr)
    mu_tr <- ifelse(mu_tr < 1, 1, mu_tr)
  }

  tr_ex <- Vectorize(tr_ex, "mu")

  df$Deaths[is.na(df_select_z_int$Deaths)] <- (9+1)/2

  ###############################


  log_lik_old <- -Inf
  log_lik_new <- Inf
  diff_lik <- Inf
  si <- -1
  tol <- 1e-4

  r <- 0

  previous_fit <- NULL

    mesh <- make_mesh(
      df,
      xy_cols = c("X","Y"),
      cutoff = diff(range(df$X, na.rm = TRUE)) / 10
    )

  while ((diff_lik > tol) & (si == -1)) {
    r <- r + 1

    cat("r:", r, ", log_lik:", log_lik_old, "\n")
    fit <- try(sdmTMB(
      formula = formula,
      data = df,
      mesh = mesh,
      time = "time_index",
      family  = family,
      spatial = spatial,
      spatiotemporal = spatiotemporal,
      offset  = df$log_pop,
      previous_fit = previous_fit
    ), silent = TRUE)

    if (class(fit)[1] == "try-error") {
      cat("  sdmTMB fitting failed at iteration", r, "\n")
      break
    }

    previous_fit <- fit

    if (r == 1) {
      init_fit <- previous_fit
    }

    log_lik_new <- logLik(fit)

    fitted_value <- fitted(fit)

    tmp_fitted_value <- fitted_value[is.na(df_select_z_int$Deaths)]
    tmp_fitted_value <- tr_ex(tmp_fitted_value)

    df$Deaths[is.na(df_select_z_int$Deaths)] <- tmp_fitted_value

    diff_lik <- (log_lik_old - log_lik_new)
    si <- sign(log_lik_old - log_lik_new)
    diff_lik <- abs(diff_lik)

    log_lik_old <- log_lik_new

  }

  list("init_fit" = init_fit, "fit" = previous_fit)

}

cv_getFit_loo <- function(df, df_select_z_int, formula,
                          family = poisson(link = "log"), spatial = "off", spatiotemporal = "off") {

  n <- nrow(df)

  notna <- which(!is.na(df$Deaths))
  K <- length(notna)

  cv_st <- matrix(NA_real_, nrow = K, ncol = 4)
  colnames(cv_st) <- c("IM_RMSE", "IM_MAE","EM_RMSE", "EM_MAE")

  r <- 0
  for (k in notna) {
    r <- r + 1
    cat("  Fold", r, "of", K, "\n")
    test_idx  <- k
    train_idx <- 1:n
    train_idx <- train_idx[train_idx != test_idx]

    df_train        <- df[train_idx, , drop = FALSE]
    df_select_train <- df_select_z_int[train_idx, , drop = FALSE]
    df_test         <- df[test_idx, , drop = FALSE]

    # Fit model on training data
      fit_k <- try(getFit(df_train, df_select_train, formula,
                          family, spatial = spatial, spatiotemporal = spatiotemporal), silent = TRUE)


    if (class(fit_k)[1] == "try-error") {
      cv_st[r, "IM_RMSE"] <- NA
      cv_st[r, "IM_MAE"] <- NA
      cv_st[r, "EM_RMSE"] <- NA
      cv_st[r, "EM_MAE"] <- NA
      next
    }

    # Predict on test data
    pred_init <- try(predict(fit_k$init_fit, newdata = df_test, offset = df_test$log_pop, type = "response"), silent = TRUE)

    if (class(pred_init)[1] == "try-error") {
      cv_st[r, "IM_RMSE"] <- NA
      cv_st[r, "IM_MAE"] <- NA
    } else {
      pred_init <- pred_init$est
      # Use only rows with observed Deaths
      pred_notna    <- pred_init[!is.na(df_test$Deaths)]
      deaths_notna  <- df_test$Deaths[!is.na(df_test$Deaths)]

      rmse <- (((pred_notna - deaths_notna)^2))
      mae <- (abs(pred_notna - deaths_notna))
      cv_st[r, "IM_RMSE"] <- rmse
      cv_st[r, "IM_MAE"] <- mae
    }



    pred <- try(predict(fit_k$fit, newdata = df_test, offset = df_test$log_pop, type = "response"), silent = TRUE)

    if (class(pred)[1] == "try-error") {
      cv_st[r, "EM_RMSE"] <- NA
      cv_st[r, "EM_MAE"] <- NA
    } else {
      pred <- pred$est
      # Use only rows with observed Deaths
      pred_notna    <- pred[!is.na(df_test$Deaths)]
      deaths_notna  <- df_test$Deaths[!is.na(df_test$Deaths)]

      rmse <- (((pred_notna - deaths_notna)^2))
      mae <- (abs(pred_notna - deaths_notna))
      cv_st[r, "EM_RMSE"] <- rmse
      cv_st[r, "EM_MAE"] <- mae
    }


  }

  cv_st <- colMeans(cv_st, na.rm = TRUE)
  cv_st[c(1, 3)] <- sqrt(cv_st[c(1, 3)])
  cv_st
}


###############################



###############################

cor_df <- df %>% select(
  ER_annual_percapita_z,
  OXYCODONE_annual_percapita_z,
  HYDROCODONE_annual_percapita_z,
  BUPRENORPHINE_annual_percapita_z,
  METHADONE_annual_percapita_z,
  unemployee_rate_z,
  gini_index_z,
  poverty_rate_z,
  disability_rate_z,
  noinsurance_rate_z
)

corrplot::corrplot(cor(cor_df, method = "spearman"))

###############################

full_formula <- Deaths ~ ER_annual_percapita_z #+
  #OXYCODONE_annual_percapita_z +
  #HYDROCODONE_annual_percapita_z +
  #BUPRENORPHINE_annual_percapita_z +
  #METHADONE_annual_percapita_z +
  #unemployee_rate_z +
  #gini_index_z +
  #poverty_rate_z +
  #disability_rate_z +
  #noinsurance_rate_z +
  #rural_factor

fit0 <- getFit(df, df_select_z_int, full_formula)

##car::vif(fit0$fit)

#################################
##  #remove the highest VIF until all less than 5
#################################
##
##wrapper <- function(X, formulas, df, df_select_z_int) {
##  
##  formula <- formulas[[X]]
##  
##  cv <- list()
##  
##  cv[[1]] <- try(cv_getFit_loo(df, df_select_z_int, formula, spatial = "off", spatiotemporal = "off"), silent = TRUE)
##  cv[[2]] <- try(cv_getFit_loo(df, df_select_z_int, formula, spatial = "on", spatiotemporal = "off"), silent = TRUE)
##  cv[[3]] <- try(cv_getFit_loo(df, df_select_z_int, formula, spatial = "off", spatiotemporal = "ar1"), silent = TRUE)
##  cv[[4]] <- try(cv_getFit_loo(df, df_select_z_int, formula, spatial = "on", spatiotemporal = "ar1"), silent = TRUE)
##  #cv[[5]] <- try(cv_getFit_loo(df, df_select_z_int, formula, family = nbinom2(link = "log"), spatial = "off", spatiotemporal = "off"), silent = TRUE)
##  #cv[[6]] <- try(cv_getFit_loo(df, df_select_z_int, formula, family = nbinom2(link = "log"), spatial = "on", spatiotemporal = "off"), silent = TRUE)
##  #cv[[7]] <- try(cv_getFit_loo(df, df_select_z_int, formula, family = nbinom2(link = "log"), spatial = "off", spatiotemporal = "ar1"), silent = TRUE)
##  #cv[[8]] <- try(cv_getFit_loo(df, df_select_z_int, formula, family = nbinom2(link = "log"), spatial = "on", spatiotemporal = "ar1"), silent = TRUE)
##
##  out <- NULL
##  
##  for (i in 1:4) {
##    if (class(cv[[i]]) != "try-error") {
##      out <- c(out, cv[[i]])
##    } else {
##      out <- c(out, rep(NA, 4))
##    }
##  }
##  
##  out
##  
##}
##
##
##
##base_vars <- c(
##  "ER_annual_percapita_z",
##  "OXYCODONE_annual_percapita_z",
##  "BUPRENORPHINE_annual_percapita_z",
##  "METHADONE_annual_percapita_z",
##  "rural_factor"
##)
##
##covars <- c("poverty_rate_z", "disability_rate_z", "noinsurance_rate_z", "rural_factor")
##
##make_formula <- function(vars){
##  rhs_main <- if(length(vars) > 0){
##    paste0("(", paste(vars, collapse = " + "), ") * intervention2017")
##  } else {
##    "intervention2017"
##  }
##  rhs_cov <- paste(covars, collapse = " + ")
##  as.formula(paste("Deaths ~", rhs_main, "+", rhs_cov))
##}
##
##make_formula1 <- function(vars){
##  rhs_main <- if(length(vars) > 0){
##    paste0("(", paste(vars, collapse = " + "), ") + intervention2017")
##  } else {
##    "intervention2017"
##  }
##  rhs_cov <- paste(covars, collapse = " + ")
##  as.formula(paste("Deaths ~", rhs_main, "+", rhs_cov))
##}
##
##subsets <- unlist(
##  lapply(0:length(base_vars), function(k)
##    combn(base_vars, k, simplify = FALSE)
##  ),
##  recursive = FALSE
##)
##
##formulas <- NULL
##
##formulas <- lapply(subsets, make_formula)
##names(formulas) <- sapply(subsets, function(v)
##  if(length(v) == 0) "none" else paste(v, collapse = "_")
##)
##
##formulas1 <- NULL
##
##formulas1 <- lapply(subsets, make_formula1)
##names(formulas) <- sapply(subsets, function(v)
##  if(length(v) == 0) "none" else paste(v, collapse = "_")
##)
##
##
##formulas2 <- list()
##
##for  (i in 1:length(formulas)) {
##  formulas2[[i]] <- formulas[[i]]
##  formulas2[[length(formulas) + (i)]] <- formulas1[[i]]
##}
##
##
##nn <- length(formulas2)
##
##formulas2[[nn + 1]] <- Deaths ~ ER_annual_percapita_z
##formulas2[[nn + 2]] <- Deaths ~ OXYCODONE_annual_percapita_z
##formulas2[[nn + 3]] <- Deaths ~ BUPRENORPHINE_annual_percapita_z
##formulas2[[nn + 4]] <- Deaths ~ METHADONE_annual_percapita_z
##formulas2[[nn + 5]] <- Deaths ~ poverty_rate_z
##formulas2[[nn + 6]] <- Deaths ~ disability_rate_z
##formulas2[[nn + 7]] <- Deaths ~ noinsurance_rate_z
##formulas2[[nn + 8]] <- Deaths ~ rural_factor
##formulas2[[nn + 9]] <- Deaths ~ intervention2017
##
##
#################################
##
##nform <- length(formulas2)
##
##cl <- parallel::makeCluster(min(floor(parallel::detectCores() * 0.8), nform))
##parallel::clusterExport(cl, c("getFit", "cv_getFit_loo", "wrapper"))
##parallel::clusterEvalQ(cl, library(dplyr))
##parallel::clusterEvalQ(cl, library(readr))
##parallel::clusterEvalQ(cl, library(lubridate))
##parallel::clusterEvalQ(cl, library(tidyr))
##parallel::clusterEvalQ(cl, library(stringr))
##parallel::clusterEvalQ(cl, library(janitor))
##parallel::clusterEvalQ(cl, library(sdmTMB))
##parallel::clusterEvalQ(cl, library(tigris))
##parallel::clusterEvalQ(cl, library(sf))
##
##
##out <- parallel::parLapplyLB(cl, X = 1:nform, fun = wrapper, formulas = formulas2, df = df, df_select_z_int = df_select_z_int)
##
##save(out, file = paste(repos, "out.Rdat"))
##
##closeAllConnections()
##
##outmatrix <- matrix(NA, nrow = length(out), ncol = length(out[[1]]))
##
##for (i in 1:length(out)) {
##  
##  outmatrix[i, ] <- out[[i]]
##  
##  
##}
##
##
##
##final_fit <- getFit(df, df_select_z_int, formulas2[[which(outmatrix == min(outmatrix[, c(2, 4, 6, 8, 10, 12, 14 ,16)]), arr.ind = TRUE)[1]]], spatial = "on", spatiotemporal = "off")
##