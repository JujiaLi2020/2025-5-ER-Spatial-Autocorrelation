install.packages("visreg")
library(visreg)

install.packages("sdmTMBextra")
library(sdmTMBextra)  # provides visreg_delta()

visreg_delta(
  fit_sincos_nb1_ar1,
  xvar   = "year_c",
  model  = 1,   # delta part 1 = presence/absence
  gg     = TRUE
)

visreg_delta(
  fit_sincos_nb1_ar1,
  xvar   = "year_c",
  model  = 1,          # presence/absence part
  gg     = TRUE,
  scale  = "response"  # <-- transforms to probability (0–1)
)



##Step 1. Predictions

# Get predictions for the fitted dataset (you can also build newdata)
#est1 = logit-scale presence LP
#est2 = log-scale positive count LP (includes offset if supplied)

pred <- predict(
  fit_sincos_nb1_ar1,
  newdata = df_use,
  offset  = df_use$log_pop,
  se_fit  = FALSE  # keep it fast
)

## Step 2. Transform predictions
pred <- pred %>%
  mutate(
    p_presence = plogis(est1),     # Pr(Y>0)
    mu_pos     = exp(est2),        # E[Y | Y>0]
    mu_total   = p_presence * mu_pos  # overall expected count
  )


## Step 3. Summarize seasonality
# Probability of at least one case (pr_presence)
# Mean positive incidence (exp_pos)
# Overall expected count (exp_total)

season_summary <- pred %>%
  group_by(month) %>%
  summarise(
    pr_presence = mean(p_presence, na.rm=TRUE),
    exp_pos     = mean(mu_pos, na.rm=TRUE),
    exp_total   = mean(mu_total, na.rm=TRUE),
    .groups="drop"
  )

print(season_summary)


## Step 4. Plot seasonal effects (like vignette Fig. 1 left side)

season_summary_long <- season_summary %>%
  tidyr::pivot_longer(-month, names_to="quantity", values_to="value")

ggplot(season_summary_long, aes(month, value, color=quantity)) +
  geom_line(size=1.2) +
  geom_point() +
  scale_x_continuous(breaks=1:12) +
  labs(x="Month", y="Predicted value",
       title="Delta-NB1 AR1 with sin/cos seasonality",
       color="Quantity") +
  theme_minimal()


ggplot(season_summary_long, aes(month, value)) +
  geom_line(color="steelblue") + geom_point() +
  facet_wrap(~quantity, scales="free_y") +
  labs(x="Month", y="Predicted value",
       title="Delta model components: sincos + NB1 + AR1") +
  theme_minimal()


## Step 5. Map predictions (optional, as in vignette)
# Example: mean across time
map_pred <- pred %>%
  group_by(County) %>%
  summarise(
    pr_presence = mean(p_presence, na.rm = TRUE),  # probability part
    exp_pos     = mean(mu_pos, na.rm = TRUE),      # positive catch part
    exp_total   = mean(mu_total, na.rm = TRUE),    # combined
    .groups = "drop"
  )


# join to county shapefile (as you did earlier with al_counties)
# compute centroids for labeling

# keep geometry by making the sf object the LHS of the join
map_dat <- counties_sf %>%
  mutate(County = toupper(County)) %>%
  left_join(map_pred %>% mutate(County = toupper(County)), by = "County")%>%
  filter(County %in% toupper(c(
    "Lauderdale", "Limestone", "Madison", "Jackson", "Colbert", "Franklin",
    "Lawrence", "Morgan", "Marshall", "Dekalb", "Marion", "Winston",
    "Cullman", "Blount", "Etowah", "Cherokee", "Lamar", "Fayette",
    "Walker", "Jefferson", "St. Clair", "Pickens", "Tuscaloosa", "Shelby"
  )))



ggplot(map_dat) +
  geom_sf(aes(fill = pr_presence), color = "white", size = 0.2) +
  geom_sf_text(
    data = map_dat, 
    aes(label = County,
        color = pr_presence < 0.5),  # TRUE/FALSE split
    size = 3
  ) +
  scale_color_manual(values = c("black", "white"), guide = "none") +
  scale_fill_viridis_c(limits = c(0, 1)) +
  labs(fill="Pr(presence)", title="Delta NB1 AR1 – presence probability") +
  theme_void()



ggplot(map_dat) +
  geom_sf(aes(fill = exp_pos), color="white", size=0.2) +
  geom_sf_text(
    data = map_dat, 
    aes(label = County,
        color = exp_pos < 30),  # TRUE/FALSE split
    size = 3
  ) +
  scale_color_manual(values = c("black", "white"), guide = "none") +
  scale_fill_viridis_c(option="plasma") +
  labs(fill="Mean | presence",
       title="Delta NB1 AR1 – expected count given presence") +
  theme_void()


ggplot(map_dat) +
  geom_sf(aes(fill = exp_total), color="white", size=0.2) +
  geom_sf_text(
    data = map_dat, 
    aes(label = County,
        color = exp_total < 30),  # TRUE/FALSE split
    size = 3
  ) +
  scale_color_manual(values = c("black", "white"), guide = "none") +
  scale_fill_viridis_c(option="magma") +
  labs(fill="Expected count",
       title="Delta NB1 AR1 – overall expected count") +
  theme_void()























