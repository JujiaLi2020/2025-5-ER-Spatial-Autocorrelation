library(dplyr)
library(stringr)
library(readr)
library(sdmTMB)
library(glmmTMB)     # optional, if comparing GLMM
library(DHARMa)
library(sf)

### 3. Spatiotemporal Analysis

# Get Geographic information
#invisible(install.packages("tigris"))
invisible(library(tigris))

# Prepare for repr system to render spatial (sf) objects in rich formats (like maps) inside Jupyter Notebooks
#invisible(install.packages("geojsonio"))
invisible(library(geojsonio))


# Prepare sdmTMB package and sf files reading
#invisible(install.packages("sdmTMB"))
head(al_counties_filted)


# Read AL counties's geographic information from tigris package
al_counties <- counties(state = "AL", cb = TRUE, class = "sf")


# Mutate County variable
al_counties$County <- toupper(al_counties$NAME) 

# Filter Counties located at northwest of AL
al_counties_filted <- al_counties%>%
  filter(County %in% toupper(c(
    "Lauderdale", "Limestone", "Madison", "Jackson", "Colbert", "Franklin",
    "Lawrence", "Morgan", "Marshall", "Dekalb", "Marion", "Winston",
    "Cullman", "Blount", "Etowah", "Cherokee", "Lamar", "Fayette",
    "Walker", "Jefferson", "St. Clair", "Pickens", "Tuscaloosa", "Shelby"
  ))
  )


# Join ER data and GIS on County
ER_GIS <- left_join(Data_county_summary_annual,al_counties_filted, by = c("County"))

#write.csv(ER_GIS, "../Data/ER_GIS.csv")

# Convert dataset to sf structure
ER_GIS_sf <- st_as_sf(ER_GIS)


# Plot Spatiotemporal for ER Visits
p <- ggplot(data = ER_GIS_sf) +
  geom_sf(aes(fill = total)) +
  facet_wrap(~ year) +
  scale_fill_viridis_c(option = "plasma") + #"cividis" "viridis"
  labs(
    title = "Opioid-Related ER Visits by County",
    subtitle = "Northwest Alabama"
  ) +
  geom_sf_text(aes(label = County), size = 2, color = "white", fontface = "bold") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )


# Display in notebook
print(p)

# Save the figure
ggsave("output/3_ER_spatiotemporal.png", plot = p, width = 9, height = 6, dpi = 300)




# Plot Spatiotemporal for ER Per Capita Visits
p <- ggplot(data = ER_GIS_sf) +
  geom_sf(aes(fill = avg_percapita)) +
  facet_wrap(~ year) +
  scale_fill_viridis_c(option = "plasma") + #"cividis" "viridis"
  labs(
    title = "Opioid-Related ER Visits Per Capita by County",
    subtitle = "Northwest Alabama"
  ) +
  geom_sf_text(aes(label = County), size = 2, color = "white", fontface = "bold") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )


# Display in notebook
print(p)

# Save the figure
ggsave("output/3_ER_per_spatiotemporal.png", plot = p, width = 9, height = 6, dpi = 300)



Data_BUPRENORPHINE <- Data_County_ARCOS_Annual_long %>%
  filter(drug == "BUPRENORPHINE")

BUPRENORPHINE_GIS <- left_join(Data_BUPRENORPHINE, al_counties_filted, by = c("County"))

write.csv(BUPRENORPHINE_GIS, "Data/BUPRENORPHINE_GIS.csv")

# Convert dataset to sf structure
BUPRENORPHINE_GIS <- st_as_sf(BUPRENORPHINE_GIS)

# Plot Spatiotemporal for Medication Visits
p <- ggplot(data = BUPRENORPHINE_GIS) +
  geom_sf(aes(fill = total)) +
  facet_wrap(~ year) +
  scale_fill_viridis_c(option = "plasma") + #"cividis" "viridis"
  labs(
    title = "Buprenorphine Consumption by County",
    subtitle = "Northwest Alabama"
  ) +
  geom_sf_text(aes(label = County), size = 2, color = "white", fontface = "bold") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )


# Display in notebook
print(p)

# Save the figure
ggsave("output/3_Buprenorphine_spatiotemporal.png", plot = p, width = 9, height = 6, dpi = 300)




# Plot Spatiotemporal for Medication Consumption (Rate)
p <- ggplot(data = BUPRENORPHINE_GIS) +
  geom_sf(aes(fill = rate)) +
  facet_wrap(~ year) +
  scale_fill_viridis_c(option = "plasma") + #"cividis" "viridis"
  labs(
    title = "Buprenorphine Consumption Rate (Per Capita) by County",
    subtitle = "Northwest Alabama"
  ) +
  geom_sf_text(aes(label = County), size = 2, color = "white", fontface = "bold") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )


# Display in notebook
print(p)

# Save the figure
ggsave("output/3_Buprenorphine_rate_spatiotemporal.png", plot = p, width = 9, height = 6, dpi = 300)



Data_HYDROCODONE <- Data_County_ARCOS_Annual_long %>%
  filter(drug == "HYDROCODONE")

HYDROCODONE_GIS <- left_join(Data_HYDROCODONE, al_counties_filted, by = c("County"))

write.csv(HYDROCODONE_GIS, "../Data/HYDROCODONE_GIS.csv")

# Convert dataset to sf structure
HYDROCODONE_GIS <- st_as_sf(HYDROCODONE_GIS)


# Plot Spatiotemporal for Medication Visits
p <- ggplot(data = HYDROCODONE_GIS) +
  geom_sf(aes(fill = total)) +
  facet_wrap(~ year) +
  scale_fill_viridis_c(option = "plasma") + #"cividis" "viridis"
  labs(
    title = "Hydrocodone Consumption by County",
    subtitle = "Northwest Alabama"
  ) +
  geom_sf_text(aes(label = County), size = 2, color = "white", fontface = "bold") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )


# Display in notebook
print(p)

# Save the figure
ggsave("output/3_Hydrocodone_spatiotemporal.png", plot = p, width = 9, height = 6, dpi = 300)




# Plot Spatiotemporal for HYDROCODONE consumption
p <- ggplot(data = HYDROCODONE_GIS) +
  geom_sf(aes(fill = rate)) +
  facet_wrap(~ year) +
  scale_fill_viridis_c(option = "plasma") + #"cividis" "viridis"
  labs(
    title = "Hydrocodone Consumption Rate (Per Capita) by County",
    subtitle = "Northwest Alabama"
  ) +
  geom_sf_text(aes(label = County), size = 2, color = "white", fontface = "bold") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )


# Display in notebook
print(p)

# Save the figure
ggsave("output/3_Hydrocodone_rate_spatiotemporal.png", plot = p, width = 9, height = 6, dpi = 300)




Data_METHADONE <- Data_County_ARCOS_Annual_long %>%
  filter(drug == "METHADONE")

METHADONE_GIS <- left_join(Data_METHADONE, al_counties_filted, by = c("County"))

write.csv(METHADONE_GIS, "Data/METHADONE_GIS.csv")

# Convert dataset to sf structure
METHADONE_GIS <- st_as_sf(METHADONE_GIS)




# Plot Spatiotemporal for Medication Visits
p <- ggplot(data = METHADONE_GIS) +
  geom_sf(aes(fill = total)) +
  facet_wrap(~ year) +
  scale_fill_viridis_c(option = "plasma") + #"cividis" "viridis"
  labs(
    title = "Methadone Consumption by County",
    subtitle = "Northwest Alabama"
  ) +
  geom_sf_text(aes(label = County), size = 2, color = "white", fontface = "bold") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )


# Display in notebook
print(p)

# Save the figure
ggsave("output/3_Methadone_spatiotemporal.png", plot = p, width = 9, height = 6, dpi = 300)




# Plot Spatiotemporal for METHADONE consumption
p <- ggplot(data = METHADONE_GIS) +
  geom_sf(aes(fill = rate)) +
  facet_wrap(~ year) +
  scale_fill_viridis_c(option = "plasma") + #"cividis" "viridis"
  labs(
    title = "Methadone Consumption Rate (Per Capita) by County",
    subtitle = "Northwest Alabama"
  ) +
  geom_sf_text(aes(label = County), size = 2, color = "white", fontface = "bold") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )


# Display in notebook
print(p)

# Save the figure
ggsave("output/3_Methadone_rate_spatiotemporal.png", plot = p, width = 9, height = 6, dpi = 300)




Data_OXYCODONE <- Data_County_ARCOS_Annual_long %>%
  filter(drug == "OXYCODONE")

OXYCODONE_GIS <- left_join(Data_OXYCODONE, al_counties_filted, by = c("County"))

write.csv(OXYCODONE_GIS, "Data/OXYCODONE_GIS.csv")

# Convert dataset to sf structure
OXYCODONE_GIS <- st_as_sf(OXYCODONE_GIS)




# Plot Spatiotemporal for Medication Visits
p <- ggplot(data = OXYCODONE_GIS) +
  geom_sf(aes(fill = total)) +
  facet_wrap(~ year) +
  scale_fill_viridis_c(option = "plasma") + #"cividis" "viridis"
  labs(
    title = "Oxycodone Consumption by County",
    subtitle = "Northwest Alabama"
  ) +
  geom_sf_text(aes(label = County), size = 2, color = "white", fontface = "bold") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )


# Display in notebook
print(p)

# Save the figure
ggsave("output/3_Oxycodone_spatiotemporal.png", plot = p, width = 9, height = 6, dpi = 300)





# Plot Spatiotemporal for OXYCODONE consumption
p <- ggplot(data = OXYCODONE_GIS) +
  geom_sf(aes(fill = rate)) +
  facet_wrap(~ year) +
  scale_fill_viridis_c(option = "plasma") + #"cividis" "viridis"
  labs(
    title = "Oxycodone Consumption Rate (Per Capita) by County",
    subtitle = "Northwest Alabama"
  ) +
  geom_sf_text(aes(label = County), size = 2, color = "white", fontface = "bold") +
  theme_minimal() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )


# Display in notebook
print(p)

# Save the figure
ggsave("output/3_Oxycodone_rate_spatiotemporal.png", plot = p, width = 9, height = 6, dpi = 300)









