library(readr)
library(dplyr)
library(psych)
library(ggplot2)
library(forcats)
# install.packages("tidyverse")
library(tidyverse)
#install.packages(c("spdep", "sf"))  # Only once
library(spdep)
library(sf)
library(stringr)

# Create folder
if (!dir.exists("output")) {
  dir.create("output")
}


# Read and Clean Data_ER
Data_ER <- read_csv("Data/ER_opioid_2016_2019_cleaned.csv")

Data_ER_filted <- Data_ER%>%
  filter(County %in% toupper(c(
    "Lauderdale", "Limestone", "Madison", "Jackson", "Colbert", "Franklin",
    "Lawrence", "Morgan", "Marshall", "Dekalb", "Marion", "Winston",
    "Cullman", "Blount", "Etowah", "Cherokee", "Lamar", "Fayette",
    "Walker", "Jefferson", "St. Clair", "Pickens", "Tuscaloosa", "Shelby"
  ))
  )

Data_ER_filted <- Data_ER_filted %>%
  mutate(
    year = as.numeric(format(Date, "%Y")),
    month = as.numeric(format(Date, "%m")),
    year_month = paste(year,month),
    week = as.numeric(format(Date, "%U"))  # Week number of year (starting from 0)
  )


# Summary of Total Visit by County
Data_county_summary_annual <- Data_ER_filted %>%
  group_by(year, County) %>%
  summarise(
    mean = mean(Count, na.rm = TRUE),
    total = sum(Count),
    avg_percapita = mean(case_percapita)
  )

Data_county_summary_total <- Data_county_summary_annual %>%
  select(year, County, total) %>%
  pivot_wider(
    names_from = year,
    values_from = total,
    names_prefix = "Total_"
  )%>%
  mutate(
    Total = rowSums(select(., starts_with("Total_")), na.rm = TRUE)
  )


Data_county_summary_total

write.csv(Data_county_summary_total, "output/ER_Total.csv")

# Summary of Per Capita Visit (per 1000000 people) by County
Data_County_summary_per <- Data_ER_filted %>%
  group_by(year, County) %>%
  summarise(
    mean = mean(Count, na.rm = TRUE),
    total = sum(Count),
    avg_pop = mean(pop, na.rm = TRUE),
    #avg_percapita = round(1000000 * mean(case_percapita), 2),
    avg_percapita = round(1000 * total/avg_pop, 2),
    .groups = "drop"
  )%>%
  select(year,  County, avg_percapita) %>%
  pivot_wider(
    names_from = year,
    values_from = avg_percapita,
    names_prefix = "Rate_"
  )%>%
  mutate(
    Avg_Rate = round(rowMeans(select(., starts_with("Rate_")), na.rm = TRUE), 2)
  )

head(Data_County_summary_per)



# Calculate average population by County across four years
County_population_average <- Data_ER_filted %>%
  group_by(year, County) %>%
  summarise(
    pop_avg = mean(pop, na.rm = TRUE),
    .groups = "drop")%>%
  select(year,  County, pop_avg) %>%
  pivot_wider(
    names_from = year,
    values_from = pop_avg,
    names_prefix = "Population_"
  )

head(County_population_average)


# merge all tables by County
Data_County_pop_sum <- merge(
  County_population_average,
  Data_County_summary_total, 
  by = "County"
)

Data_County_pop_sum_rate <- merge(
  Data_County_pop_sum,
  Data_County_summary_per, 
  by = "County"
)

Data_County_combined <- Data_County_pop_sum_rate%>%
  select(
    County,
    Population_2016, Total_2016, Rate_2016,
    Population_2017, Total_2017, Rate_2017,
    Population_2018, Total_2018, Rate_2018,
    Population_2019, Total_2019, Rate_2019,
    Total, Avg_Rate
  )

head(Data_County_combined)

# Save to CSV
write.csv(Data_County_combined, paste0(main.folder,"/output/County_summary.csv"), row.names = FALSE)




################################################################################
### 2. Descriptive Statistic for Medicine Consumption
# Read Data_ARCOS and clean for next process
Data_ARCOS <- read_csv("Data/ARCOS_AL_2016_2019_cleaned.csv")

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
  mutate(date = mdy(date))%>%
  rename(County = county)

Data_ARCOS_filted <- Data_ARCOS_filted %>%
  mutate(
    year = as.numeric(format(date, "%Y")),
    month = as.numeric(format(date, "%m")),
    year_month = paste(year,month),
    week = as.numeric(format(date, "%U"))  # Week number of year (starting from 0)
  )

head(Data_ARCOS_filted)



# Summary of Per Capita Drug Consumption by County
Data_County_ARCOS_Annual_long <- Data_ARCOS_filted %>%
  group_by(year, County, drug) %>%
  summarise(
    mean = mean(mme, na.rm = TRUE),
    total = sum(mme/1000),
    pop_avg = mean(pop, na.rm = TRUE),
    rate = round(total/pop_avg, 2), 
    .groups = "drop"
  )



Data_County_ARCOS_Annual <- Data_County_ARCOS_Annual_long %>%
  select(year, County, drug, rate) %>%
  pivot_wider(
    names_from = year,
    values_from = rate,
    names_prefix = "Rate_"
  )%>%
  mutate(
    Avg_Rate = round(rowMeans(select(., starts_with("Rate_")), na.rm = TRUE),2)
  )

head(Data_County_ARCOS_Annual)


# Save to CSV
#write.csv(Data_County_ARCOS_summary, "../output/Drug_summary.csv", row.names = FALSE)



#### 2.1 Descriptive Table of Buprenorphine
# merge all tables of BUPRENORPHINE
Data_County_ARCOS_Summary_temp <- Data_County_ARCOS_summary%>%
  filter(drug == "BUPRENORPHINE")%>%
  select(-drug)

Data_County_ARCOS_Annual_temp <- Data_County_ARCOS_Annual%>%
  filter(drug == "BUPRENORPHINE")%>%
  select(-drug)

Data_County_pop_sum <- merge(
  County_population_average,
  Data_County_ARCOS_Summary_temp, 
  by = "County"
)

Data_County_pop_sum_rate <- merge(
  Data_County_pop_sum,
  Data_County_ARCOS_Annual_temp, 
  by = "County"
)

Data_County_combined <- Data_County_pop_sum_rate%>%
  select(
    County,
    Population_2016, Total_2016, Rate_2016,
    Population_2017, Total_2017, Rate_2017,
    Population_2018, Total_2018, Rate_2018,
    Population_2019, Total_2019, Rate_2019,
    Total, Avg_Rate
  )

head(Data_County_combined)


# Save to CSV
write.csv(Data_County_combined, "output/County_BUPRENORPHINE.csv", row.names = FALSE)



#### 2.2 Descriptive Table of HYDROCODONE
# merge all tables of HYDROCODONE
Data_County_ARCOS_Summary_temp <- Data_County_ARCOS_summary%>%
  filter(drug == "HYDROCODONE")%>%
  select(-drug)

Data_County_ARCOS_Annual_temp <- Data_County_ARCOS_Annual%>%
  filter(drug == "HYDROCODONE")%>%
  select(-drug)

Data_County_pop_sum <- merge(
  County_population_average,
  Data_County_ARCOS_Summary_temp, 
  by = "County"
)

Data_County_pop_sum_rate <- merge(
  Data_County_pop_sum,
  Data_County_ARCOS_Annual_temp, 
  by = "County"
)

Data_County_combined <- Data_County_pop_sum_rate%>%
  select(
    County,
    Population_2016, Total_2016, Rate_2016,
    Population_2017, Total_2017, Rate_2017,
    Population_2018, Total_2018, Rate_2018,
    Population_2019, Total_2019, Rate_2019,
    Total, Avg_Rate
  )

head(Data_County_combined)

# Save to CSV
write.csv(Data_County_combined, "output/County_HYDROCODONE.csv", row.names = FALSE)

  
#### 2.3 Descriptive Table of METHADONE
# merge all tables of METHADONE
Data_County_ARCOS_Summary_temp <- Data_County_ARCOS_summary%>%
  filter(drug == "METHADONE")%>%
  select(-drug)

Data_County_ARCOS_Annual_temp <- Data_County_ARCOS_Annual%>%
  filter(drug == "METHADONE")%>%
  select(-drug)

Data_County_pop_sum <- merge(
  County_population_average,
  Data_County_ARCOS_Summary_temp, 
  by = "County"
)

Data_County_pop_sum_rate <- merge(
  Data_County_pop_sum,
  Data_County_ARCOS_Annual_temp, 
  by = "County"
)

Data_County_combined <- Data_County_pop_sum_rate%>%
  select(
    County,
    Population_2016, Total_2016, Rate_2016,
    Population_2017, Total_2017, Rate_2017,
    Population_2018, Total_2018, Rate_2018,
    Population_2019, Total_2019, Rate_2019,
    Total, Avg_Rate
  )

head(Data_County_combined)

# Save to CSV
write.csv(Data_County_combined, "output/County_METHADONE.csv", row.names = FALSE)



#### 2.4 Descriptive Table of OXYCODONE
# merge all tables of OXYCODONE
Data_County_ARCOS_Summary_temp <- Data_County_ARCOS_summary%>%
  filter(drug == "OXYCODONE")%>%
  select(-drug)

Data_County_ARCOS_Annual_temp <- Data_County_ARCOS_Annual%>%
  filter(drug == "OXYCODONE")%>%
  select(-drug)

Data_County_pop_sum <- merge(
  County_population_average,
  Data_County_ARCOS_Summary_temp, 
  by = "County"
)

Data_County_pop_sum_rate <- merge(
  Data_County_pop_sum,
  Data_County_ARCOS_Annual_temp, 
  by = "County"
)

Data_County_combined <- Data_County_pop_sum_rate%>%
  select(
    County,
    Population_2016, Total_2016, Rate_2016,
    Population_2017, Total_2017, Rate_2017,
    Population_2018, Total_2018, Rate_2018,
    Population_2019, Total_2019, Rate_2019,
    Total, Avg_Rate
  )

head(Data_County_combined)

# Save to CSV
write.csv(Data_County_combined, "output/County_OXYCODONE.csv", row.names = FALSE)











## Summary of County by Year
Data_ER_filted %>%
  group_by(year, County) %>%
  summarise(total = sum(Count), .groups = "drop") %>%
  ggplot(aes(x = year, y = total, group = County)) +
  geom_col(fill = "#555555") +
  # geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.8) +  # Regression line
  facet_wrap(~ County, ncol = 6) + #, scales = "free_y"
  labs(
    title = "Annual ER Opioid Visits by County",
    x = "Year", y = "Total Visits"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))

## Summary of County by Month, for Cherokee, Etowah, Lauderdale, Marion, and Colbert, Franklin, Lawrence
ordered_levels <- c(
  "2016 1",  "2016 2",  "2016 3",  "2016 4",  "2016 5",  "2016 6",
  "2016 7",  "2016 8",  "2016 9",  "2016 10", "2016 11", "2016 12",
  "2017 1",  "2017 2",  "2017 3",  "2017 4",  "2017 5",  "2017 6",
  "2017 7",  "2017 8",  "2017 9",  "2017 10", "2017 11", "2017 12",
  "2018 1",  "2018 2",  "2018 3",  "2018 4",  "2018 5",  "2018 6",
  "2018 7",  "2018 8",  "2018 9",  "2018 10", "2018 11", "2018 12",
  "2019 1",  "2019 2",  "2019 3",  "2019 4",  "2019 5",  "2019 6",
  "2019 7",  "2019 8",  "2019 9",  "2019 10", "2019 11", "2019 12"
)
colors <- c(
  "CHEROKEE" = "#8DA0CB",
  "ETOWAH" = "#FC8D62",
  "LAUDERDALE" = "steelblue",
  "MARION" = "#E78AC3"
)




Data_ER_filted %>%
  filter(County %in% c("COLBERT", "FRANKLIN", "LAWRENCE")) %>% #, "COLBERT", "FRANKLIN", "LAWRENCE"
  mutate(
    year_month = factor(year_month, levels = ordered_levels)
  ) %>%
  group_by(year_month, County) %>%
  summarise(total = sum(Count), .groups = "drop") %>%
  ggplot(aes(x = year_month, y = total, group = County, color = County)) +
  geom_point() +
  geom_line() +
  facet_wrap(~ County, ncol = 1) + #, scales = "free_y"
  labs(
    title = "Monthly ER Opioid Visits by County",
    x = "Month", y = "Total Visits"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))


## Per Capita of County by Year
Data_ER_filted %>%
  group_by(year, County) %>%
  summarise(total = 100000 * sum(case_percapita), .groups = "drop") %>%
  ggplot(aes(x = year, y = total, group = County)) +
  geom_col(fill = "#555555") +
  # geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.8) +  # Regression line
  facet_wrap(~ County, ncol = 6) + #, scales = "free_y"
  labs(
    title = "Annual Per Capita ER Visits (Per 100,000 Residents) by County",
    x = "Year", y = "Per Capita ER Visits (per 100,000 residents)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))


## Summary of County by Month, for Cherokee, Etowah, Lauderdale, Marion
Data_ER_filted %>%
  filter(County %in% c("CHEROKEE","ETOWAH","LAUDERDALE","MARION")) %>% #, "COLBERT", "FRANKLIN", "LAWRENCE"
  mutate(
    year_month = factor(year_month, levels = ordered_levels)
  ) %>%
  group_by(year_month, County) %>%
  summarise(total = sum(case_percapita), .groups = "drop") %>%
  ggplot(aes(x = year_month, y = total, group = County, color=County)) +
  geom_point() +
  geom_line() +
  # geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.8) +  # Regression line
  facet_wrap(~ County, ncol = 1) + #, scales = "free_y"
  labs(
    title = "Monthly ER Visits Per Capita by County",
    x = "Month", y = "Per Capita"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))




# ggplot(summary_by_year, aes(x = reorder(County, -total), y = total)) +
#   geom_col(fill = "steelblue") +
#   facet_grid(year~. ) +
#   labs(
#     title = "Total ER Opioid Cases by County (Faceted by Year)",
#     x = "County",
#     y = "Total Cases"
#   ) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))



## Summary of County by Month

Data_ER_filted %>%
  group_by(year_month, County) %>%
  summarise(total = sum(Count), .groups = "drop") %>%
  ggplot(aes(x = year_month, y = total, group = County)) +
  geom_point(color = "#555555", size = 0.5) +
  geom_smooth(method = "lm", color = "red", linewidth = 0.6) +
  facet_wrap(~ County, ncol = 6, scales = "free_y") +
  scale_x_discrete(
    breaks = function(x) x[seq(1, length(x), by = 12)]
  ) +
  labs(
    title = "Monthly ER Opioid Cases by County",
    x = "Month", y = "Total Cases"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6))




###############################################################################
### Descriptive Statistic for ARCOS
# Read Data_ARCOS
Data_ARCOS <- read_csv("../Data/ARCOS_AL_2016_2019_cleaned.csv")

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
  mutate(date = mdy(date))%>%
  rename(County = county)

Data_ARCOS_filted <- Data_ARCOS_filted %>%
  mutate(
    year = as.numeric(format(date, "%Y")),
    month = as.numeric(format(date, "%m")),
    year_month = paste(year,month),
    week = as.numeric(format(date, "%U"))  # Week number of year (starting from 0)
  )

head(Data_ARCOS_filted)









# Summary of County
Df <- Data_county_ARCOS_summary%>%
  arrange(desc(mean))

ggplot(Df, aes(x = reorder(county, -mean), y = mean)) +
  geom_col(fill = "steelblue") +
  facet_grid(drug~.)+
  labs(
    title = "Cases by County",
    x = "County",
    y = "Cases Per Capita"
  ) +
  theme_minimal()+
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Summary of County by Year
colors <- c(
  "OXYCODONE" = "#8DA0CB",
  "HYDROCODONE"   = "#FC8D62",
  "BUPRENORPHINE"    = "steelblue",
  "METHADONE"     = "#E78AC3" 
)




## Four Drugs are shown separately (MME Consumtion)
unique(Data_ARCOS_filted$drug)

Data_County_ARCOS_Annual_long%>%
  filter(drug == "OXYCODONE")%>%
  ggplot(aes(x = year, y = total)) +
  geom_col(position = "dodge") +
  # geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.8) +  # Regression line
  facet_wrap( ~ County, ncol = 6) + #, scales = "free_y"
  labs(
    title = "Annual Oxycodone Distribution by County in MME",
    x = "Year", y = "Total MME (in Millions)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6))


Data_County_ARCOS_Annual_long%>%
  filter(drug == "METHADONE")%>%
  ggplot(aes(x = year, y = total)) +
  geom_col(position = "dodge") +
  # geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.8) +  # Regression line
  facet_wrap( ~ County, ncol = 6) + #, scales = "free_y"
  labs(
    title = "Annually Methadone Distribution by County in MME",
    x = "Year", y = "Total MME (in Millions)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6))


Data_County_ARCOS_Annual_long%>%
  filter(drug == "BUPRENORPHINE")%>%
  ggplot(aes(x = year, y = total)) +
  geom_col(position = "dodge") +
  # geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.8) +  # Regression line
  facet_wrap( ~ County, ncol = 6) + #, scales = "free_y"
  labs(
    title = "Annually Buprenorphine Distribution by County in MME",
    x = "Year", y = "Total MME (in Millions)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6))


Data_County_ARCOS_Annual_long%>%
  filter(drug == "HYDROCODONE")%>%
  ggplot(aes(x = year, y = total)) +
  geom_col(position = "dodge") +
  # geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.8) +  # Regression line
  facet_wrap( ~ County, ncol = 6) + #, scales = "free_y"
  labs(
    title = "Annually Hydrocodone Distribution by County in MME",
    x = "Year", y = "Total MME (in Millions)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6))




## Four Drugs are shown separately (MME Rate)
Data_County_ARCOS_Annual_long%>%
  filter(drug == "OXYCODONE")%>%
  ggplot(aes(x = year, y = rate)) +
  geom_col(position = "dodge") +
  # geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.8) +  # Regression line
  facet_wrap( ~ County, ncol = 6) + #, scales = "free_y"
  labs(
    title = "Annual Oxycodone Distribution by County in MME",
    x = "Year", y = "Total MME (in Millions)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6))


Data_County_ARCOS_Annual_long%>%
  filter(drug == "METHADONE")%>%
  ggplot(aes(x = year, y = rate)) +
  geom_col(position = "dodge") +
  # geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.8) +  # Regression line
  facet_wrap( ~ County, ncol = 6) + #, scales = "free_y"
  labs(
    title = "Annually Methadone Distribution by County in MME",
    x = "Year", y = "Total MME (in Millions)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6))


Data_County_ARCOS_Annual_long%>%
  filter(drug == "BUPRENORPHINE")%>%
  ggplot(aes(x = year, y = rate)) +
  geom_col(position = "dodge") +
  # geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.8) +  # Regression line
  facet_wrap( ~ County, ncol = 6) + #, scales = "free_y"
  labs(
    title = "Annually Buprenorphine Distribution by County in MME",
    x = "Year", y = "Total MME (in Millions)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6))


Data_County_ARCOS_Annual_long%>%
  filter(drug == "HYDROCODONE")%>%
  ggplot(aes(x = year, y = rate)) +
  geom_col(position = "dodge") +
  # geom_smooth(method = "lm", se = FALSE, color = "red", linewidth = 0.8) +  # Regression line
  facet_wrap( ~ County, ncol = 6) + #, scales = "free_y"
  labs(
    title = "Annually Hydrocodone Distribution by County in MME",
    x = "Year", y = "Total MME (in Millions)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "top",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 6))
