library(tidyverse)
library(readr)
library(ggplot2)

#data <- read.csv("maine_5year-alz_prob_withoutedu_grouped_data.csv",check.names = FALSE)
data <- read.csv("maine_5year-alz_prob_withedu_grouped_data.csv",check.names = FALSE)

# Aggregate the total affected population by county
total_by_puma <- data %>%
  group_by(GEOID) %>%
  summarize(
    affected_population = sum(total_pwgtp*weighted_alzheimer_prob/100, na.rm = TRUE),
    total_population = sum(total_pwgtp, na.rm = TRUE)
  ) %>%
  mutate(
    percentage = (affected_population / total_population) * 100
  )

View(total_by_puma)

# Create a new dataframe with County, County Code, affected_population, and percentage
final_county_data <- total_by_puma %>%
  select(GEOID, affected_population, percentage)

# Write the final dataframe to a new CSV file
# write.csv(final_county_data, "ACS_Maine_Puma_Level_Alzheimer_Estimates.csv", row.names = FALSE)
write.csv(final_county_data, "ACS_Maine_Puma_Level_Alzheimer_Estimates_withedu.csv", row.names = FALSE)