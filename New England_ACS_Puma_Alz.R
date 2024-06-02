library(tidyverse)
library(readr)
library(ggplot2)

#data <- read.csv("new_england_people_5year_18_22_alzheimer_prob_withoutedu_grouped_data.csv",check.names = FALSE)
data <- read.csv("new_england_people_5year_18_22_alzheimer_prob_withedu_grouped_data.csv",check.names = FALSE)

# Aggregate the total affected population by county
total_by_puma <- data %>%
  group_by(GEOID) %>%
  summarize(
    av_education = sum(total_pwgtp*av_education)/sum(total_pwgtp),
    affected_population = sum(total_pwgtp*weighted_alzheimer_prob/100, na.rm = TRUE),
    total_population = sum(total_pwgtp, na.rm = TRUE)
  ) %>%
  mutate(
    percentage = (affected_population / total_population) * 100
  )

#View(total_by_puma)
#write.csv(total_by_puma, "ACS_New_England_Puma_Level_Alzheimer_Estimates.csv", row.names = FALSE)
write.csv(total_by_puma, "ACS_New_England_Puma_Level_Alzheimer_Estimates_withedu.csv", row.names = FALSE)
