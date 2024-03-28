library(tidyverse)
library(readr)
library(ggplot2)

# Read the CSV file and assign it to nchs_maine_over65
nchs_maine <- read.csv("Bridged race population estimates.csv")

# View the first few rows of the dataframe
head(nchs_maine)

# Ensure the Age Code column is treated as a character for manipulation
nchs_maine$Age.Code <- as.character(nchs_maine$Age.Code)

# Quick check for unique values to see if there are any unexpected formats
unique(nchs_maine$Age.Code)

# Convert Age Code to Numeric, treating "85+" as 85
# A safer approach to convert 'Age Code' to numeric, accounting for '85+' and unexpected formats
nchs_maine$AgeNumeric <- sapply(nchs_maine$Age.Code, function(x) {
  if (x == "85+") {
    return(85)
  } else {
    # Convert to numeric, safely handling NAs or conversion issues
    as.numeric(x)
  }
})

# Filter out data for AgeNumeric >= 65 only
nchs_maine_over65 <- subset(nchs_maine, AgeNumeric >= 65)

nchs_maine_over65$SortedRace <- ifelse(nchs_maine_over65$Ethnicity.Code == "2135-2", "Hispanic",
                          ifelse(nchs_maine_over65$Ethnicity.Code == "2186-5", nchs_maine_over65$Race, NA))

# Apply the model to calculate log odds
nchs_maine_over65$log_odds <- -3.455 +
  ifelse(nchs_maine_over65$Age.Code >= 65 & nchs_maine_over65$Age.Code<= 69, 0,
         ifelse(nchs_maine_over65$Age.Code >= 70 & nchs_maine_over65$Age.Code<=74, 0.577,
                ifelse(nchs_maine_over65$Age.Code >= 75 & nchs_maine_over65$Age.Code<=79, 1.126,
                       ifelse(nchs_maine_over65$Age.Code >= 80 & nchs_maine_over65$Age.Code<=84, 1.8,
                              ifelse(nchs_maine_over65$Age.Code == "85+", 2.693, NA))))) +
  ifelse(nchs_maine_over65$Gender == "Female", 0.123, 0) +
  ifelse(nchs_maine_over65$SortedRace == "Black or African American", 0.915,
         ifelse(nchs_maine_over65$SortedRace == "Hispanic", 0.548,
                ifelse(nchs_maine_over65$SortedRace == "White",0,0)))
#-0.398*(-0.5/3.5)

# Convert log odds to probability and calculate affected population
nchs_maine_over65$alzheimer_prob <- (exp(nchs_maine_over65$log_odds) / (1 + exp(nchs_maine_over65$log_odds))) * 100
nchs_maine_over65$affected_population <- nchs_maine_over65$alzheimer_prob / 100 * nchs_maine_over65$Population

# Calculate the total affected population by county
total_by_county <- aggregate(affected_population ~ County.Code, data = nchs_maine_over65, FUN = sum)

# Calculate the total population by county
total_population_by_county <- aggregate(Population ~ County.Code, data = nchs_maine_over65, FUN = sum)

# Merge the total affected population and total population by county dataframes
county_data <- merge(total_by_county, total_population_by_county, by = "County.Code", all = TRUE)

# Calculate the percentage of the population affected by Alzheimer's for each county
county_data$percentage <- (county_data$affected_population / county_data$Population) * 100

# Create a new dataframe with County.Code, affected_population, and percentage
final_county_data <- county_data[, c("County.Code", "affected_population", "percentage")]

# Write the new dataframe to a new CSV file
write.csv(final_county_data, "County_Level_Alzheimer_Percentage2.csv", row.names = FALSE)

# Calculate the overall total affected population, removing NA values
total_affected <- sum(nchs_maine_over65$affected_population, na.rm = TRUE)

# Write the dataframe to a new CSV file
write.csv(nchs_maine_over65, "Updated_Bridged_Race_Over65_Alz_Estimates2.csv", row.names = FALSE)

# Print the results
print(total_by_county)
print(paste("Total affected population:", total_affected))

data <- read.csv("Updated_Bridged_Race_Over65_Alz_Estimates2.csv")


# Assuming you have read your dataset into a variable named data
# Replace 'your_dataset.csv' with the actual path to your CSV file if you're reading it in again
# data <- read.csv('your_dataset.csv')

# Process the data
processed_data <- data %>%
  # Create binary columns for each Age Code range
  mutate(`65-69` = as.integer(Age.Code >= "65" & Age.Code <= "69"),
         `70-74` = as.integer(Age.Code >= "70" & Age.Code <= "74"),
         `75-79` = as.integer(Age.Code >= "75" & Age.Code <= "79"),
         `80-84` = as.integer(Age.Code >= "80" & Age.Code <= "84"),
         `>85` = as.integer(Age.Code == "85+"),
         # Create binary columns for Gender
         `Male` = as.integer(Gender == "Male"),
         `Female` = as.integer(Gender == "Female"),
         # Create binary columns for Ethnicity Codes
         `Hispanic` = as.integer(Ethnicity.Code == "2135-2"),
         `Black` = as.integer(Race == "Black or African American"),
         # Assuming 'Other' should identify any race not Black or African American, including 'Hispanic' if not excluded by ethnicity
         `Other` = as.integer(!(Race == "Black or African American" | Ethnicity.Code == "2135-2") & 
                                (Race == "White" | Race == "American Indian or Alaska Native" | Race == "Asian or Pacific Islander"))
  ) %>%
  # Select only the necessary columns
  select(County, `County.Code`, `65-69`, `70-74`, `75-79`, `80-84`, `>85`, Male, Female, Hispanic, Black, Other,Population)

# Write the processed data to a new CSV file
write.csv(processed_data, "processed_data.csv", row.names = FALSE)

# Calculate the proportion for each binary variable by County
proportions_data <- processed_data  %>%
  group_by(County) %>%
  summarize(
    Proportion_65_69 = mean(`65-69`, na.rm = TRUE),
    Proportion_70_74 = mean(`70-74`, na.rm = TRUE),
    Proportion_75_79 = mean(`75-79`, na.rm = TRUE),
    Proportion_80_84 = mean(`80-84`, na.rm = TRUE),
    Proportion_over_85 = mean(`>85`, na.rm = TRUE),
    Proportion_Male = mean(Male, na.rm = TRUE),
    Proportion_Female = mean(Female, na.rm = TRUE),
    Proportion_Hispanic = mean(Hispanic, na.rm = TRUE),
    Proportion_Black = mean(Black, na.rm = TRUE),
    Proportion_Other = mean(Other, na.rm = TRUE),
    Total_Population = sum(Population, na.rm = TRUE)
  ) %>%
  ungroup()  # Optional, to remove the grouping

# Write the proportions data to a new CSV file
write.csv(proportions_data, "proportions_by_county.csv", row.names = FALSE)


# Apply the model to calculate log odds for each age group, gender, and ethnicity
proportions_data <- proportions_data %>%
  mutate(
    log_odds = -3.455 +
      (`Proportion_65_69` * 0) +
      (`Proportion_70_74` * 0.577) +
      (`Proportion_75_79` * 1.126) +
      (`Proportion_80_84` * 1.8) +
      (`Proportion_over_85` * 2.693) +
      (Proportion_Male * 0) +  # Assuming Male baseline is 0, no coefficient needed
      (Proportion_Female * 0.123) +
      (Proportion_Black * 0.915) +
      (Proportion_Hispanic * 0.548) +
      (Proportion_Other * 0),  # Assuming Other baseline is 0, no coefficient needed
     #-0.398 * (-0.5 / 3.5),
    #-0.398 * 12,
    
    # Convert log odds to probability
    alzheimer_prob = (exp(log_odds) / (1 + exp(log_odds))) * 100,
    # Calculate affected population based on the probability
    affected_population = alzheimer_prob / 100 * Total_Population
  )

# Aggregate the total affected population by county
total_by_county <- proportions_data %>%
  group_by(County) %>%
  summarize(
    affected_population = sum(affected_population, na.rm = TRUE),
    total_population = sum(Total_Population, na.rm = TRUE)
  ) %>%
  mutate(
    percentage = (affected_population / total_population) * 100
  )

# Create a new dataframe with County, County Code, affected_population, and percentage
final_county_data <- total_by_county %>%
  select(County, affected_population, percentage)

# Write the final dataframe to a new CSV file
write.csv(final_county_data, "County_Level_Alzheimer_Percentage1.csv", row.names = FALSE)

# Calculate the overall total affected population, removing NA values
overall_total_affected <- sum(processed_data$affected_population, na.rm = TRUE)

print(total_by_county)
# Print the overall total affected population
print(paste("Overall total affected population:", overall_total_affected))

                                
