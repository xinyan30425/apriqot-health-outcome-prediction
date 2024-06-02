library(dplyr)

tract_data <- read.csv("/Users/xinyanliu/Desktop/NEU/Apriqot/ACS tabular/Sex by Age by Race/tract_binary.csv")

# Calculate the weighted proportions for each binary variable by Tract
proportions_data <- tract_data %>%
  group_by(ST,PUMA, tracta) %>%
  summarize(
    Proportion_65_69 = sum(`X65_69` * weight) / sum(weight),
    Proportion_70_74 = sum(`X70_74` * weight) / sum(weight),
    Proportion_75_79 = sum(`X75_79` * weight) / sum(weight),
    Proportion_80_84 = sum(`X80_84` * weight) / sum(weight),
    Proportion_85 = sum(`X85` * weight) / sum(weight),
    Proportion_Male = sum(Male * weight) / sum(weight),
    Proportion_Female = sum(Female * weight) / sum(weight),
    Proportion_Hispanic = sum(Hispanic * weight) / sum(weight),
    Proportion_Black = sum(Black * weight) / sum(weight),
    Proportion_Other = sum(Other * weight) / sum(weight),
    Total_Weight = sum(weight),
    .groups = 'drop' # Drop the grouping for further manipulations
  ) %>%
  ungroup()

# Apply the model to calculate log odds for each tract
proportions_data <- proportions_data %>%
  mutate(
    log_odds = -3.455 +
      (Proportion_70_74 * 0.577) +
      (Proportion_75_79 * 1.126) +
      (Proportion_80_84 * 1.8) +
      (Proportion_85 * 2.693) +
      (Proportion_Female * 0.123) +
      (Proportion_Black * 0.915) +
      (Proportion_Hispanic * 0.548),
    # Convert log odds to probability
    alzheimer_prob = (exp(log_odds) / (1 + exp(log_odds))) * 100,
    # Calculate affected population based on the probability and total weight
    affected_population = alzheimer_prob / 100 * Total_Weight
  )

# Write the proportions data to a new CSV file
write.csv(proportions_data, "new_england_alzheimer_probabilities2.csv", row.names = FALSE)

tract_data_edu <- read.csv("/Users/xinyanliu/Desktop/NEU/Apriqot/ACS tabular/tract_with_puma_education_estimates.csv")

# Calculate the Average_Education for each row in tract_data_edu
# Use sapply to loop over all education level variables to calculate weighted mean of education
tract_data_edu <- tract_data_edu %>%
  rowwise() %>%
  mutate(Average_Education = sum(sapply(0:24, function(x) {
    edu_col <- paste0("estimate_weighted_edu", x)
    get(edu_col) * x
  })) / weight) %>%
  ungroup()  # Make sure to ungroup after the operation

write.csv(tract_data_edu, "new_england_alzheimer_edu2.csv", row.names = FALSE)

# Remove rows with NA values
tract_data_edu <- tract_data_edu %>%
  drop_na()

proportions_data_edu <- tract_data_edu %>%
  group_by(ST, PUMA, tracta) %>%
  summarize(
    Proportion_65_69 = sum(`X65_69` * weight) / sum(weight),
    Proportion_70_74 = sum(`X70_74` * weight) / sum(weight),
    Proportion_75_79 = sum(`X75_79` * weight) / sum(weight),
    Proportion_80_84 = sum(`X80_84` * weight) / sum(weight),
    Proportion_85 = sum(`X85` * weight) / sum(weight),
    Proportion_Male = sum(Male * weight) / sum(weight),
    Proportion_Female = sum(Female * weight) / sum(weight),
    Proportion_Hispanic = sum(Hispanic * weight) / sum(weight),
    Proportion_Black = sum(Black * weight) / sum(weight),
    Proportion_Other = sum(Other * weight) / sum(weight),
    Average_Education = sum(Average_Education * weight) / sum(weight),
    Total_Weight = sum(weight),
    .groups = 'drop'
  ) %>%
  ungroup()


# Apply the model to calculate log odds for each tract including education
proportions_data_edu <- proportions_data_edu %>%
  mutate(
    log_odds = -3.455 +
      (Proportion_70_74 * 0.577) +
      (Proportion_75_79 * 1.126) +
      (Proportion_80_84 * 1.8) +
      (Proportion_85 * 2.693) +
      (Proportion_Female * 0.123) +
      (Proportion_Black * 0.915) +
      (Proportion_Hispanic * 0.548) +
      ((Average_Education - 12.3) / 3.5 * -0.398), # Adjusting by mean education and SD
    # Convert log odds to probability
    alzheimer_prob = (exp(log_odds) / (1 + exp(log_odds))) * 100,
    # Calculate affected population based on the probability and total weight
    affected_population = alzheimer_prob / 100 * Total_Weight
  )

# Write the proportions data to a new CSV file
write.csv(proportions_data_edu, "new_england_alzheimer_edu_probabilities2.csv", row.names = FALSE)

