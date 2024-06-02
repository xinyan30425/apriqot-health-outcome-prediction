library(dplyr)
library(tidyr)
library(ipfp)
library(readr)
library(purrr)
library(stringr)

# Load puma data
puma_data <- read.csv("new_england_people_5year_18_22_alzheimer_prob_withedu_grouped_data.csv")

schl_to_years <- function(schl) {
  case_when(
    schl == 1 ~ 0,    # No schooling completed
    schl == 2 ~ 0,    # Nursery school, preschool
    schl == 3 ~ 0,    # Kindergarten
    schl == 4 ~ 1,    # Grades 1  without diploma
    schl == 5 ~ 2,    # Grades 2 without diploma
    schl == 6 ~ 3,    # Grades 3 without diploma
    schl == 7 ~ 4,    # Grades 4 without diploma
    schl == 8 ~ 5,    # Grades 5 without diploma
    schl == 9 ~ 6,    # Grades 6 without diploma
    schl == 10 ~ 7,   # Grades 7 without diploma
    schl == 11 ~ 8,   # Grades 8 without diploma
    schl == 12 ~ 9,   # Grades 9 without diploma
    schl == 13 ~ 10,  # Grades 10 without diploma
    schl == 14 ~ 11,  # Grades 11 without diploma
    schl == 15 ~ 12,  # Grades 12 without diploma
    schl == 16 ~ 12,  # Regular high school diploma
    schl == 17 ~ 12,  # GED or alternative credential
    schl == 18 ~ 14,  # Some college, but less than 1 year
    schl == 19 ~ 14,  # 1 or more years of college credit, no degree
    schl == 20 ~ 14,  # Associate's degree
    schl == 21 ~ 16,  # Bachelor's degree
    schl == 22 ~ 18,  # Master's degree
    schl == 23 ~ 19,  # Professional degree
    schl == 24 ~ 24,  # Doctorate degree change it to 20
    TRUE ~ NA_real_   # NA for any other codes or missing data
  )
}

# Convert education levels to binary format
convert_education_to_binary <- function(schl) {
  
  schl <- round(schl)
  # Convert the schooling code to years first
  years <- schl_to_years(schl)
  
  # Create a named vector with 25 elements (edu0 to edu24) and set all to 0
  edu_binary <- setNames(as.numeric(rep(0, 25)), paste0("edu", 0:24))
  
  # Based on the years, set the corresponding binary column to 1
  if (!is.na(years)) {
    edu_binary[paste0("edu", years)] <- 1
  }
  
  # Return the binary education vector
  return(edu_binary)
}

puma_data <- puma_data %>%
  rowwise() %>%
  mutate(edu_binary = list(convert_education_to_binary(av_education))) %>%
  unnest_wider(edu_binary)

write.csv(puma_data, "puma_binary.csv")

puma_data<- read.csv("/Users/xinyanliu/Desktop/NEU/Apriqot/Alzheimer/Alz prediction on ACS/5_year_people/maine_puma_binary.csv")


# Calculate the total weighted count and proportion for each education category in PUMA data
# Group by demographic variables and sum the product of each education level and total_pwgtp to get weighted counts
puma_education_distribution <- puma_data %>%
  group_by(PUMA,Male, Female, Other, Hispanic, Black, `X65_69`, `X70_74`, `X75_79`, `X80_84`, `X85`) %>%
  summarise(across(starts_with("edu"), ~ sum(.x * total_pwgtp), .names = "weighted_{.col}")) %>%
  
  ungroup() %>%
  # Create a total weighted count for each demographic group to calculate proportions
  mutate(total_weighted = rowSums(across(starts_with("weighted_")))) %>%
  # Calculate the proportion for each education level within the demographic group
  mutate(across(starts_with("weighted_"), ~ .x / total_weighted, .names = "prop_{.col}"))

write.csv(puma_education_distribution, "maine_puma_education_distribution.csv")


# Load tract data
tract_data <- read.csv("/Users/xinyanliu/Desktop/NEU/Apriqot/ACS tabular/maine_transformed_binary.csv")
puma_education_distribution <-read.csv("maine_puma_education_distribution.csv")

# Convert column names of both datasets to lowercase
names(tract_data) <- tolower(names(tract_data))
names(puma_education_distribution) <- tolower(names(puma_education_distribution))

# Prepare to apply these proportions to the tract-level data
# Join tract data with the calculated PUMA education distribution based on demographic groups
tract_with_puma_education <- tract_data %>%
  left_join(puma_education_distribution, by = c("puma", 
                                                "male", 
                                                "female", 
                                                "other", 
                                                "hispanic", 
                                                "black", 
                                                "x65_to_69" = "x65_69", 
                                                "x70_to_74" = "x70_74", 
                                                "x75_to_79" = "x75_79", 
                                                "x80_to_84" = "x80_84", 
                                                "x85_and_over" = "x85"))


# Create new columns for each education level estimate in the tract data
# Calculate estimates for education levels based on proportions from puma_education_distribution
for (edu_col in grep("prop_weighted_edu", names(tract_with_puma_education), value = TRUE)) {
  new_estimate_col <- sub("prop_", "estimate_", edu_col)
  tract_with_puma_education <- tract_with_puma_education %>%
    mutate(!!new_estimate_col := .data[[edu_col]] * .data$weight)
}

# Calculate the total education weight
tract_with_puma_education <- tract_with_puma_education %>%
  mutate(total_edu_weight = rowSums(select(., starts_with("estimate_"))))

# Remove columns that start with 'weighted_edu'
tract_with_puma_education <- tract_with_puma_education %>%
  select(-starts_with("weighted_edu"))

# Replace all NA values with 0 in the dataset
tract_with_puma_education <- tract_with_puma_education %>%
  mutate_all(~replace(., is.na(.), 0))

write.csv(tract_with_puma_education, "maine_tract_with_puma_education_estimates2.csv")



# Read in the dataset
tract_data <- read.csv("maine_tract_with_puma_education_estimates2.csv")

# Rename the columns by removing the prefix 'estimate_weighted_edu'
new_col_names <- gsub("estimate_weighted_edu", "", names(tract_data))
names(tract_data) <- new_col_names

# Check the new column names to ensure the change was successful
print(names(tract_data))

# Transpose the education columns
tract_data_long <- tract_data %>%
  pivot_longer(
    cols = `0`:`24`,    # Specifies the range of column names that have been converted to numbers
    names_to = "education_level",  # New column for storing the transposed column names
    values_to = "education_weight"  # Using a different name to avoid conflict
  )

# Optionally, view a few rows to ensure it looks correct
print(head(tract_data_long))

# Write the expanded dataset to a new CSV file
write.csv(tract_data_long, "expanded_education_dataset.csv", row.names = FALSE)

# Categorize the education levels and compute total weights for each category
tract_data_long <- read.csv("expanded_education_dataset.csv")

data_transformed <- tract_data_long %>%
  mutate(categorized_education_level = case_when(
    education_level <= 9 ~ 9,
    education_level > 9 & education_level <= 12 ~ 12,
    education_level > 12 & education_level <= 14 ~ 14,
    education_level > 14 & education_level <= 16 ~ 16,
    TRUE ~ 19
  )) %>%
  group_by(statea, county,puma, tracta, male, female, hispanic, black, other, x65_to_69, x70_to_74, x75_to_79, x80_to_84, x85_and_over, categorized_education_level) %>%
  summarise(total_weight_estimates = sum(education_weight), .groups = 'drop')

# Check the first few rows of the transformed dataset
# print(head(data_transformed))

# Write the transformed data to a new CSV file
write.csv(data_transformed, "maine_categorized_education_data.csv", row.names = FALSE)

distinct_education_levels <- unique(data_transformed$categorized_education_level)

# Print the distinct values
print(distinct_education_levels)


# age_sex_race_education transform from binary to factor variable
# First, create new columns for sex, age, and race
data_transformed$sex <- ifelse(data_transformed$male == 1, "Male", "Female")
data_transformed$age <- ifelse(data_transformed$x65_to_69 == 1, "65_69",
                 ifelse(data_transformed$x70_to_74 == 1, "70_74",
                        ifelse(data_transformed$x75_to_79 == 1, "75_79",
                               ifelse(data_transformed$x80_to_84 == 1, "80_84",
                                      ifelse(data_transformed$x85_and_over == 1, "85", NA)))))
data_transformed$race <- ifelse(data_transformed$hispanic == 1, "hispanic",
                  ifelse(data_transformed$black == 1, "black",
                         ifelse(data_transformed$other == 1, "other", NA)))

# Remove the original columns
data_transformed <- data_transformed[, !names(data_transformed) %in% c("male", "female", "hispanic", "black", "other",
                             "x65_to_69", "x70_to_74", "x75_to_79", "x80_to_84", "x85_and_over")]

# Now, let's reorder the columns to have ST, county, PUMA, tracta, sex, age, race,
# categorized_education_level, and total_weight_estimates
data_transformed <- data_transformed[, c("statea", "county","puma", "tracta", "sex", "age", "race",
             "categorized_education_level", "total_weight_estimates")]

distinct_education_levels <- unique(data_transformed$categorized_education_level)

# Print the distinct values
print(distinct_education_levels)

# Print the first few rows of the transformed data
write.csv(data_transformed, "sex_age_race_education_fact_variable_format.csv", row.names = FALSE)


# sex_age_race
data_transformed <- read.csv("sex_age_race_education_fact_variable_format.csv")

# Selecting required columns
new_data_transformed  <- data_transformed [, c("statea", "county","puma", "tracta", "sex", "age", "race", "total_weight_estimates")]

# Aggregating total_weight_estimates by specified columns
agg_data_transformed  <- aggregate(total_weight_estimates ~ ., data = new_data_transformed , sum)

# Saving the aggregated dataframe as a new CSV file
write.csv(agg_data_transformed , file = "sex_age_race_fact_variable_format_aggregate.csv", row.names = FALSE)


setwd("/Users/xinyanliu/Desktop/NEU/Apriqot/ACS tabular/Sex by Age by Educational_over_18")
# tract_education
tract_education<- read.csv("maine_sex_age_education_over65_cleaned.csv")

education_cols <- c("edu09", "edu12", "edu14", "edu16", "edu19")

# Transpose the education columns
long_tract_education <- tract_education %>%
  pivot_longer(
    cols = all_of(education_cols),
    names_to = "categorized_education_level",
    values_to = "total_weight"
  ) %>%
  mutate(categorized_education_level = str_extract(categorized_education_level, "\\d+")) %>%
  select(STUSAB, STATEA,COUNTYA, TRACTA, categorized_education_level, total_weight)

long_tract_education <- long_tract_education %>%
  rename_with(~ str_replace_all(tolower(.), "countya", "county"))

# Save the long format dataframe to a new CSV file
write.csv(long_tract_education, file = "maine_tract_education.csv", row.names = FALSE)



library(mipfp)
setwd("/Users/xinyanliu/Desktop/NEU/Apriqot/ACS tabular")
sex_age_race_education<-read.csv("sex_age_race_education_fact_variable_format.csv")
setwd("/Users/xinyanliu/Desktop/NEU/Apriqot/ACS tabular/Sex by Age by Educational_over_18")
tract_education<-read.csv("maine_tract_education.csv")
sex_age_race <- read.csv("sex_age_race.csv")

# Transform the data
sex_age_race_education <- sex_age_race_education %>%
  mutate(
    tracta = sprintf("%06d", tracta),  # Format tracta as 6 digits
    county = sprintf("%03d", county),  # Format county as 3 digits
    geocode = paste0(statea, county, tracta)  # Concatenate to create geocode
  )

tract_education <- tract_education %>%
  mutate(
    tracta = sprintf("%06d", tracta),  # Format tracta as 6 digits
    county = sprintf("%03d", county),  # Format county as 3 digits
    geocode = paste0(statea, county, tracta)  # Concatenate to create geocode
  )


################################

# Create a full grid of combinations
all_combinations <- expand.grid(
  geocode = unique(sex_age_race_education$geocode),
  sex = unique(sex_age_race_education$sex),
  age = unique(sex_age_race_education$age),
  race = unique(sex_age_race_education$race),
  categorized_education_level = unique(sex_age_race_education$categorized_education_level)
)

#Print the counts of unique values for each column in sex_age_race_education
# print(paste("Unique counts of geocode:", length(unique(sex_age_race_education$geocode))))
# print(paste("Unique counts of sex:", length(unique(sex_age_race_education$sex))))
# print(paste("Unique counts of age:", length(unique(sex_age_race_education$age))))
# print(paste("Unique counts of race:", length(unique(sex_age_race_education$race))))
# print(paste("Unique counts of categorized_education_level:", length(unique(sex_age_race_education$categorized_education_level))))
# print(paste("Unique counts of tracta_education:", length(unique(tract_education$geocode))))


# Check for duplicates
duplicates <- sex_age_race_education %>%
  group_by(geocode, sex, age, race, categorized_education_level) %>%
  summarise(count = n(), .groups = 'drop') %>%
  filter(count > 1)

if (nrow(duplicates) > 0) {
  print("Duplicates found. Consider handling them before merging.")
}

# Optionally remove duplicates or aggregate data as necessary
sex_age_race_education <- sex_age_race_education %>%
  group_by(geocode,sex, age, race, categorized_education_level) %>%
  summarise(total_weight_estimates = sum(total_weight_estimates), .groups = 'drop')


# Ensure that the number of rows matches expected full factorial design
expected_rows <- nrow(all_combinations)
print(paste("Expected rows:", expected_rows))


# Calculate the sum of total_weight in tract_education
total_weight_sum <- sum(tract_education$total_weight, na.rm = TRUE)

# Calculate the sum of total_weight_estimates in sex_age_race_education
total_weight_estimates_sum <- sum(sex_age_race_education$total_weight_estimates, na.rm = TRUE)

# Calculate the normalization factor
normalization_factor <- total_weight_sum / total_weight_estimates_sum

# Add the normalized_weight column
sex_age_race_education$normalized_weight <- sex_age_race_education$total_weight_estimates * normalization_factor
print(sum(sex_age_race_education$normalized_weight))


# Merge again after handling duplicates
full_data <- merge(all_combinations, sex_age_race_education, by = c("geocode","sex", "age", "race", "categorized_education_level"), all.x = TRUE)
full_data$normalized_weight[is.na(full_data$normalized_weight)] <- 0  # Replace NA weights with zero

# Check rows after handling duplicates
actual_rows_after_handling <- nrow(full_data)
print(paste("Rows after handling duplicates:", actual_rows_after_handling))


#Create Initial Array (Seed Matrix)
n_geocode <- length(unique(full_data$geocode))
n_sex <- length(unique(full_data$sex))
n_age <- length(unique(full_data$age))
n_race <- length(unique(full_data$race))
n_education <- length(unique(full_data$categorized_education_level))

# Create the initial array
initial_array <- array(data = full_data$total_weight_estimates,
                       dim = c(n_geocode, n_sex, n_age, n_race, n_education),
                       dimnames = list(unique(full_data$geocode),
                                       unique(full_data$sex),
                                       unique(full_data$age),
                                       unique(full_data$race),
                                       unique(full_data$categorized_education_level)))


# prepare for target array
n_geocode_education <- length(unique(tract_education$geocode))

target_education <- array(data = tract_education$total_weight,
                          dim = c(n_geocode_education, n_education),
                          dimnames = list(unique(tract_education$geocode),
                                          unique(tract_education$categorized_education_level)))

# run mipfp
results <- mipfp::Ipfp(
  seed = initial_array,
  target.list = list(c(1, 5)),  # Matching geocode and education level dimensions
  target.data = list(target_education),
  iter = 1000,
  tol = 1e-4
)

# Check results
print(results)

updated_weights_vector <- as.vector(results$x.hat)

full_data$updated_weight <- updated_weights_vector
write.csv(full_data, "mipfp_weight2.csv", row.names = FALSE)


# Summarize the updated weights by geocode and education level
mipfp_totals <- full_data %>%
  group_by(geocode, categorized_education_level) %>%
  summarise(mipfp_weight = sum(updated_weight), .groups = 'drop')

puma_estimates_total <- full_data %>%
  group_by(geocode, categorized_education_level) %>%
  summarise(puma_estimates_weight = sum(total_weight_estimates), .groups = 'drop')

# Join the data
# First join mipfp_totals and tract_education
temp_data <- left_join(mipfp_totals, tract_education, by = c("geocode", "categorized_education_level"))

# Now join the result with puma_estimates_total
comparison_data <- left_join(temp_data, puma_estimates_total, by = c("geocode", "categorized_education_level"))

# Write the resulting data to CSV
write.csv(comparison_data, "tract_education_comparison_data_dataset.csv", row.names = FALSE)

comparison_data <- read.csv("tract_education_comparison_data_dataset.csv")
# Summarizing the data to get total weights by geocode
summarized_data <- comparison_data %>%
  group_by(geocode) %>%
  summarise(
    total_tract_weight = sum(total_weight, na.rm = TRUE),
    total_tract_puma_estimates_normalized_weight = sum(puma_estimates_normalized_weight, na.rm = TRUE),
    total_tract_mipfp_weight = sum(mipfp_weight, na.rm = TRUE)
  )

write.csv(summarized_data, 'summarized_weight_by_tract.csv', row.names = FALSE)





















education_margin <- full_data %>%
  group_by(geocode,categorized_education_level) %>%
  summarise(total_weight = sum(normalized_weight), .groups = 'drop')

s_a_r_margin <- full_data %>%
  group_by(geocode, sex, age, race) %>%
  summarise(total_weight = sum(normalized_weight), .groups = 'drop') # Initialize an array with full data

n_geocode <- length(unique(full_data$geocode))
n_sex <- length(unique(full_data$sex))
n_age <- length(unique(full_data$age))
n_race <- length(unique(full_data$race))
n_education <- length(unique(full_data$categorized_education_level))
initial_array <- array(data = full_data$normalized_weight, dim = c(n_geocode, n_sex, n_age, n_race, n_education))

# Ensure that tract_education is correctly aggregated to match MIPFP targets
tract_education_agg <- tract_education %>%
  group_by(geocode, categorized_education_level) %>%
  summarise(total_weight = sum(total_weight), .groups = 'drop')

# Ensure dimensions match, especially for tract_education
target_tract_education <- array(tract_education_agg$total_weight, dim = c(n_geocode, n_education))


# Run MIPFP
results <- mipfp::Ipfp(
  seed = initial_array,
  target.list = list(c(1, 5), c(1, 2, 3, 4)),
  target.data = list(
    target_tract_education,
    array(s_a_r_margin$total_weight, dim = c(n_geocode, n_sex, n_age, n_race))
  ),
  print = TRUE,
  iter = 1000,
  tol = 1e-4,
  tol.margins = 1e-4
)


if (results$conv) {
  print("IPFP converged successfully.")
} else {
  print("IPFP did not converge, adjust parameters or check the data/margins.")
}

updated_weights <- sum(results$x.hat)
expected_total <- sum(sex_age_race_education$normalized_weight)

full_data$updated_weight <- as.vector(updated_weights)

# Calculate scaling factor and apply
scaling_factor <- expected_total / updated_weights
scaled_weights <- results$x.hat * scaling_factor

# Update the dataframe with scaled weights
full_data$updated_weight <- as.vector(scaled_weights)

# After running IPFP, update the full_data dataframe
updated_vector <- as.vector(results$x.hat)


# Save the dataframe to a CSV file
write.csv(full_data, "mipfp_weight2.csv", row.names = FALSE)


























# Recalculate margins based on the full data
education_margin <- full_data %>%
  group_by(tracta, county,categorized_education_level) %>%
  summarise(total_weight = sum(normalized_weight), .groups = 'drop')

s_a_r_margin <- full_data %>%
  group_by(tracta,county, sex, age, race) %>%
  summarise(total_weight = sum(normalized_weight), .groups = 'drop')


# Initialize an array with full data
n_tracta <- length(unique(full_data$tracta))
n_county <- length(unique(full_data$county))
n_sex <- length(unique(full_data$sex))
n_age <- length(unique(full_data$age))
n_race <- length(unique(full_data$race))
n_education <- length(unique(full_data$categorized_education_level))
initial_array <- array(data = full_data$normalized_weight, dim = c(n_tracta,n_county, n_sex, n_age, n_race, n_education))

# Define target arrays with correct dimensions
education_target_array <- array(education_margin$total_weight, dim = c(n_tracta, n_county, n_education))
sar_target_array <- array(s_a_r_margin$total_weight, dim = c(n_tracta, n_county, n_sex, n_age, n_race))

# Run MIPFP
results <- mipfp::Ipfp(
  seed = initial_array,
  target.list = list(c(1, 2, 6), c(1, 2, 3, 4, 5)),  # Updated to reflect the actual dimensions
  target.data = list(
    education_target_array,
    sar_target_array
  ),
  print = TRUE,
  iter = 1000,
  tol = 1e-4,
  tol.margins = 1e-4
)

if (results$conv) {
  print("IPFP converged successfully.")
} else {
  print("IPFP did not converge, adjust parameters or check the data/margins.")
}

updated_weights <- sum(results$x.hat)
expected_total <- sum(sex_age_race_education$normalized_weight)

# Calculate scaling factor and apply
scaling_factor <- expected_total / updated_weights
scaled_weights <- results$x.hat * scaling_factor

# Update the dataframe with scaled weights
full_data$updated_normalized_weight <- as.vector(scaled_weights)

# After running IPFP, update the full_data dataframe
updated_vector <- as.vector(results$x.hat)


# Save the dataframe to a CSV file
write.csv(full_data, "mipfp_weight2.csv", row.names = FALSE)

############### ipfp
library(ipfp)
library(Matrix)

# Aggregate total_weight by tract and education level, ensuring order
y_df <- aggregate(total_weight ~ tracta + categorized_education_level, data = tract_education, sum)

# Merge the full_data with y_df to align the data by 'tracta' and 'categorized_education_level'
combined <- merge(full_data, y_df, by = c("tracta", "categorized_education_level"), all.x = TRUE)
combined$normalized_weight[is.na(combined$normalized_weight)] <- 0.001  # Replace NA values with a small positive number to avoid zero weights

library(Matrix)

# Assuming there is a direct correspondence in the order of rows after merging
A <- sparseMatrix(i = 1:nrow(combined), j = 1:nrow(combined), x = 1, dims = c(nrow(combined), nrow(combined)))


# The target weights are directly taken from the merged data
y <- combined$total_weight
x0 <- combined$normalized_weight

# Filter out entries in 'y' where the target weight is zero
valid_indices <- which(y > 0)

# Adjust 'A', 'x0', and 'y' to only include valid indices
A_valid <- A[valid_indices, valid_indices, drop = FALSE]  # Adjust matrix 'A' to exclude rows and columns with zero target weights
y_valid <- y[valid_indices]  # Adjust vector 'y' to exclude zero target weights
x0_valid <- x0[valid_indices]  # Adjust initial weights 'x0' to match the filtered entries

# # Apply IPFP
# results <- ipfp(A = A_valid, y = y_valid, x0 = x0_valid, tol = sqrt(.Machine$double.eps), maxit = 1000)
# 
# # Check for convergence and update the weights
# if (results$convergence) {
#   cat("IPFP converged\n")
#   combined$updated_normalized_weight <- results$x  # Update the normalized weights
#   print(head(combined))
# } else {
#   cat("IPFP did not converge\n")
# }


custom_ipfp <- function(A, y, x0, tol = sqrt(.Machine$double.eps), maxit = 1000) {
  x <- x0
  for (iter in 1:maxit) {
    # Update x based on the constraint Ax = y
    x_new <- x * (y / (A %*% x))
    # Check for convergence
    if (sqrt(sum((A %*% x_new - y)^2)) < tol) {
      cat("Converged in", iter, "iterations\n")
      return(x_new)
    }
    x <- x_new
  }
  cat("Did not converge after", maxit, "iterations\n")
  return(x)
}

# Run custom IPFP
x_estimated <- custom_ipfp(A = A_valid, y = y_valid, x0 = x0_valid)

# Update combined with estimated weights
combined$updated_normalized_weight <- rep(NA, nrow(combined))
combined$updated_normalized_weight[valid_indices] <- x_estimated

# View updated data
head(combined)

write.csv(combined, "ipfp_weight2.csv", row.names = FALSE)





















library(dplyr)
# Calculate total weights for normalization
combined_transformed <- combined %>%
  group_by(tracta) %>%
  mutate(total_weight = sum(updated_normalized_weight)) %>%
  ungroup() # Ensuring total_weight is accessible

# Calculate education proportions
education_proportions <- combined_transformed %>%
  group_by(tracta, categorized_education_level, total_weight) %>% # Include total_weight in grouping
  summarize(education_weight = sum(updated_normalized_weight), .groups = 'drop') %>%
  mutate(education_proportion = education_weight / total_weight)

# Sex Proportions
sex_proportions <- combined_transformed %>%
  group_by(tracta, sex, total_weight) %>%  # Include total_weight in the grouping
  summarize(sex_weight = sum(updated_normalized_weight), .groups = 'drop') %>%
  mutate(sex_proportion = sex_weight / total_weight)

# Age Proportions
age_proportions <- combined_transformed %>%
  group_by(tracta, age, total_weight) %>%  # Include total_weight in the grouping
  summarize(age_weight = sum(updated_normalized_weight), .groups = 'drop') %>%
  mutate(age_proportion = age_weight / total_weight)

# Race Proportions
race_proportions <- combined_transformed %>%
  group_by(tracta, race, total_weight) %>%  # Include total_weight in the grouping
  summarize(race_weight = sum(updated_normalized_weight), .groups = 'drop') %>%
  mutate(race_proportion = race_weight / total_weight)

# Example for sex proportions
sex_proportions_wide <- sex_proportions %>%
  select(tracta, sex, sex_proportion) %>%
  pivot_wider(names_from = sex, values_from = sex_proportion, values_fill = list(sex_proportion = 0)) %>%
  left_join(sex_proportions %>% select(tracta, total_weight) %>% distinct(), by = "tracta")

# Example for age proportions
age_proportions_wide <- age_proportions %>%
  select(tracta, age, age_proportion) %>%
  pivot_wider(names_from = age, values_from = age_proportion, values_fill = list(age_proportion = 0)) %>%
  left_join(age_proportions %>% select(tracta, total_weight) %>% distinct(), by = "tracta")

race_proportions_wide <- race_proportions %>%
  select(tracta, race, race_proportion) %>%
  pivot_wider(names_from = race, values_from = race_proportion, values_fill = list(race_proportion = 0)) %>%
  # Optionally add total_weight to the wide format if it varies by tract only and not by race
  left_join(race_proportions %>% select(tracta, total_weight) %>% distinct(), by = "tracta")


# Example for education level proportions
education_proportions_wide <- education_proportions %>%
  select(tracta, categorized_education_level, education_proportion) %>%
  pivot_wider(names_from = categorized_education_level, values_from = education_proportion, values_fill = list(education_proportion = 0)) %>%
  left_join(education_proportions %>% select(tracta, total_weight) %>% distinct(), by = "tracta")

# You might want to merge all these wide format data frames into one combined dataframe
combined_proportions_wide <- reduce(list(race_proportions_wide, sex_proportions_wide, age_proportions_wide, education_proportions_wide), 
                                    full_join, by = "tracta")


write.csv(combined_proportions_wide, "ipfp_weight_proportion.csv", row.names = FALSE)

combined_proportions_wide <- read.csv("ipfp_weight_proportion.csv")


combined_proportions_wide$education <- with(combined_proportions_wide, 
                       (9 * `X9` + 
                          12 * `X12` + 
                          14 * `X14` + 
                          16 * `X16` + 
                          19 * `X19`) / 
                         (`X9` + `X12` + `X14` + `X16` + `X19`)
)


# Apply the model to calculate log odds for each tract including education

log_odds <- with(combined_proportions_wide, -3.455 + 
                   (0.577 * X70_74) + 
                   (1.126 * X75_79) +
                   (1.8 * X80_84) +
                   (2.693 *X85) +
                   (0.123 * Female) +  # Assuming male is the reference category
                   (0.915 * Black) +
                   (0.548 * Hispanic) +
                   (-0.398 * (education - 12.3) / 3.5)  # Assuming continuous years of education
)

probabilities <- 1 / (1 + exp(-log_odds))*100
combined_proportions_wide$Alzheimers_probability <- probabilities
# Write the proportions data to a new CSV file
write.csv(combined_proportions_wide, "maine_ipfp_alz_estimates.csv", row.names = FALSE)



###### one function accept dataset and perform transformations,calculations of proportions, and outputting of the final dataframe
calculate_demographic_proportions <- function(data) {
  # Calculate total weights for normalization
  combined_transformed <- data %>%
    group_by(geocode) %>%
    mutate(total_weight = sum(updated_weight)) %>%
    ungroup() # Ensuring total_weight is accessible
  
  # Calculate education proportions
  education_proportions <- combined_transformed %>%
    group_by(geocode, categorized_education_level, total_weight) %>%
    summarize(education_weight = sum(updated_weight), .groups = 'drop') %>%
    mutate(education_proportion = education_weight / total_weight)
  
  # Sex Proportions
  sex_proportions <- combined_transformed %>%
    group_by(geocode, sex, total_weight) %>%
    summarize(sex_weight = sum(updated_weight), .groups = 'drop') %>%
    mutate(sex_proportion = sex_weight / total_weight)
  
  # Age Proportions
  age_proportions <- combined_transformed %>%
    group_by(geocode, age, total_weight) %>%
    summarize(age_weight = sum(updated_weight), .groups = 'drop') %>%
    mutate(age_proportion = age_weight / total_weight)
  
  # Race Proportions
  race_proportions <- combined_transformed %>%
    group_by(geocode, race, total_weight) %>%
    summarize(race_weight = sum(updated_weight), .groups = 'drop') %>%
    mutate(race_proportion = race_weight / total_weight)
  
  # Convert each proportions data frame to wide format and join with total_weight
  convert_to_wide <- function(proportions_df, name_col, value_col) {
    proportions_wide <- proportions_df %>%
      select(geocode, !!sym(name_col), !!sym(value_col)) %>%
      pivot_wider(names_from = !!sym(name_col), 
                  values_from = !!sym(value_col), 
                  values_fill = setNames(list(0), value_col)) %>%
      left_join(proportions_df %>% select(geocode, total_weight) %>% distinct(), by = "geocode")
    return(proportions_wide)
  }
  
  # Applying function to proportion data frames
  sex_proportions_wide <- convert_to_wide(sex_proportions, "sex", "sex_proportion")
  age_proportions_wide <- convert_to_wide(age_proportions, "age", "age_proportion")
  race_proportions_wide <- convert_to_wide(race_proportions, "race", "race_proportion")
  education_proportions_wide <- convert_to_wide(education_proportions, "categorized_education_level", "education_proportion")
  
  # Merging all wide format data frames into one combined data frame
  combined_proportions_wide <- reduce(list(race_proportions_wide, sex_proportions_wide, age_proportions_wide, education_proportions_wide), 
                                      full_join, by = "geocode")
  
  # Merging all wide format data frames into one combined data frame, ensuring total_weight is handled properly
  combined_proportions_wide <- reduce(list(race_proportions_wide, sex_proportions_wide, age_proportions_wide, education_proportions_wide), 
                                      function(x, y) {
                                        # First, check if 'total_weight' column exists in both data frames
                                        common_columns <- intersect(names(x), names(y))
                                        if ('total_weight' %in% common_columns) {
                                          # Make sure to coalesce 'total_weight' during the join to avoid duplicates
                                          x <- mutate(x, total_weight = coalesce(total_weight, y$total_weight))
                                          y <- select(y, -total_weight)  # Remove 'total_weight' from y before join
                                        }
                                        full_join(x, y, by = "geocode")
                                      })
  
  # After merging, check for and clean any remaining duplicate 'total_weight' columns
  column_patterns <- grep("total_weight", names(combined_proportions_wide), value = TRUE)
  if (length(column_patterns) > 1) {
    # Assuming all total_weight columns are identical, we keep the first one and remove the rest
    combined_proportions_wide <- combined_proportions_wide %>%
      select(-one_of(column_patterns[-1])) %>%
      rename(total_weight = !!column_patterns[1])
  }
  
  # Writing output to CSV
  write.csv(combined_proportions_wide, "mipfp_weight_proportion2.csv", row.names = FALSE)
  
  return(combined_proportions_wide)
}


result <- calculate_demographic_proportions(full_data)


result$education <- with(result,(9 * `9` +12 * `12` +14 * `14` + 16 * `16` + 19 * `19`) / (`9` + `12` + `14` + `16` + `19`))

# Apply the model to calculate log odds for each tract including education

log_odds <- with(result, -3.455 + 
                   (0.577 * `70_74`) + 
                   (1.126 * `75_79`) +
                   (1.8 * `80_84`) +
                   (2.693 *`85`) +
                   (0.123 * Female) +  # Assuming male is the reference category
                   (0.915 * black) +
                   (0.548 * hispanic) +
                   (-0.398 * (education - 12.3) / 3.5)  # Assuming continuous years of education
)

probabilities <- 1 / (1 + exp(-log_odds))*100
result$Alzheimers_probability <- probabilities
# Write the proportions data to a new CSV file
write.csv(result, "maine_mipfp_alz_estimates2.csv", row.names = FALSE)





















































