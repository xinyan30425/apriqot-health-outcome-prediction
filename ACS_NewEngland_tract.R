library(dplyr)
library(tidyr)

df <- read.csv("tract_race_newengland.csv")

# Step 1: Remove "U7..." from the column names
names(df) <- gsub("U7.*", "", names(df))

# Define a function to check if the age in the column name is less than 65
contains_number_less_than_65 <- function(column_name) {
  # Extract parts of the column name that potentially describe age using a regex that matches age-related terms
  age_descriptions <- regmatches(column_name, gregexpr("(under|to|and)_([0-9])", column_name))
  if (length(age_descriptions[[1]]) > 0) {
    # Flatten the list, extract numbers, and convert to numeric
    numbers <- as.numeric(gsub("[^0-9]", "", unlist(age_descriptions)))
    # Return TRUE if any number is less than 65
    return(any(numbers < 65, na.rm = TRUE))
  }
  return(FALSE)
}
# Filter columns based on the condition
over_65 <- df %>% select(where(~!contains_number_less_than_65(.)))

# Optionally, write the cleaned data frame back to a CSV
write.csv(over_65, 'tract_race_newengland_age_over65.csv', row.names = FALSE)


# List of age groups to exclude 
age_groups_to_exclude <- c("under_5_", "5_to_9_", "10_to_14_", "15_to_17_", "18_and_19_", 
                           "_20", "_21", "22_to_24_", "25_to_29_", "30_to_34_", "35_to_39_", 
                           "40_to_44_", "45_to_49_", "50_to_54_", "55_to_59_", "60_and_61_", 
                           "62_to_64_")

# Create a regex pattern to match any of the age groups
pattern_to_exclude <- paste(age_groups_to_exclude, collapse = "|")

# Filter out columns that match the pattern
age_over_65 <- df %>%
  select(-matches(pattern_to_exclude))

# Show the structure of the filtered data
str(age_over_65)

write.csv(age_over_65, 'tract_race_newengland_age_over65.csv', row.names = FALSE)

# Define the columns to retain
cols_to_retain <- c("FIPS", "statea", "stusab", "puma", "county", "countya", "tracta", "year")

# Select columns that contain the word "total" and the specific columns you want to retain
filtered_data <- age_over_65 %>%
  select(
    all_of(cols_to_retain),
    matches("total")
  )
 

# Separate data based on male and female
male_data <- df %>%
  select(matches("^male_"))
female_data <- df %>%
  select(matches("^female_"))

# Further separate data based on race
race_categories <- c("white", "black", "asian", "hispanic", "indigenous", "othermulti")

# Function to separate data based on race categories
separate_race_data <- function(data, gender) {
  race_data <- lapply(race_categories, function(race) {
    # Directly use selection helpers within the select() function
    data %>%
      select(
        FIPS, statea, stusab, puma, county, countya, tracta, year,
        starts_with(gender),
        matches(paste0("^", gender, "_", race))
      ) %>%
      group_by(FIPS, statea, stusab, puma, county, countya, tracta, year) %>% # Group by these columns to keep them
      summarise(across(matches(paste0("^", gender, "_", race)), sum)) %>%
      rename_with(~ gsub(paste0("^", gender, "_"), "", .x), matches(paste0("^", gender, "_", race)))
  })
  return(race_data)
}

male_race_data <- separate_race_data(df, "male")
female_race_data <- separate_race_data(df, "female")

# Further separate data based on age groups
age_groups <- c("under_5_", "5_to_9_", "10_to_14_", "15_to_17_", "18_and_19_", 
                "20_", "21_", "22_to_24_", "25_to_29_", "30_to_34_", "35_to_39_", 
                "40_to_44_", "45_to_49_", "50_to_54_", "55_to_59_", "60_and_61_", 
                "62_to_64_", "65_and_66_", "67_to_69_", "70_to_74_", "75_to_79_", 
                "80_to_84_", "85_and_over_")

# Function to separate data based on age groups
separate_age_data <- function(data) {
  age_data <- lapply(age_groups, function(age) {
    age_cols <- grep(paste0(age), names(data), value = TRUE)  # Find columns that match the age pattern
    if (length(age_cols) > 0) {# Check if any columns match the age pattern
      data %>%
        select(all_of(c("FIPS", "statea", "stusab", "puma", "county", "countya", "tracta", "year", age_cols))) %>%
        group_by(FIPS, statea, stusab, puma, county, countya, tracta, year) %>%  # Group by non-demographic data
        summarise(across(all_of(age_cols), sum, .names = "sum_{.col}"), .groups = "drop") %>%
        rename_with(~ gsub(paste0(age, "_"), "", .x), matches(paste0("sum_", age)))
    } else {
      NULL  # Return NULL if no columns match to avoid errors
    }
  })
  return(age_data)
}

male_race_age_data <- lapply(male_race_data, separate_age_data)
female_race_age_data <- lapply(female_race_data, separate_age_data)


# Combine male and female data lists into one list for easier processing
all_data <- c(male_race_age_data, female_race_age_data)

# Filter out NULL elements if any (happens if no age columns matched)
all_data <- Filter(Negate(is.null), all_data)

# Combine all data frames into one, assuming they all have the same structure
final_data <- bind_rows(all_data)

# Optional: Check if final_data has complete cases or handle NAs
final_data <- final_data[complete.cases(final_data), ]


# Combine the summed values into a new dataset
combined_data <- list()

for (i in seq_along(race_categories)) {
  race <- race_categories[i]
  combined_data[[race]] <- bind_cols(male_race_age_data[[i]], female_race_age_data[[i]])
}

final_data <- bind_cols(combined_data) %>%
  bind_cols(FIPS, statea, stusab, puma, county, countya, tracta, year)

# Rename columns
colnames(final_data) <- c("FIPS", "statea", "stusab", "puma", "county", "countya", "tracta", "year", "male", "female", "white", "black", "asian", "hispanic", "indigenous", "othermulti", age_groups)

# Fill NA values with 0
final_data[is.na(final_data)] <- 0

write.csv(final_data, 'tract_race_newengland_processed.csv', row.names = FALSE)


# Create binary variables for gender and race
df_transformed <- df %>%
  mutate(
    # Gender binary variables
    male = ifelse(rowSums(select(., starts_with("total_male_"))) > 0, 1, 0),
    female = ifelse(rowSums(select(., starts_with("total_female_"))) > 0, 1, 0),
    # Race binary variables
    asian = ifelse(rowSums(select(., contains("asian_"))) > 0, 1, 0),
    white = ifelse(rowSums(select(., contains("white_"))) > 0, 1, 0),
    black = ifelse(rowSums(select(., contains("black_"))) > 0, 1, 0),
    indigenous = ifelse(rowSums(select(., contains("indigenous_"))) > 0, 1, 0),
    hispanic = ifelse(rowSums(select(., contains("hispanic_"))) > 0, 1, 0),
    othermulti = ifelse(rowSums(select(., contains("othermulti_"))) > 0, 1, 0)
  )

# Create binary variables for age groups
age_groups <- c("under_5_", "5_to_9_", "10_to_14_", "15_to_17_", "18_and_19_", 
                "20_", "21_", "22_to_24_", "25_to_29_", "30_to_34_", "35_to_39_", 
                "40_to_44_", "45_to_49_", "50_to_54_", "55_to_59_", "60_and_61_", 
                "62_to_64_", "65_and_66_", "67_to_69_", "70_to_74_", "75_to_79_", 
                "80_to_84_", "85_and_over_")

for (age_group in age_groups) {
  df_transformed <- df_transformed %>%
    mutate(across(contains(age_group), ~ ifelse(. > 0, 1, 0), .names = "age_{col}"))
}

# Select the desired columns
df_transformed <- df_transformed %>%
  select(FIPS, statea, stusab, puma, county, countya, tracta, year,
         male, female, asian, white, black, indigenous, hispanic, othermulti,
         matches("^age_"))

# Write the final dataframe to a CSV file
write.csv(df_transformed, 'tract_race_newengland_processed.csv', row.names = FALSE)
