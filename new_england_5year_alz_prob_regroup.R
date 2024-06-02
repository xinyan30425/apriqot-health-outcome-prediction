library(dplyr)
library(tidyr)

# Load your dataset
# df <- read.csv("/Users/xinyanliu/Desktop/NEU/Apriqot/Alzheimer/new_england_people_5year_18_22_alzheimer_prob_addgeoid_addweight_withoutedu.csv")
df <- read.csv("/Users/xinyanliu/Desktop/NEU/Apriqot/Alzheimer/new_england_people_5year_18_22_alzheimer_prob_addgeoid_addweight_withedu.csv")

# Create binary columns for each age group
df <- df %>%
  mutate(`65-69` = as.integer(AGEG == '65-69'),
         `70-74` = as.integer(AGEG == '70-74'),
         `75-79` = as.integer(AGEG == '75-79'),
         `80-84` = as.integer(AGEG == '80-84'),
         `>85` = as.integer(AGEG == '85+'))

# Create binary columns for sex
df <- df %>%
  mutate(`Male` = as.integer(SEX == 'Male'),
         `Female` = as.integer(SEX == 'Female'))

# Create binary columns for race
df <- df %>%
  mutate(`Other` = as.integer(RACE == 'Other'),
        `Hispanic` = as.integer(RACE == 'Hispanic'),
         `Black` = as.integer(RACE == 'Black'))

# Now drop the original 'AGEG', 'SEX', and 'RACE' columns
df <- df %>%
  select(-AGEG, -SEX, -RACE)

# Save the transformed DataFrame to a new CSV file
#write.csv(df, 'new_england_5year-alz_prob_withoutedu_transformed_data.csv', row.names = FALSE)
write.csv(df, 'new_england_5year-alz_prob_withedu_transformed_data.csv', row.names = FALSE)

# Filter rows where ST is 23
df_filtered <- df %>% 
  filter(ST == 23)

# Write the filtered data to a new CSV file
#write.csv(df_filtered, 'maine_5year-alz_prob_withoutedu_transformed_data.csv', row.names = FALSE)
write.csv(df_filtered, 'maine_5year-alz_prob_withedu_transformed_data.csv', row.names = FALSE)

# Group by all variables except pwgtp and alzheimer_prob
# Sum the pwgtp within these groups
# Calculate the weighted average of alzheimer_prob
df_grouped <- df %>%
  group_by(across(-c(PWGTP, alzheimer_prob,EDUCATION))) %>%
  summarise(
    total_pwgtp = sum(PWGTP,na.rm = TRUE),
    av_education= mean(EDUCATION,na.rm = TRUE),
    weighted_alzheimer_prob = sum(alzheimer_prob * PWGTP) / sum(PWGTP)
  ) %>%
  ungroup()  # Ungroup to remove the grouping

# View the first few rows of the new dataset
# head(df_grouped)

# Save the transformed DataFrame to a new CSV file
#write.csv(df_grouped, 'new_england_people_5year_18_22_alzheimer_prob_withoutedu_grouped_data.csv', row.names = FALSE)
write.csv(df_grouped, 'new_england_people_5year_18_22_alzheimer_prob_withedu_grouped_data.csv', row.names = FALSE)

# Filter rows where ST is 23
df_grouped_filtered <- df_grouped %>% 
  filter(ST == 23)

# Write the filtered data to a new CSV file
#write.csv(df_grouped_filtered, 'maine_5year-alz_prob_withoutedu_grouped_data.csv', row.names = FALSE)
write.csv(df_grouped_filtered, 'maine_5year-alz_prob_withedu_grouped_data.csv', row.names = FALSE)


df <- read.csv("/Users/xinyanliu/Desktop/NEU/Apriqot/NCHS/new_england_processed_data.csv")

# Group by all variables except Population and alzheimer_prob
# Sum the pwgtp within these groups
# Calculate the weighted average of alzheimer_prob
df_grouped <- df %>%
  group_by(across(-c(Population))) %>%
  summarise(
    total_population = sum(Population),
  ) %>%
  ungroup()  # Ungroup to remove the grouping

# View the first few rows of the new dataset
head(df_grouped)

# Save the transformed DataFrame to a new CSV file
write.csv(df_grouped, '/Users/xinyanliu/Desktop/NEU/Apriqot/NCHS/new_england_processed_grouped_data.csv', row.names = FALSE)




