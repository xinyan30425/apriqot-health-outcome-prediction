# Install packages
install.packages("haven")
install.packages("ggplot2")
install.packages("survey")
# Load the packages
library(conflicted)
library(haven)
library(ggplot2)
library(dplyr)
library(scales)  # Load the scales library for formatting
library(gridExtra)
library(forcats) 
library(readr)
library(gridExtra)
library(caret)
library(stringr)
library(purrr) # For map_df()
# Use survey package 
library(survey)
library(tidyverse)

read_and_process <- function(file_path) {
  print(paste("Processing:", file_path))
  # Read the dataset
  data <- read_csv(file_path, show_col_types = FALSE)
  
  # Convert column names to uppercase immediately after reading the dataset
  names(data) <- toupper(names(data))
  
  # Now perform data transformations
  data <- data %>%
    mutate(
      # Create binary variables while excluding unwanted values
      ASTHMA = if_else(ASTHMA3 == 1, 1, 0),
      # DIABETES = if_else(DIABETE3 == 1, 1, 0), # diabetes variable in 2016,2018 dataset are labeled as DIABETE3
      DIABETES = if_else(DIABETE4 == 1, 1, 0), # diabetes variable in 2020,2022 dataset are labeled as DIABETE4
      COGNITIVE_DECLINE = if_else(CIMEMLOS == 1, 1, 0),
      # SEX = if_else(SEX1 == 1, 1, 0), # SEX variable in 2016,2018 dataset are labeled as SEX1
      BIRTHSEX = if_else(BIRTHSEX == 1, 1, 0), # SEX variable in 2020 dataset is labeled as BIRTHSEX
      # SEX = if_else(SEX == 1, 1, 0), # SEX variable in 2022 dataset is labeled as SEX
      EMPLOYMENT_STATUS = if_else(EMPLOY1 %in% c(1, 2), 1, 0),
      # POVERTY = ifelse(INCOMG %in% c(1, 2), 1, 0),# income level variable in 2016,2018,2020 dataset is labeled as INCOMG
      POVERTY = ifelse(INCOME3 %in% c(1, 2), 1, 0), # income level variable in 2022 dataset is labeled as INCOME3
      OVER_65 = if_else(AGE65YR == 2, 1,0),
      # Handling RACE separately due to multiple conditions
      RACE = case_when(
        RACE1 == 1 ~ "White",
        RACE1 == 2 ~ "Black",
        RACE1 == 3 ~ "AmericanIndian",
        RACE1 == 4 ~ "Asian",
        TRUE ~ "Other"
      ) # race variable in 2022 dataset is labeled as RACE1, with RACE for the others
    ) %>%
    mutate(RACE = factor(RACE)) %>%
    mutate(RACE = relevel(RACE, ref = "White")) %>%
    mutate(
      # Create DISABILITY variable based on conditions
      DISABILITY = case_when(
        DEAF == 1 ~ "deaf",
        BLIND == 1 ~ "blind",
        DIFFWALK == 1 ~ "diffwalk",
        DIFFDRES == 1 ~ "diffdres",
        DIFFALON == 1 ~ "diffalon",
        TRUE ~ "none" # Consider adding a default category if none of the conditions are met
      )
    ) %>%
    mutate(DISABILITY = factor(DISABILITY)) %>%
    mutate(DISABILITY = relevel(DISABILITY, ref = "diffalon"))
  
  # Extract year from file path
  year <- as.integer(str_extract(file_path, "\\d{4}"))
  data$YEAR <- year
  
  
  # Save the processed dataset as a CSV file
  processed_file_path <- str_replace(file_path, ".csv", paste0("_PROCESSED_", year, ".csv"))
  write_csv(data, processed_file_path)
  
  return(data)
}

# Process each file separately; this will save each processed file as a new CSV
# processed_data_list <- lapply(file_paths, read_and_process)

# Combine all processed datasets for analysis
# all_data_processed <- bind_rows(processed_data_list)
# all_data_processed <- all_data_processed[!is.na(all_data_processed$LLCPWT2), ]

# file_path <-"/Users/xinyanliu/Desktop/NEU/Apriqot/BRFSS/LLCP2016/LLCP16V1_New_England.csv"
# year <- "2016"

# file_path <-"/Users/xinyanliu/Desktop/NEU/Apriqot/BRFSS/LLCP2018/LLCP18V1_New_England.csv"
# year <- "2018"

# file_path <-"/Users/xinyanliu/Desktop/NEU/Apriqot/BRFSS/LLCP2020/LLCP2020_New_England.csv"
# year <-"2020"

file_path <-"/Users/xinyanliu/Desktop/NEU/Apriqot/BRFSS/LLCP2022/LLCP2022_New_England.csv"
year <- "2022"

# Process the file; this will save the processed file as a new CSV
processed_data <- read_and_process(file_path)

# Ensure binary variables are treated as numeric
# processed_data$ASTHMA <- as.numeric(processed_data$ASTHMA)
# processed_data$DIABETES <- as.numeric(processed_data$DIABETES)
# processed_data$COGNITIVE_DECLINE <- as.numeric(processed_data$COGNITIVE_DECLINE)
# processed_data$SEX <- as.numeric(processed_data$SEX)
# processed_data$EMPLOYMENT_STATUS <- as.numeric(processed_data$EMPLOYMENT_STATUS)
# processed_data$DISABILITY <- as.numeric(processed_data$DISABILITY)
# processed_data$OVER_65 <- as.numeric(processed_data$OVER_65)
# processed_data$POVERTY <- as.numeric(processed_data$POVERTY)

# Ensure 'RACE' is a factor with all expected levels
# processed_data$RACE <- factor(processed_data$RACE, levels = c("White", "Black", "AmericanIndian", "Asian", "Other"))

# Check levels of each factor variable
# lapply(processed_data[c("SEX", "EMPLOYMENT_STATUS", "RACE", "DISABILITY", "OVER_65", "POVERTY")], function(x) levels(factor(x)))

# Confirm presence of multiple levels in the data
# sapply(processed_data[c("SEX", "EMPLOYMENT_STATUS", "RACE", "DISABILITY", "OVER_65", "POVERTY")], function(x) length(unique(x)))

# If any of the factor variables have only one level, you need to address this before fitting the model.

# Assuming no stratification and treating each observation as its own PSU
brfss_design <- svydesign(ids = ~1, weights = ~LLCPWT2, data = processed_data)

asthma_model_svy <- svyglm(ASTHMA ~ SEX + EMPLOYMENT_STATUS + RACE + OVER_65 + POVERTY, 
                    design = brfss_design, 
                    family = binomial(link = "logit"))
summary(asthma_model_svy)

diabetes_model_svy <- svyglm(DIABETES ~ SEX + EMPLOYMENT_STATUS + RACE + OVER_65 + POVERTY, 
                    design = brfss_design, 
                    family = binomial(link = "logit"))
summary(diabetes_model_svy)


congdec_model_svy <- svyglm(COGNITIVE_DECLINE ~ SEX + EMPLOYMENT_STATUS + RACE + OVER_65 + POVERTY, 
                    design = brfss_design, 
                    family = binomial(link = "logit"))
summary(congdec_model_svy)


# combine four year dataset, add disability as categorical variable
read_and_process_combine <- function(file_path) {
  print(paste("Processing:", file_path))
  
  # Extract year from file path to handle different variable names by year
  year <- as.integer(str_extract(file_path, "\\d{4}"))
  
  # Read the dataset
  data <- read_csv(file_path, show_col_types = FALSE)
  
  # Convert column names to uppercase immediately after reading the dataset
  names(data) <- toupper(names(data))
  
  # Handle variable naming differences by year
  # diabetes variable in 2016,2018 dataset are labeled as DIABETE3
  # diabetes variable in 2020,2022 dataset are labeled as DIABETE4
  # SEX variable in 2018 dataset are labeled as SEX1
  # SEX variable in 2020 dataset is labeled as BIRTHSEX
  # SEX variable in 2016,2022 dataset is labeled as SEX
  # income level variable in 2016,2018,2020 dataset is labeled as INCOMG
  # income level variable in 2022 dataset is labeled as INCOME3
  # race variable in 2022 dataset is labeled as RACE1, with RACE for the others
  
  # Handle variable naming differences by year
  diabetes_var <- ifelse(year %in% c(2016, 2018), "DIABETE3", "DIABETE4")
  sex_var <- ifelse(year == 2020, "BIRTHSEX", 
                    ifelse(year == 2018, "SEX1", "SEX"))
  income_var <- ifelse(year == 2022, "INCOME3", "INCOMG")
  race_var <- ifelse(year == 2022, "RACE1", "RACE")
  
  # Now perform data transformations
  data <- data %>%
    mutate(
      ASTHMA = if_else(ASTHMA3 == 1, 1, 0),
      DIABETES = if_else(!!sym(diabetes_var) == 1, 1, 0),
      COGNITIVE_DECLINE = if_else(CIMEMLOS == 1, 1, 0),
      SEX = if_else(!!sym(sex_var) == 1, 1, 0),
      EMPLOYMENT_STATUS = if_else(EMPLOY1 %in% c(1, 2), 1, 0),
      POVERTY = if_else(!!sym(income_var) %in% c(1, 2, 3), 1, 0),
      OVER_65 = if_else(AGE65YR == 2, 1, 0),
      RACE = case_when(
        !!sym(race_var) == 1 ~ "White",# White only
        !!sym(race_var) == 2 ~ "Black", # Black or African American only
        !!sym(race_var) == 3 ~ "AmericanIndian", # American Indian or Alaskan Native only
        !!sym(race_var) == 4 ~ "Asian",# Asian Only
        !!sym(race_var) == 5 ~ "Hawaiian", # Native Hawaiian or other Pacific Islander only
        TRUE ~ "Other"
      ),
      # DISABILITY = case_when(
      #   DEAF == 1 ~ "deaf",
      #   BLIND == 1 ~ "blind",
      #   DIFFWALK == 1 ~ "diffwalk",
      #   DIFFDRES == 1 ~ "diffdres",
      #   DIFFALON == 1 ~ "diffalon",
      #   TRUE ~ "none"
      # ),
      # Add binary variable for having any disability
      HAVE_DISABILITY = if_else(DEAF == 1 | BLIND == 1 | DIFFWALK == 1 | DIFFDRES == 1 | DIFFALON == 1, 1, 0),
      YEAR = year
    ) %>%
    mutate(RACE = factor(RACE), RACE = relevel(RACE, ref = "White"))
           #DISABILITY = factor(DISABILITY), DISABILITY = relevel(DISABILITY, ref = "diffalon"))
  
  # Construct the output file path
  output_file_path <- str_replace(file_path, ".csv", paste0("_PROCESSED_", year, ".csv"))
  
  # Save the processed data to a CSV file
  write_csv(data, output_file_path)
  
  return(data)
}

# Define the file paths
file_paths <- c(
  "2016" = "/Users/xinyanliu/Desktop/NEU/Apriqot/BRFSS/LLCP2016/LLCP16V1_New_England.csv",
  "2018" = "/Users/xinyanliu/Desktop/NEU/Apriqot/BRFSS/LLCP2018/LLCP18V1_New_England.csv",
  "2020" = "/Users/xinyanliu/Desktop/NEU/Apriqot/BRFSS/LLCP2020/LLCP2020_New_England.csv",
  "2022" = "/Users/xinyanliu/Desktop/NEU/Apriqot/BRFSS/LLCP2022/LLCP2022_New_England.csv"
)

# Process each dataset and save the processed file as a new CSV
processed_data <- read_and_process_combine(file_path)

# Process each dataset and combine them
combined_data <- map_df(file_paths, read_and_process_combine)

save_path <- "/Users/xinyanliu/Desktop/NEU/Apriqot/BRFSS/new_england_combined_dataset.csv"
write_csv(combined_data, save_path)

# Select only the specified columns
combined_data_processed <- combined_data %>%
  select(ASTHMA, DIABETES, COGNITIVE_DECLINE,POVERTY, SEX, EMPLOYMENT_STATUS, OVER_65, RACE, HAVE_DISABILITY, YEAR, STATE, FMONTH, IYEAR, DEAF, BLIND, DIFFWALK, DIFFDRES, DIFFALON, AGEG)


# Save the combined dataset with only selected columns to a new CSV file
save_path <- "/Users/xinyanliu/Desktop/NEU/Apriqot/BRFSS/new_england_combined_dataset_processed.csv"
write_csv(combined_data_processed, save_path)

# Print a message to indicate completion
print(paste("Combined dataset saved to:", save_path))


brfss_design <- svydesign(ids = ~1, weights = ~LLCPWT2, data = combined_data)

# Asthma model with DISABILITY and YEAR
asthma_model_svy <- svyglm(ASTHMA ~ SEX + EMPLOYMENT_STATUS + RACE + OVER_65 + POVERTY + HAVE_DISABILITY+ YEAR, 
                           design = brfss_design, 
                           family = binomial(link = "logit"))
summary(asthma_model_svy)

# Diabetes model with DISABILITY and YEAR
diabetes_model_svy <- svyglm(DIABETES ~ SEX + EMPLOYMENT_STATUS + RACE + OVER_65 + POVERTY  + YEAR + HAVE_DISABILITY, 
                             design = brfss_design, 
                             family = binomial(link = "logit"))
summary(diabetes_model_svy)

# Cognitive decline model with DISABILITY and YEAR
congdec_model_svy <- svyglm(COGNITIVE_DECLINE ~ SEX + EMPLOYMENT_STATUS + RACE + OVER_65 + POVERTY + YEAR+ HAVE_DISABILITY, 
                            design = brfss_design, 
                            family = binomial(link = "logit"))
summary(congdec_model_svy)

# Asthma model with DISABILITY
asthma_model_svy <- svyglm(ASTHMA ~ SEX + EMPLOYMENT_STATUS + RACE + OVER_65 + POVERTY + HAVE_DISABILITY, 
                           design = brfss_design, 
                           family = binomial(link = "logit"))
summary(asthma_model_svy)

# Diabetes model with DISABILITY and YEAR
diabetes_model_svy <- svyglm(DIABETES ~ SEX + EMPLOYMENT_STATUS + RACE + OVER_65 + POVERTY  + HAVE_DISABILITY, 
                             design = brfss_design, 
                             family = binomial(link = "logit"))
summary(diabetes_model_svy)

# Cognitive decline model with DISABILITY and YEAR
congdec_model_svy <- svyglm(COGNITIVE_DECLINE ~ SEX + EMPLOYMENT_STATUS + RACE + OVER_65 + POVERTY + HAVE_DISABILITY, 
                            design = brfss_design, 
                            family = binomial(link = "logit"))
summary(congdec_model_svy)


# combine ACS 1 year data(2018-2022), add year variable
# Define the file paths and the corresponding year for each file

file_paths <- c(
                "/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/18csv_pct/psam_p09.csv",
                "/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/19csv_pct/psam_p09.csv",
                "/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/20csv_pct/psam_p09.csv",
                "/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/21csv_pct/psam_p09.csv",
                "/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/22csv_pct/psam_p09.csv",
  
                "/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/18csv_pme/psam_p23.csv",
                "/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/19csv_pme/psam_p23.csv",
                "/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/20csv_pme/psam_p23.csv",
                "/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/21csv_pme/psam_p23.csv",
                "/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/22csv_pme/psam_p23.csv",
                
                "/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/18csv_pma/psam_p25.csv",
                "/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/19csv_pma/psam_p25.csv",
                "/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/20csv_pma/psam_p25.csv",
                "/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/21csv_pma/psam_p25.csv",
                "/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/22csv_pma/psam_p25.csv",
                
                "/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/18csv_pnh/psam_p33.csv",
                "/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/19csv_pnh/psam_p33.csv",
                "/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/20csv_pnh/psam_p33.csv",
                "/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/21csv_pnh/psam_p33.csv",
                "/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/22csv_pnh/psam_p33.csv",
                
                "/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/18csv_pri/psam_p44.csv",
                "/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/19csv_pri/psam_p44.csv",
                "/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/20csv_pri/psam_p44.csv",
                "/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/21csv_pri/psam_p44.csv",
                "/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/22csv_pri/psam_p44.csv",
                
                "/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/18csv_pvt/psam_p50.csv",
                "/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/19csv_pvt/psam_p50.csv",
                "/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/20csv_pvt/psam_p50.csv",
                "/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/21csv_pvt/psam_p50.csv",
                "/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/22csv_pvt/psam_p50.csv"
                )

years <- c(2018, 2019, 2020, 2021, 2022,2018, 2019, 2020, 2021, 2022,2018, 2019, 2020, 2021, 2022,2018, 2019, 2020, 2021, 2022,2018, 2019, 2020, 2021, 2022,2018, 2019, 2020, 2021, 2022)

# Initialize an empty list to store data frames
data_list <- list()

# Loop over the file paths and years, read each file, add the YEAR column, and store in the list
for(i in 1:length(file_paths)) {
  df <- read.csv(file_paths[i])
  df$YEAR <- years[i]
  data_list[[i]] <- df
}

# Combine all data frames into one
combined_df <- bind_rows(data_list)

# Write the combined data frame to a new CSV file
write.csv(combined_df, "/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/newengland_2018_2022_combined_dataset.csv", row.names = FALSE)

# Apply model fit coefficent to ACS dataset
process_new_dataset <- function(file_path) {
  # Read the dataset
  new_data <- read_csv(file_path, show_col_types = FALSE)
  
  # Create binary variable for age
  new_data <- new_data %>%
    mutate(
      # FOOD_INSECURITY = NA_integer_, # Placeholder, as predictions will replace this
      SEX = if_else(SEX == 1, 1, 0),
      POVERTY = if_else(POVPIP <= 185, 1, 0),
      OVER_65 = if_else(AGEP > 65, 1, 0),
      # Create binary employment status variable
      EMPLOYMENT_STATUS = if_else(ESR %in% c("1", "2", "4", "5", "6"), 1, 0),
      # Convert DIS to binary variable
      HAVE_DISABILITY = if_else(DIS == 1, 1, 0),
      # US_BORN = if_else(NATIVITY == 1, 1, 0),
      # Recode race code of the householder
      WHITE = if_else(RAC1P == 1, 1, 0),
      BLACK = if_else(RAC1P == 2, 1, 0),
      AMERICANINDIAN = if_else(RAC1P == 3, 1, 0),
      ASIAN = if_else(RAC1P == 6, 1, 0),
      HAWAIIAN = if_else(RAC1P == 7, 1, 0),
      RACE = case_when(
        RAC1P == 1 ~ "White",
        RAC1P == 2 ~ "Black",
        RAC1P == 3 ~ "AmericanIndian",
        RAC1P == 6 ~ "Asian",
        RAC1P == 7 ~ "Hawaiian",
        TRUE ~ "Other" # Handle other cases or include additional race categories if needed
      ),
      # Add AGEG based on AGEP
      # AGEG = case_when(
      #   AGEP >= 18 & AGEP <= 24 ~ 1,
      #   AGEP >= 25 & AGEP <= 29 ~ 2,
      #   AGEP >= 30 & AGEP <= 34 ~ 3,
      #   AGEP >= 35 & AGEP <= 39 ~ 4,
      #   AGEP >= 40 & AGEP <= 44 ~ 5,
      #   AGEP >= 45 & AGEP <= 49 ~ 6,
      #   AGEP >= 50 & AGEP <= 54 ~ 7,
      #   AGEP >= 55 & AGEP <= 59 ~ 8,
      #   AGEP >= 60 & AGEP <= 64 ~ 9,
      #   AGEP >= 65 & AGEP <= 69 ~ 10,
      #   AGEP >= 70 & AGEP <= 74 ~ 11,
      #   AGEP >= 75 & AGEP <= 79 ~ 12,
      #   AGEP >= 80 & AGEP <= 99 ~ 13
      # ),
      AGEG = case_when(
        AGEP >= 18 & AGEP <= 24 ~ 1,
        AGEP >= 25 & AGEP <= 34 ~ 2,
        AGEP >= 34 & AGEP <= 44 ~ 3,
        AGEP >= 45 & AGEP <= 54 ~ 4,
        AGEP >= 55 & AGEP <= 64 ~ 5,
        AGEP >= 65 ~ 6,
      ),
      STATE = ST # Rename ST to STATE
    ) %>%
    mutate(RACE = factor(RACE))%>%
    mutate(RACE = relevel(RACE, ref = "White")) %>%
    select(SEX, EMPLOYMENT_STATUS,OVER_65,POVERTY,RACE,HAVE_DISABILITY,AGEG,YEAR,STATE)
  
  # Apply drop_na
  new_data <- drop_na(new_data)
  
  # Save the processed data to a new CSV file
  write_csv(new_data, "new_england_people_18_22_combined_processed2.csv")
  
  # Return the processed new data
  return(new_data)
}

# Read and process the new dataset
new_dataset_file_path <- "/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/newengland_2018_2022_combined_dataset.csv"
new_data_processed <- process_new_dataset(new_dataset_file_path)

summary(new_data_processed)


# Apply the model to the new dataset to get predictions for FOOD_INSECURITY
asthma_predictions <- predict(asthma_model_svy, newdata = new_data_processed, type = "response")
diabetes_predictions <- predict(diabetes_model_svy, newdata = new_data_processed, type = "response")
congdec_predictions <- predict(congdec_model_svy, newdata = new_data_processed, type = "response")

# Add predictions back to the new data frame if you want to analyze them together
new_data_processed$ASTHMA_PREDICTIONS <- asthma_predictions
new_data_processed$DIABETES_PREDICTIONS <- diabetes_predictions
new_data_processed$CONGDEC_PREDICTIONS <- congdec_predictions

# Output the new dataset with predictions
write_csv(new_data_processed, "/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/new_england_people_18_22_asthma_diabetes_congdec_prediction.csv")

# View summary of predictions
summary(asthma_predictions)
summary(diabetes_predictions)
summary(congdec_predictions)

# Convert RACE to numeric for plotting (this is just for visualization and may not be ideal for interpretation)
new_data_processed$RACE_numeric <- as.numeric(new_data_processed$RACE)

# Create a scatter plot with jitter for binary variables and use facets for RACE
# Adjust the previous plot code with a smaller size for scatter points
# Adjust the previous plot code to reflect the change to percentages


asthma_scatter_plot<- ggplot(new_data_processed, aes(y = ASTHMA_PREDICTIONS*100)) +
  geom_jitter(aes(x = as.numeric(SEX) * 0.5, color = "SEX"), width = 0.05, alpha = 0.5, size = 0.5) +
  geom_jitter(aes(x = as.numeric(EMPLOYMENT_STATUS) * 0.5 + 2, color = "EMPLOYMENT_STATUS"), width = 0.05, alpha = 0.5, size = 0.5) +
  geom_jitter(aes(x = as.numeric(HAVE_DISABILITY) * 0.5 + 4, color = "DISABILITY"), width = 0.05, alpha = 0.5, size = 0.5) +
  geom_jitter(aes(x = as.numeric(OVER_65) * 0.5 + 6, color = "OVER_65"), width = 0.05, alpha = 0.5, size = 0.5) +
  geom_jitter(aes(x = as.numeric(POVERTY) * 0.5 + 8, color = "POVERTY"), width = 0.05, alpha = 0.5, size = 0.5) +
  scale_x_continuous(breaks = c(0.125, 0.5, 2, 2.5, 4, 4.5, 6, 6.5,8, 8.5),
                     labels = c("Female", "Male","No Disability","Disability","Unemployed","Employed","Below 65","Over 65","Above-Poverty","Below-Poverty"),
                     #labels = c("0", "1", "0", "1", "0", "1", "0", "1","0", "1"),
                     limits = c(0, 9)) +
  scale_color_manual(values = c("SEX" = "blue", "DISABILITY" = "red","EMPLOYMENT_STATUS" = "green", "OVER_65" = "orange","POVERTY"="yellow")) +
  labs(color = "Variables", x = "Independent Variables", y = "Predicted Probability of Asthma (%)") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~RACE, ncol = 1)
  #facet_wrap(~RACE, ncol = 1, labeller = labeller(RACE = c(`1` = "White", `2` = "Black", `3` = "AmericanIndian", `4` = "Asian", `5` = "Other")))

# Print the plot
print(asthma_scatter_plot)
ggsave("acs_new_england_asthma_prediction.png", plot = asthma_scatter_plot, width = 20, height = 8)

diabetes_scatter_plot<- ggplot(new_data_processed, aes(y = DIABETES_PREDICTIONS*100)) +
  geom_jitter(aes(x = as.numeric(SEX) * 0.5, color = "SEX"), width = 0.05, alpha = 0.5, size = 0.5) +
  geom_jitter(aes(x = as.numeric(EMPLOYMENT_STATUS) * 0.5 + 2, color = "EMPLOYMENT_STATUS"), width = 0.05, alpha = 0.5, size = 0.5) +
  geom_jitter(aes(x = as.numeric(HAVE_DISABILITY) * 0.5 + 4, color = "DISABILITY"), width = 0.05, alpha = 0.5, size = 0.5) +
  geom_jitter(aes(x = as.numeric(OVER_65) * 0.5 + 6, color = "OVER_65"), width = 0.05, alpha = 0.5, size = 0.5) +
  geom_jitter(aes(x = as.numeric(POVERTY) * 0.5 + 8, color = "POVERTY"), width = 0.05, alpha = 0.5, size = 0.5) +
  scale_x_continuous(breaks = c(0.125, 0.5, 2, 2.5, 4, 4.5, 6, 6.5,8, 8.5),
                     labels = c("Female", "Male","No Disability","Disability","Unemployed","Employed","Below 65","Over 65","Above-Poverty","Below-Poverty"),
                     #labels = c("0", "1", "0", "1", "0", "1", "0", "1","0", "1"),
                     limits = c(0, 9)) +
  scale_color_manual(values = c("SEX" = "blue","DISABILITY" = "red", "EMPLOYMENT_STATUS" = "green", "OVER_65" = "orange","POVERTY"="yellow")) +
  labs(color = "Variables", x = "Independent Variables", y = "Predicted Probability of Diabetes (%)") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~RACE, ncol = 1)
  #facet_wrap(~RACE, ncol = 1, labeller = labeller(RACE = c(`1` = "White", `2` = "Black", `3` = "AmericanIndian", `4` = "Asian", `5` = "Other")))

ggsave("acs_new_england_diabetes_prediction.png", plot = diabetes_scatter_plot, width = 20, height = 8)

congdec_scatter_plot<- ggplot(new_data_processed, aes(y = CONGDEC_PREDICTIONS*100)) +
  geom_jitter(aes(x = as.numeric(SEX) * 0.5, color = "SEX"), width = 0.05, alpha = 0.5, size = 0.5) +
  geom_jitter(aes(x = as.numeric(EMPLOYMENT_STATUS) * 0.5 + 2, color = "EMPLOYMENT_STATUS"), width = 0.05, alpha = 0.5, size = 0.5) +
  geom_jitter(aes(x = as.numeric(HAVE_DISABILITY) * 0.5 + 4, color = "DISABILITY"), width = 0.05, alpha = 0.5, size = 0.5) +
  geom_jitter(aes(x = as.numeric(OVER_65) * 0.5 + 6, color = "OVER_65"), width = 0.05, alpha = 0.5, size = 0.5) +
  geom_jitter(aes(x = as.numeric(POVERTY) * 0.5 + 8, color = "POVERTY"), width = 0.05, alpha = 0.5, size = 0.5) +
  scale_x_continuous(breaks = c(0.125, 0.5, 2, 2.5, 4, 4.5, 6, 6.5,8, 8.5),
                     labels = c("Female", "Male","No Disability","Disability","Unemployed","Employed","Below 65","Over 65","Above-Poverty","Below-Poverty"),
                     #labels = c("0", "1", "0", "1", "0", "1", "0", "1", "0", "1"),
                     limits = c(0, 9)) +
  scale_color_manual(values = c("SEX" = "blue", "DISABILITY" = "red","EMPLOYMENT_STATUS" = "green", "OVER_65" = "orange","POVERTY"="yellow")) +
  labs(color = "Variables", x = "Independent Variables", y = "Predicted Probability of Diabetes (%)") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~RACE, ncol = 1)
  #facet_wrap(~RACE, ncol = 1, labeller = labeller(RACE = c(`1` = "White", `2` = "Black", `3` = "AmericanIndian", `4` = "Asian", `5` = "Other")))

ggsave("acs_new_england_congdec_prediction.png", plot = congdec_scatter_plot, width = 20, height = 8)




