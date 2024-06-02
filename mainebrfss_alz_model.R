library(readr)
library(dplyr)
library(survey)

# seperate years 
# IYEAR 
# CNF_MMRY:During the past 12 months, have you experienced confusion or memory loss that is happening more often or is getting worse? 1=yes,2=no,7=dont know,9=refuse
# AGE: 45-64,65-69,70-74,75-79,80-85,85+
# HISPANIC: Are you Hispanic or Latino? 1 = yes, 2 = no, 7 = dont know, not sure
# MRACE:20 = Black or African American,10 = White,30 =American Indian or Alaska Native, 40,41,42,43,44,45,46,47,50,51,52,53,54=Asian,0ther to other

# MRACE:Non-Hispanic Race including Multiracial,  1= White Only, 2= Black or African American Only, 3,4=Asian,PI,5= American Indian or Alaskan Native, other

# MRACE1 10= White,20= Black or African American,30= American Indian or Alaska Native,40,41,42,43,44,45,46,47,50,51,52,53,54 = Asian,60 = other
# (2013-2021)

# RACE,1 = White only, non-Hispanic, 2 = Black only, non-Hispanic, 3 = American Indian or Alaskan Native only, 
# Non-Hispanic,4 = Asian only, non-Hispanic,5 = Native Hawaiian or other Pacific,Islander only, Non-Hispanic,
# 6 = Other race only, non-Hispanic,8 = Hispanic

# RACE2: 1= White Only, Non-Hispanic, 2= Black Only, Non-Hispanic,3,4 Asian,PI Non-Hispanic,5=American Indian, 8,Hispanic,other=other
# SEX: 1 = Male, 2 = Female
# EDUCA: What is the highest grade or year of school you completed?
# do category 
# 1= Never attended school or kindergarten only
# 2= Grades 1 through 8 (Elementary)
# 3= Grades 9 through 11 (Some High school)
# 4= Grade 12 or GED (High school graduate)
# 5= College 1 year to 3 years (Some college or technical school)
# 6= College 4 years or more (College graduate)
# 9= Refused


setwd("/Users/xinyanliu/Desktop/NEU/Apriqot/MaineCDCBRFSS")
me21 <- read.csv("me21_remove_prefix.csv")

############ check variables ############
# Function to print unique IYEAR values after removing NA in a specific column
print_unique_YEAR <- function(data, column) {
  filtered_data <- data %>%
    filter(!is.na(!!sym(column))) %>%
    select(YEAR)
  
  unique_values <- unique(filtered_data$YEAR)
  
  cat(paste("Unique values in IYEAR after removing NA from", column, ":\n"))
  print(unique_values)
  cat("\n")
}

# Columns to check
columns_to_check <- c("CNF_MMRY", "AGE", "HISPANIC", "RACE2", "EDUCA")

# Loop through each column and print unique IYEAR values
for (column in columns_to_check) {
  print_unique_YEAR(me21, column)
}


# Remove rows with NA in specified columns
me21_removena <- me21[!is.na(me21$YEAR) & !is.na(me21$CNF_MMRY) & !is.na(me21$AGE) & !is.na(me21$HISPANIC) & 
               !is.na(me21$RACE2) & !is.na(me21$EDUCA), ]


# Select specific columns
me21_filtered <- me21_removena[, c("YEAR", "CNF_MMRY", "AGE", "HISPANIC", "RACE2", "EDUCA")]

# Print the unique values for each selected column
unique_IYEAR <- unique(me21_filtered$YEAR)
unique_CNF_MMRY <- unique(me21_filtered$CNF_MMRY)
unique_AGE <- unique(me21_filtered$AGE)
unique_HISPANIC <- unique(me21_filtered$HISPANIC)
unique_RACE2 <- unique(me21_filtered$RACE2)
unique_EDUCA <- unique(me21_filtered$EDUCA)

cat("Unique values in IYEAR:\n")
print(unique_IYEAR)
cat("\nUnique values in CNF_MMRY:\n")
print(unique_CNF_MMRY)
cat("\nUnique values in AGE:\n")
print(unique_AGE)
cat("\nUnique values in HISPANIC:\n")
print(unique_HISPANIC)
cat("\nUnique values in RACE2:\n")
print(unique_RACE2)
cat("\nUnique values in EDUCA:\n")
print(unique_EDUCA)
############

# Remove rows with NA in CNF_MMRY column
me21_no_na_cnf_mmry <- me21 %>%
  filter(!is.na(CNF_MMRY))
# Select specific columns
me21_filtered <- me21_no_na_cnf_mmry %>%
  select(IYEAR,AGE,RACE, EDUCA, SEX,CNF_MMRY,LLCPWT,STSTR)

# Print the unique values in IYEAR column
unique_IYEAR <- unique(me21_filtered$IYEAR)
unique_cnf <- unique(me21_filtered$CNF_MMRY)
unique_RACE <- unique(me21_filtered$RACE)
cat("Unique values in IYEAR after removing NA from CNF_MMRY and selecting specific columns:\n")
print(unique_IYEAR)
print(unique_cnf)
print(unique_RACE)

# Regroup AGE data
me21_filtered <- me21_filtered %>%
  mutate(AGE_GROUP = case_when(
    AGE >= 45 & AGE <= 64 ~ "45-64",
    AGE >= 65 & AGE <= 69 ~ "65-69",
    AGE >= 70 & AGE <= 74 ~ "70-74",
    AGE >= 75 & AGE <= 79 ~ "75-79",
    AGE >= 80 & AGE <= 85 ~ "80-85",
    AGE > 85 ~ "85+"
  ))

# Rename SEX values
me21_filtered <- me21_filtered %>%
  mutate(SEX_GROUP = case_when(
    SEX == 1 ~ "Male",
    SEX == 2 ~ "Female"
  ))

# Regroup RACE2 data
me21_filtered <- me21_filtered %>%
  mutate(RACE_GROUP = case_when(
    RACE == 1 ~ "White",
    RACE == 2 ~ "Black",
    RACE == 3 ~ "American Indian or Alaskan Native",
    RACE %in% c(4, 5) ~ "Asian and PI",
    RACE == 8 ~ "Hispanic",
    TRUE ~ "Other"
  ))

# Select relevant columns for the final output
me21_final <- me21_filtered %>%
  select(IYEAR, AGE_GROUP, RACE_GROUP, EDUCA, SEX_GROUP, CNF_MMRY,LLCPWT,STSTR)

# Save the processed data to a CSV file
write.csv(me21_final, "me21_cleaned.csv", row.names = FALSE)

# Print the first few rows of the processed data
print(head(me21_final))

# Create survey design
brfssdsgn <- svydesign(
  id = ~1,
  strata = ~STSTR,
  weights = ~LLCPWT,
  data = me21_final
)

# Print the survey design object
print(brfssdsgn)

# Perform regression analysis
regression_model <- svyglm(CNF_MMRY ~ AGE_GROUP + RACE_GROUP + EDUCA + SEX_GROUP, design = brfssdsgn)

# Print summary of the regression model
summary(regression_model)

svymean(~CNF_MMRY, brfssdsgn, na.rm = TRUE)

######################################### Perform logistic regression analysis

# Convert CNF_MMRY to binary
me21_final$CNF_MMRY_BINARY <- ifelse(me21_final$CNF_MMRY == 1, 1, 0)

# Relevel factors to set new reference groups
me21_final$AGE_GROUP <- relevel(factor(me21_final$AGE_GROUP), ref = "65-69")
me21_final$SEX_GROUP <- relevel(factor(me21_final$SEX_GROUP), ref = "Male")
me21_final$RACE_GROUP <- relevel(factor(me21_final$RACE_GROUP), ref = "White")


# Set options for allowing a single observation per stratum
options(survey.lonely.psu = "adjust")

# Create survey design
brfssdsgn <- svydesign(
  id = ~1,
  strata = ~STSTR,
  weights = ~LLCPWT,
  data = me21_final
)

logistic_model <- svyglm(CNF_MMRY_BINARY ~ AGE_GROUP + RACE_GROUP + EDUCA + SEX_GROUP, 
                         design = brfssdsgn, 
                         family = quasibinomial())

# Print summary of the logistic regression model
summary(logistic_model)

# Extract coefficients and calculate odds ratios (OR)
coefficients <- summary(logistic_model)$coefficients
odds_ratios <- exp(coefficients[, 1])
conf_int <- exp(confint(logistic_model))

# Create a data frame for the results
results <- data.frame(
  Estimate = coefficients[, 1],
  OR = odds_ratios,
  `95% CI Lower` = conf_int[, 1],
  `95% CI Upper` = conf_int[, 2],
  `P-value` = coefficients[, 4]
)

# Print the results
print(results)

######## include year as an independent variable
# Perform logistic regression analysis including IYEAR as an independent variable
logistic_model_addyear <- svyglm(CNF_MMRY_BINARY ~ IYEAR + AGE_GROUP + RACE_GROUP + EDUCA + SEX_GROUP, 
                         design = brfssdsgn, 
                         family = quasibinomial())

# Print summary of the logistic regression model
summary(logistic_model_addyear)

# Extract coefficients and calculate odds ratios (OR)
coefficients <- summary(logistic_model_addyear)$coefficients
odds_ratios <- exp(coefficients[, 1])
conf_int <- exp(confint(logistic_model_addyear))

# Create a data frame for the results
results <- data.frame(
  Estimate = coefficients[, 1],           # These are the beta coefficients
  OR = odds_ratios,                       # These are the exponentiated coefficients (odds ratios)
  `95% CI Lower` = conf_int[, 1],         # Lower bound of the 95% confidence interval
  `95% CI Upper` = conf_int[, 2],         # Upper bound of the 95% confidence interval
  `P-value` = coefficients[, 4]           # P-values for the coefficients
)

# Print the results
print(results)



