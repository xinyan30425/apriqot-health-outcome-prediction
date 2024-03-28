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

##############################################################################################
# logistic regression on the cpsdecMaine data: 
# dependent variable: 
# food_security (HRFS12MD=4 Low food security),
# independent variables:
# sex, Race (tried “white” as binary variable), 
# Country of born (us_born as binary variable), 
# employment_status (employed and unemployed), 
# and disability 

# Read CSV file into R.
# Filter the HRFS12MD variable so that only those with a value of 1 are considered as 'high food security', and exclude the other values, including 'no response' (-9).
# Recode PESEX into a binary variable (PESEX=1 for Male, 0 for Female)
# Recode PEMLR variable into a binary variable for disability,(PEMLR=6:not in labor force-disabled,and all other values are 'not disabled').
# Recode PTDTRACE into a binary variable (eg. 1 for White, 0 for Non-White) 
# Recode PENATVTY into a binary variable (eg. 1 for US born,0 for foreign born)
# Perform logistic regression with 'food security' as the dependent variable and the others as independent variables.

# Read the dataset
data <- read_csv("/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2022/cpsdec22_Maine.csv")

# Adjust for 4 implied decimal places in HWHHWGT
data$HWHHWGT <- data$HWHHWGT / 10000

# Process PEMLR - create a new variable for disability
data <- data %>%
  mutate(disability = if_else(PEMLR == 6, 1, 0))

# HRFS12MD = 4 represents low food security,
# create a binary variable where 1 = low food security, 0 = not low food security
# Also, exclude the 'no response' cases by filtering them out or coding them as 0
data <- data %>%
  filter(HRFS12MD != -9) %>% # Optional, if want to exclude 'no response' cases
  mutate(food_insecurity = if_else(HRFS12MD == 4, 1, 0))

# Recode PESEX into a binary variable (1 for Male, 0 for Female)
data <- data %>%
  mutate(sex = if_else(PESEX == 1, 1, 0))

# Recode PENATVTY 
# For example, US born vs foreign born
data <- data %>%
  mutate(us_born = if_else(PENATVTY == 57, 1, 0))

# Recode PREXPLF into a binary variable (1 for Employed, 0 for Unemployed)
data <- data %>%
  mutate(employment_status = if_else(PREXPLF == 1, 1, 0))

# Recode PTDTRACE into a binary variable (1 for White, 0 for Non-white)
data <- data %>%
  mutate(white = if_else(PTDTRACE == 1, 1, 0))

data <- data %>%
  mutate(black = if_else(PTDTRACE == 2, 1, 0))

data <- data %>%
  mutate(indian = if_else(PTDTRACE == 3, 1, 0))

data <- data %>%
  mutate(asian = if_else(PTDTRACE == 4, 1, 0))

data <- data %>%
  mutate(indian = if_else(PTDTRACE == 3, 1, 0))


# Run the logistic regression model with household weight
model <- glm(food_insecurity ~ sex + black+employment_status + disability, 
             family = binomial(link = "logit"), data = data, weights = HWHHWGT)

# Output the summary of the model to get the coefficients
summary(model)


##############################################################################################
# logistic regression on the cpsdecMaine 2020-2022 data, and combined data for newengland: 
read_and_process <- function(file_path) {
  print(paste("Processing:", file_path))
  # Read the dataset
  data <- read_csv(file_path, show_col_types = FALSE)
  
  # Convert column names to uppercase immediately after reading the dataset
  names(data) <- toupper(names(data))
  
  # Ensure HRHHID and HRHHID2 are character types
  if("HRHHID" %in% names(data)) {
    data$HRHHID <- as.character(data$HRHHID)
  }
  if("HRHHID2" %in% names(data)) {
    data$HRHHID2 <- as.character(data$HRHHID2)
  }
  
  # Now perform data transformations
  data <- data %>%
    mutate(
      # Create binary variables while excluding unwanted values
      DISABILITY = if_else(PEMLR == 6, 1, 0),
      FOOD_INSECURITY = if_else(HRFS12MD == 4, 1, 0),
      SEX = if_else(PESEX == 1, 1, 0),
      US_BORN = if_else(PENATVTY == 57, 1,0),
      EMPLOYMENT_STATUS = if_else(PREXPLF == 1, 1, 0),
      POVERTY = if_else(HRPOOR == 1, 1, 0),
      OVER_65 = if_else(PRTAGE > 65, 1,0),
      # Handling RACE separately due to multiple conditions
      RACE = case_when(
        PTDTRACE == 1 ~ "White",
        PTDTRACE == 2 ~ "Black",
        PTDTRACE == 3 ~ "AmericanIndian",
        PTDTRACE == 4 ~ "Asian",
        TRUE ~ "Other"
      )
    ) %>%
    mutate(RACE = factor(RACE)) %>%
    mutate(RACE = relevel(RACE, ref = "White"))
  
  
  # Extract year from file path
  year <- as.integer(str_extract(file_path, "\\d{4}"))
  data$YEAR <- year
  
  # Adjust for 4 implied decimal places in HWHHWGT
  data$HWHHWGT <- data$HWHHWGT / 10000
  
  # Filter data for PEMLR == 6 and then count -1 values for specified variables
  data %>%
    dplyr::filter(PEMLR == 6) %>%
    summarise(
      Count_FOOD_INSECURITY_minus1 = sum(FOOD_INSECURITY == -1, na.rm = TRUE),
      Count_SEX_minus1 = sum(SEX == -1, na.rm = TRUE),
      Count_US_BORN_minus1 = sum(US_BORN == -1, na.rm = TRUE),
      Count_EMPLOYMENT_STATUS_minus1 = sum(EMPLOYMENT_STATUS == -1, na.rm = TRUE),
      #Count_POVERTY_minus1 = sum(POVERTY == -1, na.rm = TRUE),
      Count_OVER_65_minus1 = sum(OVER_65 == -1, na.rm = TRUE)
    ) -> negative_ones_analysis
  
  print("Count of -1 values for PEMLR == 6 in specified variables:")
  print(negative_ones_analysis)
  
  # Save the processed dataset as a CSV file
  processed_file_path <- str_replace(file_path, ".csv", paste0("_PROCESSED_", year, ".csv"))
  write_csv(data, processed_file_path)
  
  return(data)
}

# Define file paths and corresponding years
# maine_file_paths <- c(
#   "2022" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2022/cpsdec22_Maine.csv",
#   "2021" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2021/cpsdec21_Maine.csv",
#   "2020" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2020/cpsdec20_Maine.csv",
#   "2017" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2017/cpsdec17_Maine.csv",
#   "2016" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2016/cpsdec16_Maine.csv",
#   "2015" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2015/cpsdec15_Maine.csv",
#   "2014" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2014/cpsdec14_Maine.csv",
#   "2013" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2013/cpsdec13_Maine.csv",
#   "2012" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2012/cpsdec12_Maine.csv",
#   "2011" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2011/cpsdec11_Maine.csv",
#   "2010" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2010/cpsdec10_Maine.csv"
# )

new_england_file_paths <- c(
  "2022" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2022/cpsdec22_NewEngland.csv",
  "2021" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2021/cpsdec21_NewEngland.csv",
  "2020" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2020/cpsdec20_NewEngland.csv",
  "2017" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2017/cpsdec17_NewEngland.csv",
  "2016" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2016/cpsdec16_NewEngland.csv",
  "2015" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2015/cpsdec15_NewEngland.csv",
  "2014" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2014/cpsdec14_NewEngland.csv",
  "2013" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2013/cpsdec13_NewEngland.csv",
  "2012" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2012/cpsdec12_NewEngland.csv",
  "2011" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2011/cpsdec11_NewEngland.csv",
  "2010" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2010/cpsdec10_NewEngland.csv"
)


# Process each file separately; this will save each processed file as a new CSV
processed_data_list <- lapply(new_england_file_paths, read_and_process)

# Combine all processed datasets for analysis
all_data_processed <- bind_rows(processed_data_list)
all_data_processed <- all_data_processed[!is.na(all_data_processed$HWHHWGT), ]


# Assuming no stratification and treating each observation as its own PSU
cps_design <- svydesign(ids = ~1, weights = ~HWHHWGT, data = all_data_processed)

model_svy <- svyglm(FOOD_INSECURITY ~ SEX + EMPLOYMENT_STATUS + US_BORN + RACE + DISABILITY + OVER_65+POVERTY, 
                    design = cps_design, 
                    family = binomial(link = "logit"))
summary(model_svy)

# Create a summary table for the race distribution
race_distribution <- all_data_processed %>%
  count(RACE) %>%
  mutate(percentage = n / sum(n) * 100)

# pie chart for the race distribution
cps_pie_chart <- ggplot(race_distribution, aes(x = "", y = percentage, fill = RACE)) +
  geom_bar(stat = "identity", width = 1) +
  coord_polar("y", start = 0) +
  theme_void() +
  theme(legend.title = element_blank()) +
  scale_fill_brewer(palette = "Set3") +
  labs(fill = "Race", title = "Percentage Distribution of Race")

# Print the pie chart
print(cps_pie_chart)

# Save the pie chart
ggsave("CPS_newengland_race.png", plot = cps_pie_chart, width = 10, height = 8)

# Logistic regression model for race as binary variable (need to fit the model for each race seperatedly)
# model_svy <- svyglm(FOOD_INSECURITY ~ SEX + EMPLOYMENT_STATUS + US_BORN + WHITE + DISABILITY+ OVER_65, 
#                     design = cps_design, 
#                     family = binomial(link = "logit"))
# 
# model_svy <- svyglm(FOOD_INSECURITY ~ SEX + EMPLOYMENT_STATUS + US_BORN + BLACK + DISABILITY+ OVER_65, 
#                     design = cps_design, 
#                     family = binomial(link = "logit"))
# 
# model_svy <- svyglm(FOOD_INSECURITY ~ SEX + EMPLOYMENT_STATUS + US_BORN + AMERICANINDIAN + DISABILITY+ OVER_65, 
#                     design = cps_design, 
#                     family = binomial(link = "logit"))
# 
# model_svy <- svyglm(FOOD_INSECURITY ~ SEX + EMPLOYMENT_STATUS + US_BORN + ASIAN + DISABILITY+ OVER_ 65, 
#                     design = cps_design, 
#                     family = binomial(link = "logit"))
# 
# summary(model_svy)


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
      EMPLOYMENT_STATUS = if_else(ESR %in% c("1", "2", "4", "5","6"), 1, 0),
      # Convert DIS to binary variable
      DISABILITY = if_else(DIS == 1, 1, 0),
      US_BORN = if_else(NATIVITY == 1, 1, 0),
      # Recode race code of the householder
      WHITE = if_else(RAC1P == 1, 1, 0),
      BLACK = if_else(RAC1P == 2, 1, 0),
      AMERICANINDIAN = if_else(RAC1P == 3, 1, 0),
      ASIAN = if_else(RAC1P == 6, 1, 0),
      RACE = case_when(
        RAC1P == 1 ~ "White",
        RAC1P == 2 ~ "Black",
        RAC1P == 3 ~ "AmericanIndian",
        RAC1P == 6 ~ "Asian",
        TRUE ~ "Other" # Handle other cases or include additional race categories if needed
      )
    ) %>%
    mutate(RACE = factor(RACE))%>%
    mutate(RACE = relevel(RACE, ref = "White")) %>%
    select(SEX, EMPLOYMENT_STATUS,OVER_65, US_BORN, POVERTY, DISABILITY,RACE)
    # Apply drop_na
    new_data <- drop_na(new_data)

  # Summary before na.omit
  # print(summary(new_data))
  
  # Apply na.omit
  # new_data <- na.omit(new_data)
  
  # Return the processed new data
  return(new_data)
}

# Read and process the new dataset
new_dataset_file_path <- "/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/new_england_people_18_22.csv"
new_data_processed <- process_new_dataset(new_dataset_file_path)

summary(new_data_processed)

# Apply the model to the new dataset to get predictions for FOOD_INSECURITY
predictions <- predict(model_svy, newdata = new_data_processed, type = "response")


# Add predictions back to the new data frame if you want to analyze them together
new_data_processed$FOOD_INSECURITY_PREDICTIONS <- predictions

# Output the new dataset with predictions
write_csv(new_data_processed, "/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/new_england_people_18_22_food_security_prediction.csv")

# View summary of predictions
summary(predictions)


# Pie chart plot on race 
# Convert factors to numeric for plotting
new_data_processed$RACE <- as.numeric(as.factor(new_data_processed$RACE))

new_data_processed$FOOD_INSECURITY_PREDICTIONS <- as.numeric(new_data_processed$FOOD_INSECURITY_PREDICTIONS)

# Reshape the data to long format
long_data <- new_data_processed %>%
  pivot_longer(
    cols = c(SEX, EMPLOYMENT_STATUS, DISABILITY, RACE, OVER_65, US_BORN,POVERTY),     
    names_to = "Variable",
    values_to = "Value"
  )

# Now, plot with ggplot2
p <- ggplot(long_data, aes(x = Value, y = FOOD_INSECURITY_PREDICTIONS, color = Variable)) +
  geom_point(alpha = 0.5) +
  facet_wrap(~ Variable, scales = "free_x") +
  scale_color_viridis_d() +
  labs(x = "Variable Value", y = "Predicted Probability of Food Insecurity", color = "Variable") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Print the plot
print(p)


# Convert RACE to numeric for plotting (this is just for visualization and may not be ideal for interpretation)
new_data_processed$RACE_numeric <- as.numeric(new_data_processed$RACE)

# Create a scatter plot with jitter for binary variables and use facets for RACE
# Adjust the previous plot code with a smaller size for scatter points
# Adjust the previous plot code to reflect the change to percentages
scatter_plot<- ggplot(new_data_processed, aes(y = FOOD_INSECURITY_PREDICTIONS*100)) +
  geom_jitter(aes(x = as.numeric(SEX) * 0.5, color = "SEX"), width = 0.05, alpha = 0.5, size = 0.5) +
  geom_jitter(aes(x = as.numeric(EMPLOYMENT_STATUS) * 0.5 + 2, color = "EMPLOYMENT_STATUS"), width = 0.05, alpha = 0.5, size = 0.5) +
  geom_jitter(aes(x = as.numeric(DISABILITY) * 0.5 + 4, color = "DISABILITY"), width = 0.05, alpha = 0.5, size = 0.5) +
  geom_jitter(aes(x = as.numeric(OVER_65) * 0.5 + 6, color = "OVER_65"), width = 0.05, alpha = 0.5, size = 0.5) +
  geom_jitter(aes(x = as.numeric(US_BORN) * 0.5 + 8, color = "US_BORN"), width = 0.05, alpha = 0.5, size = 0.5) +
  geom_jitter(aes(x = as.numeric(POVERTY) * 0.5 + 10, color = "POVERTY"), width = 0.05, alpha = 0.5, size = 0.5) +
  scale_x_continuous(breaks = c(0.125, 0.5, 2, 2.5, 4, 4.5, 6, 6.5, 8, 8.5,10,10.5),
                     labels = c("Female", "Male","Unemployed","Employed","No Disability","Disability","Below 65","Over 65","Non-US Born","US Born","Above-Poverty","Below-Poverty"),
                     #labels = c("0", "1", "0", "1", "0", "1", "0", "1", "0", "1","0", "1"),
                     limits = c(0, 11)) +
  scale_color_manual(values = c("SEX" = "blue", "EMPLOYMENT_STATUS" = "green", "DISABILITY" = "red", "OVER_65" = "orange", "US_BORN" = "brown","POVERTY"="black")) +
  labs(color = "Variables", x = "Independent Variables", y = "Predicted Probability of Food Insecurity (%)") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  facet_wrap(~RACE, ncol = 1, labeller = labeller(RACE = c(`1` = "White", `2` = "Black", `3` = "AmericanIndian", `4` = "Asian", `5` = "Other")))

# Print the plot
print(scatter_plot)

# Save the plot if needed
ggsave("acs_new_england_food_security_prediction.png", plot = scatter_plot, width = 20, height = 8)


##############################################################################################
# Define a helper function to create weighted variable data frame
create_weighted_variable_df <- function(variable, weights, variable_name, zero_desc, one_desc) {
  var_df <- data %>%
    group_by(!!rlang::sym(variable)) %>%
    summarise(Freq = sum(!!rlang::sym(weights))) %>%
    mutate(Variable = variable_name,
           Description = ifelse(!!rlang::sym(variable) == 0, zero_desc, one_desc)) %>%
    select(Variable, Category = !!rlang::sym(variable), Freq, Description)
  return(var_df)
}

# Create weighted data frames for each variable
food_security_df <- create_weighted_variable_df("food_security", "HWHHWGT", "food_security", "Not low food security", "Low food security")
disability_df <- create_weighted_variable_df("disability", "HWHHWGT", "disability", "Not disabled", "Disabled")
sex_df <- create_weighted_variable_df("sex", "HWHHWGT", "sex", "Female", "Male")
employment_status_df <- create_weighted_variable_df("employment_status", "HWHHWGT", "employment_status", "Unemployed", "Employed")
white_df <- create_weighted_variable_df("white", "HWHHWGT", "white", "Non-white", "White")
us_born_df <- create_weighted_variable_df("us_born", "HWHHWGT", "us_born", "Foreign born", "US born")

# Combine these data frames into one
counts_df <- rbind(food_security_df, disability_df, sex_df, employment_status_df, white_df, us_born_df)

# Print the combined table
print(counts_df)

##############################################################################################
# the two-way tables for counts of independent variables (unweighted counts of observations)
# List of binary variables
binary_vars <- c("food_security", "disability", "sex", "employment_status", "white", "us_born")
descriptions <- list(
  food_security = c("Not Low Food Security", "Low Food Security"),
  disability = c("Not Disabled", "Disabled"),
  sex = c("Female", "Male"),
  employment_status = c("Unemployed", "Employed"),
  white = c("Non-white", "White"),
  us_born = c("Foreign Born", "US Born")
)

# Create an empty list to store the tables
two_way_tables <- list()

# Generate all combinations of two-way tables
for (i in 1:(length(binary_vars) - 1)) {
  for (j in (i + 1):length(binary_vars)) {
    var1 <- binary_vars[i]
    var2 <- binary_vars[j]
    
    # Create the two-way table
    table_name <- paste(var1, var2, sep = "_")
    temp_table <- table(data[[var1]], data[[var2]])
    
    # Assign descriptive names to rows and columns
    dimnames(temp_table) <- list(descriptions[[var1]], descriptions[[var2]])
    two_way_tables[[table_name]] <- temp_table
  }
}

# Print the two-way tables
two_way_tables

# logistic regression on food insecurity based on sex, employment status, and disability,race (white) and country of birth (us_born)
process_and_analyze_data <- function(file_path) {
  # Load necessary libraries
  library(readr)
  library(dplyr)
  
  # Read the dataset
  data <- read_csv(file_path)
  
  # Data processing steps
  data <- data %>%
    mutate(
      disability = if_else(PEMLR == 6, 1, 0),
      food_insecurity = if_else(HRFS12MD == 4, 1, 0),
      sex = if_else(PESEX == 1, 1, 0),
      us_born = if_else(PENATVTY == 57, 1, 0),
      employment_status = if_else(PREXPLF == 1, 1, 0),
      white = if_else(PTDTRACE == 1, 1, 0)
    ) %>%
    filter(HRFS12MD != -9)
  
  # Logistic regression model
  model <- glm(food_insecurity ~ sex + employment_status + disability, 
               family = binomial(link = "logit"), data = data)
  
  # Output the summary of the model
  model_summary <- summary(model)
  
  # List of binary variables
  binary_vars <- c("food_insecurity", "disability", "sex", "employment_status", "white", "us_born")
  
  # Descriptions for binary variables (adjust if needed)
  descriptions <- list(
    food_insecurity = c("Not Low Food Security", "Low Food Security"),
    disability = c("Not Disabled", "Disabled"),
    sex = c("Female", "Male"),
    employment_status = c("Unemployed", "Employed"),
    white = c("Non-white", "White"),
    us_born = c("Foreign Born", "US Born")
  )
  
  # Create two-way tables
  two_way_tables <- list()
  for (i in 1:(length(binary_vars) - 1)) {
    for (j in (i + 1):length(binary_vars)) {
      var1 <- binary_vars[i]
      var2 <- binary_vars[j]
      table_name <- paste(var1, var2, sep = "_")
      temp_table <- table(data[[var1]], data[[var2]])
      dimnames(temp_table) <- list(descriptions[[var1]], descriptions[[var2]])
      two_way_tables[[table_name]] <- temp_table
    }
  }
  
  # Return results as a list
  return(list(
    model_summary = model_summary,
    two_way_tables = two_way_tables
  ))
}

##############################################################################################
# the two-way tables for counts of independent variables (unweighted counts of observations)
# count of household by food security levels per year
count_food_security_levels <- function(file_path,year) {
  # Read the dataset
  data <- read_csv(file_path)
  
  # Function to safely get the column name, assuming there's only one match
  get_colname <- function(data, pattern) {
    # Create a regex pattern to match the whole name with case insensitivity
    regex_pattern <- paste0("^", pattern, "$")
    
    matches <- names(data)[str_detect(names(data), regex(regex_pattern, ignore_case = TRUE))]
    if (length(matches) != 1) {
      stop(paste("Expected one column matching pattern", pattern, "but found", length(matches)))
    }
    matches
  }
  
  # Identify the household ID and food security status column names (case-insensitive)
  household_id <- get_colname(data, "hrhhid")
  food_security_status <- get_colname(data, "hrfs12md")
  
  # Exclude 'No Response' and group by the household ID to count each level of food security per household
  household_food_security <- data %>%
    filter(!!sym(food_security_status) != -9) %>%
    group_by(!!sym(household_id)) %>%
    summarise(
      High_Food_Security = sum(!!sym(food_security_status) == 1, na.rm = TRUE),
      Marginal_Food_Security = sum(!!sym(food_security_status) == 2, na.rm = TRUE),
      Low_Food_Security = sum(!!sym(food_security_status) == 3, na.rm = TRUE),
      Very_Low_Food_Security = sum(!!sym(food_security_status) == 4, na.rm = TRUE),
      .groups = 'drop'
    )%>%
  mutate(Year = year)  # Add the year column
  
  # Count the number of households in each food security category
  food_security_counts <- household_food_security %>%
    summarise(
      Count_High_Food_Security = sum(High_Food_Security > 0),
      Count_Marginal_Food_Security = sum(Marginal_Food_Security > 0),
      Count_Low_Food_Security = sum(Low_Food_Security > 0),
      Count_Very_Low_Food_Security = sum(Very_Low_Food_Security > 0),
      Year = first(Year)
    )
  
  # Add the year column to the counts
  household_food_security_counts$Year <- year
  
  # Return the counts for each food security level
  return(food_security_counts)
}

# List of file paths and corresponding years
file_paths_years <- list(
  "2022" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2022/cpsdec22_Maine.csv",
  "2021" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2021/cpsdec21_Maine.csv",
  "2020" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2020/cpsdec20_Maine.csv",
  "2018" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2018/cpsdec18_Maine.csv",
  "2017" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2017/cpsdec17_Maine.csv",
  "2016" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2016/cpsdec16_Maine.csv",
  "2015" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2015/cpsdec15_Maine.csv",
  "2014" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2014/cpsdec14_Maine.csv",
  "2013" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2013/cpsdec13_Maine.csv",
  "2012" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2012/cpsdec12_Maine.csv",
  "2011" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2011/cpsdec11_Maine.csv",
  "2010" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2010/cpsdec10_Maine.csv"
)

# Apply the function to all datasets and store the results in a list
results_list <- lapply(names(file_paths_years), function(year) {
  file_path <- file_paths_years[[year]]
  count_food_security_levels(file_path, as.integer(year))
})
# Combine all the results into one data frame
all_results <- bind_rows(results_list)

# Print the combined results
print(all_results)

