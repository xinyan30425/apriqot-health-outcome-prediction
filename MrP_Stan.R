library(lme4)
library(survey)
library(readr)
library(dplyr)
library(purrr)
library(arm)
library(rstan)
library(dplyr)


process_data <- function(file_path) {
  data <- read_csv(file_path) %>%
    mutate(
      HRHHID = as.character(HRHHID),  # Convert HRHHID to character
      HRHHID2 = as.character(HRHHID2),
      SEX = as.factor(if_else(PESEX == 1, "Male", "Female")),
      RACE = as.factor(case_when(
        PTDTRACE == '1' ~ "White",
        PTDTRACE == '2' ~ "Black",
        PTDTRACE == '3' ~ "American Indian",
        PTDTRACE == '4' ~ "Asian",
        TRUE ~ "Other"
      )),
      US_BORN = as.factor(if_else(PENATVTY == '57', "US_Born", "Foreign_Born")),
      EMPLOYMENT_STATUS = as.factor(if_else(PREXPLF == 1, "Employed", "Unemployed")),
      DISABILITY = as.factor(if_else(PEMLR == 6, "Disabled", "Not_Disabled")),
      OVER_65 = as.factor(if_else(PRTAGE > 65, "Over_65", "65_or_Under")),
      FOOD_INSECURITY = if_else(HRFS12MD == 4, 1, 0),
      STATE_ID = as.factor(GESTFIPS),
      HWHHWGT = HWHHWGT / 10000 # Adjust the weights,
    ) %>%
    droplevels() %>%
    filter(!is.na(HWHHWGT) & !is.na(FOOD_INSECURITY))
  # Adjust indexing for Stan
  data <- data %>%
    mutate(
      SEX_index = as.integer(SEX),
      RACE_index = as.integer(RACE),
      US_BORN_index = as.integer(US_BORN),
      EMPLOYMENT_STATUS_index = as.integer(EMPLOYMENT_STATUS),
      DISABILITY_index = as.integer(DISABILITY),
      OVER_65_index = as.integer(OVER_65),
      STATE_ID_index = as.integer(STATE_ID)
    )
  return(data)
}

new_england_file_paths <- c(
  "2022" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2022/cpsdec22_NewEngland_PROCESSED_2022.csv",
  "2021" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2021/cpsdec21_NewEngland_PROCESSED_2021.csv",
  "2020" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2020/cpsdec20_NewEngland_PROCESSED_2020.csv",
  "2017" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2017/cpsdec17_NewEngland_PROCESSED_2017.csv",
  "2016" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2016/cpsdec16_NewEngland_PROCESSED_2016.csv",
  "2015" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2015/cpsdec15_NewEngland_PROCESSED_2015.csv",
  "2014" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2014/cpsdec14_NewEngland_PROCESSED_2014.csv",
  "2013" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2013/cpsdec13_NewEngland_PROCESSED_2013.csv",
  "2012" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2012/cpsdec12_NewEngland_PROCESSED_2012.csv",
  "2011" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2011/cpsdec11_NewEngland_PROCESSED_2011.csv",
  "2010" = "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2010/cpsdec10_NewEngland_PROCESSED_2010.csv"
)

# Combine all datasets
combined_data <- map_df(new_england_file_paths, process_data)

# combined_data$FOOD_INSECURITY <- as.numeric(as.factor(combined_data$FOOD_INSECURITY)) - 1
# unique(combined_data$FOOD_INSECURITY)

stan_data <- list(
  N = nrow(combined_data),
  J = length(unique(combined_data$STATE_ID)),
  K_sex = 2, # 'Male' and 'Female'
  K_race = 5, # 'White', 'Black', 'American Indian', 'Asian', 'Other'
  K_us_born = 2, # 'US_Born' and 'Foreign_Born'
  K_employment_status = 2, # 'Employed' and 'Unemployed'
  K_disability = 2,# 'Disabled' and 'Not_Disabled'
  K_over_65 = 2, # 'Over_65' and '65_or_Under'
  group = combined_data$STATE_ID_index,
  Y = combined_data$FOOD_INSECURITY,
  sex = combined_data$SEX_index,
  race = combined_data$RACE_index,
  us_born = combined_data$US_BORN_index,
  employment_status = combined_data$EMPLOYMENT_STATUS_index,
  disability = combined_data$DISABILITY_index,
  over_65 = combined_data$OVER_65_index
)

# Run the Stan model
fit <- stan(file = 'multilevel_model.stan', data = stan_data, iter = 2000, chains = 4)

fit_summary <- summary(fit)

parameter_names <- rownames(fit_summary$summary)
print(parameter_names)

# trace plot check the mixing and convergence of the chains
traceplot(fit, pars = c("beta_race[1]"))

# pair plots visualize relationships between parameters
pairs(fit, pars = c("beta_race[1]", "beta_race[2]","beta_race[3]","beta_race[4]","beta_race[5]"))


# Example poststratification step (assuming `population_data` exists)
# This should be adapted based on your specific needs

# Predict probabilities for each row in `population_data`
# You might need to adjust this step depending on your exact model and predictors
population_predictions <- predict(fit, newdata = population_data, type = "response")

# Aggregate predictions based on your population structure
# This step is highly specific to your analysis goals and requires detailed population data
