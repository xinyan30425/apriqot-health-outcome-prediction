library(lme4)
library(survey)
library(readr)
library(dplyr)
library(purrr)
library(arm)

library(doParallel)
library(foreach)

# Register parallel backend to use multiple cores
numCores <- detectCores() - 1
registerDoParallel(cores = numCores)

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
    # Ensure no missing values in the weight or response
    filter(!is.na(HWHHWGT) & !is.na(FOOD_INSECURITY)) 
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

combined_data$FOOD_INSECURITY <- as.numeric(as.factor(combined_data$FOOD_INSECURITY)) - 1
# unique(combined_data$FOOD_INSECURITY)

# Assuming you have a variable 'STATE_ID' representing different states in your dataset
multilevel_model <- glmer(FOOD_INSECURITY ~ SEX + RACE + US_BORN + EMPLOYMENT_STATUS + DISABILITY + OVER_65 + (1 | STATE_ID), 
                          data = combined_data, 
                          family = binomial, 
                          weights = HWHHWGT,
                          control = glmerControl(optimizer = "nloptwrap", optCtrl = list(maxfun = 100000),tol = 1e-2))

# Directly check the model object for convergence information
if (multilevel_model@optinfo$convergence != 0) {
  warning("Model failed to converge")
}

# Check for singularity
if (isSingular(multilevel_model)) {
  warning("Model fit is singular")
}

# Check for convergence issues
if (!is.null(summary(multilevel_model)@optinfo$convergence)) {
  if (summary(multilevel_model)@optinfo$convergence != 0) {
    warning("Model failed to converge")
  }
} else {
  warning("Convergence status unknown")
}

# Assume `population_data` is a data frame containing the distribution of your stratifying variables in the population
population_data <- data.frame(
  SEX = factor(c("Male", "Female")),
  RACE = factor(c("White", "Black", "American Indian", "Asian", "Other")),
  US_BORN = factor(c("US_Born", "Foreign_Born")),
  EMPLOYMENT_STATUS = factor(c("Employed", "Unemployed")),
  DISABILITY = factor(c("Disabled", "Not_Disabled")),
  OVER_65 = factor(c("Over_65", "65_or_Under"))
)

# Merge the population data with the model predictions
post_strat_data <- merge(combined_data, population_data, by = c("SEX", "RACE", "US_BORN", "EMPLOYMENT_STATUS", "DISABILITY", "OVER_65"), all = TRUE)

# Calculate the predicted probabilities for each stratum
post_strat_data$predicted <- predict(multilevel_model, newdata = post_strat_data, type = "response")

# Adjust the predicted probabilities based on the known population margins
post_strat_data$adjusted <- post_strat_data$predicted * post_strat_data$population_weight / sum(post_strat_data$predicted * post_strat_data$population_weight)

# Adjust the predicted probabilities based on the known population margins
# This will calculate the adjusted predicted probability for each stratum
post_strat_data$adjusted <- with(post_strat_data, predicted * population_weight / sum(predicted * population_weight))

# View the adjusted predictions
print(post_strat_data$adjusted)


