
library(tidyverse)
library(readr)

# Apply model fit coefficent to ACS dataset
process_new_dataset <- function(file_path) {
  # Read the dataset
  new_data <- read_csv(file_path, show_col_types = FALSE)
  
  # Convert SCHL to numeric years of education
  # You need to define the conversion based on the specific details of your educational system
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
      schl == 24 ~ 24,  # Doctorate degree
      TRUE ~ NA_real_   # NA for any other codes or missing data
    )
  }
  
  # Apply the conversions and create the model variables
  new_data <- new_data %>%
    mutate(
      SEX = if_else(SEX == 1, "Male", "Female"), # Assuming 1 is male and 2 is female
      RACE = case_when(
        HISP != 1 ~ "Hispanic", # Hispanic
        RAC1P == 2 ~ "Black", # Black or African American
        TRUE ~ "White"
      ),
      AGEG = case_when(
        AGEP >= 65 & AGEP <= 69 ~ "65-69",
        AGEP >= 70 & AGEP <= 74 ~ "70-74",
        AGEP >= 75 & AGEP <= 79 ~ "75-79",
        AGEP >= 80 & AGEP <= 84 ~ "80-84",
        AGEP >= 85 ~ "85+"
      ),
      EDUCATION = schl_to_years(SCHL),
      #mean_education <- 12.3,
      #sd_education <- 3.5,
      EDUCATION_STD = (EDUCATION - 12.3) / 3.5
    ) %>%
    filter(!is.na(AGEG) & !is.na(RACE) & !is.na(EDUCATION)) %>%
    # Adjust the AGEG values to match the coefficients from the table
    mutate(
      # Adjust the AGEG values to match the coefficients from the table
      log_odds = -3.455 +
        if_else(AGEG == "65-69", 0, 
                if_else(AGEG == "70-74", 0.577, 
                        if_else(AGEG == "75-79", 1.126, 
                                if_else(AGEG == "80-84", 1.8, 
                                        if_else(AGEG == "85+", 2.693, NA_real_))))) +
        if_else(SEX == "Female", 0.123, 0) +
        if_else(RACE == "Black or African American", 0.915, 
                if_else(RACE == "Hispanic", 0.548, 0)) +
        -0.398 * EDUCATION_STD,
      # Convert log odds to probability
      alzheimer_prob = (exp(log_odds) / (1 + exp(log_odds)))*100
    ) %>%
    select(SEX, RACE, EDUCATION, AGEG,YEAR,ST,alzheimer_prob)
  
  # Save the processed data to a new CSV file
  write_csv(new_data, "new_england_people_18_22_combined_processed_with_alzheimer_prob.csv")
  return(new_data)
}

result <- process_new_dataset("/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/newengland_2018_2022_combined_dataset.csv")
View(result)


# Plotting Functions
plot_by_sex <- function(data) {
  ggplot(data, aes(x = factor(SEX), y = alzheimer_prob, fill = factor(SEX))) +
    geom_boxplot() +
    labs(title = "Alzheimer's Disease Probability by Sex", y = "Probability", x = "Sex") +
    scale_fill_brewer(palette = "Pastel1") +
    theme_minimal()
}

plot_by_race <- function(data) {
  ggplot(data, aes(x = factor(RACE), y = alzheimer_prob, fill = factor(RACE))) +
    geom_boxplot() +
    labs(title = "Alzheimer's Disease Probability by Race", y = "Probability", x = "Race") +
    scale_fill_brewer(palette = "Pastel2") +
    theme_minimal()
}

plot_by_education <- function(data) {
  # Create a labeled factor for the EDUCATION variable based on SCHL codes
  data$EDUCATION_LABEL <- factor(
    data$EDUCATION,
    levels = c(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 14, 16, 18, 19, 24),
    labels = c("0 - None/Preschool/Kindergarten",
               "1 - 1st Grade", "2 - 2nd Grade", "3 - 3rd Grade", "4 - 4th Grade", "5 - 5th Grade",
               "6 - 6th Grade", "7 - 7th Grade", "8 - 8th Grade", "9 - 9th Grade", "10 - 10th Grade",
               "11 - 11th Grade", "12 - Grade12/Regular high school/GED No Diploma",
               "14 - Some College < 1 Yr/1 or more year college no degree/Associate's",
               "16 - Bachelor's", "18 - Master's", "19 - Professional", "24 - Doctorate")
  )
  # Plot the data
  ggplot(data, aes(x = as.factor(EDUCATION), y = alzheimer_prob, fill = EDUCATION_LABEL)) +
    geom_boxplot() +
    labs(title = "Alzheimer's Disease Probability by Education Level",
         y = "Probability (%)",
         x = "Education Level") +
    scale_fill_manual(values = setNames(rep("gray", length(unique(data$EDUCATION_LABEL))), unique(data$EDUCATION_LABEL))) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 0, hjust = 1),legend.key = element_blank())
}

plot_by_age_group <- function(data) {
  ggplot(data, aes(x = factor(AGEG), y = alzheimer_prob, fill = factor(AGEG))) +
    geom_boxplot() +
    labs(title = "Alzheimer's Disease Probability by Age Group", y = "Probability", x = "Age Group") +
    scale_fill_viridis_d() +
    theme_minimal()
}

plot_by_state <- function(data) {
  # Create a named vector for state codes to state names
  state_labels <- setNames(
    c("CT", "ME", "MA", "NH", "RI", "VT"),
    c("9", "23", "25", "33", "44", "50")
  )
  
ggplot(data, aes(x = as.factor(ST), y = alzheimer_prob, fill = as.factor(ST))) +
    geom_boxplot() +
    scale_x_discrete(labels = state_labels) +
    labs(title = "Alzheimer's Disease Probability by State", y = "Probability", x = "State") +
    scale_fill_brewer(palette = "Set3") +
    theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
    theme_minimal()
}


plot_by_year <- function(data) {
  ggplot(data, aes(x = factor(YEAR), y = alzheimer_prob, fill = factor(YEAR))) +
    geom_boxplot() +
    labs(title = "Alzheimer's Disease Probability by Year", y = "Probability", x = "Year") +
    scale_fill_brewer(palette = "Set3") +
    theme_minimal()
}

save_plot <- function(plot, filename) {
  ggsave(filename, plot, width = 10, height = 8, dpi = 300)
}


processed_data <- process_new_dataset("/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/newengland_2018_2022_combined_dataset.csv") 

sex_plot <- plot_by_sex(processed_data)
race_plot <- plot_by_race(processed_data)
education_plot <- plot_by_education(processed_data)
age_group_plot <- plot_by_age_group(processed_data)
state_plot <- plot_by_state(processed_data)
year_plot <- plot_by_year(processed_data)

# Function to Save Plots to a Specific Path
save_plot_to_path <- function(plot, filename) {
  file_path <- paste0("/Users/xinyanliu/Desktop/NEU/Apriqot/Alzheimer/Alz prediction on ACS/", filename)
  ggsave(file_path, plot, width = 12, height = 8, dpi = 300)
}

save_plot_to_path(sex_plot, "sex_plot.png")
save_plot_to_path(race_plot, "race_plot.png")
save_plot_to_path(education_plot, "education_plot.png")
save_plot_to_path(age_group_plot, "age_group_plot.png")
save_plot_to_path(state_plot, "state_plot.png")
save_plot_to_path(year_plot, "year_plot.png")


