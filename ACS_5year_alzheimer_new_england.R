library(tidyverse)
library(readr)
library(sf)         # For handling spatial data
library(ggplot2)    # For plotting
library(dplyr)      # For data manipulation
#install.packages("tigris")

library(tigris)
options(tigris_use_cache = TRUE)

data <- read.csv("/Users/xinyanliu/Desktop/NEU/Apriqot/ACS micro/new_england_people_18_22.csv")
data <- data %>%
  mutate(
    RACE = case_when(
      HISP != 1 ~ "Hispanic",    
      #HISP == 1 & RAC1P == 1 ~ "White",
      HISP == 1 & RAC1P == 2 ~ "Black", 
      TRUE ~ "Other"          # Other
    )
  )
write_csv(data, "/Users/xinyanliu/Desktop/NEU/Apriqot/Alzheimer/data.csv")

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

# Process the dataset
process_new_dataset <- data %>%
  mutate(
    SEX = if_else(SEX == 1, "Male", "Female"), # Assuming 1 is male and 2 is female
    AGEG = case_when(
      AGEP >= 65 & AGEP <= 69 ~ "65-69",
      AGEP >= 70 & AGEP <= 74 ~ "70-74",
      AGEP >= 75 & AGEP <= 79 ~ "75-79",
      AGEP >= 80 & AGEP <= 84 ~ "80-84",
      AGEP >= 85 ~ "85+",
      TRUE ~ NA_character_ # To handle any other cases as NA
    ),
    EDUCATION = schl_to_years(SCHL)
  ) %>%
  mutate(
    EDUCATION_STD = (EDUCATION - 12.3) / 3.5,
    log_odds = -3.455 +
      case_when(
        AGEG == "65-69" ~ 0, 
        AGEG == "70-74" ~ 0.577, 
        AGEG == "75-79" ~ 1.126, 
        AGEG == "80-84" ~ 1.8, 
        AGEG == "85+" ~ 2.693,
        TRUE ~ NA_real_
      ) +
      if_else(SEX == "Female", 0.123, 0) +
      case_when(
        RACE == "Black" ~ 0.915,
        RACE == "Hispanic" ~ 0.548,
        TRUE ~ 0
      ),
    # Uncomment the following line if you want to include it in the log odds calculation
     -0.398 * EDUCATION_STD
  ) %>%
  mutate(
    alzheimer_prob = (exp(log_odds) / (1 + exp(log_odds))) * 100
  ) %>%
  select(PUMA10, PUMA20, PWGTP, SEX, RACE, EDUCATION, AGEG, ST, alzheimer_prob) %>%
  filter(!is.na(AGEG) & !is.na(RACE) & !is.na(EDUCATION))

# Write the processed data to a CSV file
write_csv(process_new_dataset, "/Users/xinyanliu/Desktop/NEU/Apriqot/Alzheimer/new_england_people_5year_18_22_alzheimer_prob_addweight_withedu.csv")
#write_csv(process_new_dataset, "/Users/xinyanliu/Desktop/NEU/Apriqot/Alzheimer/new_england_people_5year_18_22_alzheimer_prob_addweight_withoutedu.csv")


# Save the spatial data into geojson format
# st_write(pumas_2020_new_england, "/Users/xinyanliu/Desktop/NEU/Apriqot/Alzheimer/pumas_2020_new_england.geojson", driver = "GeoJSON")

# Save the spatial data into shapefile format
# st_write(pumas_2020_new_england, "/Users/xinyanliu/Desktop/NEU/Apriqot/Alzheimer/pumas_2020_new_england.shp", driver = "ESRI Shapefile")

# Add a new column GEOID concatenate state FIPS code with PUMA code

new_data <- process_new_dataset %>%
  #filter(PUMA10 != -9) %>%
  mutate(
    PUMA = case_when(
      PUMA10 == -9 ~ as.character(PUMA20),  # Convert puma20 to character if puma10 is -9
      PUMA20 == -9 ~ as.character(PUMA10),  # Convert puma10 to character if puma20 is -9
      ),
    ST = sprintf("%02d", as.integer(ST)), # Format ST as a two-digit number with leading zeros
    PUMA = case_when(
      nchar(as.character(PUMA)) == 3 ~ paste0("00", PUMA),  # If PUMA10 is 3 digits, add "00" in front
      nchar(as.character(PUMA)) == 4 ~ paste0("0", PUMA),   # If PUMA10 is 4 digits, add "0" in front
      TRUE ~ as.character(PUMA)                                # Else, keep it as is
    ),
    GEOID = paste0(ST, PUMA) # Concatenate state FIPS code with PUMA code
  )
write.csv(new_data, "/Users/xinyanliu/Desktop/NEU/Apriqot/Alzheimer/new_england_people_5year_18_22_alzheimer_prob_addgeoid_addweight_withedu.csv", row.names = FALSE)
#write.csv(new_data, "/Users/xinyanliu/Desktop/NEU/Apriqot/Alzheimer/new_england_people_5year_18_22_alzheimer_prob_addgeoid_addweight_withoutedu.csv", row.names = FALSE)


# Filter rows where ST is 23
new_data_ME <- new_data %>% 
  filter(ST == 23)

# Write the filtered data to a new CSV file
write.csv(new_data_ME, "/Users/xinyanliu/Desktop/NEU/Apriqot/Alzheimer/maine_people_5year_18_22_alzheimer_prob_addgeoid_addweight_withoutedu.csv", row.names = FALSE)

new_data <- new_data %>%
  mutate(
    GEOID = if_else(str_starts(GEOID, "9") | nchar(GEOID) == 6, str_pad(GEOID, width = 7, pad = "0"), GEOID)
  )

# List of New England state abbreviations
new_england_states <- c("CT", "ME", "MA", "NH", "RI", "VT")

# Initialize an empty list to store the shapefiles for each state
pumas_2020_list <- list()

# Loop through the New England states, downloading the PUMA shapefiles for each
for (state in new_england_states) {
  pumas_2020_list[[state]] <- tigris::pumas(year = 2020, class = "sf", state = state)
}

# Combine the shapefiles into a single sf object
pumas_2020_new_england <- bind_rows(pumas_2020_list)

new_data$GEOID <- as.character(new_data$GEOID) 
pumas_2020_new_england$GEOID10 <- as.character(pumas_2020_new_england$GEOID10)


data <- read.csv("/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/Alzheimer/maine_people_5year_18_22_alzheimer_prob_addgeoid_addweight_withoutedu.csv")


