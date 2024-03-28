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