# Load the haven package
library(haven)
library(ggplot2)
library(dplyr)
library(scales)  # Load the scales library for formatting
library(gridExtra)
library(forcats) 
library(readr)
library(gridExtra)

my_data <- read_xpt("LLCP2015.xpt")
View(my_data)
write.csv(my_data, "LLCP2015.csv",row.names = FALSE)
LLCP2015 <- read.csv("LLCP2015.csv")
LLCP2015_Maine <- subset(LLCP2015,STATE == 23)
write.csv(LLCP2015_Maine, "LLCP2015_Maine.csv", row.names = FALSE)

new_england_states <- c(9, 23, 25, 33, 44, 50)
LLCP2015_New_England <- subset(LLCP2015,STATE %in% new_england_states)
write.csv(LLCP2015_New_England, "LLCP2015_New_England.csv", row.names = FALSE)

#CRGVPRB2
my_data <- read_xpt("LLCP2017.xpt")
View(my_data)
write.csv(my_data, "LLCP2017.csv",row.names = FALSE)
LLCP2017 <- read.csv("LLCP2017.csv")
LLCP2017_Maine <- subset(LLCP2017,STATE == 23)
write.csv(LLCP2017_Maine, "LLCP2017_Maine.csv", row.names = FALSE)
new_england_states <- c(9, 23, 25, 33, 44, 50)
LLCP2017_New_England <- subset(LLCP2017,STATE %in% new_england_states)
write.csv(LLCP2017_New_England, "LLCP2017_New_England.csv", row.names = FALSE)

my_data <- read_xpt("LLCP2019.xpt")
View(my_data)
write.csv(my_data, "LLCP2019.csv",row.names = FALSE)
LLCP2019 <- read.csv("LLCP2019.csv")
LLCP2019_Maine <- subset(LLCP2019,STATE == 23)
write.csv(LLCP2019_Maine, "LLCP2019_Maine.csv", row.names = FALSE)

new_england_states <- c(9, 23, 25, 33, 44, 50)
LLCP2019_New_England <- subset(LLCP2019,STATE %in% new_england_states)
write.csv(LLCP2019_New_England, "LLCP2019_New_England.csv", row.names = FALSE)

#CRGVPRB3
my_data <- read_xpt("LLCP2021.xpt")
View(my_data)
write.csv(my_data, "LLCP2021.csv",row.names = FALSE)
LLCP2021 <- read.csv("LLCP2021.csv")
LLCP2021_Maine <- subset(LLCP2021,STATE == 23)
write.csv(LLCP2021_Maine, "LLCP2021_Maine.csv", row.names = FALSE)

new_england_states <- c(9, 23, 25, 33, 44, 50)
LLCP2021_New_England <- subset(LLCP2021,STATE %in% new_england_states)
write.csv(LLCP2021_New_England, "LLCP2021_New_England.csv", row.names = FALSE)


# Define a vector with the STATE codes for New England
new_england_states <- c(9, 23, 25, 33, 44, 50)

# Define the file paths
file_paths <- c(
  "2016" = "/Users/xinyanliu/Desktop/NEU/Apriqot/BRFSS/LLCP2016/LLCP16V1.csv",
  "2018" = "/Users/xinyanliu/Desktop/NEU/Apriqot/BRFSS/LLCP2018/LLCP18V1.csv",
  "2020" = "/Users/xinyanliu/Desktop/NEU/Apriqot/BRFSS/LLCP2020/LLCP2020.csv",
  "2022" = "/Users/xinyanliu/Desktop/NEU/Apriqot/BRFSS/LLCP2022/LLCP2022.csv"
)

file_paths <- c(
  "2017" = "/Users/xinyanliu/Desktop/NEU/Apriqot/BRFSS/LLCP2017/LLCP2017.csv",
  "2019" = "/Users/xinyanliu/Desktop/NEU/Apriqot/BRFSS/LLCP2019/LLCP2019.csv",
  "2021" = "/Users/xinyanliu/Desktop/NEU/Apriqot/BRFSS/LLCP2021/LLCP2021.csv",
  "2023" = "/Users/xinyanliu/Desktop/NEU/Apriqot/BRFSS/LLCP2023/LLCP2023.csv"
)


# Function to read, filter, and save the data
process_and_save_new_england_data <- function(file_path, year) {
  # Construct the output file name based on the input file path
  output_file_name <- sub("\\.csv$", "_New_England.csv", basename(file_path))
  output_file_path <- dirname(file_path) # Save in the same folder
  
  # Read the data
  data <- read.csv(file_path)
  
  # Filter for New England states
  data_new_england <- subset(data, STATE %in% new_england_states)
  
  # Save the filtered data
  write.csv(data_new_england, file.path(output_file_path, output_file_name), row.names = FALSE)
  
  cat("Processed and saved data for year", year, "to", output_file_path, "\n")
}


# Process and save data for each year
lapply(names(file_paths), function(year) process_and_save_new_england_data(file_paths[[year]], year))