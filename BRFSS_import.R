# Install packages
install.packages("haven")
install.packages("ggplot2")
# Load the haven package
library(haven)
library(ggplot2)
library(dplyr)
library(scales)  # Load the scales library for formatting
library(gridExtra)
library(forcats) 
library(readr)
library(gridExtra)

##########################################################################################
# # Read the SAS data file,SAS to csv
cps_sas_data_2019 <- read_sas("/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2019/cpsdec2019.sas7bdat")
write.csv(cps_sas_data_2019, "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2019/cpsdec2019.csv", row.names = FALSE)
fcp_sas_data_2019 <- read_sas("/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2019/fcpdec2019.sas7bdat")
write.csv(fcp_sas_data_2019, "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2019/fcpdec2019.csv", row.names = FALSE)

##########################################################################################
# BRFSS data
# Read the .XPT file
my_data <- read_xpt("LLCP18V1.xpt")
View(my_data)
write.csv(my_data, "LLCP18V1.csv",row.names = FALSE)
LLCP18V1 <- read.csv("LLCP18V1.csv")
LLCP18V1_Maine <- subset(LLCP18V1,STATE == 23)
write.csv(LLCP18V1_Maine, "LLCP18V1_Maine.csv", row.names = FALSE)
head(LLCP18V2_Maine$CIMEMLOS, 20)

my_data <- read_xpt("LLCP16V1.xpt")
View(my_data)
write.csv(my_data, "LLCP16V1.csv",row.names = FALSE)
LLCP16V1 <- read.csv("LLCP16V1.csv")
LLCP16V1_Maine <- subset(LLCP16V1,STATE == 23)
write.csv(LLCP16V1_Maine, "LLCP16V1_Maine.csv", row.names = FALSE)
head(LLCP16V1_Maine$CIMEMLOS, 20)

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


# CIMEMLOS:During the past 12 months, have you experienced confusion or memory loss that is happening more often or is getting worse?
LLCP2016_Maine <- read.csv("/Users/xinyanliu/Desktop/NEU/Apriqot/BRFSS/LLCP2016/LLCP16V1_Maine.csv")
LLCP2018_Maine <- read.csv("/Users/xinyanliu/Desktop/NEU/Apriqot/BRFSS/LLCP2018/LLCP18V1_Maine.csv")
LLCP2020_Maine <- read.csv("/Users/xinyanliu/Desktop/NEU/Apriqot/BRFSS/LLCP2020/LLCP2020_Maine.csv")
LLCP2022_Maine <- read.csv("/Users/xinyanliu/Desktop/NEU/Apriqot/BRFSS/LLCP2022/LLCP2022_Maine.csv")

LLCP2022_Maine$CIMEMLOS <- factor(LLCP2022_Maine$CIMEMLOS, levels = c(1, 2, 7, 9),
                            labels = c("Yes", "No", "Don't know/Not sure", "Refused"))

LLCP2020_Maine$CIMEMLOS <- factor(LLCP2020_Maine$CIMEMLOS, levels = c(1, 2, 7, 9),
                                  labels = c("Yes", "No", "Don't know/Not sure", "Refused"))

LLCP2018_Maine$CIMEMLOS <- factor(LLCP2018_Maine$CIMEMLOS, levels = c(1, 2, 7, 9),
                                  labels = c("Yes", "No", "Don't know/Not sure", "Refused"))

LLCP2016_Maine$CIMEMLOS <- factor(LLCP2016_Maine$CIMEMLOS, levels = c(1, 2, 7, 9),
                                  labels = c("Yes", "No", "Don't know/Not sure", "Refused"))


LLCP2022_Maine$AGEG5YR <- factor(LLCP2022_Maine$AGEG5YR, 
                                 levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
                                 labels = c("18-24", "25-29", "30-34", "35-39", "40-44", 
                                            "45-49", "50-54", "55-59", "60-64", "65-69", 
                                            "70-74", "75-79", ">80", 
                                            "7-9 Unknown"))

LLCP2020_Maine$AGEG5YR <- factor(LLCP2020_Maine$AGEG5YR, 
                                 levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
                                 labels = c("18-24", "25-29", "30-34", "35-39", "40-44", 
                                            "45-49", "50-54", "55-59", "60-64", "65-69", 
                                            "70-74", "75-79", ">80", 
                                            "7-9 Unknown"))

LLCP2018_Maine$AGEG5YR <- factor(LLCP2018_Maine$AGEG5YR, 
                                 levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
                                 labels = c("18-24", "25-29", "30-34", "35-39", "40-44", 
                                            "45-49", "50-54", "55-59", "60-64", "65-69", 
                                            "70-74", "75-79", ">80", 
                                            "7-9 Unknown"))

LLCP2016_Maine$AGEG5YR <- factor(LLCP2016_Maine$AGEG5YR, 
                                 levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
                                 labels = c("18-24", "25-29", "30-34", "35-39", "40-44", 
                                            "45-49", "50-54", "55-59", "60-64", "65-69", 
                                            "70-74", "75-79", ">80", 
                                            "7-9 Unknown"))

LLCP2022_Maine$LLCPWT <- as.numeric(as.character(LLCP2022_Maine$LLCPWT))
LLCP2020_Maine$LLCPWT <- as.numeric(as.character(LLCP2020_Maine$LLCPWT))
LLCP2018_Maine$LLCPWT <- as.numeric(as.character(LLCP2018_Maine$LLCPWT))
LLCP2016_Maine$LLCPWT <- as.numeric(as.character(LLCP2016_Maine$LLCPWT))

# Filter for 45+ age groups (adjust the levels based on your specific age group labels)
LLCP2022_Maine_45_plus <- subset(LLCP2022_Maine, AGEG5YR %in% c("45-49", "50-54", "55-59", "60-64", "65-69", 
                                                                "70-74", "75-79", ">80"))

LLCP2020_Maine_45_plus <- subset(LLCP2020_Maine, AGEG5YR %in% c("45-49", "50-54", "55-59", "60-64", "65-69", 
                                                                "70-74", "75-79", ">80"))

LLCP2018_Maine_45_plus <- subset(LLCP2018_Maine, AGEG5YR %in% c("45-49", "50-54", "55-59", "60-64", "65-69", 
                                                                "70-74", "75-79", ">80"))

LLCP2016_Maine_45_plus <- subset(LLCP2016_Maine, AGEG5YR %in% c("45-49", "50-54", "55-59", "60-64", "65-69", 
                                                                "70-74", "75-79", ">80"))

# Calculate the weighted percentages
LLCP2022_Maine_percentages <- LLCP2022_Maine_45_plus %>%
  filter(CIMEMLOS %in% c("Yes", "No")) %>%  # Filter to include only Yes and No responses
  group_by(AGEG5YR) %>%
  summarize(
    TotalYes = sum(ifelse(CIMEMLOS == "Yes", LLCPWT, 0), na.rm = TRUE), # Weighted sum of 'Yes'
    TotalNo = sum(ifelse(CIMEMLOS == "No", LLCPWT, 0), na.rm = TRUE),   # Weighted sum of 'No'
    PercentYes = TotalYes / (TotalYes + TotalNo) * 100                  # Weighted percentage of 'Yes'
  ) %>%
  ungroup()

LLCP2020_Maine_percentages <- LLCP2020_Maine_45_plus %>%
  filter(CIMEMLOS %in% c("Yes", "No")) %>%  # Filter to include only Yes and No responses
  group_by(AGEG5YR) %>%
  summarize(
    TotalYes = sum(ifelse(CIMEMLOS == "Yes", LLCPWT, 0), na.rm = TRUE), # Weighted sum of 'Yes'
    TotalNo = sum(ifelse(CIMEMLOS == "No", LLCPWT, 0), na.rm = TRUE),   # Weighted sum of 'No'
    PercentYes = TotalYes / (TotalYes + TotalNo) * 100                  # Weighted percentage of 'Yes'
  ) %>%
  ungroup()

LLCP2018_Maine_percentages <- LLCP2018_Maine_45_plus %>%
  filter(CIMEMLOS %in% c("Yes", "No")) %>%  # Filter to include only Yes and No responses
  group_by(AGEG5YR) %>%
  summarize(
    TotalYes = sum(ifelse(CIMEMLOS == "Yes", LLCPWT, 0), na.rm = TRUE), # Weighted sum of 'Yes'
    TotalNo = sum(ifelse(CIMEMLOS == "No", LLCPWT, 0), na.rm = TRUE),   # Weighted sum of 'No'
    PercentYes = TotalYes / (TotalYes + TotalNo) * 100                  # Weighted percentage of 'Yes'
  ) %>%
  ungroup()

LLCP2016_Maine_percentages <- LLCP2016_Maine_45_plus %>%
  filter(CIMEMLOS %in% c("Yes", "No")) %>%  # Filter to include only Yes and No responses
  group_by(AGEG5YR) %>%
  summarize(
    TotalYes = sum(ifelse(CIMEMLOS == "Yes", LLCPWT, 0), na.rm = TRUE), # Weighted sum of 'Yes'
    TotalNo = sum(ifelse(CIMEMLOS == "No", LLCPWT, 0), na.rm = TRUE),   # Weighted sum of 'No'
    PercentYes = TotalYes / (TotalYes + TotalNo) * 100                  # Weighted percentage of 'Yes'
  ) %>%
  ungroup()

LLCP2022_Maine_percentages$Year <- 2022
LLCP2020_Maine_percentages$Year <- 2020
LLCP2018_Maine_percentages$Year <- 2018
LLCP2016_Maine_percentages$Year <- 2016

combined_data <- rbind(LLCP2022_Maine_percentages, LLCP2020_Maine_percentages,LLCP2018_Maine_percentages,LLCP2016_Maine_percentages)

# Scatter plot for weighted percentages 2022,2020
combined_plot <- ggplot(combined_data, aes(x = AGEG5YR, y = PercentYes, color = as.factor(Year))) +
  geom_point() +
  labs(x = "Age Group", y = "Weighted Percentage Saying Yes", 
       title = "Weighted Percentage of 'Yes' Responses by Age Group (45+) Across Years") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
combined_plot

combined_plot <- ggplot(combined_data, aes(x = AGEG5YR, y = PercentYes, color = as.factor(Year), group = as.factor(Year))) +
  geom_point() +
  geom_line() + # This will connect the points for each year
  labs(x = "Age Group", 
       y = "Weighted Percentage Saying Yes", 
       title = "Weighted Percentage of 'Yes' Responses by Age Group (45+) Across Years",
       subtitle = "During the past 12 months, have you experienced confusion or memory loss that is happening more often or is getting worse?") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        plot.title = element_text(size = 14),
        plot.subtitle = element_text(size = 10)) +
  scale_y_continuous(limits = c(0, NA)) # Set y-axis to start from 0

combined_plot

ggsave("Combined_Weighted_Percentage.png", plot = combined_plot, width = 12, height = 6, units = "in")

# Scatter plot for weighted percentages
maine_yes_plot_22 <- ggplot(LLCP2022_Maine_percentages, aes(x = AGEG5YR, y = PercentYes)) +
  geom_point() +
  labs(x = "Age Group", y = "Weighted Percentage Saying Yes", 
       title = "Maine 2022 Weighted Percentage of 'Yes' Responses by Age Group (45+)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
maine_yes_plot_22

ggsave("Maine_Congnitive_Decline_Percentage_45+years.png", plot = maine_yes_plot_22, width = 10, height = 6, units = "in")

# Filter for age 65 and above
LLCP2022_Maine_65_plus <- subset(LLCP2022_Maine, AGEG5YR %in% c("65-69", "70-74", "75-79", ">80"))
LLCP2020_Maine_65_plus <- subset(LLCP2020_Maine, AGEG5YR %in% c("65-69", "70-74", "75-79", ">80"))
LLCP2018_Maine_65_plus <- subset(LLCP2018_Maine, AGEG5YR %in% c("65-69", "70-74", "75-79", ">80"))
LLCP2016_Maine_65_plus <- subset(LLCP2016_Maine, AGEG5YR %in% c("65-69", "70-74", "75-79", ">80"))

# Check if NUMADULT is present and a factor, if not, you might need to adjust this part
# LLCP2022_Maine_65_plus$NUMADULT <- factor(LLCP2022_Maine_65_plus$NUMADULT)

# Define the columns you want to keep for the analysis
common_columns <- c("NUMADULT", "LLCPWT", "AGEG5YR", "Year")

# Add the 'Year' column and select only the common columns for each dataset
LLCP2022_Maine_65_plus <- subset(LLCP2022_Maine, AGEG5YR %in% c("65-69", "70-74", "75-79", ">80"))
LLCP2022_Maine_65_plus$Year <- '2022'
LLCP2022_Maine_65_plus <- LLCP2022_Maine_65_plus[common_columns]

LLCP2020_Maine_65_plus <- subset(LLCP2020_Maine, AGEG5YR %in% c("65-69", "70-74", "75-79", ">80"))
LLCP2020_Maine_65_plus$Year <- '2020'
LLCP2020_Maine_65_plus <- LLCP2020_Maine_65_plus[common_columns]

LLCP2018_Maine_65_plus <- subset(LLCP2018_Maine, AGEG5YR %in% c("65-69", "70-74", "75-79", ">80"))
LLCP2018_Maine_65_plus$Year <- '2018'
LLCP2018_Maine_65_plus <- LLCP2018_Maine_65_plus[common_columns]

LLCP2016_Maine_65_plus <- subset(LLCP2016_Maine, AGEG5YR %in% c("65-69", "70-74", "75-79", ">80"))
LLCP2016_Maine_65_plus$Year <- '2016'
LLCP2016_Maine_65_plus <- LLCP2016_Maine_65_plus[common_columns]


##########################################################################################
# repeat the same process for other years
process_LLCP_data <- function(filepath, year) {
  # Read the dataset
  dataset <- read.csv(filepath)
  
  # Convert CIMEMLOS to a factor with specified levels and labels
  dataset$CIMEMLOS <- factor(dataset$CIMEMLOS, levels = c(1, 2, 7, 9),
                             labels = c("Yes", "No", "Don't know/Not sure", "Refused"))
  
  # Convert AGEG5YR to a factor with specified levels and labels
  dataset$AGEG5YR <- factor(dataset$AGEG5YR, 
                            levels = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14),
                            labels = c("18-24", "25-29", "30-34", "35-39", "40-44", 
                                       "45-49", "50-54", "55-59", "60-64", "65-69", 
                                       "70-74", "75-79", ">80", "7-9 Unknown"))
  
  # Convert LLCPWT to numeric
  dataset$LLCPWT <- as.numeric(as.character(dataset$LLCPWT))
  
  # Convert NUMADULT to numeric
  dataset$NUMADULT <- as.numeric(as.character(dataset$NUMADULT))
  
  # Filter for 45+ age groups
  dataset_45_plus <- subset(dataset, AGEG5YR %in% c("45-49", "50-54", "55-59", "60-64", "65-69", 
                                                    "70-74", "75-79", ">80"))
  
  # Filter for age 65 and above
  dataset_65_plus <- subset(dataset, AGEG5YR %in% c("65-69", "70-74", "75-79", ">80"))
  
  # Add flag for isolated seniors (living alone, age 65+)
  dataset_65_plus$Isolated_Senior_Flag <- ifelse(dataset_65_plus$NUMADULT == 1, 1, 0)
  
  # Calculate the total count of seniors 65+
  total_seniors_65_plus <- dataset_65_plus %>%
    summarise(Total_Seniors_65_Plus = sum(LLCPWT, na.rm = TRUE)) %>%
    mutate(Year = as.factor(year))  # Add Year column
  
  # Calculate the weighted percentages
  percentages <- dataset_45_plus %>%
    filter(CIMEMLOS %in% c("Yes", "No")) %>%  # Filter to include only Yes and No responses
    group_by(AGEG5YR) %>%
    summarize(
      TotalYes = sum(ifelse(CIMEMLOS == "Yes", LLCPWT, 0), na.rm = TRUE), # Weighted sum of 'Yes'
      TotalNo = sum(ifelse(CIMEMLOS == "No", LLCPWT, 0), na.rm = TRUE),   # Weighted sum of 'No'
      PercentYes = TotalYes / (TotalYes + TotalNo) * 100                  # Weighted percentage of 'Yes'
    ) %>%
    ungroup() %>%
    mutate(Year = year)
  
  # Calculate the count of isolated seniors by NUMADULT
  isolated_seniors_count <- dataset_65_plus %>%
    group_by(NUMADULT) %>%
    summarise(Weighted_Isolated_Seniors = sum(ifelse(Isolated_Senior_Flag == 1, LLCPWT, 0), na.rm = TRUE)) %>%
    ungroup() %>%
    mutate(Year = year)
  
  # Debugging: Inspect the isolated seniors count
  print(head(isolated_seniors_count))

  return(list(percentages = percentages, isolated_seniors_count = isolated_seniors_count, total_seniors_65_plus = total_seniors_65_plus))
  
}

results_2016 <- process_LLCP_data("/Users/xinyanliu/Desktop/NEU/Apriqot/BRFSS/LLCP2016/LLCP16V1_Maine.csv", 2016)
results_2018 <- process_LLCP_data("/Users/xinyanliu/Desktop/NEU/Apriqot/BRFSS/LLCP2018/LLCP18V1_Maine.csv", 2018)
results_2020 <- process_LLCP_data("/Users/xinyanliu/Desktop/NEU/Apriqot/BRFSS/LLCP2020/LLCP2020_Maine.csv", 2020)
results_2022 <- process_LLCP_data("/Users/xinyanliu/Desktop/NEU/Apriqot/BRFSS/LLCP2022/LLCP2022_Maine.csv", 2022)

# LLCP2016_Maine_Isolated_Seniors <- results_2016$isolated_seniors_count
# LLCP2018_Maine_Isolated_Seniors <- results_2018$isolated_seniors_count
# LLCP2020_Maine_Isolated_Seniors <- results_2020$isolated_seniors_count
# LLCP2022_Maine_Isolated_Seniors <- results_2022$isolated_seniors_count

LLCP2016_Maine_percentages <- results_2016$percentages
LLCP2018_Maine_percentages <- results_2018$percentages
LLCP2020_Maine_percentages <- results_2020$percentages
LLCP2022_Maine_percentages <- results_2022$percentages

# Extract the count of isolated seniors for NUMADULT = 1 for each year
isolated_seniors_2016 <- results_2016$isolated_seniors_count %>% filter(NUMADULT == 1)
isolated_seniors_2018 <- results_2018$isolated_seniors_count %>% filter(NUMADULT == 1)
isolated_seniors_2020 <- results_2020$isolated_seniors_count %>% filter(NUMADULT == 1)
isolated_seniors_2022 <- results_2022$isolated_seniors_count %>% filter(NUMADULT == 1)

# Combine the total seniors data
combined_total_seniors <- rbind(
  results_2016$total_seniors_65_plus,
  results_2018$total_seniors_65_plus,
  results_2020$total_seniors_65_plus,
  results_2022$total_seniors_65_plus
)


total_seniors_65_plus_plot <- ggplot(combined_total_seniors, aes(x = Year, y = Total_Seniors_65_Plus)) +
  geom_line(group = 1) +
  geom_point() +
  geom_text(aes(label = round(Total_Seniors_65_Plus)), vjust = -0.5, hjust = 1.5, color = "blue") +
  theme_minimal() +
  labs(title = "Yearly Total Count of Seniors Aged 65+ (BRFSS)",
       x = "Year",
       y = "Total Count of Seniors 65+") +
  scale_y_continuous(limits = c(0, NA),  # Start y-axis at 0 and extend to maximum value
                     breaks = waiver(),  # Use default breaks or set specific breaks if needed
                     labels = label_number())  # Format numbers without scientific notation

# Display the plot
print(total_seniors_65_plus_plot)

# Optionally save the plot
ggsave("Yearly_Total_Count_Seniors_65_Plus_LLCP.png", plot = total_seniors_65_plus_plot, width = 10, height = 6)


# Combine the data
combined_data_65_plus <- rbind(LLCP2022_Maine_65_plus, 
                               LLCP2020_Maine_65_plus, 
                               LLCP2018_Maine_65_plus, 
                               LLCP2016_Maine_65_plus)

# Combine the isolated seniors data
combined_isolated_seniors <- rbind(isolated_seniors_2016,
                                   isolated_seniors_2018,
                                   isolated_seniors_2020,
                                   isolated_seniors_2022)

# Plotting the weighted count of NUMADULT for age 65 and above
weighted_count_plot <- ggplot(LLCP2022_Maine_65_plus, aes(x = NUMADULT, weight = LLCPWT)) +
  geom_bar() +
  labs(x = "Number of Adults in Household", y = "Weighted Count", 
       title = "2022 Maine Weighted Count of Number of Adults in Household for Age 65+") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  scale_x_discrete(limits = as.character(1:6))
  

# Plotting the weighted count of NUMADULT for age 65 and above for all years
combined_weighted_count_plot <- ggplot(combined_data_65_plus, aes(x = NUMADULT, weight = LLCPWT, fill = Year)) +
  geom_bar(position = "dodge") +
  labs(x = "Number of Adults in Household", y = "Weighted Count", 
       title = "Weighted Count of Number of Adults in Household for Age 65+ Across Years") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_x_discrete(limits = as.character(1:6)) +
  facet_wrap(~Year, ncol = 1)  # Facet by Year to separate the plots for each year

# Pre-calculate the sums for each combination of Year and NUMADULT
weighted_sums <- combined_data_65_plus %>%
  group_by(Year, NUMADULT) %>%
  summarise(TotalWeight = sum(LLCPWT, na.rm = TRUE))

# Plotting the weighted count of NUMADULT for age 65 and above for all years
combined_weighted_count_plot <- ggplot(weighted_sums, aes(x = NUMADULT, y = TotalWeight, fill = Year)) +
  geom_col(position = position_dodge()) +
  geom_text(aes(label = round(TotalWeight, 1), y = TotalWeight), 
            position = position_dodge(width = 0.9), vjust = -0.25, size = 3) +
  labs(x = "Number of Adults in Household", y = "Weighted Count", 
       title = "Weighted Count of Number of Adults in Household for Age 65+ Across Years") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  scale_x_discrete(limits = as.character(1:6)) +
  facet_wrap(~Year, ncol = 1)

# Display the plot
combined_weighted_count_plot

# Optionally save the plot
ggsave("Combined_Weighted_Count_NumAdult_65_Plus.png", plot = combined_weighted_count_plot, width = 10, height = 6, units = "in")

# Optionally save the plot
# ggsave("LLCP2022_Weighted_Count_NumAdult_65_Plus.png", plot = weighted_count_plot, width = 10, height = 6, units = "in")

# Plotting the count of isolated seniors vs year
isolated_seniors_year_plot <- ggplot(combined_isolated_seniors_data, aes(x = as.factor(Year), y = Weighted_Isolated_Seniors, fill = as.factor(Year))) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(x = "Year", y = "Weighted Count of Isolated Seniors", 
       title = "Yearly Count of Isolated Seniors (Living Alone)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
isolated_seniors_year_plot

# Optionally save the plot
ggsave("Yearly_Count_Isolated_Seniors.png", plot = isolated_seniors_year_plot, width = 10, height = 6, units = "in")

# Plotting the count of isolated seniors vs year as a line and point plot
isolated_seniors_year_line_plot <- ggplot(combined_isolated_seniors, aes(x = as.factor(Year), y = Weighted_Isolated_Seniors, group = 1)) +
  geom_point(aes(color = as.factor(Year)), size = 3) +  # Add points
  geom_line(aes(color = as.factor(Year)), size = 1) +  # Add lines
  labs(x = "Year", y = "Weighted Count of Isolated Seniors", 
       title = "BRFSS-Yearly Count of Isolated Seniors (Living Alone)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_discrete(name = "Year")

# Display the plot
isolated_seniors_year_line_plot

# Optionally save the plot
ggsave("Yearly_Count_Isolated_Seniors_Line_Plot.png", plot = isolated_seniors_year_line_plot, width = 10, height = 6, units = "in")

##########################################################################################

##########################################################################################
# CPS data
# PRTAGE: age
# PTDTRACE: race
# GESTFIPS: sate code
# GTCO: county code
# 005 Cumberland 
# HRMONTH: month of interview
# PWSSWGT: final weight used for most tabulations, controlled to independent estimates for state,origin,sex,age,race
# HRNUMHOU: total number of persons living in the household

cpsdec22 <- read.csv("/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2022/dec22pub.csv")
cpsdec22_Maine <- subset(cpsdec22,GESTFIPS == 23)
write.csv(cpsdec22_Maine, "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2022/cpsdec22_Maine.csv", row.names = FALSE)
cpsdec22_Maine <- read.csv("/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2022/cpsdec22_Maine.csv")
cpsdec22_Maine_Cumberland <- subset(cpsdec22_Maine,GTCO == 05)
write.csv(cpsdec22_Maine_Cumberland, "cpsdec22_Maine_Cumberland.csv", row.names = FALSE)

cpsdec21 <- read.csv("/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2021/dec21pub.csv")
cpsdec21_Maine <- subset(cpsdec21,GESTFIPS == 23)
write.csv(cpsdec21_Maine, "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2021/cpsdec21_Maine.csv", row.names = FALSE)
cpsdec21_Maine <- read.csv("/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2021/cpsdec21_Maine.csv")
cpsdec21_Maine_Cumberland <- subset(cpsdec21_Maine,GTCO == 05)
write.csv(cpsdec21_Maine_Cumberland, "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2021/cpsdec21_Maine_Cumberland.csv", row.names = FALSE)

cpsdec20 <- read.csv("/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2020/dec20pub.csv")
cpsdec20_Maine <- subset(cpsdec20,GESTFIPS == 23)
write.csv(cpsdec20_Maine, "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2020/cpsdec20_Maine.csv", row.names = FALSE)
cpsdec20_Maine <- read.csv("/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2020/cpsdec20_Maine.csv")
cpsdec20_Maine_Cumberland <- subset(cpsdec20_Maine,GTCO == 05)
write.csv(cpsdec21_Maine_Cumberland, "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2020/cpsdec20_Maine_Cumberland.csv", row.names = FALSE)


# If the data is comma-separated and has headers
data <- read.csv("/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2018/dec18pub.dat", sep = " ", header = TRUE)
write.csv(data, "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2018/dec18pub.csv", row.names = FALSE)

data <- read.csv("/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2019/dec19pub.dat", sep = " ", header = TRUE)
write.csv(data, "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2019/dec19pubdat.csv", row.names = FALSE)

##########################################################################################
# function to process cps data, split to the State of Maine only, and Cumberland county only
# also split to new england
# GESTFIPS:
# Connecticut:09
# Maine:23
# Massachusetts:25
# New Hampshire:33
# Rhode islands:44
# Vermont:50

process_cps_data <- function(input_file_path, maine_output_path, cumberland_output_path, newengland_output_path) {
  # Read the entire CPS dataset
  cps_data <- read.csv(input_file_path)
  
  # Standardize column names to uppercase
  names(cps_data) <- toupper(names(cps_data))
  
  # Filter for Maine (GESTFIPS == 23)
  cps_maine <- subset(cps_data, gestfips == 23)
  write.csv(cps_maine, maine_output_path, row.names = FALSE)
  
  # Reload the Maine data (optional, could use cps_maine directly)
  cps_maine <- read.csv(maine_output_path)
  
  # Filter for Cumberland County (GTCO == 05)
  cps_cumberland <- subset(cps_maine, gtco == 05)
  write.csv(cps_cumberland, cumberland_output_path, row.names = FALSE)
  
  # Filter for New England (GESTFIPS == 23, 9, 25, 33, 44, 50)
  cps_newengland <- subset(cps_data, GESTFIPS %in% c(23, 9, 25, 33, 44, 50))
  write.csv(cps_newengland, maine_output_path, row.names = FALSE)
}

# Apply the function to each year's dataset
process_cps_data("/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2010/cpsdec2010.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2010/cpsdec10_Maine.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2010/cpsdec10_Maine_Cumberland.csv")

process_cps_data("/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2011/cpsdec2011.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2011/cpsdec11_Maine.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2011/cpsdec11_Maine_Cumberland.csv")

process_cps_data("/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2012/cpsdec2012.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2012/cpsdec12_Maine.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2012/cpsdec12_Maine_Cumberland.csv")

process_cps_data("/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2013/cpsdec2013.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2013/cpsdec13_Maine.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2013/cpsdec13_Maine_Cumberland.csv")


process_cps_data("/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2014/cpsdec2014.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2014/cpsdec14_Maine.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2014/cpsdec14_Maine_Cumberland.csv")

process_cps_data("/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2015/cpsdec2015.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2015/cpsdec15_Maine.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2015/cpsdec15_Maine_Cumberland.csv")

process_cps_data("/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2016/cpsdec2016.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2016/cpsdec16_Maine.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2016/cpsdec16_Maine_Cumberland.csv")

process_cps_data("/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2017/cpsdec2017.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2017/cpsdec17_Maine.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2017/cpsdec17_Maine_Cumberland.csv")

process_cps_data("/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2018/cpsdec2018.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2018/cpsdec18_Maine.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2018/cpsdec18_Maine_Cumberland.csv")

process_cps_data("/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2019/cpsdec2019.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2019/cpsdec19_Maine.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2019/cpsdec19_Maine_Cumberland.csv")

process_cps_data("/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2022/dec22pub.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2022/cpsdec22_Maine.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2022/cpsdec22_Maine_Cumberland.csv")

process_cps_data("/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2021/dec21pub.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2021/cpsdec21_Maine.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2021/cpsdec21_Maine_Cumberland.csv")

process_cps_data("/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2020/dec20pub.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2020/cpsdec20_Maine.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2020/cpsdec20_Maine_Cumberland.csv")


process_cps_data("/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2020/dec20pub.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2020/cpsdec20_Maine.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2020/cpsdec20_Maine_Cumberland.csv")



# Filtered New England data
process_cps_data <- function(input_file_path, newengland_output_path) {
  # Read the entire CPS dataset
  cps_data <- read.csv(input_file_path)
  
  # Filter for New England (GESTFIPS == 23, 9, 25, 33, 44, 50)
  cps_newengland <- subset(cps_data, gestfips%in% c(23, 9, 25, 33, 44, 50))
  write.csv(cps_newengland, newengland_output_path, row.names = FALSE)
}

# Apply the function to each year's dataset
process_cps_data("/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2022/dec22pub.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2022/cpsdec22_NewEngland.csv")

process_cps_data("/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2021/dec21pub.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2021/cpsdec21_NewEngland.csv")


process_cps_data("/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2020/dec20pub.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2020/cpsdec20_NewEngland.csv")

process_cps_data("/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2017/cpsdec2017.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2017/cpsdec17_NewEngland.csv")

process_cps_data("/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2016/cpsdec2016.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2016/cpsdec16_NewEngland.csv")

process_cps_data("/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2015/cpsdec2015.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2015/cpsdec15_NewEngland.csv")

process_cps_data("/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2014/cpsdec2014.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2014/cpsdec14_NewEngland.csv")

process_cps_data("/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2013/cpsdec2013.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2013/cpsdec13_NewEngland.csv")

process_cps_data("/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2012/cpsdec2012.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2012/cpsdec12_NewEngland.csv")

process_cps_data("/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2011/cpsdec2011.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2011/cpsdec11_NewEngland.csv")

process_cps_data("/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2010/cpsdec2010.csv",
                 "/Users/xinyanliu/Desktop/NEU/Apriqot/CPS/2010/cpsdec10_NewEngland.csv")



##########################################################################################

# Update the weights to include the four implied decimals
cpsdec22_Maine <- cpsdec22_Maine %>%
  mutate(
    PWSSWGT = as.numeric(as.character(substr(PWSSWGT, 1, nchar(PWSSWGT)-4))) * 10^(4-nchar(substr(PWSSWGT, nchar(PWSSWGT)-3, nchar(PWSSWGT))))
  )

# Refactor GTCO values and count PRTAGE > 65 for each county group using PWSSWGT
age_count_by_county <- cpsdec22_Maine %>%
  mutate(
    GTCO = as.numeric(as.character(GTCO)),  # Ensure GTCO is numeric
    PRTAGE = as.numeric(as.character(PRTAGE)),  # Ensure PRTAGE is numeric
    PWSSWGT = ifelse(is.na(PWSSWGT), 0, PWSSWGT),
    County = case_when(
      GTCO == 01 ~ "Androscoggin",
      GTCO == 05 ~ "Cumberland",
      GTCO == 11 ~ "Kennebec",
      GTCO == 19 ~ "Penobscot",
      TRUE ~ "Other"  # Group all other GTCO values as "Other"
    )
  ) %>%
  group_by(County) %>%
  summarise(
    Unweighted_Count_65_Plus = sum(PRTAGE > 65),  # Unweighted count of individuals over 65
    Weighted_Count_65_Plus = sum(ifelse(PRTAGE > 65, PWSSWGT, 0)),  # Weighted count of individuals over 65
    Weighted_Count_All_Ages = sum(PWSSWGT),  # Weighted count of all ages
    Unweighted_Count_All_Ages = n()  # Unweighted count of all ages
  )

# Ensure age_count_by_county is a data frame
age_count_by_county <- as.data.frame(age_count_by_county)

# Calculate the sum for each column (except 'County') to create the 'Overall' row
overall_summary <- data.frame(
  County = "Overall",
  Unweighted_Count_65_Plus = sum(age_count_by_county$Unweighted_Count_65_Plus),
  Weighted_Count_65_Plus = sum(age_count_by_county$Weighted_Count_65_Plus),
  Weighted_Count_All_Ages = sum(age_count_by_county$Weighted_Count_All_Ages),
  Unweighted_Count_All_Ages = sum(age_count_by_county$Unweighted_Count_All_Ages)
)

# Separate the "Other" and "Overall" rows
other_row <- final_counts[final_counts$County == "Other", ]
overall_row <- final_counts[final_counts$County == "Overall", ]

# Remove the "Other" and "Overall" rows from the original data frame
final_counts <- final_counts[!final_counts$County %in% c("Other", "Overall"), ]

# Reassemble the data frame with "Other" row just above the "Overall" row
final_counts <- rbind(final_counts, other_row, overall_row)

# View the final result
print(final_counts)

# Filter the data for age 65 and above
cps_maine_22_age_65_plus <- cpsdec22_Maine %>%
  filter(PRTAGE >= 65)

# Summarize the data by HRNUMHOU using the weights
household_weighted_counts <- cps_maine_22_age_65_plus %>%
  group_by(HRNUMHOU) %>%
  summarise(Weighted_Count = sum(PWSSWGT, na.rm = TRUE)) %>%
  ungroup()

# Plotting the weighted data
ggplot(household_weighted_counts, aes(x = HRNUMHOU, y = Weighted_Count)) +
  geom_col() +
  labs(title = "Weighted Count of Persons in Household for Age 65+",
       x = "Number of Persons in Household",
       y = "Weighted Count of Individuals") +
  theme_minimal()


# Convert the data frame to a grid
table_plot <- tableGrob(final_counts)

# Use ggplot to display the table with a title
Maine_22_Age_Summary <- ggplot() +
  annotation_custom(table_plot) +
  labs(title = "2022 Maine Age and Weight Summary by County") +
  theme_void()


# Save the plot as a PNG file
ggsave("2022_Maine_Age_Summary_by_County.png", Maine_22_Age_Summary, width = 10, height = 6)


# repeat same process to other years 
process_and_save_age_summary <- function(cps_data, output_filename) {
  cps_data$GTCO <- as.numeric(as.character(cps_data$GTCO))
  
  # Refactor GTCO values and count PRTAGE > 65 for each county group using PWSSWGT
  cps_data <- cps_data %>%
    mutate(
      PWSSWGT = as.numeric(as.character(substr(PWSSWGT, 1, nchar(PWSSWGT)-4))) * 10^(4-nchar(substr(PWSSWGT, nchar(PWSSWGT)-3, nchar(PWSSWGT)))),
      GTCO = as.numeric(as.character(GTCO)),
      PRTAGE = as.numeric(as.character(PRTAGE)),
      PWSSWGT = ifelse(is.na(PWSSWGT), 0, PWSSWGT),
      County = case_when(
        GTCO == 1 ~ "Androscoggin",
        GTCO == 5 ~ "Cumberland",
        GTCO == 11 ~ "Kennebec",
        GTCO == 19 ~ "Penobscot",
        TRUE ~ "Other"
      )
    )

  # Refactor GTCO values and count PRTAGE > 65 for each county group using PWSSWGT
  age_count_by_county <- cps_data %>%
    group_by(County) %>%
    summarise(
      Unweighted_Count_65_Plus = sum(PRTAGE > 65),
      Weighted_Count_65_Plus = sum(ifelse(PRTAGE > 65, PWSSWGT, 0)),
      Weighted_Count_All_Ages = sum(PWSSWGT),
      Unweighted_Count_All_Ages = n()
    ) %>%
    as.data.frame()
  
  # Debugging: Check if County column is created
  if(!"County" %in% names(age_count_by_county)) {
    stop("County column not created. Check GTCO values and assignment logic.")
  } else {
    print(head(age_count_by_county)) # Print first few rows for inspection
  }
  
  # Calculate the 'Overall' summary
  overall_summary <- data.frame(
    County = "Overall",
    Unweighted_Count_65_Plus = sum(age_count_by_county$Unweighted_Count_65_Plus),
    Weighted_Count_65_Plus = sum(age_count_by_county$Weighted_Count_65_Plus),
    Weighted_Count_All_Ages = sum(age_count_by_county$Weighted_Count_All_Ages),
    Unweighted_Count_All_Ages = sum(age_count_by_county$Unweighted_Count_All_Ages)
  )
  
  # Combine and reorder rows
  final_counts <- rbind(age_count_by_county, overall_summary)
  other_row <- final_counts[final_counts$County == "Other", ]
  overall_row <- final_counts[final_counts$County == "Overall", ]
  # Remove the "Other" and "Overall" rows from the original data frame
  final_counts <- final_counts[!final_counts$County %in% c("Other", "Overall"), ]
  # Reassemble the data frame with "Other" row just above the "Overall" row
  final_counts <- rbind(final_counts, other_row, overall_row)
  
  # Calculate and plot the weighted number of persons in household for age 65+ people
  age_65_plus_household <- cps_data %>%
    filter(PRTAGE >= 65) %>%
    mutate(Isolated_Senior_Flag = ifelse(HRNUMHOU == 1, 1, 0)) %>% # HRNUMHOU indicates the number of adults living in the household
    group_by(HRNUMHOU) %>%
    summarise(
      Weighted_Count = sum(PWSSWGT, na.rm = TRUE),
      Isolated_Seniors = sum(ifelse(Isolated_Senior_Flag == 1, PWSSWGT, 0), na.rm = TRUE) # Sum of weights for isolated seniors
    ) %>%
    ungroup()
  
  household_plot <- ggplot(age_65_plus_household, aes(x = HRNUMHOU, y = Weighted_Count)) +
    geom_col() +
    geom_text(aes(label = round(Weighted_Count, 1)), vjust = -0.5, size = 3) + # Add text on top of bars
    labs(title = paste(output_filename, "- Weighted Number of Persons in Household for Age 65+"),
         x = "Number of Persons in Household",
         y = "Weighted Count") +
    theme_minimal() +
    scale_x_continuous(breaks = 1:max(age_65_plus_household$HRNUMHOU)) # Set x-axis breaks
  
  # Convert to a grid and create a ggplot
  table_plot <- tableGrob(final_counts)
  plot <- ggplot() +
    annotation_custom(table_plot) +
    labs(title = paste(output_filename, "Age and Weight Summary by County")) +
    theme_void()
  
  # Save the plot as a PNG file
  ggsave(paste0(output_filename, "_Age_Summary_by_County.png"), plot, width = 18, height = 6)
  ggsave(paste0(output_filename, "_Number_of_People_Household_Plot.png"), household_plot, width = 18, height = 6)
  
  # Calculate the count of isolated seniors by county
  isolated_seniors_by_county <- cps_data %>%
    filter(PRTAGE >= 65) %>%
    mutate(Isolated_Senior_Flag = ifelse(HRNUMHOU == 1, 1, 0)) %>%
    group_by(County) %>%
    summarise(Isolated_Seniors_Count = sum(ifelse(Isolated_Senior_Flag == 1, PWSSWGT, 0), na.rm = TRUE)) %>%
    ungroup()
  
  # Reorder County so that "Other" is last
  isolated_seniors_by_county$County <- fct_relevel(isolated_seniors_by_county$County, "Other", after = Inf)
  
  # Plot the count of isolated seniors vs county
  isolated_seniors_county_plot <- ggplot(isolated_seniors_by_county, aes(x = County, y = Isolated_Seniors_Count)) +
    geom_col(fill = "lightblue") +
    geom_text(aes(label = round(Isolated_Seniors_Count, 1)), vjust = -0.5, size = 3) +
    labs(title = paste(output_filename, "- Count of Isolated Seniors by County"),
         x = "County",
         y = "Count of Isolated Seniors") +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  # Save the plot as a PNG file
  ggsave(paste0(output_filename, "_Isolated_Seniors_by_County.png"), isolated_seniors_county_plot, width = 12, height = 6)
}

# Apply the function to cpsdec21_Maine and cpsdec20_Maine
process_and_save_age_summary(cpsdec22_Maine, "2022_Maine")
process_and_save_age_summary(cpsdec21_Maine, "2021_Maine")
process_and_save_age_summary(cpsdec20_Maine, "2020_Maine")

calculate_isolated_seniors_count <- function(cps_data, year) {
  cps_data <- cps_data %>%
    mutate(
      PRTAGE = as.numeric(as.character(PRTAGE)),
      HRNUMHOU = as.numeric(as.character(HRNUMHOU)),
      Isolated_Senior = ifelse(PRTAGE >= 65 & HRNUMHOU == 1, 1, 0) # Identify isolated seniors
    )
  
  isolated_seniors_count <- sum(cps_data$PWSSWGT[cps_data$Isolated_Senior == 1], na.rm = TRUE)
  
  return(data.frame(Year = year, Isolated_Seniors_Count = isolated_seniors_count))
}

# Apply the function to each year's data
count_2020 <- calculate_isolated_seniors_count(cpsdec20_Maine, "2020")
count_2021 <- calculate_isolated_seniors_count(cpsdec21_Maine, "2021")
count_2022 <- calculate_isolated_seniors_count(cpsdec22_Maine, "2022")

# Combine the counts
combined_counts <- rbind(count_2020, count_2021, count_2022)

# Create the plot
isolated_seniors_year_plot <- ggplot(combined_counts, aes(x = Year, y = Isolated_Seniors_Count)) +
  geom_line(group=1) +
  geom_point() +
  theme_minimal() +
  labs(title = "CPS-Yearly Count of Isolated Seniors (Living Alone)",
       x = "Year",
       y = "Count of Isolated Seniors")

# Display the plot
print(isolated_seniors_year_plot)

# Optionally save the plot
ggsave("Yearly_Count_Isolated_Seniors_CPS.png", plot = isolated_seniors_year_plot, width = 10, height = 6)


# Function to calculate the total count of seniors aged 65+ for each year
calculate_total_seniors_65_plus <- function(cps_data, year) {
  cps_data <- cps_data %>%
    mutate(
      PRTAGE = as.numeric(as.character(PRTAGE)),
      PWSSWGT = as.numeric(as.character(substr(PWSSWGT, 1, nchar(PWSSWGT)-4))) * 10^(4-nchar(substr(PWSSWGT, nchar(PWSSWGT)-3, nchar(PWSSWGT))))
    )
  
  total_seniors_65_plus_count <- sum(cps_data$PWSSWGT[cps_data$PRTAGE >= 65], na.rm = TRUE)
  
  return(data.frame(Year = year, Total_Seniors_65_Plus = total_seniors_65_plus_count))
}

# Apply the function to each year's data
total_seniors_count_2020 <- calculate_total_seniors_65_plus(cpsdec20_Maine, "2020")
total_seniors_count_2021 <- calculate_total_seniors_65_plus(cpsdec21_Maine, "2021")
total_seniors_count_2022 <- calculate_total_seniors_65_plus(cpsdec22_Maine, "2022")

# Combine the total counts
combined_total_seniors_counts <- rbind(total_seniors_count_2020, total_seniors_count_2021, total_seniors_count_2022)

# Create the plot for total seniors 65+ count
total_seniors_65_plus_year_plot <- ggplot(combined_total_seniors_counts, aes(x = Year, y = Total_Seniors_65_Plus)) +
  geom_line(group=1) +
  geom_point() +
  geom_text(aes(label = round(Total_Seniors_65_Plus, 0)), vjust = -0.5, hjust = 1.5, color = "blue") +
  theme_minimal() +
  labs(title = "Total Yearly Count of Seniors Aged 65+ (CPS)",
       x = "Year",
       y = "Total Count of Seniors 65+")+
  scale_y_continuous(limits = c(0, NA),  # Start y-axis at 0 and extend to maximum value
                     breaks = waiver(),  # Use default breaks or set specific breaks if needed
                     labels = label_number())  # Format numbers without scientific notation

# Display the plot
print(total_seniors_65_plus_year_plot)

# Optionally save the plot
ggsave("Total_Yearly_Count_Seniors_65_Plus_CPS.png", plot = total_seniors_65_plus_year_plot, width = 10, height = 6)

# combined cps and brfss data for number of isolated seniors over the year
cps_isolated_seniors <- data.frame(Year = c("2020", "2021", "2022"), Isolated_Seniors_Count = c(97115, 99663, 99244))
brfss_isolated_seniors <- data.frame(Year = c("2022", "2020", "2018","2016"), Isolated_Seniors_Count = c(77141,90203, 52390, 42376))

# Add a source column to each data frame
cps_isolated_seniors$Source <- "CPS"
brfss_isolated_seniors$Source <- "BRFSS"

# Create the plot
combined_plot_isolated_seniors <- ggplot(combined_data_isolated_seniors, aes(x = Year, y = Isolated_Seniors_Count, color = Source, group = Source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(0, max(combined_data_isolated_seniors$Isolated_Seniors_Count)), 
                     labels = scales::comma) + # Using scales::comma to format labels
  theme_minimal() +
  labs(title = "Yearly Count of Isolated Seniors (Living Alone) - CPS vs BRFSS",
       x = "Year",
       y = "Count of Isolated Seniors")

# Display the plot
print(combined_plot_isolated_seniors)

# Optionally save the plot
ggsave("Combined_Yearly_Count_Isolated_Seniors_CPS_BRFSS.png", plot = combined_plot_isolated_seniors, width = 10, height = 6)


# Have already calculated total seniors count for CPS and BRFSS
cps_total_seniors <- data.frame(Year = c("2020", "2021", "2022"), Total_Seniors_65_Plus = c(330211, 360689, 336322))
brfss_total_seniors <- data.frame(Year = c("2022", "2020", "2018", "2016"), Total_Seniors_65_Plus = c(307787, 287123, 215310, 185180))

# Add a source column to each data frame
cps_total_seniors$Source <- "CPS"
brfss_total_seniors$Source <- "BRFSS"

# Combine the data
combined_data_total_seniors <- rbind(cps_total_seniors, brfss_total_seniors)

# Create the plot with 0 included on the y-axis and non-scientific notation
combined_plot_total_seniors <- ggplot(combined_data_total_seniors, aes(x = Year, y = Total_Seniors_65_Plus, color = Source, group = Source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(limits = c(0, max(combined_data_total_seniors$Total_Seniors_65_Plus)), 
                     labels = scales::comma) + # Using scales::comma to format labels
  theme_minimal() +
  labs(title = "Yearly Total Count of Seniors Aged 65+ - CPS vs BRFSS",
       x = "Year",
       y = "Total Count of Seniors 65+")

# Display the plot
print(combined_plot_total_seniors)

# Optionally save the plot
ggsave("Combined_Yearly_Total_Count_Seniors_65_Plus_CPS_BRFSS.png", plot = combined_plot_total_seniors, width = 10, height = 6)


##########################################################################################
# ASC 2018-2022
# AQO1E003: households with one or more people 65+:1-person household
# YEAR
# COUNTY

# Read the CSV file
data <- read.csv("/Users/xinyanliu/Desktop/NEU/Apriqot/ACS/ACS_2018_2022.csv")

# Calculate the sum of counts for each county
summarized_data <- data %>%
  group_by(COUNTY) %>%
  summarize(Total_Seniors = sum(AQO1E001, na.rm = TRUE),
            Total_Isolated_Seniors = sum(AQO1E003, na.rm = TRUE)) %>%
  mutate(Percentage_Isolated_Seniors = (Total_Isolated_Seniors / Total_Seniors) * 100)

# Merge the sum with the original data
merged_data <- merge(data, summarized_data, by = "COUNTY")

# Plotting the number of isolated seniors for each county with total sum for each county
Isolated_Seniors_per_County_ACS<-ggplot(merged_data, aes(x = COUNTY, y = AQO1E003)) +
  geom_bar(stat = "identity", fill = "steelblue") +  # Bar plot
  geom_text(aes(y = max(AQO1E003) + 10, label = Total), vjust = 0,size = 3.5) +
  theme_minimal() +
  labs(title = "Number of Isolated Seniors (65+) per County_2018_2022_ACS",
       x = "County",
       y = "Count of Isolated Seniors") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels

print(Isolated_Seniors_per_County_ACS)

ggsave("Number of Isolated Seniors per County_ACS.png", plot = Isolated_Seniors_per_County_ACS, width = 10, height = 6)

# Plot the percentage of isolated seniors by county
percentage_plot <- ggplot(summarized_data, aes(x = COUNTY, y = Percentage_Isolated_Seniors)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage_Isolated_Seniors)),
            vjust = -0.5, size = 3) +
  theme_minimal() +
  labs(title = "Percentage of Isolated Seniors (65+) by County_ACS",
       x = "County",
       y = "Percentage of Isolated Seniors") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the plot
print(percentage_plot)

ggsave("Percentage of Isolated Seniors per County_ACS.png", plot = percentage_plot, width = 10, height = 6)

# Group by COUNTY, and calculate the percentage
summarized_data_isolated_senior <- data %>%
  group_by(COUNTY) %>%
  summarize(One_Person_65_Over = sum(AQO1E003, na.rm = TRUE),
            Total_Households = sum(AQO1E001, na.rm = TRUE),
            Households_No_65_Over = sum(AQO1E007, na.rm = TRUE)) %>%
  mutate(Percentage_Isolated_Seniors = (One_Person_65_Over / (Total_Households - Households_No_65_Over)) * 100)

# Plot the percentage of people over 65 who are isolated seniors by county
percentage_plot_isolated_senior <- ggplot(summarized_data_isolated_senior, aes(x = COUNTY, y = Percentage_Isolated_Seniors)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = sprintf("%.1f%%", Percentage_Isolated_Seniors)),
            vjust = -0.5, size = 3) +
  theme_minimal() +
  labs(title = "Percentage of People Over 65 Who are Isolated Seniors by County_ACS",
       x = "County",
       y = "Percentage") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Print the plot
print(percentage_plot_isolated_senior)

ggsave("Percentage of People Over 65 Who are Isolated Seniors by County_ACS.png", plot = percentage_plot_isolated_senior, width = 10, height = 6)
