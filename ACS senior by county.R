library(haven)
library(ggplot2)
library(dplyr)
library(scales)  # Load the scales library for formatting
library(gridExtra)
library(forcats) 
library(readr)
library(gridExtra)


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
