library(dplyr)
library(readr)  # For write_csv function

df <- read.csv("Maine_County_Level_Alzheimer_Estimates.csv",check.names = FALSE)

# Perform the operations
df_grouped <- df %>%
  group_by(`Puma Code`) %>%
  summarise(
    `Alz ass total pop 65` = sum(`Alz ass total pop 65`, na.rm = TRUE),
    `Apriqot total pop 65` = sum(`Apriqot total pop 65`, na.rm = TRUE),
    County_Combined = paste(unique(County), collapse = ", "),
    County_Code_Combined = paste(unique(`County Code`), collapse = ", "),
    M1_affected_population = sum(`M1 affected_population`, na.rm = TRUE),
    M2_affected_population = sum(`M2 affected_population`, na.rm = TRUE),
    reported_affected_population = sum(`reported affected_population`, na.rm = TRUE),
    `reported percentage`= reported_affected_population / `Alz ass total pop 65` * 100
  ) %>%
  mutate(
    `M1 percentage` = (M1_affected_population / `Alz ass total pop 65`) * 100,
    `M2 percentage` = (M2_affected_population / `Alz ass total pop 65`) * 100
  )
  
# Save the grouped data to a new CSV file
write_csv(df_grouped, "Maine_County_to_PUMA_Alzheimer_Estimates.csv")

