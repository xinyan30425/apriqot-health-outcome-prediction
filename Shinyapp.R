# Load necessary packages
library(shiny)
library(tidycensus)
library(ggplot2)
library(sf)
library(readr)
library(tidyr)
library(dplyr)
library(tigris)
library(viridis)

# Define the UI
ui <- fluidPage(
  titlePanel("Proportion of People in Poverty Rely on Public Transport Census Tract"),
  mainPanel(
    plotOutput("censusMap") # Display the map here
  )
)

# Define the server logic
server <- function(input, output) {
  
  # You can include all the data processing code here or, for efficiency, move everything not related to plotting outside the server function.
  # Read the data using readr
  data <- readr::read_csv("PUMA census tract relationships.txt")
  
  # Write the data to a .csv file
  readr::write_csv(data, "PUMA_tract_relationships.csv")
  
  # Subset the data based on STATEFP and TRACTCE columns
  subset_df <- data[data$STATEFP == "23" & data$PUMA5CE == "01000", ]
  
  # Save the subsetted data to a new CSV file
  readr::write_csv(subset_df, "subsetted_tracts.csv")
  
  # Retrieve the data
  vars <- c("B08122_013", "B08122_002", "B08122_026")
  
  # Fetch the data
  cumberland_data <- get_acs(geography = "tract",  # function fetches American Community Survey (ACS) data
                             variables = vars, 
                             state = 'ME', 
                             county = "Cumberland",
                             puma = '01000')
  
  
  # Pivot the data 
  reshaped_data <- cumberland_data %>%
    spread(key = variable, value = estimate) %>%
    group_by(GEOID, NAME) %>%
    summarise(B08122_013 = sum(B08122_013, na.rm = TRUE),
              B08122_002 = sum(B08122_002, na.rm = TRUE),
              B08122_026 = sum(B08122_026, na.rm = TRUE)) %>%
    ungroup()
  
  # Calculate the proportion for each tract
  reshaped_data$proportion_public_transport <- with(reshaped_data, B08122_013 / (B08122_002 - B08122_026)*100)
  
  # Extracting TRACTCE from the GEOID
  reshaped_data_filtered <- reshaped_data %>%
    mutate(     
      TRACTCE = substr(GEOID, 6, 11)       
    )
  
  # Filtering data based on specific TRACTCE
  filtered_data <- reshaped_data_filtered %>%
    filter(TRACTCE %in% subset_df$TRACTCE)
  
  
  # Get the geographic boundaries for the census tracts in Maine, Cumberland
  options(tigris_class = "sf")
  maine_tracts <- tracts(state = "ME", county = "Cumberland",cb = TRUE)
  
  # Filtering the maine_tracts data based on specific TRACTCE
  filtered_maine_tracts <- maine_tracts %>%
    filter(TRACTCE %in% subset_df$TRACTCE)
  
  # Join the geographic boundaries with reshaped data on the TRACTCE:
  map_data <- left_join(filtered_maine_tracts, filtered_data, by = c("TRACTCE" = "TRACTCE"))
  
  output$censusMap <- renderPlot({
    # Assuming you've processed the map_data as in your original code
    gg <- ggplot(data = map_data) +
      geom_sf(aes(fill = proportion_public_transport), color = NA) +
      scale_fill_viridis_c() +
      theme_minimal() +
      labs(title = "Proportion of People in Poverty Rely on Public Transport Census Tract",
           fill = "Proportion %")
    
    return(gg)
  })
  
}

# Run the app
shinyApp(ui = ui, server = server)

