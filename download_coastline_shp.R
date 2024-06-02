install.packages("tigris")
if (!requireNamespace("tigris", quietly = TRUE)) install.packages("tigris")
library(tigris)
library(ggplot2)
library(dplyr) # Make sure dplyr is loaded to use its filter function

if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
library(sf)

# Install rnaturalearth if it's not already installed
if (!requireNamespace("rnaturalearth", quietly = TRUE)) {
  install.packages("rnaturalearth")
}

# Load the rnaturalearth package
library(rnaturalearth)

# Download global coastline data
global_coastline <- ne_coastline(scale = 'medium', returnclass = "sf")

ggplot(data = global_coastline) +
  geom_sf() +
  ggtitle("Global Coastline")

# Save the global coastline data as a shapefile
st_write(global_coastline, "/Users/xinyanliu/Desktop/NEU/Apriqot/Alzheimer/global_coastline.shp")

# States included in New England
new_england_states <- c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont")

# Download state boundaries
states <- states(cb = TRUE, resolution = "20m")

# Filter for New England
new_england <- states[states$NAME %in% new_england_states, ]

# Make sure 'coastline' and 'new_england' (or your specific state) are in the same CRS
global_coastline <- st_transform(global_coastline, st_crs(new_england))

# Clip coastline to New England
new_england_coastline <- st_intersection(global_coastline, new_england)

ggplot(data = new_england_coastline) +
  geom_sf() +
  ggtitle("New England Coastline")

# Save New England coastline
st_write(new_england_coastline, "/Users/xinyanliu/Desktop/NEU/Apriqot/Alzheimer/new_england_coastline.shp")

# For just Maine
maine_coastline <- st_intersection(global_coastline, states[states$NAME == "Maine", ])

ggplot(data = maine_coastline) +
  geom_sf() +
  ggtitle("Maine Coastline")

# Save Maine coastline
st_write(maine_coastline, "/Users/xinyanliu/Desktop/NEU/Apriqot/Alzheimer/maine_coastline.shp")

# Get the boundaries of all states
states <- states(cb = TRUE, class = "sf") 

# Filter for Maine by name
# Explicitly use dplyr's filter function to avoid any potential conflicts
maine_boundary <- states %>%
  dplyr::filter(NAME == "Maine")

# Ensure the CRS matches between the datasets
maine_boundary <- st_transform(maine_boundary, st_crs(land))

# Intersect the land data with Maine's boundary to get land within Maine
maine_land <- st_intersection(land, maine_boundary)

st_write(maine_land, "/Users/xinyanliu/Desktop/NEU/Apriqot/Alzheimer/maine_land.shp")
# Filter for New England by name
new_england_boundary <- states %>% 
  filter(NAME %in% new_england_states)

# Ensure the CRS matches
new_england_boundary <- st_transform(new_england_boundary, st_crs(land))

# Intersect the land data with New England's boundary
new_england_land <- st_intersection(land, new_england_boundary)

st_write(new_england_land, "/Users/xinyanliu/Desktop/NEU/Apriqot/Alzheimer/new_england_land.shp")
