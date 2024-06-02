install.packages("tigris")
if (!requireNamespace("tigris", quietly = TRUE)) install.packages("tigris")
library(tigris)
library(ggplot2)

if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
library(sf)
library(rnaturalearth)

options(tigris_use_cache = TRUE)

# Download county-level shapefile for Maine
maine_counties <- counties(state = "23", cb = TRUE)
connecticut_counties <- counties(state = "09", cb = TRUE)
mass_counties <- counties(state = "25", cb = TRUE)
nh_counties <- counties(state = "33", cb = TRUE)
rh_counties <- counties(state = "44", cb = TRUE)
vermont_counties <- counties(state = "50", cb = TRUE)

ggplot(maine_counties) +
  geom_sf() +
  ggtitle("County-level Shapefile for Maine")

# Set path for saving the shapefile
shapefile_path <- "/Users/xinyanliu/Desktop/NEU/Apriqot/Alzheimer/maine_counties.shp"

# Use st_write from sf package to save the shapefile
sf::st_write(maine_counties, dsn = shapefile_path)

# Combine shapefiles for all New England states
new_england_counties <- rbind(maine_counties, connecticut_counties, mass_counties, nh_counties, rh_counties, vermont_counties)

# Plot the combined shapefile
plot(new_england_counties)

# Set path for saving the combined shapefile
shapefile_path <- "/Users/xinyanliu/Desktop/NEU/Apriqot/Alzheimer/new_england_counties.shp"

# Use st_write from sf package to save the combined shapefile
st_write(new_england_counties, dsn = shapefile_path)

head(new_england_counties)