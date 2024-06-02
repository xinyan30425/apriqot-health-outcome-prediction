library(tigris)
library(sf)

install.packages("geojsonio")
library(geojsonio)

# Download the US boundary shapefile
us_boundary <- states(cb = TRUE)

# Convert the shapefile to GeoJSON
geojson_file <- geojsonio::geojson_write(us_boundary, file = "us_boundary.geojson")
