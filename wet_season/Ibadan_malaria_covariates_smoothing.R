rm(list=ls())

metropolis_name <- "Ibadan"

source("load_paths.R")

library(ggplot2)
library(gstat)
library(sp)
library(raster)


newdata <- read.csv( file.path(cleaned_data_path, metropolis_name, "ibadan_analysis_data.csv")) %>% 
  filter(ward == "Agugu") %>% 
  dplyr::select(longitude, latitude, average_EVI)



coordinates(newdata) <- ~longitude+latitude


inverse_distance_model <- gstat(formula = average_EVI ~ 1, 
                                data = newdata, nmax = 5)

# Define the spatial extent for interpolation (adjust to your specific needs)
extent_values <- extent(c(min(newdata$longitude),
                          max(newdata$longitude),
                          min(newdata$latitude),
                          max(newdata$latitude)))

# Create a grid for the interpolation
grid <- expand.grid(longitude = seq(from = extent_values@xmin,
                              to = extent_values@xmax, by = 0.1),
                    latitude = seq(from = extent_values@ymin, 
                              to = extent_values@ymax, by = 0.1))



coordinates(grid) <- ~longitude + latitude

gridded(grid) <- TRUE

## Perform IDW interpolation

interpolated <- raster::interpolate(grid, inverse_distance_model)

# Convert raster to dataframe for ggplot

interpolated_df <- as.data.frame(rasterToPoints(interpolated))

# Plot
ggplot() +
  geom_tile(data = interpolated_df, aes(x = lon, y = lat, fill = value)) +
  geom_point(data = points_data, aes(x = longitude, y = latitude), color = "red", size = 2) +  # Optionally add original points
  scale_fill_viridis_c() +  # Use a nice color scale
  labs(title = "Smoothed Map of Ward", x = "Longitude", y = "Latitude", fill = "Value") +
  coord_fixed()  # Keep aspect ratio

##################################################################################################
# Krigging 
##################################################################################################


library(sp); library(raster)

# Assuming 'points_data' is your data.frame with 'lon' and 'lat' columns
coordinates(points_data) <- ~lon+lat  # Convert to SpatialPointsDataFrame


coordinates(points_data) <- ~lon+lat


interpolated_values <- raster::extract(raster_data, points_data)

library(gstat)

# Example of IDW interpolation with gstat (note: gstat uses 'idw' function for IDW interpolation)
# Ensure your data is in SpatialPointsDataFrame or similar spatial format


