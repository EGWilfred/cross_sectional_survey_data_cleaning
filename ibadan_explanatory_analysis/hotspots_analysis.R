# Load necessary libraries
library(sp)
library(spdep)
library(raster)
library(leaflet)

# Sample dataset with malaria prevalence and coordinates
# Replace this with your own dataset
data <- data.frame(
  Longitude = c(10.1, 10.2, 10.3, 10.4, 10.5),
  Latitude = c(50.1, 50.2, 50.3, 50.4, 50.5),
  Prevalence = c(0.05, 0.1, 0.3, 0.15, 0.25)
)

# Create a SpatialPointsDataFrame
coordinates(data) <- c("Longitude", "Latitude")

# Create a neighbor list
nb <- dnearneigh(data, d1 = 0, d2 = 1)

# Convert nb to a listw object
W <- nb2listw(nb)

# Calculate Moran's I to test for spatial autocorrelation
moran <- moran.test(data$Prevalence, W)

# Create a hotspot map using leaflet
hotspot_map <- leaflet(data) %>%
  addTiles() %>%
  addCircles(
    radius = 10000,  # Adjust the radius as needed
    color = ifelse(moran$p.value <= 0.05, "red", "blue"),  # Significant or not
    label = ~Prevalence
  ) %>%
  addLegend(
    colors = c("red", "green"),
    labels = c("Hotspot", "Not Significant"),
    position = "bottomright"
  )

# Display the hotspot map
hotspot_map
