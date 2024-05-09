# Load required libraries
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(sf)
library(ggplot2)




# Generate example data
set.seed(123)
library(sf)

# Manually defined larger polygon coordinates
larger_polygon_coords <- matrix(c(
  0.7, 0.7,  # Bottom left
  2.3, 0.7,  # Bottom right
  2.3, 2.3,  # Top right
  0.7, 2.3,  # Top left
  0.7, 0.7   # Closing the polygon back to the start
), ncol = 2, byrow = TRUE)

# Create an sf polygon object for the larger polygon
larger_polygon <- st_polygon(list(larger_polygon_coords))
larger_polygon_sf <- st_sfc(larger_polygon, crs = 4326)  # Assign the same CRS as other data
larger_polygon_sf <- st_sf(geometry = larger_polygon_sf)



data <- data.frame(
  x = c(rnorm(10, 1, 0.1), rnorm(10, 2, 0.1), rnorm(10, 1.5, 0.1), rnorm(10, 1.8, 0.1), rnorm(10, 1.3, 0.1)),
  y = c(rnorm(10, 1, 0.1), rnorm(10, 2, 0.1), rnorm(10, 1.5, 0.1), rnorm(10, 1.8, 0.1), rnorm(10, 1.3, 0.1)),
  group = factor(rep(1:5, each = 10))
)

# Calculate centroids for each group
centroid_data <- aggregate(cbind(x, y) ~ group, data = data, FUN = mean)

# Function to calculate convex hull and convert to sf polygon
create_hull_sf <- function(group_data) {
  hull_indices <- chull(group_data$x, group_data$y)
  hull_points <- group_data[hull_indices, , drop = FALSE]
  hull_points <- rbind(hull_points, hull_points[1, , drop = FALSE])  # Close the polygon
  pol <- st_polygon(list(as.matrix(hull_points[c("x", "y")])))
  return(st_sfc(pol))
}

# Apply function to each group
polygons <- do.call(c, lapply(split(data, data$group), create_hull_sf))
polygon_sf <- st_sf(
  geometry = st_sfc(polygons, crs = 4326),
  group = as.character(1:5)
)

# Create an sf object for centroids for better handling in ggplot2
centroids_sf <- st_as_sf(centroid_data, coords = c("x", "y"), crs = 4326)



# Complete visualization with convex hulls, centroids, and data points
library(ggplot2)

# Assuming you have `data`, `polygon_sf`, and `centroids_sf` as previously created
ggplot() +
  geom_sf(data = larger_polygon_sf, fill = NA, color = "blue", size = 1, linetype = "dashed") +
  geom_sf(data = polygon_sf, aes(color = group), fill = NA, size = 1, show.legend = "line") +
  geom_point(data = data, aes(x = x, y = y, color = group), size = 2) +
  geom_sf(data = centroids_sf, color = "red", size = 4, shape = 4) +
  geom_text(data = centroids_sf %>% st_set_geometry(NULL), aes(label = group, x = x, y = y), vjust = -1, color = "red") +
  scale_color_manual(values = rainbow(length(unique(data$group)))) +
  labs(title = "Data Points, Centroids, Convex Hulls, and Larger Polygon") +
  theme_minimal()


st_write(polygon_sf, "polygons.shp")
