# Load required libraries
if (!requireNamespace("sf", quietly = TRUE)) install.packages("sf")
if (!requireNamespace("ggplot2", quietly = TRUE)) install.packages("ggplot2")
library(sf); library(ggplot2)

# Generate example data
set.seed(123)





# Manually defined larger polygon coordinates
larger_polygon_coords <- matrix(c(
  0.7, 0.7, 
  2.3, 0.7,  
  2.3, 2.3,  
  0.7, 2.3,  
  0.7, 0.7   
), ncol = 2, byrow = TRUE)

 

#  sf polygon 
larger_polygon <- st_polygon(list(larger_polygon_coords))
larger_polygon_sf <- st_sfc(larger_polygon, crs = 4326)  
larger_polygon_sf <- st_sf(geometry = larger_polygon_sf)



data <- data.frame(
  x = c(rnorm(10, 1, 0.1), rnorm(10, 2, 0.1), rnorm(10, 1.5, 0.1), rnorm(10, 1.8, 0.1), rnorm(10, 1.3, 0.1)),
  y = c(rnorm(10, 1, 0.1), rnorm(10, 2, 0.1), rnorm(10, 1.5, 0.1), rnorm(10, 1.8, 0.1), rnorm(10, 1.3, 0.1)),
  group = factor(rep(1:5, each = 10))
)


Ibadan_shapefile <- sf::read_sf(shapefile) %>% 
  dplyr::filter( WardName == "Agugu")

data <- read.csv(file.path(cleaned_data_path, metropolis_name,"Ibadan_centoids_data.csv"))


Ibadan_data_malaria_data <- read.csv(file.path(cleaned_data_path, metropolis_name,"spatial_data_analysis.csv")) %>% 
  dplyr::select(ea_numbers_new, Ward, longitude, latitude) %>% 
  distinct()

write.csv(Ibadan_data_malaria_data, file.path(cleaned_data_path, metropolis_name,"coordinates_ibadan.csv")) 


# Convert the data points to an sf object
data_points_sf <- st_as_sf(Ibadan_data_malaria_data, coords = c("longitude", "latitude"), crs = 4326)

# Calculate centroids for each ea_numbers_new
centroids <- data_points_sf %>%
  group_by(ea_numbers_new) %>%
  summarise(geometry = st_centroid(st_union(geometry)))

# Create a convex hull polygon around the data points
polygons <- data_points_sf %>%
  group_by(ea_numbers_new) %>%
  summarise(geometry = st_convex_hull(st_union(geometry)))

# Plot the results
ggplot() +
  geom_sf(data = Ibadan_shapefile, fill = "grey") +
  # geom_sf(data = polygons,  alpha = 0.5) +
  geom_sf(data = data_points_sf, color = "black", size = 2) +
  ylim(c(7.371043, 7.391085))+xlim(c(3.916961, 3.93985)) +
  labs(title = "Polygon Describing Data Points Area", fill = "EA Numbers") +
  theme(legend.position = "none")

# Save the convex hull polygon as a shapefile
st_write(convex_hull, "path/to/convex_hull.shp")

# Calculate centroids for each group
centroid_data <- Ibadan_data_malaria_data

# Function to calculate convex hull and convert to sf polygon
create_hull_sf <- function(group_data) {
  hull_indices <- chull(group_data$longitude, group_data$latitude)
  hull_points <- group_data[hull_indices, , drop = FALSE]
  hull_points <- rbind(hull_points, hull_points[1, , drop = FALSE])  # Close the polygon
  pol <- st_polygon(list(as.matrix(hull_points[c("x", "y")])))
  return(st_sfc(pol))
}

# Apply function to each group
polygons <- do.call(c, lapply(split(Ibadan_data_malaria_data, Ibadan_data_malaria_data$ea_numbers_new), create_hull_sf))

polygon_sf <- st_sf(
  geometry = st_sfc(polygons, crs = 4326),
  group = as.character(1:5)
)

# Create an sf object for centroids for better handling in ggplot2
centroids_sf <- st_as_sf(centroid_data, coords = c("x", "y"), crs = 4326)



# Complete visualization with convex hulls, centroids, and data points

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
