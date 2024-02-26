
install.packages("geosphere")
library(geosphere)
library(ggplot2)
library(sf)
library(sp)


kano_settlement_type <- read.csv("C:/Users/lml6626/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/EA_data/Copy of kano_eas_coord - kano_eas_coord.csv.csv")

split_list <- split(kano_settlement_type, kano_settlement_type$Ward)


distance_matrix <- lapply(seq_along(split_list), function(x) distm(split_list[[x]][, c("Longitude", "Latitude")], fun = distHaversine))

rownames(distance_matrix[[1]]) <- split_list[[1]]$EA.NAME
rownames(distance_matrix[[2]]) <- split_list[[2]]$EA.NAME
rownames(distance_matrix[[3]]) <- split_list[[3]]$EA.NAME
rownames(distance_matrix[[4]]) <- split_list[[4]]$EA.NAME
rownames(distance_matrix[[5]]) <- split_list[[5]]$EA.NAME
# colnames(distance_matrix) <- split_list$EA.NAME

colnames(distance_matrix[[1]]) <- split_list[[1]]$EA.NAME
colnames(distance_matrix[[2]]) <- split_list[[2]]$EA.NAME
colnames(distance_matrix[[3]]) <- split_list[[3]]$EA.NAME
colnames(distance_matrix[[4]]) <- split_list[[4]]$EA.NAME
colnames(distance_matrix[[5]]) <- split_list[[5]]$EA.NAME



distance_df <- data.table::rbindlist(
  lapply(seq_along(distance_matrix), function(x) as.data.frame(as.table(distance_matrix[[x]])))
  )

names(distance_df) <- c("Departure", "Destination", "Distance(m)")

# Convert data frame to an sf object
coordinates(df) <- ~longitude+latitude
sf_df <- st_as_sf(df)

# Plotting
ggplot(data = sf_df) +
  geom_sf() +
  theme_minimal() +
  labs(title = "Spatial Data Points in Giginyu Ward")
