rm(list=ls())

metropolis_name <- "Ibadan"

source("load_paths.R")
library(geosphere)

dhsDir <- file.path(DriveDir, "data")
nigeria_data <-  file.path(DriveDir,"data","nigeria")

EA_weight_adjusted_tpr <- read.csv(file.path(cleaned_data_path, metropolis_name,"EA_weight_adjusted_tpr.csv"))
Ibadan_data_malaria_data <- read.csv(file.path(cleaned_data_path, metropolis_name,"spatial_data_analysis.csv")) 
shapefile <- file.path( nigeria_data, "kano_ibadan/kano_ibadan_shape_files/Ibadan_metro_ward_fiveLGAs/Ibadan_metro_fiveLGAs.shp")
Ibadan_shapefile <- sf::read_sf(shapefile)

# Example data: Latitude and Longitude
centroids_Ibadan <- Ibadan_data_malaria_data %>% 
  dplyr::select(longitude, latitude, Ward, ea_numbers_new, settlement_type_new) %>% 
  dplyr::distinct()


split_data <- split(centroids_Ibadan, centroids_Ibadan$ea_numbers_new)

###############################################################################
# functions 

deg2rad <- function(degrees) {
  return(degrees * pi / 180)
}

rad2deg <- function(radians) {
  return(radians * 180 / pi)
}


###############################################################################
#

centroids <- list()


for (index in seq_along(split_data)){
  
    data <- split_data[[index]]


    # Convert degrees to radians
    data$radians_lat <- deg2rad(data$latitude)
    data$radians_lon <- deg2rad(data$longitude)
    
    # Convert to Cartesian coordinates
    data$x <- cos(data$radians_lat) * cos(data$radians_lon)
    data$y <- cos(data$radians_lat) * sin(data$radians_lon)
    data$z <- sin(data$radians_lat)
    
    # Calculate mean of Cartesian coordinates
    mean_x <- mean(data$x)
    mean_y <- mean(data$y)
    mean_z <- mean(data$z)
    
    # Convert back to latitude and longitude
    centroid_lon <- atan2(mean_y, mean_x)
    centroid_lat <- atan2(mean_z, sqrt(mean_x^2 + mean_y^2))
    
    # Convert radians back to degrees
    centroid_lon <- rad2deg(centroid_lon)
    centroid_lat <- rad2deg(centroid_lat)
    

    centroids_data <- data.frame(settlement_type_new = data$settlement_type_new[1],
                                 Ward = data$Ward[1], 
                                 ea_numbers_new = data$ea_numbers_new[1], 
                                 longitude = centroid_lon, 
                                 latitude = centroid_lat)
    
    centroids[[index]] <- centroids_data
}


final_centroid_data <- data.table::rbindlist(centroids)


newdata <- EA_weight_adjusted_tpr %>% 
  inner_join(final_centroid_data) %>% 
  mutate(Ward == "OLOGUNERU", "OLOPOMEWA", Ward, 
         tpr_cut = cut(tpr, breaks = c(0, 2, 4, 8, 12, 20, 50),
                       include.lowest = TRUE, include.highest = TRUE, right = TRUE ))
  

color_palette <- grDevices::colorRampPalette(colors = c("darkgreen","yellow", "red"))
class_colors <- color_palette(6)

wards_interest <- c("Bashorun",  "Challenge",
                    "Olopomewa", "Agugu" )

wards_interest00 <- unique(newdata$Ward)

sf_newdata = sf::st_as_sf(newdata, 
                          coords = c( "longitude", "latitude"),  
                          crs = 4326)

plots <- list()


for (index in seq_along(wards_interest)){
  
  wards = wards_interest[index]
  wards00 <-  wards_interest00[index]
  
  shapefile_interest <- Ibadan_shapefile %>% 
    filter(WardName == wards)
  
  ward_hh_data <- sf_newdata %>% 
    filter(Ward == wards00)
  
  sf::st_crs(shapefile_interest) <- 4326
  sf::st_crs(ward_hh_data) <- 4326
  
  coordinate_intersection <- sf::st_intersection(shapefile_interest, ward_hh_data)
  
  
 ggplot(shapefile_interest)+
    geom_sf(fill = NA) +
    geom_point(data = coordinate_intersection,  
               aes(geometry = geometry, col = tpr_cut, shape = settlement_type_new), 
               stat= "sf_coordinates", size = 4) +
    scale_color_manual(values = c("[0,2]" = class_colors[1], "(2,4]" = class_colors[2],
                                  "(4,8]" = class_colors[3] ,"(8,12]" = class_colors[4], 
                                  "(12,20]" = class_colors[5], "(20,50]" = class_colors[6]))+
    # scale_color_gradient(low = "yellow", high = "red", na.value = NA) +
    scale_shape_manual(values = c(Formal = 15, Informal = 16 , Slum = 17))+
    guides(size = FALSE)+
    map_theme()+ 
    ylab("")+
    xlab("")+
    labs(title= paste(wards_interest[index]), 
         color = "TPR", 
         shape = "settlement type")+
    coord_sf()
  
  ggsave(file.path(results, metropolis_name, paste0(wards ,"_tpr_map.pdf")), 
         dpi = 400, width = 15, height = 10)
  
}




greater_than_twenty <- newdata %>% 
  filter(tpr > 15) %>% 
  mutate(tpr_cut = cut(tpr, breaks = c(15,20, 25, 30, 35, 40, 45, 50),
                       include.lowest = TRUE, include.highest = TRUE, right = TRUE )) %>% 
  group_by(tpr_cut) %>% 
  summarise(value = n())

# %>% 
#   inner_join(Ibadan_data_malaria_data, by = "ea_numbers_new") 

ggplot(data = greater_than_twenty) +
  geom_bar(aes(x = tpr_cut, y = value), stat = "identity") +
  labs(x = "test positivity rate",
       y = "frequency",
       fill = "") +
  theme_bw(base_size = 12, base_family = "")


ggsave(file.path(results, metropolis_name, paste0(wards ,"tpr_over_15.pdf")), 
       dpi = 400, width = 15, height = 10)




finally_than_twenty <- newdata %>% 
  filter(tpr > 15) %>%
  inner_join(Ibadan_data_malaria_data, by = "ea_numbers_new") %>% 
  dplyr::select(serial_number, longitude.x, latitude.x, Ward.x, ea_numbers_new, tpr) %>% 
  distinct()

write.csv(finally_than_twenty, file.path(cleaned_data_path, metropolis_name,"EA_weight_adjusted_tpr_greater_than15.csv"))

