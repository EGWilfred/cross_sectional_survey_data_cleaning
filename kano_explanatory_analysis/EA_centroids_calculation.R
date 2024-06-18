

combined_data_corrected_eas <- read.csv(file.path(cleaned_data, metropolis_name,"combined_data_corrected_eas.csv"))





corrected_st01 <- read.csv("C:/Users/lml6626/Downloads/different_settlement_type.csv") %>% 
  dplyr::select(ea_names, settlement_type_new = X.1 ) %>% 
  mutate(settlement_type_new = ifelse(ea_names == "ZANGO/1450", "Informal", settlement_type_new))


corrected_st00 <- combined_data_corrected_eas %>%
  # dplyr::select(longitude, latidude, ward, ea_names, settlement_type) %>%
  dplyr::select(ward, ea_names, settlement_type) %>%
  dplyr::distinct() %>%
  group_by(ea_names, ward) %>%
  summarise(count = n()) %>%
  filter(count != 1 ) %>% 
  inner_join(corrected_st01) %>% 
  dplyr::select(ea_names, ward ,  count, settlement_type = settlement_type_new) 



corrected_st_final <- combined_data_corrected_eas %>%
  # dplyr::select(longitude, latidude, ward, ea_names, settlement_type) %>%
  dplyr::select(ward, ea_names, settlement_type) %>%
  dplyr::distinct() %>%
  group_by(ea_names, ward, settlement_type) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(ea_names, ward) %>% 
  mutate(count = n()) %>% 
  filter(count == 1 ) %>% 
  #dplyr::select(ea_names, ward ,  count, settlement_type = settlement_type_new) %>% 
  mutate(settlement_type  = as.numeric(settlement_type), 
         settlement_type  = ifelse(settlement_type == 1, "Formal",
                                   ifelse(settlement_type == 2, "Informal",
                                          ifelse(settlement_type == 3, "Slum", settlement_type))))


new_data = rbind(as.data.frame(corrected_st_final), as.data.frame(corrected_st00)) %>% 
  dplyr::select(ea_names, ward, settlement_type_new = settlement_type)
  
names(new_data) <- c(ea_names, ward, settlement_type, count)

# write.csv(new_data, file.path(cleaned_data, metropolis_name,"final_ea_settlement_type.csv"))


centroids_kano <- inner_join(combined_data_corrected_eas, new_data)%>% 
  dplyr::select(serial_number, longitude, latitude = latidude, 
                ward, ea_names, settlement_type) %>% 
  mutate(longitude = as.numeric(longitude), 
         latitude = as.numeric(latitude))
  
  
  
  
split_data <- split(centroids_kano, centroids_kano$ea_names)

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
  
  
  centroids_data <- data.frame(settlement_type_new = data$settlement_type[1],
                               Ward = data$ward[1], 
                               ea_numbers_new = data$ea_names[1], 
                               longitude = centroid_lon, 
                               latitude = centroid_lat)
  
  centroids[[index]] <- centroids_data
}





final_centroid_data <- data.table::rbindlist(centroids) %>% 
  mutate()

write.csv(final_centroid_data, file.path(cleaned_data_path, metropolis_name, "kano_centoids_data.csv"))


 
new_data <- read.csv("C:/Users/lml6626/Downloads/Kano_centoids_data_completed_YJ.csv") %>% 
  mutate(ward = case_when(Ward == 1 ~ "Zango",
                          Ward == 2 ~ "Dorayi",
                          Ward == 3 ~ "Tudun Wazurchi",
                          Ward == 4 ~ "Fagge D2",
                          Ward == 5 ~ "Gobirawa",
                          Ward == 6 ~ "Giginyu"), 
         settlement_type = case_when(settlement_type_new  == 1 ~ "formal",
                          settlement_type_new == 2 ~ "informal",
                          settlement_type_new == 3 ~ "slum")) %>% 
  dplyr::select(longitude, latitude, ward, 
                ea_names = ea_numbers_new, 
                settlement_type)



write.csv(new_data, file.path(cleaned_data_path, metropolis_name, "kano_final_centroids_data.csv"))



# prepare Merlin's data 

centroids_ea <- read.csv(file.path(cleaned_data_path,  "/Kano/kano_centoids_data.csv")) %>% 
  dplyr::select(centroid_lon = longitude, centroid_lat = latitude, 
                settlement_type_new, Ward, ea_numbers_new)


all_hseholds <- centroids_kano %>% 
  inner_join(centroids_ea, by = c("ward" = "Ward",
                                  "settlement_type" = "settlement_type_new", 
                                  "ea_names" = "ea_numbers_new")) %>% 
  dplyr::select(longitude, 
                latitude, 
                hh_serial_number = serial_number, 
                ward, settlement_type,
                ea_names, 
                centroid_lon,
                centroid_lat)


write.csv(all_hseholds, file.path(cleaned_data_path, metropolis_name,"Kano_all_hseholds_cordinates.csv"))




