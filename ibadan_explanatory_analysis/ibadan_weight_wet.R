ib_we <- read_csv("/Users/Mac/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/Field data/Ibadan_data/Household Survey/UrbanMalariaHousehol-ExportedData_DATA_LABELS_2024-01-05_1921.csv")

clean <- read_csv("/Users/Mac/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/IB_wet coordinate, EAs and Settlement.csv")

join_ib_wet <- ib_we%>%
  left_join(clean, by = c(`Serial Number` = "sn"))

Ibadan_weight_data <- read.csv(file.path(cleaned_data_path, metropolis_name, "all_selected_hh_weights.csv")) %>% 
  dplyr::select(ward = Ward, enumeration_area = Enumeration_Area, 
                longitude = X_Enter_GPS_Location_longitude,
                latitude = X_Enter_GPS_Location_latitude, 
                prob_selected_ward_srs =  prob_selected_ward,
                prob_selected_eas_settlement, 
                prob_selected_hh_structure) %>% 
  mutate( prob_selected_ward = ifelse(ward == "Agugu", 0.5, prob_selected_ward_srs),
          ward_weight = 1/prob_selected_ward, 
          ea_settlement_weight = 1/prob_selected_eas_settlement, 
          hhs_weights = 1/prob_selected_hh_structure)



add_totals <- join_ib_wet %>%
  mutate(settlement.y = tolower(settlement.y))%>%
  mutate(Ward.y = ifelse(Ward.y == "basorun", "bashorun", Ward.y),
         settlement.y = ifelse(settlement.y == "formal" & Ward.y == "agugu", 
                                  "informal", settlement.y), 
         overall_total = n()) %>% 
  group_by(Ward.y) %>% 
  mutate(ward_total = n()) %>% 
  ungroup() %>% 
  group_by(Ward.y, enumeration_area) %>% 
  mutate(ea_total = n()) %>%
  ungroup() %>% 
  group_by(Ward.y, enumeration_area, `Serial Number`) %>% 
  mutate(hh_total = n()) %>%
  ungroup() %>% 
  mutate(agebin = cut(as.numeric(`Age: How old was   (NAME) as at last birthday?...29`), c(0,5,10,17,30, 122), include.lowest = T)) %>% 
  group_by(Ward.y, enumeration_area, `Serial Number`, agebin) %>%
  mutate(age_total = n())





add_totals <- add_totals %>% 
  tidyr::drop_na(Longitude,Latitude)





add_totals_sf <- sf::st_as_sf(add_totals, coords = c("Longitude", "Latitude"), crs = 4326)

Ibadan_weight_sf <- sf::st_as_sf(Ibadan_weight_data, coords = c("longitude", "latitude"), crs = 4326)

merged_dataset <- sf::st_join(add_totals_sf, Ibadan_weight_sf)

glimpse(merged_dataset)

missing_weights <- merged_dataset %>% 
  filter(is.na(hhs_weights))

view(merged_dataset)


modified_merged_dataset <- merged_dataset %>% 
  group_by(Ward.y) %>%
  mutate(ward_weight = ifelse(is.na(ward_weight)==T, mean(ward_weight, na.rm = T), ward_weight)) %>% 
  ungroup() %>% 
  group_by(Ward.y,  settlement.y) %>% 
  mutate(ea_settlement_weight = ifelse(is.na(ea_settlement_weight)==T, 
                                       mean(ea_settlement_weight, na.rm = T), 
                                       ea_settlement_weight)) %>% 
  ungroup() %>% 
  group_by(Ward.y, enumeration_area.x ,settlement.y) %>% 
  mutate(hhs_weights = ifelse(is.na(hhs_weights)==T, 
                              mean(hhs_weights, na.rm = T), 
                              hhs_weights))



# Rows with NA hhweight
na_hhweight <- modified_merged_dataset[is.na(modified_merged_dataset$hhs_weights), ]

# Rows with non-NA hhweight
non_na_hhweight <- modified_merged_dataset[!is.na(modified_merged_dataset$hhs_weights), ]

view(non_na_hhweight)

# Find indices of nearest non-NA hhweight geometries
nearest_indices <- sf::st_nearest_feature(na_hhweight, non_na_hhweight)


# Assign hhweight from nearest non-NA neighbors
na_hhweight$hhs_weights <- non_na_hhweight$hhs_weights[nearest_indices]



# Combine the two subsets back into a single sf dataframe
modified_merged_dataset_updated <- rbind(na_hhweight, non_na_hhweight) %>% 
  group_by(Ward.y, settlement.y, enumeration_area.x, `Household Number`, agebin) %>% 
  mutate(ind_total = n(),
         prob_ind_hh = 1/ind_total, 
         ind_weights_hh = 1/prob_ind_hh, 
         overall_hh_weight  = ind_weights_hh * ward_weight *
           ea_settlement_weight * hhs_weights) %>% 
  ungroup()


modified_merged_dataset_mod <- modified_merged_dataset_updated %>% 
  mutate(e_a = enumeration_area) %>% 
  tidyr::separate(e_a, into = c("Ward.y", "code", "cluster_number"), sep = "[_/]", remove = TRUE) %>% 
  mutate(Ward = case_when(Ward %in% c("", "15", "24", "EA", "AKMA") ~ ward.x, TRUE ~ Ward ),
         Ward = case_when(Ward %in% c("Challenge", "CHALLENGE-",  "CHALLENGE ", "CHALLENGE AREA") ~ "CHALLENGE", TRUE ~ Ward ), 
         Ward = case_when(Ward %in% c("OLOGUNER", "OLOGUNERU ", "OLOGUNERU-23", "OLOGUNERU28", "Olopomewa") ~ "OLOGUNERU", TRUE ~ Ward ),
         Ward = case_when(Ward %in% c("Bashorun", " BASHORUN", "BASHORUN", "BAAHORUN", "BASORUN", "BASHORUN ") ~ "BASHORUN", TRUE ~ Ward),
         Ward = case_when(Ward %in% c("Agugu", "AGUGU ", " AGUGU") ~ "AGUGU", TRUE ~ Ward),
         code = as.numeric(code), 
         code = ifelse(!is.na(code), sprintf("%03d", code), code),
         cluster_number = ifelse(cluster_number == "AKINGBOLA", "22", cluster_number), 
         cluster_number = as.numeric(cluster_number), 
         cluster_number = ifelse(!is.na(cluster_number), sprintf("%03d", cluster_number), cluster_number), 
         ea_number = paste0(Ward, "_", code, "/", cluster_number))

# %>% 
#   select(-X, everything())

coords <- sf::st_coordinates(modified_merged_dataset_mod)


modified_merged_dataset_mod$longitude <- coords[, 'X']
modified_merged_dataset_mod$latitude <- coords[, 'Y']

modified_merged_dataset_mod <- modified_merged_dataset_mod %>% 
  sf::st_drop_geometry()

write.csv(modified_merged_dataset_mod, file.path(cleaned_data_path, metropolis_name,"ibadan_malaria_weighted_information_v00_gift.csv"))


coordinates <- modified_merged_dataset_mod %>% 
  dplyr::select(Ward, longitude, latitude) %>% 
  distinct()

write.csv(coordinates, file.path(cleaned_data_path, metropolis_name,"coordinates_gift.csv"))

