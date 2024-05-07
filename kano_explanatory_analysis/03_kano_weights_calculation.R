
###Household Listed
kano_hh_listed_00<- read_excel(file.path(dropbox, "Kano_with_new_ward_2023-12-01.xlsx"))
kano_hh_listed_01 <- read_excel(file.path(dropbox, "Kano_HH_Listing_2023-12-01 (1).xlsx"))


kano_hh_listed <- rbind(kano_hh_listed_00, kano_hh_listed_01)

# kano_hh_sampled <- read.csv(file.path(NuDir , "excel files", "Ib_sampled_list_final.csv"))



###Household Sampled
kano_hh_listed <- as.data.frame(kano_hh_listed)

names(kano_hh_listed) <- c("start", "end", "location", "latitude", "longitude", 
                           "altitude", "precision", "date_time", "city", "ward",
                           "enumeration_area", "ea_serial_number", "settlement", 
                           "lister_name", "phone_number", "structure_serial_number", 
                           "address", "residential_structure", "hh_serial_number", 
                           "visit_one", "notes_one", "save_one", "visit_two", 
                           "notes_two", "save_two", "visit_three", "hh_gender", "number_in_hh", 
                           "final_completion", "gratitude", "id", "uuid", "submission",
                           "validation_status", "notes_three", "status", "submitted",
                           "version",  "tags", "index" )


listed_households <- kano_hh_listed %>% 
  mutate(prob_selected_ward = case_when(ward == "Zango"~ 1,
                                        ward == "Dorayi"~ 1/2,
                                        ward == "Tudun Wazurchi"~ 1,
                                        ward == "Gobirawa"~ 1, 
                                        ward == "Giginyu"~ 1,
                                        ward == "Fagge D2"~ 1)) %>%
  group_by(ea_serial_number, hh_serial_number) %>%
  mutate(total_hh_listed_structure = n())


selected_household <- kano_hh_sampled %>% 
  mutate(
    prob_selected_eas_settlement = case_when(
      ward == "Zango" & settlement == "informal" ~ 1,
      ward == "Zango" & settlement == "slum" ~ 1,
      ward == "Zango" & settlement == "formal" ~ 1,
      
      ward == "Gobirawa" & settlement == "formal" ~ 3/14,
      ward == "Gobirawa" & settlement == "informal" ~ 37/228,
      ward == "Gobirawa" & settlement == "slum" ~ 0,
      
      ward == "Giginyu" & settlement == "formal" ~ 1,
      ward == "Giginyu" & settlement == "informal" ~ 1,
      ward == "Giginyu" & settlement == "slum" ~ 0,
      
      ward == "Fagge D2" & settlement == "formal" ~ 1,
      ward == "Fagge D2" & settlement == "informal" ~ 1,
      ward == "Fagge D2" & settlement == "slum" ~ 0,
      
      ward == "Dorayi" & settlement == "formal" ~ 1,
      ward == "Dorayi" & settlement == "informal" ~ 27/29,
      ward == "Dorayi" & settlement == "slum" ~ 0
      
      #TRUE ~ Value  # If none of the conditions match, keep the original 'Value'
    )
  ) %>% 
  group_by(enumeration_area, X_001_serial_number_of_structure) %>% 
  mutate(total_hh_selected_structure = n()) %>% 
  ungroup()




all_selected_hh <- inner_join(listed_households, 
                              selected_household, 
                              by = c("X_index")) %>% 
  mutate(prob_selected_hh_structure = total_hh_selected_structure/total_hh_listed_structure)



weights_data <- all_selected_hh %>% 
  dplyr::select(longitude = X_Enter_GPS_Location_longitude, 
                latitude = X_Enter_GPS_Location_latitude, 
                prob_selected_ward, prob_selected_eas_settlement,
                prob_selected_hh_structure)


write.csv(weights_data, file.path(NuDir, "weights_data.csv"))

write.csv(all_selected_hh, file.path(NuDir, "all_selected_hh.csv"))


ea_names <- unique(ib_hh_sampled$enumeration_area)

write.csv(ea_names, file.path(NuDir, "ea_names.csv"))


ib_hh_sampled_eas <- ib_hh_sampled %>% 
  dplyr::select(cluster_number, enumeration_area)

write.csv(ib_hh_sampled_eas, file.path(NuDir, "ib_hh_sampled_eas.csv"))