rm(list=ls())

metropolis_name <- "Ibadan"

source("load_paths.R")

Ibadan_data <- read.csv(file.path(cleaned_data_path, metropolis_name, "ibadan_malaria_individual_information.csv")) #ibadan_malaria_individual_hh_individual_information.csv

malaria_data_section <- read.csv(file.path(cleaned_data_path, metropolis_name,"ibadan_malaria_individual_hh_individual_information.csv"))


# all_selected_hh_weights
Ibadan_weight_data <- read.csv(file.path(cleaned_data_path, metropolis_name, "all_selected_hh_weights.csv")) %>% 
  dplyr::select(ward = Ward, enumeration_area = Enumeration_Area, 
                longitude = X_Enter_GPS_Location_longitude,
               latitude = X_Enter_GPS_Location_latitude, 
               prob_selected_ward_srs =  prob_selected_ward,
               prob_selected_eas_settlement, 
               prob_selected_hh_structure) %>% 
  mutate( prob_selected_ward = ifelse(ward == "Agugu", 0.5),
         ward_weight = 1/prob_selected_ward, 
         ea_settlement_weight = 1/prob_selected_eas_settlement, 
         hhs_weights = 1/prob_selected_hh_structure)


add_totals <- Ibadan_data %>% 
  # mutate() %>% 
  mutate(ward = ifelse(ward == "Basorun", "Bashorun", ward),
         settlement_type = ifelse(settlement_type == "Formal" & ward == "Agugu", 
                                  "Informal", settlement_type), 
         ward = ifelse(ward == "Challenge" & grepl("^BA", enumaration_area)| grepl("^Ba", enumaration_area) | grepl("^ BA", enumaration_area), "Bashorun",
                       ifelse(ward == "Bashorun" & grepl("^CH", enumaration_area)|grepl("^ CH", enumaration_area), "Challenge", 
                              ifelse(ward == "Bashorun" & grepl("^AG", enumaration_area)|grepl("^ AG", enumaration_area), "Agugu", 
                                     ifelse(ward == "", "Challenge", ward)))), 
         overall_total = n()) %>% 
  group_by(ward) %>% 
  mutate(ward_total = n()) %>% 
  ungroup() %>% 
  group_by(ward, enumaration_area) %>% 
  mutate(ea_total = n()) %>%
  ungroup() %>% 
  group_by(ward, enumaration_area, serial_number) %>% 
  mutate(hh_total = n()) %>%
  ungroup() %>% 
  mutate(agebin = cut(age, c(0,5,10,17,30, 122), include.lowest = T)) %>% 
  group_by(ward, enumaration_area, serial_number, agebin) %>%
  mutate(age_total = n())


# write.csv(add_totals, file.path(cleaned_data_path, metropolis_name, "ibadan_malaria_individual_information_totals.csv"), row.names = F)


add_totals <- add_totals %>% 
  tidyr::drop_na(longitude,latitude) 





add_totals_sf <- sf::st_as_sf(add_totals, coords = c("longitude", "latitude"), crs = 4326)
Ibadan_weight_sf <- sf::st_as_sf(Ibadan_weight_data, coords = c("longitude", "latitude"), crs = 4326)



merged_dataset <- sf::st_join(add_totals_sf, Ibadan_weight_sf)



missing_weights <- merged_dataset %>% 
  filter(is.na(hhs_weights))


modified_merged_dataset <- merged_dataset %>% 
  mutate(enumeration_area = enumaration_area) %>% 
  group_by(ward.x) %>%
  mutate(ward_weight = ifelse(is.na(ward_weight)==T, mean(ward_weight, na.rm = T), ward_weight)) %>% 
  ungroup() %>% 
  group_by(ward.x,  settlement_type) %>% 
  mutate(ea_settlement_weight = ifelse(is.na(ea_settlement_weight)==T, 
                                       mean(ea_settlement_weight, na.rm = T), 
                                       ea_settlement_weight)) %>% 
  ungroup() %>% 
  group_by(ward.x, enumeration_area ,settlement_type) %>% 
  mutate(hhs_weights = ifelse(is.na(hhs_weights)==T, 
                              mean(hhs_weights, na.rm = T), 
                              hhs_weights)) 



# Rows with NA hhweight
na_hhweight <- modified_merged_dataset[is.na(modified_merged_dataset$hhs_weights), ]

# Rows with non-NA hhweight
non_na_hhweight <- modified_merged_dataset[!is.na(modified_merged_dataset$hhs_weights), ]


# Find indices of nearest non-NA hhweight geometries
nearest_indices <- sf::st_nearest_feature(na_hhweight, non_na_hhweight)


# Assign hhweight from nearest non-NA neighbors
na_hhweight$hhs_weights <- non_na_hhweight$hhs_weights[nearest_indices]



# Combine the two subsets back into a single sf dataframe
modified_merged_dataset_updated <- rbind(na_hhweight, non_na_hhweight) %>% 
  group_by(ward.x, settlement_type, enumaration_area, hh_number, agebin) %>% 
  mutate(ind_total = n(),
         prob_ind_hh = 1/ind_total, 
         ind_weights_hh = 1/prob_ind_hh, 
         overall_hh_weight  = ind_weights_hh * ward_weight *
           ea_settlement_weight * hhs_weights) %>% 
  ungroup() 


modified_merged_dataset_mod <- modified_merged_dataset_updated %>% 
  mutate(e_a = enumaration_area) %>% 
  tidyr::separate(e_a, into = c("Ward", "code", "cluster_number"), sep = "[_/]", remove = TRUE) %>% 
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

write.csv(modified_merged_dataset_mod, file.path(cleaned_data_path, metropolis_name,"ibadan_malaria_weighted_information_v00.csv")) 

coordinates <- modified_merged_dataset_mod %>% 
  select(Ward, longitude, latitude) %>% 
  distinct()

write.csv(coordinates, file.path(cleaned_data_path, metropolis_name,"coordinates.csv")) 


######################################################################################################################################
# run code from here for analysis
######################################################################################################################################



modified_merged_dataset_mod <- read.csv(file.path(cleaned_data_path,metropolis_name,"corrected_eas_ibadan_malaria_data00.csv")) 


malaria_data <- inner_join(modified_merged_dataset_mod,
                           malaria_data_section, 
                           by = "unique_id")


Ibadan_data_malaria_data <- malaria_data %>% 
  select(serial_number = serial_number.x, unique_id, repeat_instrument = repeat_instrument.x, 
         repeat_instance = repeat_instance.x,request_consent,  
         household_residents = household_residents.x, 
         relatioship_head_household = relatioship_head_household.x,
         gender = gender.x, agebin , dob = dob.x, age = age.x,
         mother_present = mother_present.x, marital_status = marital_status.x,
         rdt_eligibility = rdt_eligibility.x, 
         ward = ward.x, settlement_type_new = settlement_type_new,
         community_name = community_name.x, #enumaration_area = enumaration_area.x, 
         hh_number = hh_number.x, hhs_weights, name_household_head = name_household_head.x,
         consent_rdt = consent_rdt, rdt_test_result = rdt_test_result,   
         dried_blood_sample = dried_blood_sample,
         dbs_code = dbs_code, overall_hh_weight = overall_hh_weight, ea_number, ea_numbers_new) %>% 
  group_by(ward, settlement_type_new,  hh_number, ea_numbers_new) %>% 
  mutate(members_tested_hh = n()) %>% 
  ungroup() %>% 
  group_by(ward, settlement_type_new, ea_numbers_new) %>% 
  mutate(members_tested_ea = n()) %>% 
  distinct() 




duplicates <- Ibadan_data_malaria_data %>% 
  group_by(unique_id) %>% 
  select(ward, settlement_type_new, ea_number, ea_numbers_new) %>% 
  summarise(count = n()) %>% 
  filter(count>1)


serial_nums <- duplicates$unique_id  


ind_dup <- Ibadan_data_malaria_data %>%
  filter(unique_id %in% serial_nums)
  


Ibadan_data_malaria_data <- Ibadan_data_malaria_data %>% 
  filter(!unique_id %in% serial_nums)
  
  

  weight_adjusted_tpr <- Ibadan_data_malaria_data %>%
    filter(settlement_type_new != "", rdt_test_result != "Undeterminate") %>% 
    # st_drop_geometry() %>%
    mutate(malaria_test = ifelse(rdt_test_result == "POSITIVE", 1, 0)) %>%
    group_by(settlement_type_new) %>% 
    summarise(positive = sum(malaria_test), 
              total = n(),
              negative = total - positive,
              tpr = round(sum(malaria_test * overall_hh_weight, na.rm = T) / sum(overall_hh_weight, na.rm = T) * 100, 3),
              compliment = 100 - tpr)
  

  
  
new_data <- weight_adjusted_tpr %>% 
  dplyr::select(settlement_type_new, positive, negative) %>% 
  reshape2::melt(id = c("settlement_type_new"))

names(new_data) <- c("settlement_type", "result", "value")

 

labels_new_data <- weight_adjusted_tpr %>% 
  dplyr::select(settlement_type_new, tpr, compliment) %>% 
  reshape2::melt(id = c("settlement_type_new")) %>% 
  mutate(variable = ifelse(variable == "tpr", "positive", "negative"))

names(labels_new_data) <- c("settlement_type", "result", "percentage")

plotting_data <- inner_join(new_data, labels_new_data) %>% 
  mutate(plot_position = cumsum(value) - ( value))


ggplot(data = plotting_data) +
  geom_bar(aes(x = settlement_type, y = value, fill = result), 
           stat = "identity", position = "stack") +
  geom_text(aes(x = settlement_type, y = value, label = paste(percentage, "(%)")),  
            color = "black",
            size = 3.5, size = 3.5, nudge_y = 10) +
  scale_fill_manual(values = c("negative" = "#FFE7E7", "positive" = "#944E63")) +
  labs(title = "Malaria test results by settlement type",
       x = "Settlement Type",
       y = "Number of people tested for malaria",
       fill = "malaria RDT result") +
  theme_bw(base_size = 12, base_family = "")

  

  
  # box plot 
  
  EA_weight_adjusted_tpr <- Ibadan_data_malaria_data %>%
    filter(settlement_type_new != "", rdt_test_result != "Undeterminate") %>% 
    # st_drop_geometry() %>%
    mutate(malaria_test = ifelse(rdt_test_result == "POSITIVE", 1, 0)) %>%
    group_by(settlement_type_new, ea_numbers_new, members_tested_ea ) %>% 
    summarise(positive = sum(malaria_test), 
              total = n(),
              negative = total - positive,
              tpr = round(sum(malaria_test * overall_hh_weight) / sum(overall_hh_weight) * 100, 3),
              compliment = 100 - tpr)
  
 
  # EA_weight_adjusted_tpr %>% 
  #   group_by(settlement_type) %>% 
  #   summarise(count = n())
  
# write.csv(EA_weight_adjusted_tpr, file.path(cleaned_data_path, metropolis_name,"EA_weight_adjusted_tpr.csv"), row.names = F)  
 

 # %>% 
  #   group_by(ward, age, settlement_type) %>% 
  #   summarise(total_ward = n())
  
  
  ggplot(EA_weight_adjusted_tpr, aes(x = settlement_type_new, y = tpr),  fill = settlement_type_new) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(aes(color = settlement_type_new, size = members_tested_ea), width = 0.08)+
    scale_color_manual(values=c("#FFE7E7",  "#F2A6A2", "#B47B84")) +
    labs(title = "Distribution of TPR in enumeration area",
         x = "Settlement Type",
         y = "Enumaration area TPR ", 
         color ="Settlement type", 
         size = "number tested per EA") +
    #theme_manuscript()+ 
    theme(legend.position = "none") +
    theme_bw(base_size = 12, base_family = "") 
  
  
  
  
  ggplot(EA_weight_adjusted_tpr, aes(x = settlement_type, y = tpr),  fill = settlement_type) +
    geom_boxplot(outlier.shape = NA) +
    geom_jitter(aes(color = settlement_type, size = members_tested_hh), width = 0.08)+
    scale_color_manual(values=c("#FFE7E7",  "#F2A6A2", "#B47B84")) +
    labs(title = "Distribution of TPR in enumeration area",
         x = "Settlement Type",
         y = "Enumaration area TPR ", 
         color ="Settlement type", 
         size = "number tested per EA") +
    #theme_manuscript()+ 
    theme(legend.position = "none") +
    theme_bw(base_size = 12, base_family = "") 
  
  ea_names <- sort(unique(EA_weight_adjusted_tpr$enumaration_area))
  
  write.csv(ea_names, file.path(cleaned_data_path, metropolis_name,"EA_names.csv"), row.names = F)  
  

  
  
 newdata <-  EA_weight_adjusted_tpr %>% 
    group_by(ea_number) %>% 
    summarise(total_tested = sum(members_tested_ea))
 