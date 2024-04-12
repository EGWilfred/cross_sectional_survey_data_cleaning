rm(list=ls())

metropolis_name <- "Ibadan"

source("load_paths.R")

names_data <- read.csv(file.path(cleaned_data_path, metropolis_name, "EA_weight_adjusted_tpr_old.csv")) #ibadan_malaria_individual_hh_individual_information.csv

correction <- read.csv(file.path(cleaned_data_path, metropolis_name,"ib_hh_sampled_ea_set.csv"))

# 
# not_ordered <- correction %>% 
#   filter(settlement == ""| settlement == "don't know")


corrected_data <- correction %>% 
  mutate(enumeration_area = toupper(enumeration_area), 
         enumeration_area = ifelse(ward == "Olopomewa", 
                                   sub("OLOPOMEWA", "OLOGUNERU", enumeration_area),
                                   enumeration_area), 
         cluster_number = sprintf("%03d", cluster_number), 
         new_ea_number = paste0(enumeration_area, "/", cluster_number), 
         e_a = new_ea_number)%>% 
  tidyr::separate(e_a, into = c("Ward", "code", "cluster_numbers"), sep = "[_/]", remove = TRUE)


names_data <- names_data %>% 
  mutate(e_a = ea_number) %>% 
  tidyr::separate(e_a, into = c("Ward", "code", "cluster_numbers"), sep = "[_/]", remove = TRUE)



combined_data <- merge(corrected_data, names_data, 
                       by = c("Ward" = "Ward", "code" = "code", 
                              "cluster_numbers" = "cluster_numbers"), 
                       all = TRUE)



combined_data_mod <-  combined_data %>% 
  #unidentified EA names 
  filter(is.na(X)) %>% 
  select(Ward, code) %>% 
  mutate(eacode = paste0(Ward, "_", code)) %>% 
  group_by(eacode) %>% 
  summarise(num = n()) %>% 
  mutate()



combined_data_mod_cor <-  combined_data %>% 
  #correct EA names 
  filter(!is.na(X)) %>% 
  select(Ward, code, cluster_numbers) %>% 
  mutate(eacode = paste0(Ward, "_", code)) %>% 
  group_by(eacode, cluster_numbers) %>% 
  summarise(nums = n())



combined_data_cor <- merge(combined_data_mod, 
  # merging these 2 ensures part of the unidentified EAS are accounted for using the eacode
                       combined_data_mod_cor, 
                       by = c("eacode" = "eacode"), 
                       all = TRUE)


nonexistant <- combined_data_cor %>% 
  dplyr::filter(is.na(cluster_numbers))
  

existant <- combined_data_cor %>% 
  dplyr::filter(!is.na(cluster_numbers))





# putting all together 
corrected_eas = read.csv(file.path(cleaned_data_path, metropolis_name,"EA_names_nonexistant.csv")) 
# %>% 
#   mutate(cluster_number = sprintf("%03d", cluster_number))

names(corrected_eas) <- c("old_eanames", "total", 
                          "cluster_numberss", "totals", 
                          "eacode_new", "cluster_number")

names(existant) <- c("old_eanames", "num", "cluster_numbers", "nums"  )

correctedEAs <- merge(corrected_eas, 
# merging these 2 ensures part of the unidentified EAS are accounted for using the eacode
                      existant, 
                      by = c("old_eanames" = "old_eanames"), all = TRUE) %>% 
  mutate(eacode_new = ifelse(is.na(eacode_new), old_eanames, eacode_new), 
         cluster_number = ifelse(is.na(cluster_number), cluster_numbers, cluster_number), 
         cluster_number_new = sprintf("%03d", as.numeric(cluster_number))) %>% 
  select(oldeacode = old_eanames, eacode_new,  cluster_number_new) %>% 
  mutate(ea_numbers_new = paste0(eacode_new, "/",  cluster_number_new))
  




newdata = combined_data %>% 
  mutate(oldeacode  = enumeration_area, 
         new_ea_number = ifelse(is.na(new_ea_number), 
                            paste0(Ward, "_",code, "/", cluster_numbers),
                            new_ea_number), 
         ea_number = ifelse(is.na(ea_number), new_ea_number, ea_number))


final_combined_data_cor <- merge(correctedEAs, 
                                 newdata, 
                           by = c("oldeacode" = "oldeacode"), 
                           all = TRUE) 



subset_data <- final_combined_data_cor %>%
  mutate(settlement_type_new = str_to_title(settlement), 
         settlement_type_new = case_when( eacode_new %in% c("OLOGUNERU_004", "OLOGUNERU_006", "OLOGUNERU_016",
                        "OLOGUNERU_051", "OLOGUNERU_019", "OLOGUNERU_044") ~ "Informal",
                        eacode_new == "OLOGUNERU_010" ~ "Formal", TRUE ~ settlement_type_new ))


# oldeacode




old_data <- read.csv(file.path(cleaned_data_path,metropolis_name,"ibadan_malaria_weighted_information_v00.csv")) %>% 
  mutate(oldeacode = paste0(Ward, "_",sprintf("%03d", code))) 

oldcodes <- old_data %>% 
  mutate(old_ea_number = paste0(oldeacode, "/",sprintf("%03d", cluster_number))) %>% 
  select(oldeacode, old_ea_number) %>% 
  distinct() %>% 
  inner_join(subset_data, by = "oldeacode") 


corrected_eas <- oldcodes %>% 
  select(oldeacode, old_ea_number, 
         ea_numbers_new, settlement_type_new) %>% 
  mutate(settlement_type_new = ifelse(is.na(settlement_type_new), settlement_type_new[1],
                                      settlement_type_new)) %>% 
  distinct(old_ea_number, ea_numbers_new ,settlement_type_new) 

# %>% 
#   mutate(filtered = ifelse(old_ea_number == ea_numbers_new, "discard", "keep")) %>% 
#   filter(filtered == "keep")

  
ea_numbers_data <- unique(names_data$ea_number)

mislabelled = c("BASHORUN_060/110",  "BASHORUN_060/010",  "OLOGUNERU_011/028", "OLOGUNERU_008/022",
                "OLOGUNERU_006/010","AGUGU_015/005", "AGUGU_026/013", "AGUGU_024/038", "AGUGU_045/003",
                "AGUGU_026/005", "CHALLENGE_039/019", "CHALLENGE_025/015", "CHALLENGE_007/005",
                "CHALLENGE_007/016")

correct_mislabelled = c("BASHORUN_060/011",  "BASHORUN_060/011",  "OLOGUNERU_028/023", "OLOGUNERU_022/005",
                "OLOGUNERU_006/010","AGUGU_015/004", "AGUGU_026/003", "AGUGU_024/037", "AGUGU_045/031",
                "AGUGU_026/003", "CHALLENGE_039/007", "CHALLENGE_015/030", "CHALLENGE_007/017",
                "CHALLENGE_007/017")

replacement_vector <- setNames(correct_mislabelled, mislabelled)

# Replace ea_number in old_data using the replacement vector
new_data <- old_data %>%
  mutate(ea_number = ifelse(ea_number %in% names(replacement_vector),
                            replacement_vector[ea_number], ea_number)) %>% 
  rbind(old_data)



corrected_data <- inner_join(old_data, corrected_eas, 
                             by = c( "ea_number" = "old_ea_number"), relationship = "many-to-many") %>% 
  # dplyr::select(cluster_number, ea_number, ea_numbers_new, settlement_type_new) %>% 
  distinct() %>% 
  # mutate(ea_numbers_new = ifelse(ea_number == "OLOGUNERU_023/028", "OLOGUNERU_028/023", ea_numbers_new )) %>% 
  filter(ea_numbers_new != "AGUGU_004/034", 
         ea_numbers_new != "AGUGU_032/021",
         ea_numbers_new != "AGUGU_035/029", 
         ea_numbers_new != "BASHORUN_021/035",
         ea_numbers_new != "OLOGUNERU_023/001",
         ea_numbers_new != "OLOGUNERU_023/037", 
         ea_number %in% ea_numbers_data)



data_not_above <- new_data %>% 
  filter(!ea_number  %in% ea_numbers_data)



# write.csv(corrected_data, file.path(cleaned_data_path, metropolis_name,
#                                                  "corrected_eas_ibadan_malaria_data01.csv"))


############################################################################################################
# Analysis 
############################################################################################################

old_data_codes <- read.csv(file.path(cleaned_data_path,metropolis_name,"ibadan_malaria_weighted_information_v00.csv")) %>% 
  mutate(oldeacode = paste0(Ward, "_",sprintf("%03d", code))) %>% 
  transmute(ea_number, oldeacode, cluster_number) %>% 
  merge(correctedEAs, by = c("oldeacode" = "oldeacode"), all = TRUE) %>% 
  merge(corrected_eas, by = c("ea_numbers_new" = "ea_numbers_new"), all = TRUE) %>% 
  select(oldeacode, cluster_number, ea_number, eacode_new,
         cluster_number_new, ea_numbers_new, settlement_type_new) %>% 
  distinct()


# write.csv(old_data_codes, file.path(cleaned_data_path, metropolis_name,
#                                     "eas_corrected_00.csv"))

#############################################################################################################################
# Merging the correct datasets 
#############################################################################################################################


new_codes <- read.csv(file.path(cleaned_data_path, metropolis_name, "eas_corrected_00.csv")) %>% 
  mutate(cluster_number = as.numeric(cluster_number), 
         cluster_number = ifelse(!is.na(cluster_number), sprintf("%03d", cluster_number), cluster_number))

names(new_codes) <- c("oldeacode","ea_number_old", "cluster_number_old", 
                      "eacode_new","cluster_number_new","ea_numbers_new", 
                      "settlement_type_new")


Ibadan_data <- read.csv(file.path(cleaned_data_path, metropolis_name, "ibadan_malaria_individual_information.csv")) 

malaria_data_section <- read.csv(file.path(cleaned_data_path, metropolis_name,"ibadan_malaria_individual_hh_individual_information.csv"))


Ibadan_weight_data <- read.csv(file.path(cleaned_data_path, metropolis_name, "all_selected_hh_weights.csv")) %>% 
  dplyr::select(ward = Ward, enumeration_area = Enumeration_Area, 
                longitude = X_Enter_GPS_Location_longitude,
                latitude = X_Enter_GPS_Location_latitude, 
                prob_selected_ward,
                prob_selected_eas_settlement, 
                prob_selected_hh_structure) %>% 
  mutate(ward_weight = 1/prob_selected_ward, 
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
  mutate(e_a = enumaration_area) %>% 
  tidyr::separate(e_a, into = c("Ward", "code", "cluster_number"), sep = "[_/]", remove = TRUE) %>% 
  mutate(Ward = case_when(Ward %in% c("", "15", "24", "EA", "AKMA") ~ ward, TRUE ~ Ward ),
         Ward = case_when(Ward %in% c("Challenge", "CHALLENGE-",  "CHALLENGE ", "CHALLENGE AREA") ~ "CHALLENGE", TRUE ~ Ward ), 
         Ward = case_when(Ward %in% c("OLOGUNER", "OLOGUNERU ", "OLOGUNERU-23", "OLOGUNERU28", "Olopomewa") ~ "OLOGUNERU", TRUE ~ Ward ),
         Ward = case_when(Ward %in% c("Bashorun", " BASHORUN", "BASHORUN", "BAAHORUN", "BASORUN", "BASHORUN ") ~ "BASHORUN", TRUE ~ Ward),
         Ward = case_when(Ward %in% c("Agugu", "AGUGU ", " AGUGU") ~ "AGUGU", TRUE ~ Ward),
         code = as.numeric(code), 
         code = ifelse(!is.na(code), sprintf("%03d", code), code),
         cluster_number = ifelse(cluster_number == "AKINGBOLA", "22", cluster_number), 
         cluster_number = as.numeric(cluster_number), 
         cluster_number = ifelse(!is.na(cluster_number), sprintf("%03d", cluster_number), cluster_number), 
         enumeration_area_number = paste0(Ward, "_", code, "/", cluster_number))  

#locating duplicates

added_totals <- add_totals %>% 
  inner_join(new_codes, by = c("enumeration_area_number" = "ea_number_old",
                               "cluster_number" = "cluster_number_old")) %>% 
  mutate(agebin = cut(age, c(0,5,10,17,30, 122), include.lowest = T)) 

duplicates_summary  <- added_totals%>% 
  # take the only first row 
  group_by(unique_id) %>% 
  summarise(count = n()) %>% 
  filter(count > 1)

duplicated <- added_totals %>% 
  filter(unique_id %in% duplicates_summary$unique_id) 

selected_dulicates <- duplicated %>% 
  group_by(unique_id, agebin) %>% 
  mutate(num = 1:n()) %>% 
  filter(num == 1) %>% 
  select(-num)
  

unduplicated <- added_totals %>% 
  filter(!unique_id %in% duplicates_summary$unique_id) %>% 
  rbind(selected_dulicates) %>% 
  group_by(Ward) %>% 
  mutate(ward_total = n()) %>% 
  ungroup() %>% 
  group_by(Ward, ea_numbers_new) %>% 
  mutate(ea_total = n()) %>%
  ungroup() %>% 
  group_by(Ward, ea_numbers_new, serial_number) %>% 
  mutate(hh_total = n()) %>%
  ungroup() %>% 
  group_by(Ward, ea_numbers_new, serial_number, agebin) %>%
  mutate(age_total = n()) %>% 
  distinct()


# write.csv(add_totals, file.path(cleaned_data_path, metropolis_name, "ibadan_malaria_individual_information_totals_00.csv"), row.names = F)

add_totals <- unduplicated %>% 
  tidyr::drop_na(longitude,latitude) 



add_totals_sf <- sf::st_as_sf(add_totals, coords = c("longitude", "latitude"), crs = 4326)
Ibadan_weight_sf <- sf::st_as_sf(Ibadan_weight_data, coords = c("longitude", "latitude"), crs = 4326)



merged_dataset <- sf::st_join(add_totals_sf, Ibadan_weight_sf)



missing_weights <- merged_dataset %>% 
  filter(is.na(hhs_weights))


modified_merged_dataset <- merged_dataset %>% 
  mutate(ea_numbers_new = ea_numbers_new) %>% 
  group_by(ward.x) %>%
  mutate(ward_weight = ifelse(is.na(ward_weight)==T, mean(ward_weight, na.rm = T), ward_weight)) %>% 
  ungroup() %>% 
  group_by(ward.x,  settlement_type_new) %>% 
  mutate(ea_settlement_weight = ifelse(is.na(ea_settlement_weight)==T, 
                                       mean(ea_settlement_weight, na.rm = T), 
                                       ea_settlement_weight)) %>% 
  ungroup() %>% 
  group_by(ward.x, ea_numbers_new ,settlement_type_new) %>% 
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
modified_merged_dataset_mod <- rbind(na_hhweight, non_na_hhweight) %>% 
  group_by(ward.x, settlement_type_new, ea_numbers_new, hh_number, agebin) %>% 
  mutate(ind_total = n(),
         prob_ind_hh = 1/ind_total, 
         ind_weights_hh = 1/prob_ind_hh, 
         overall_hh_weight  = ind_weights_hh * ward_weight *
           ea_settlement_weight * hhs_weights) %>% 
  ungroup() 




coords <- sf::st_coordinates(modified_merged_dataset_mod)


modified_merged_dataset_mod$longitude <- coords[, 'X']
modified_merged_dataset_mod$latitude <- coords[, 'Y']

EAS_new <- c("BASHORUN_016/027",  "BASHORUN_112/036",  "BASHORUN_120/004",  "CHALLENGE_037/004", "OLOGUNERU_002/031",
             "OLOGUNERU_023/017", "OLOGUNERU_030/007", "AGUGU_033/009",     "AGUGU_059/030" ,    "BASHORUN_016/027",
             "BASHORUN_112/036",  "BASHORUN_120/004",  "CHALLENGE_037/004", "OLOGUNERU_002/031", "OLOGUNERU_023/017",
             "OLOGUNERU_030/007", "AGUGU_033/009",    "AGUGU_059/030")

st_corrections <- c("Formal",  "Formal",  "Formal",  "Formal", "Formal",
                    "Formal", "Formal", "Slum",     "Slum" ,    "Formal",
                    "Formal",  "Formal",  "Formal", "Formal", "Formal",
                    "Formal", "Slum",    "Slum")

replacement_vector <- setNames(st_corrections, EAS_new)



replacement_vector <- setNames(st_corrections, EAS_new)

# Replace settlement_type based on ea_numbers_new matching EAS_new
modified_merged_dataset_mod <- modified_merged_dataset_mod %>%
  mutate(settlement_type_new = ifelse(ea_numbers_new %in% names(replacement_vector), 
                                      replacement_vector[ea_numbers_new], settlement_type_new))

  
  

# write.csv(modified_merged_dataset_mod, file.path(cleaned_data_path, metropolis_name,"ibadan_malaria_weighted_information_v00.csv")) 

# coordinates <- modified_merged_dataset_mod %>% 
#   select(Ward, longitude, latitude) %>% 
#   distinct()
# 
# write.csv(coordinates, file.path(cleaned_data_path, metropolis_name,"coordinates.csv")) 


######################################################################################################################################
# run code from here for analysis
######################################################################################################################################

# modified_merged_dataset_mod <- read.csv(file.path(cleaned_data_path,metropolis_name,"corrected_eas_ibadan_malaria_data00.csv")) 


malaria_data <- inner_join(modified_merged_dataset_mod,
                           malaria_data_section, 
                           by = "unique_id")


malaria_cleaned <- malaria_data %>%
  select(-matches("\\.y$")) %>% 
  select(-c(ward.x, repeat_instrument.x) ) %>% 
  dplyr::rename_with(~ sub("\\.x$", "", .x), ends_with(".x"))


