source("load_paths.R")

# source("load_paths.R")


# ensure metropolis_name is Kano in the load paths script 
# rename the dataframe

names(Kano_data) <- gsub(" ", "", new_names$new_name)


Kano_data_modified <- Kano_data %>% 
  mutate(repeat_instrument = ifelse(line_number01 != "", "Household List", 
                                    ifelse(line_number02 != "", "Section 3 Malaria Screening", 
                                           ifelse(complete03 != "", "Visitors", 
                                                  ifelse( complete11 != "","Net Inspection",
                                                          ifelse(complete06 != "" , "Tfhi",
                                                                 ifelse(complete07 != "", "Vh", ""))))))) %>% 
  select(serial_number, repeat_instrument, everything())





hh_answers <- Kano_data_modified %>% 
  # household level information 
  group_by(serial_number) %>% 
  slice(1) %>% 
  ungroup %>% 
  select(where(~ any(!is.na(.)))) %>% 
  select(where(~ any(. != ""))) %>% 
  mutate(serial_number = ifelse(serial_number == "Muhammad", "00001",
                                ifelse(serial_number == "Household", "00002",
                                       ifelse(serial_number == "HASSANA AHMAD", "00003",serial_number))))


# write.csv(hh_answers, file.path(cleaned_data, metropolis_name,"Kano_household_level_information.csv"))

individual_answers <- Kano_data_modified %>% 
  # household level information 
  group_by(serial_number) %>% 
  slice(-1) %>% 
  ungroup %>% 
  select(where(~ any(!is.na(.)))) %>% 
  select(where(~ any(. != "")))


hh_individual_answers <- individual_answers %>% 
  # individual level data 
  filter(repeat_instrument == "Household List") %>% 
  # ungroup %>% 
  select(where(~ any(!is.na(.)))) %>% 
  select(where(~ any(. != ""))) 

# write.csv(hh_individual_answers, file.path(cleaned_data, metropolis_name,
#                                            "Kano_individual_level_information.csv"))

visitos_individual_answers <- individual_answers %>% 
  # individual level data 
  filter(repeat_instrument == "Visitors") %>% 
  # ungroup %>% 
  select(where(~ any(!is.na(.)))) %>% 
  select(where(~ any(. != ""))) 

# write.csv(visitos_individual_answers, file.path(cleaned_data,  metropolis_name,
#                                            "Kano_Visitors_level_information.csv"))

Tfhi_individual_answers <- individual_answers %>% 
  # traveller's  data 
  filter(repeat_instrument == "Tfhi") %>% 
  select(where(~ any(!is.na(.)))) %>% 
  select(where(~ any(. != ""))) 

# write.csv(Tfhi_individual_answers, file.path(cleaned_data,  metropolis_name,
#                                            "Kano_traveller_two_qtns_information.csv"))

Vh_individual_answers <- individual_answers %>% 
  # visitors related  data 
  filter(repeat_instrument == "Vh") %>% 
  select(where(~ any(!is.na(.)))) %>% 
  select(where(~ any(. != ""))) 

# write.csv(Vh_individual_answers, file.path(cleaned_data, metropolis_name,
#                                              "Kano_traveller_one_qtns_information.csv"))



malaria_individual_answers <- individual_answers %>% 
  # malaria related  data 
  filter(repeat_instrument == "Section 3 Malaria Screening") %>% 
  select(where(~ any(!is.na(.)))) %>% 
  select(where(~ any(. != ""))) 
# %>% 
#   mutate(unique_id = paste0(serial_number, "_", line_number02))


malaria_individual_answers_duplicates <- malaria_individual_answers %>% 
  # talk to eniola about this 
  group_by(serial_number, line_number02, dbs_code) %>%
  filter(n() > 1) %>%
  ungroup() 



malaria_individual_answers_noconsent <-malaria_individual_answers %>% 
  filter(str_detect(unique_id, "_$"), 
         request_consent == "No", 
         consent_rdt == "No") 


# write.csv(malaria_individual_answers_noconsent, file.path(cleaned_data,
#                                            "Kano_declined_malaria_test.csv"))


malaria_individual_consented <-malaria_individual_answers %>% 
  filter(!str_detect(unique_id, "_$"), request_consent == "Yes", 
         consent_rdt == "Yes") 


# write.csv(malaria_individual_consented, file.path(cleaned_data,
#                                              "Kano_consented_malaria_test.csv"))



malaria_individual_answers_duplicated <- malaria_individual_consented %>%
  # filter duplicates and clean them 
  group_by(unique_id) %>%
  filter(n() > 1) %>%
  ungroup() %>% 
  mutate(last_A = str_sub(line_number01, -1), 
         last_B = str_sub(dbs_code, -1), 
         unique_id = ifelse(last_A != last_B, str_sub(unique_id, 1, str_length(unique_id) - 1) %>% 
                              paste0(., last_B), unique_id )) %>%
  select(-last_A, -last_B) 


malaria_individual_unduplicated <- malaria_individual_consented %>%
  # filter distinct
  rbind(malaria_individual_answers_duplicated)%>% 
  distinct(unique_id, .keep_all = TRUE)


modified_duplicates <- malaria_individual_answers_duplicated %>% 
  # talk to eniola about this 
  group_by(unique_id) %>%
  filter(n() > 1) %>%
  ungroup() 



# write.csv(malaria_individual_unduplicated, file.path(cleaned_data,
#                                            "Kano_consented_unduplicated_malaria_test.csv"))

####################################################################################################################################
# combining the individual datasets
####################################################################################################################################

aliging_hh_indv_data <- inner_join(hh_answers, hh_individual_answers) %>% 
  mutate(unique_id = paste0(serial_number, "_", line_number))

#identify duplicates and correct 

aliging_hh_indv_data_duplicated <- aliging_hh_indv_data %>%
  group_by(unique_id) %>%
  filter(n() > 1) %>%
  ungroup()


# write.csv(aliging_hh_indv_data, file.path(cleaned_data,
#                                            "Kano_malaria_individual_information.csv"))





aliging_hh_indv_malaria_data <- inner_join(aliging_hh_indv_data, 
                                           malaria_individual_unduplicated, 
                                           by = c("unique_id", 
                                                  "serial_number",
                                                  "repeat_instance"))


# write.csv(aliging_hh_indv_malaria_data, file.path(cleaned_data,
#                                            "Kano_malaria_individual_hh_individual_information.csv"))

#############################################################################################################################################
#
#############################################################################################################################################





