
Kano_data_malaria_screening <- read_dta(file.path(dropbox, "KN Wet season household malaria screening.dta"))
Kano_data_hh_individuals <- read_dta(file.path(dropbox, "KN wet season household list wt xteristics.dta"))


names(Kano_data_malaria_screening) <-  c("serial_number", "repeat_instrument", "repeat_instance",
                                         "request_consent",  "line_number02", "consent_rdt", "rdt_test_result" ,   
                                         "dried_blood_sample" ,"dbs_code" ,"interviwer_name"  ,"complete00")



names(Kano_data_hh_individuals) <-c("serial_number", "repeat_instrument", "repeat_instance",
                                    "line_number01", "household_residents", "relationship_head_household",
                                    "gender", "age", "dob", "mother_present", "marital_status", "rdt_eligibility", 
                                    "complete02", "lga", "ward", "settlement_type","community_name", "enumaration_area", 
                                    "hh_number", "longitude", "latidude", "name_household_head", "number_duplicated")



Kano_data_hh_individuals_cleaned <- Kano_data_hh_individuals %>% 
  # clean out the weird serial number 
  mutate(serial_number = ifelse(serial_number == "Muhammad", "00001",
                                ifelse(serial_number == "Household", "00002",
                                       ifelse(serial_number == "HASSANA AHMAD", "00003",serial_number))),
         unique_id = paste0(serial_number, "_", line_number01))



malaria_individual_answers_duplicates <- Kano_data_hh_individuals_cleaned %>% 
  # duplicates in Kano_data_hh_individuals_cleaned talk to eniola about this 
  group_by(serial_number, line_number01, unique_id) %>%
  filter(n() > 1) %>%
  ungroup() 
  


Kano_data_malaria_screening_cleaned <- Kano_data_malaria_screening %>% 
  # clean out the weird serial number 
  mutate(serial_number = ifelse(serial_number == "Muhammad", "00001",
                                ifelse(serial_number == "Household", "00002",
                                       ifelse(serial_number == "HASSANA AHMAD", "00003",serial_number))),
         unique_id = paste0(serial_number, "_", line_number02))


malaria_malaria_screen_duplicates <- Kano_data_malaria_screening_cleaned %>% 
  # duplicates in Kano_data_hh_individuals_cleaned talk to eniola about this 
  group_by(serial_number, line_number02, dbs_code, unique_id) %>%
  filter(n() > 1) %>%
  ungroup() 

