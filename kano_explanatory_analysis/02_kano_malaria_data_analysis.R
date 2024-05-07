rm(list=ls())

metropolis_name <- "Kano"

source("load_paths.R")


Kano_data_malaria_screening <- read_dta(file.path(dropbox, "KN Wet season household malaria screening.dta"))
Kano_data_hh_individuals <- read_dta(file.path(dropbox, "KN wet season household list wt xteristics.dta"))


names(Kano_data_malaria_screening) <-  c("serial_number", "repeat_instrument", "repeat_instance",
                                         # renamed to fit the data dictionary and match with the 
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



# malaria_individual_answers_duplicates <- Kano_data_hh_individuals_cleaned %>% 
#   # duplicates in Kano_data_hh_individuals_cleaned talk to eniola about this 
#   group_by(serial_number, line_number01, unique_id) %>%
#   filter(n() > 1) %>%
#   ungroup() 
# 
# write_dta(malaria_individual_answers_duplicates,
#           file.path(cleaned_data, metropolis_name,
#                     "duplicated_data",
#                     "kano_individuals_duplicates.csv"))
  


Kano_data_malaria_screening_cleaned <- Kano_data_malaria_screening %>% 
  # clean out the weird serial number 
  mutate(serial_number = ifelse(serial_number == "Muhammad", "00001",
                                ifelse(serial_number == "Household", "00002",
                                       ifelse(serial_number == "HASSANA AHMAD", "00003",serial_number))),
         unique_id = paste0(serial_number, "_", line_number02))


# malaria_malaria_screen_duplicates <- Kano_data_malaria_screening_cleaned %>% 
#   # duplicates in Kano_data_hh_individuals_cleaned talk to eniola about this 
#   group_by(serial_number, line_number02, dbs_code, unique_id) %>%
#   filter(n() > 1) %>%
#   ungroup()
# 
# write.csv(malaria_malaria_screen_duplicates,
#           file.path(cleaned_data, metropolis_name,
#                     "duplicated_data",
#                     "kano_malaria_malaria_duplicated.csv"))


kano_all_data <-  inner_join(Kano_data_hh_individuals_cleaned, 
                        Kano_data_malaria_screening_cleaned, 
                        by = join_by("serial_number",
                                     "repeat_instance", 
                                     "unique_id"))


# kano_all_data_duplicates <- kano_all_data %>%
#   # duplicates in Kano_data_hh_individuals_cleaned talk to eniola about this
#   group_by(serial_number, line_number02, dbs_code, unique_id) %>%
#   filter(n() > 1) %>%
#   ungroup()
# 
# write.csv(kano_all_data_duplicates,
#           file.path(cleaned_data, metropolis_name,
#                     "duplicated_data",
#                     "kano_alldata_duplicated.csv"))


##########################################################################################################
# ANALYSIS
##########################################################################################################

newdata <- kano_all_data %>% 
  mutate(agebin = cut(age, c(0,5,10,15,20,30,40,50, 60, 70, 122), include.lowest = T))

ggplot(newdata, aes(x = agebin, fill = as.factor(gender)))+
  geom_bar() +
  theme_minimal() +
  scale_fill_manual(values = c("1" = "#FFE7E7", "2" = "#944E63"), 
                        labels = c("males", "females"))+
  labs(title = "Kano age sex distribution", 
       x = "age group", y = "Frequency", fill = "gender")
  

newdata %>% 
  # filter() %>% 
  ggplot(aes(x = agebin, fill = as.factor(settlement_type)))+
  geom_bar() +
  theme_minimal() +
  scale_fill_manual(values = c("1" = "#FFE7E7", "2" = "#B47B84", "3" = "#944E63"), 
                    labels = c("formal", "informal","slums"))+
  labs(title = "Kano age distribution by settlement type", 
       x = "age group", y = "Frequency", fill = "settlement type")


newdata %>% 
  filter(!is.na(ward)) %>% 
  ggplot(aes(x = agebin, fill = as.factor(settlement_type)))+
  geom_bar() +
  facet_wrap(~ward,  labeller = labeller(ward = c("1" = "Zango", "2" = "Dorayi", "3" = "Tundun Wazurchi", 
                                                  "4" = "Fagge 2", "5" = "Gobirawa", "6" = "Others")))+
  theme_minimal() +
  scale_fill_manual(values = c("1" = "#FFE7E7", "2" = "#B47B84", "3" = "#944E63"), 
                    labels = c("formal", "informal","slums"))+
  labs(title = "Kano age distribution by settlement type", 
       x = "age group", y = "Frequency", fill = "settlement type")



newdata$ward <- factor(newdata$ward, levels = c("1", "2", "3", "4", "5", "6"))


newdata %>% 
  filter(!is.na(settlement_type),!is.na(rdt_test_result), 
         !is.na(ward), ward != "3") %>% 
  ggplot(aes(x = ward, fill = as.factor(rdt_test_result)))+
  geom_bar() +
  facet_wrap(~settlement_type,  labeller = labeller(settlement_type = c("1" = "formal", "2" = "informal",
                                                                        "3" = "slums")))+
  theme_minimal() +
  scale_fill_manual(values = c("1" = "#FFE7E7", "2" = "#B47B84", "3" = "#944E63"), 
                    labels = c("positive", "negative", "undeterminate"))+
  scale_x_discrete(labels = c("1"= "Zango", "2" = "Dorayi", # "3" = "Tundun Wazurchi", 
                              "4" = "Fagge 2", "5" = "Gobirawa", "6" = "Giginyu"))+
  labs(title = "Kano age distribution by settlement type", 
       x = "age group", y = "Frequency", fill = "test result")


# extract the covariates from Kano raster file 
# plot the data collection points on the respective shape files 
# create a geospatial model for the data area level and one that incoporates Krigging 
# fits a smooth surface over the data points 
