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
  mutate(line_number00 = repeat_instance, 
         unique_id = paste0(serial_number, "_", line_number00), 
         serial_number = ifelse(serial_number == "Muhammad", "00001",
      ifelse(serial_number == "Household", "00002",
             ifelse(serial_number == "HASSANA AHMAD", "00003",serial_number))))


alaria_malaria_screen_duplicates <- Kano_data_malaria_screening_cleaned %>% 
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

# write.csv(kano_all_data,
#           file.path(cleaned_data, metropolis_name,
#                   
#                     "kano_malaria_all_data.csv"))

  

# Figure out the weird EA name and correct them 

# weird_ea_name <- kano_all_data %>% 
#   dplyr::select(enumaration_area, settlement_type,lga, ward) %>% 
#   group_by( EA, settlement_type,lga, ward) %>% 
#   summarise(total = n()) %>%
#   mutate(manipulate_enumaration_area = enumaration_area,
#          manipulate_enumaration_area00 = enumaration_area,
#          manipulate_enumaration_area = str_replace_all(enumaration_area, "[ ,]", ""), 
#          manipulate_enumaration_area = str_replace_all(manipulate_enumaration_area, "[']", "")
#   ) %>%  #remove space and commas 
#   separate(col = manipulate_enumaration_area00, into = c("ea_name00", "cluster_number00"),
#            sep = "/", #|(?<=[a-zA-Z])(?=[0-9])|(?<=[0-9])(?=[a-zA-Z])"
#            extra = "merge") %>%
#   mutate(ea_name = toupper(str_extract(manipulate_enumaration_area, "[A-Za-z]+")),
#          cluster_number = str_extract(manipulate_enumaration_area, "[0-9]+"), 
#          ea_name  = ifelse(ea_name == "ZONGO"|ea_name == "ZAGO", "ZANGO", 
#                            ifelse(ea_name == "FORRESTREE", "FORESTRY", ea_name)), 
#          ea_name =  if_else(str_detect(ea_name, "^TR"), "TRIUMPH", ea_name)) %>% 
#   inner_join(corrected_ea_names, by = c("ea_name" = "oldeas_names")) %>% 
#   mutate(ea_names = paste0(new_eas_names, "/", cluster_number)) %>% 
#   filter(ward != 3, 
#          )


# write.csv(weird_ea_name, file.path(cleaned_data_path, metropolis_name, "weird_ea_name.csv"), row.names = F)
# 
# 
# old_eas_names <- c(NA, "AFORESTRY", "ALHAJIALI", "BABADAWALAYOUT", "BABANGWARI", "BADAWA", "BADAWALAYOUT",                    
#                    "BURHANA", "CHIKAL", "CHIKALA", "CHIKALAROAD", "CIKINGARI", "DARMANAWA", "DORAYI",
#                    "DORAYIBABBA", "DORAYIKARAMA", "DORAYIYAMADAWA", "DUKAWA", "DUNIYARYANGARUWA",                
#                    "EMEKA", "FAGGED", "FILIN", "FILINDURUMI", "FILINGIDI", "FILINIDI","FORESTER", 
#                    "FORESTRY", "FORRESTER", "FORSTRY", "G" ,"GDUKAWA", "GDUKAWAL" ,"GIDANBABANGORI", 
#                    "GIDANBABANGWARI", "GIGINYU" ,"GIGINYUB","GIGINYUC",  "GIGINYUNC","GOBARAWA","GOBERAWA" ,
#                    "GOBIRAWA", "GOBIRAWAA", "GOBIRAWAB", "GOBIRAWAKURNA" ,"GOBIRAWAMAIUNGUWASALISU" , 
#                    "HAJHAUWA","HAJIYAHAUWAMAI", "HAJIYAHAUWAMAISAKA","HAJIYAHAUWAMAISAKAROAD", "HAJIYAHAUWAMESAKA",
#                    "HAJIYAHAUWAMESAQA", "HAJIYAHAUWMESAKA", "HAURE",   "HAUSAWA", "HAUWAMAISAKA", "HOTORO",  "JAENJIGAWA",
#                    "JIGAWAJAEN", "KAFARMATADYEPITS", "KAMADA","KASUWARMATA","KASUWARMATA", "KAWO", "KAWOAREWA"  , "KAWOCIKI","KAWOCIKIMAIM", 
#                    "KAWOCIKIN", "KAWOCIKINGARI" ,"KAWOKUDU","KAWOMAIGARI" , "KAWONMAIGARI" ,"KAWONMEGARI", "KOFARMATA"  ,
#                    "KOFARMATADYEPITS", "KURNAA",  "LAYINALHAJIALI"  ,"LAYINALHAJIALINOCASE", "LAYINALHAJINOCASE", 
#                    "LAYINDABINAI", "LAYINDANUJILE", "LAYINDANWANZAN", "LAYINHAJIYAHAUWAMESAKA", "LAYINMAKERA",
#                    "LAYINMEUNGUWA", "LAYINTANKOMAIMAI", "LOKONMAKEA" ,"LOKONMAKERA","M",  "MAILIKAFA","MAILIKKAFA",
#                    "MAKERA","MALUFAI", "MLIKAFA", "MMSH",  "MURTALA", "MURTALAMUHAMMAD" , "MURTALAMUHAMMADHOSPITAL" ,
#                    "MURTALAMUHAMMADSOCIALISTHOSPITAL",  "MURTALAMUHAMMADSPECIALHOSPITAL",   "MURTALAMUHAMMADSPECIALISTHOSPITAL",
#                    "NASSARAWA", "NASSARAWAG"  , "NASSARAWAGRA" , "R",       "SALLARBABBA" , "SALLARI", "SHAGO",   "SHAGOTARA","SHAWUCI",
#                    "SHIGOTARA","T",       "TRIUMPH", "TUDUNBOJUWA","TUDUNBUJUWA","U",  "UNGUWARJAKADA","UNGUWARMATA" , "UNGUWARWAMBAI",                    
#                    "UNGUWAWABAI" , "WAPA",    "YAMADAWA", "YAMAWADA","YANALAWA","YANALEWA", "YANGANDA","YANGANDU","YANMUDUBI", 
#                    "ZANGO",   "ZANGOGURGAMA")
# 
# 
# all_eas_names <- c(NA, "FORESTRY", "ALHAJIALI", "BADAWALAYOUT", "BABANGWARI", "BADAWA", "BADAWALAYOUT",                    
#                    "BURHANA", "CHIKALA", "CHIKALA", "CHIKALA", "CIKINGARI", "DARMANAWA", "DORAYI",
#                    "DORAYIBABBA", "DORAYIKARAMA", "YAMADAWA", "DUKAWA", "DUNIYARYANGARUWA",                
#                    "EMEKA", "FAGGED", "FILINIDI", "FILINDURUMI", "FILINIDI", "FILINIDI","FORESTRY", 
#                    "FORESTRY", "FORESTRY", "FORESTRY", "G" ,"G", "G" ,"GIDANBABANGORI", 
#                    "GIDANBABANGWARI", "GIGINYU" ,"GIGINYUB","GIGINYUC",  "GIGINYUNC","GOBIRAWA","GOBIRAWA" ,
#                    "GOBIRAWA", "GOBIRAWAA", "GOBIRAWAB", "GOBIRAWAKURNA" ,"GOBIRAWAMAIUNGUWASALISU" , 
#                    "HAJIYAHAUWAMAI","HAJIYAHAUWAMAI", "HAJIYAHAUWAMAI","HAJIYAHAUWAMAI", "HAJIYAHAUWAMAI",
#                    "HAJIYAHAUWAMAI", "HAJIYAHAUWAMAI", "HAURE",   "HAUSAWA", "HAJIYAHAUWAMAI", "HOTORO",  "JAENJIGAWA",
#                    "JAENJIGAWA", "KOFARMATADYEPITS", "KAMADA","KASUWARMATA", "KASUWARMATA", "KAWO", "KAWO"  , "KAWO","KAWO", 
#                    "KAWO", "KAWO" ,"KAWO","KAWO" , "KAWO" ,"KAWO", "KOFARMATADYEPITS"  ,
#                    "KOFARMATADYEPITS", "KURNAA",  "LAYINALHAJIALI"  ,"LAYINALHAJIALI", "LAYINALHAJIALI", 
#                    "LAYINDABINAI", "LAYINDANUJILE", "LAYINDANWANZAN", "LAYINHAJIYAHAUWAMESAKA", "LAYINMAKERA",
#                    "LAYINMEUNGUWA", "LAYINTANKOMAIMAI", "LOKONMAKEA" ,"LOKONMAKERA","M",  "MAILIKAFA","MAILIKAFA",
#                    "MAKERA","MALUFAI", "MLIKAFA", "MMSH",  "MURTALA", "MURTALAMUHAMMAD" , "MURTALAMUHAMMAD" ,
#                    "MURTALAMUHAMMAD",  "MURTALAMUHAMMAD",   "MURTALAMUHAMMAD", "NASSARAWA", "NASSARAWAG"  , 
#                    "NASSARAWAGRA" , "R",       "SALLARBABBA" , "SALLARI", "SHAGOTARA",   "SHAGOTARA","SHAWUCI",
#                    "SHAGOTARA","T", "TRIUMPH", "TUDUNBUJUWA","TUDUNBUJUWA","U",  "UNGUWARJAKADA","UNGUWARMATA" , "UNGUWARWAMBAI",                    
#                    "UNGUWARWAMBAI" , "WAPA",    "YAMADAWA", "YAMAWADA","YANALAWA","YANALAWA", "YANGANDA","YANGANDA","YANMUDUBI", 
#                    "ZANGO",   "ZANGOGURGAMA")
# 
# 
# 
# 
# 
# corrected_ea_names = data.frame(oldeas_names = old_eas_names, 
#                                 new_eas_names = all_eas_names)



kano_hh_listed_01 <- read.csv("C:/Users/lml6626/Downloads/weird_ea_name_Yusuf_Draft (1).csv")
kano_hh_listed_02 <- read.csv(file.path(cleaned_data, metropolis_name, "weird_ea_name00.csv"))


all_kano_ea_listed <- inner_join(kano_hh_listed_02,
                                 kano_hh_listed_01) %>% 
  mutate(new_eas_names = ifelse(new_eas_names == "G", "GOBIRAWA", new_eas_names), 
         ea_names = paste0(new_eas_names, "/", cluster_number),
         ea_names = case_when(ea_names == "GIGINYUNC/29"~ "GIGINYUC/29",
                              ea_names == "HOTORO/29" ~ "GIGINYUC/29", 
                              ea_names == "FORESTRY/1492" ~ "FORESTRY/1392", 
                              ea_names == "DORAYIBABBA/1367" ~ "DORAYIBABBA/1376",
                              ea_names == "DUNIYARYANGARUWA/2298" ~ "GOBIRAWA/2298", 
                              ea_names == "CIKINGARI/2252"  ~ "GOBIRAWA/2252",
                              ea_names == "GOBIRAWA/2145"  ~ "GOBIRAWA/2154",
                              ea_names == "R/2252" ~ "GOBIRAWA/2252",
                              ea_names == "GOBIRAWA/1168" ~ "GOBIRAWA/2168",
                              ea_names == "KAMADA/1476" ~ "MMSH/1476",
                              ea_names == "KASUWARMATA/332" ~ "WAPA/332",
                              ea_names == "KASUWARMATA/366" ~ "UNGUWARMATA/336",
                              ea_names == "LOKONMAKEA/1484" ~ "LOKONMAKERA/1484",
                              ea_names == "ALHAJIALI/0318" ~ "ALHAJIALI/0318",
                              ea_names == "LAYINDANWANZAN/2440"~ "SHAGOTARA/2440",
                              ea_names == "SHAGOTARA/NA"  ~"SHAGOTARA/2440",
                              ea_names == "SHAGOTARA/440"  ~"SHAGOTARA/2440",
                              ea_names == "SHAGOTARA/102"~ "SHAGOTARA/2102",
                              ea_names == "SHAGOTARA/2409" ~ "SHAGOTARA/2492",
                              ea_names == "T/2058" ~ "GOBIRAWA/2058",
                              ea_names == "YANGANDA/2220" ~ "TUDUNBUJUWA/2220",
                              ea_names == "BADAWA/37" ~"BADAWALAYOUT/47",
                              ea_names == "ALHAJIALI/318" ~ "LAYINALHAJIALI/318",
                              ea_names == "BABANGWARI/354" ~ "GIDANBABANGWARI/354",
                              ea_names == "KASUWARMATA/336" ~ "UNGUWARMATA/336",
                              ea_names == "LAYINHAJIYAHAUWAMESAKA/346" ~ "HAJIYAHAUWAMAI/346",
                              ea_names == "DORAYIBABBA/1376" ~ "DORAYIBABBA/1366", 
                              ea_names == "DORAYIBABBA/1378" ~ "YAMADAWA/1378", 
                              ea_names == "YAMAWADA/1396" ~ "YAMADAWA/1396",
                              ea_names == "DORAYIKARAMA/1398" ~ "YAMADAWA/1398", 
                              ea_names == "MALUFAI/1448" ~ "FILINIDI/1448",
                              ea_names == "YANALAWA/1450" ~ "ZANGO/1450",
                              ea_names == "FILINIDI/1450" ~ "ZANGO/1450",
                              ea_names == "YANALAWA/1452" ~ "ZANGO/1452",
                              ea_names == "FILINIDI/1452" ~ "ZANGO/1452",
                              ea_names == "FILINIDI/1456"~ "KOFARMATADYEPITS/1456",
                              ea_names == "ZANGO/1470"~ "KOFARMATADYEPITS/1470",
                              ea_names == "ZANGO/1474"~ "HAURE/1474", 
                              ea_names == "DORAYI/1476"~ "MMSH/1476",
                              ea_names == "ZANGO/1478"~ "MAKERA/1478",
                              ea_names == "ZANGO/1480"~ "YANMUDUBI/1480",
                              ea_names == "DORAYI/1482"~ "MMSH/1482",
                              ea_names == "YANMUDUBI/1484"~ "LOKONMAKERA/1484",
                              ea_names == "ZANGO/1484"~ "LOKONMAKERA/1484",
                              ea_names == "GOBIRAWAA/2374"~ "LAYINMEUNGUWA/2374",
                              ea_names == "SHAGOTARA/2374"~ "LAYINMEUNGUWA/2374",
                              ea_names == "SHAGOTARA/2424"~ "GOBIRAWA/2424",
                              ea_names == "SHAGOTARA/2432"~ "GOBIRAWA/2432",
                              ea_names == "KURNAA/2492"~ "GOBIRAWA/2492",
                              ea_names == "SHAWUCI/1476" ~ "MMSH/1476",
                              .default = ea_names)) %>% 
  dplyr::filter(ward != 3, 
                ea_names != "EMEKA/NANA",
                ea_names != "EMEKA/NA", 
                ea_names != "UNGUWARJAKADA/NA",
                ea_names != "SALLARBABBA/8165",
                ea_names != "SALLARI/231",
                ea_names != "SALLARI/4041", 
                ea_names != "HAUSAWA/8165", 
                ea_names != "DARMANAWA/9288", 
                ea_names != "NA/NA") %>% 
  mutate(ward = ifelse(ward == 2 & ea_names == "BADAWALAYOUT/48", 2, ward)) 
  
  

check00 <- all_kano_ea_listed %>% 
  dplyr::select(ward, enumaration_area, ea_names,cluster_number,total) %>% 
  group_by(ward, ea_names, cluster_number) %>% 
  summarise(total = sum(total))

# check01 <- all_kano_ea_listed %>% 
#   dplyr::select(ward, ea_names,total) %>% 
#   group_by(ward, ea_names) %>% 
#   summarise(total = sum(total))
# 
# 
# write.csv(check,
#           file.path(cleaned_data, metropolis_name,
#                     "duplicated_data",
#                     "hh_checks.csv"))


# GIGINYUNC/29, HOTORO/29 - GIGINYUC/29
# FORESTRY/1492 - FORESTRY/1392, 
# EMEKA/NANA - remove 
# GOBIRAWA/1168 = GOBIRAWA/2168
# DORAYIBABBA/1367 - DORAYIBABBA/1376
# DUNIYARYANGARUWA/2298 - GOBIRAWA/2298
# G/2252, CIKINGARI/2252,  - GOBIRAWA/2252
# GIGINYU/6158 - 
# GOBIRAWA/2145 - GOBIRAWA/2154
# KAMADA/1476 - MMSH/1476
# KASUWARMATA/332 - WAPA/332
# KASUWARMATA/366 - KASUWARMATA/336
# LOKONMAKEA/1484 - LOKONMAKERA/1484
# ALHAJIALI/0318 - ALHAJIALI/0318
# LAYINDANWANZAN/2440, SHAGOTARA/440 SHAGOTARA/NA  - SHAGOTARA/2440
# SHAGOTARA/102 - SHAGOTARA/2102
#SHAGOTARA/2409 - SHAGOTARA/2492
# T/2058 - GOBIRAWA/2058
# YANGANDA/2220 - TUDUNBUJUWA/2220













# HAURE/1474 - ZANGO/1474






corrected_eanames <- all_kano_ea_listed %>% 
  dplyr::select(ward, enumaration_area, ea_names,cluster_number,total) %>% 
  group_by(ward, cluster_number, enumaration_area, ea_names) %>% 
  summarise(total = sum(total))


# corrections_done <- combined_data_corrected_eas %>% 
#   dplyr::select(cluster_number, enumaration_area, ea_names) %>% 
#   distinct()


# write.csv(corrections_done, file.path(cleaned_data, metropolis_name,"corrections_done.csv"))



combined_data_corrected_eas <- inner_join(Kano_data_hh_individuals_cleaned, corrected_eanames) %>% 
  mutate(line_number00 = repeat_instance, 
         unique_id = paste0(serial_number, "_", line_number00), 
         agebin = cut(age, c(0, 5, 10, 17, 30, 100), include.lowest = T)) %>%
  inner_join(Kano_data_malaria_screening_cleaned, by = c("serial_number", "unique_id")) %>% 
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
  mutate(age_total = n(), 
         longitude = as.numeric(longitude), 
         latitude = as.numeric(latidude)) 
  
  


add_totals <- combined_data_corrected_eas %>% 
  mutate(longitude = as.numeric(longitude), 
         latitude = as.numeric(latidude)) %>%  
  tidyr::drop_na(longitude,latitude) 


# write.csv(combined_data_corrected_eas, file.path(cleaned_data, metropolis_name,"combined_data_corrected_eas.csv"))


# View(combined_data_corrected_eas %>% dplyr::select(serial_number,line_number01, repeat_instance, unique_id))



# 
# View(kano_hh_listed_01 %>% 
#   group_by(total) %>% 
#   summarise(totals = n()))
# 
# 
# View(kano_hh_listed_01 %>% 
#        group_by(Ward, eas_names) %>% 
       # summarise(total = sum(total)))


# str_detect(ea_name, "^TR"), "TRIUMPH", ea_name

# weird_ea_name$enumaration_area
  


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
kano_weights <- read.csv(file.path(dropbox, "kano_weights_data.csv")) %>% 
  transmute(longitude, latitude, ward, enumeration_area,ea_serial_number, 
           hh_serial_number, structure_serial_number, 
           ward_weight = 1/prob_selected_ward, 
           ea_settlement_weight = 1/prob_selected_eas_settlement, 
           hhs_weights = 1/prob_selected_hh_structure)


add_totals_sf <- sf::st_as_sf(add_totals, coords = c("longitude", "latitude"), crs = 4326)
kano_weight_sf <- sf::st_as_sf(kano_weights, coords = c("longitude", "latitude"), crs = 4326)

merged_dataset <- sf::st_join(add_totals_sf, kano_weight_sf)



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



coords <- sf::st_coordinates(modified_merged_dataset_updated)


modified_merged_dataset_updated$longitude <- coords[, 'X']
modified_merged_dataset_updated$latitude <- coords[, 'Y']

modified_merged_dataset_mod <- modified_merged_dataset_updated %>% 
  sf::st_drop_geometry()

modified_merged_dataset_mod <- modified_merged_dataset_mod[!duplicated(modified_merged_dataset_mod$unique_id, fromLast = TRUE), ]

write.csv(modified_merged_dataset_mod, file.path(cleaned_data_path, metropolis_name,"kano_malaria_weighted_information_v00.csv")) 

coordinates <- modified_merged_dataset_mod %>% 
  dplyr::select(Ward = ward.x, longitude, latitude) %>% 
  distinct()

write.csv(coordinates, file.path(cleaned_data_path, metropolis_name,"coordinates.csv")) 


#######################################ANALYSIS###################################################


kano_data_malaria_data <- read.csv(file.path(cleaned_data_path, metropolis_name,"kano_malaria_weighted_information_v00.csv")) %>% 
  group_by(ward.x, settlement_type,  hh_number, ea_names) %>% 
  mutate(members_tested_hh = n()) %>% 
  ungroup() %>% 
  group_by(ward.x, settlement_type, ea_names) %>% 
  mutate(members_tested_ea = n()) %>% 
  distinct() 



weight_adjusted_tpr <- kano_data_malaria_data %>%
  drop_na(rdt_test_result, settlement_type) %>% 
  mutate(malaria_test = ifelse(rdt_test_result == 1, 1, 0)) %>%
  group_by(settlement_type) %>% 
  summarise(positive = sum(malaria_test), 
            total = n(),
            negative = total - positive,
            tpr = round(sum(malaria_test * overall_hh_weight, na.rm = T) / sum(overall_hh_weight, na.rm = T) * 100, 3),
            compliment = 100 - tpr)




new_data <- weight_adjusted_tpr %>% 
  dplyr::select(settlement_type, positive, negative) %>% 
  reshape2::melt(id = c("settlement_type"))

names(new_data) <- c("settlement_type", "result", "value")



labels_new_data <- weight_adjusted_tpr %>% 
  dplyr::select(settlement_type, tpr, compliment) %>% 
  reshape2::melt(id = c("settlement_type")) %>% 
  mutate(variable = ifelse(variable == "tpr", "positive", "negative"))

names(labels_new_data) <- c("settlement_type", "result", "percentage")

plotting_data <- inner_join(new_data, labels_new_data) %>% 
  mutate(plot_position = cumsum(value) - ( value))


ggplot(data = plotting_data) +
  geom_bar(aes(x = settlement_type, y = value, fill = result), 
           stat = "identity", position = "stack") +
  geom_text(aes(x = settlement_type, y = value, label = paste(percentage, "(%)")),  
            color = "black",
            nudge_y = 10, size = 8) +
  scale_fill_manual(values = c("negative" = "#FFE7E7", "positive" = "#944E63")) +
  labs(title = "",
       x = "settlement Type",
       y = "number of people tested for malaria",
       fill = "malaria RDT result") +
  theme_bw(base_size = 20, base_family = "")


ggsave(file.path(results, metropolis_name, "kano_tpr_settlement_type_02.pdf"), 
       dpi = 400, width = 15,
       height = 10,)


ggsave(file.path(results, metropolis_name, "kano_tpr_settlement_type_02.png"), 
       dpi = 400, width = 15,
       height = 10,)

# box plot 

EA_weight_adjusted_tpr <- kano_data_malaria_data %>%
  drop_na(rdt_test_result, settlement_type) %>% 
  # st_drop_geometry() %>%
  mutate(malaria_test = ifelse(rdt_test_result == 1, 1, 0)) %>%
  group_by(ward.x, settlement_type, ea_names, members_tested_ea) %>% 
  summarise(positive = sum(malaria_test), 
            total = n(),
            negative = total - positive,
            tpr = round(sum((malaria_test * overall_hh_weight), na.rm = T) / sum(overall_hh_weight,na.rm = T) * 100, 3),
            compliment = 100 - tpr) %>% 
  mutate(settlement_type  = ifelse(settlement_type == 1, "Formal", 
                                   ifelse(settlement_type == 2, "Informal", "Slum")))






less_than_5 = EA_weight_adjusted_tpr %>% filter(!is.nan(tpr)) %>% 
  mutate(target = ifelse(tpr < 5, "less than 5%", 
                         "greater than 5%")) %>% 
  group_by(target) %>% 
  summarise(totals = sum(total))



less_than_1 = EA_weight_adjusted_tpr %>% filter(!is.nan(tpr)) %>% 
  mutate(target = ifelse(tpr < 1, "less than 1%", 
                         "greater than 1%")) %>% 
  group_by(target) %>% 
  summarise(totals = sum(total))


duplicated <- EA_weight_adjusted_tpr %>% 
  group_by(ward.x, ea_names, settlement_type) %>% 
  summarise(total = n()) %>% 
  mutate(ward = case_when(ward.x == 1 ~ "ZANGO", 
                          ward.x == 2 ~ "DORAYI",
                          ward.x == 4 ~ "FADDE D2", 
                          ward.x == 5 ~ "GOBIRAWA", 
                          ward.x == 6 ~ "GIGINYU"))
  
# %>% 
#   filter(total >1)

write.csv(duplicated, file.path(cleaned_data_path, metropolis_name,"alleas.ea_names.csv"), row.names = F)  



# write.csv(EA_weight_adjusted_tpr, file.path(cleaned_data_path, metropolis_name,"EA_weight_adjusted_tpr.csv"), row.names = F)  



ggplot(EA_weight_adjusted_tpr, aes(x = settlement_type, y = tpr),  fill = settlement_type_new ) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = settlement_type, size = members_tested_ea), width = 0.08, alpha = 0.5)+
  # scale_color_manual(values=c("#FFE7E7",  "#F2A6A2", "#B47B84")) +
  scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
  labs(title = "",
       x = "settlement type",
       y = "enumaration area test positivity rate", 
       color ="settlement type", 
       size = "number tested") +
  #theme_manuscript()+ 
  theme(legend.position = "none") +
  theme_bw(base_size = 20, base_family = "") 



ggsave(file.path(results, metropolis_name, "kano_tpr_wardlevel00.pdf"), 
       dpi = 300, width = 12,
       height = 10,)

ggsave(file.path(results, metropolis_name, "kano_tpr_wardlevel00.png"), 
       dpi = 400, width = 15,
       height = 8,)


# write.csv(EA_weight_adjusted_tpr, file.path(cleaned_data_path, metropolis_name,"EA_weight_adjusted_tpr.csv"), row.names = F)  

# estimate the prevalence by ward and settlement type 

weight_adjusted_ward_tpr <- Ibadan_data_malaria_data %>%
  filter(settlement_type_new != "", rdt_test_result != "Undeterminate") %>% 
  mutate(malaria_test = ifelse(rdt_test_result == "POSITIVE", 1, 0)) %>%
  group_by(settlement_type_new, Ward) %>% 
  summarise(positive = sum(malaria_test), 
            total = n(),
            negative = total - positive,
            tpr = round(sum(malaria_test * overall_hh_weight, na.rm = T) / sum(overall_hh_weight, na.rm = T) * 100, 3),
            compliment = 100 - tpr)



new_ward_data <- weight_adjusted_ward_tpr %>% 
  dplyr::select(settlement_type_new, Ward, positive, negative) %>% 
  reshape2::melt(id = c("settlement_type_new", "Ward"))


names(new_ward_data) <- c("settlement_type", "Ward", "result", "value")



labels_ward_new_data <- weight_adjusted_ward_tpr %>% 
  dplyr::select(settlement_type_new, Ward, tpr, compliment) %>% 
  reshape2::melt(id = c("settlement_type_new", "Ward")) %>% 
  mutate(variable = ifelse(variable == "tpr", "positive", "negative"))

names(labels_ward_new_data) <- c("settlement_type", "Ward", "result", "percentage")

plotting_data <- inner_join(new_ward_data, labels_ward_new_data) %>% 
  mutate(plot_position = cumsum(value) - ( value))


ggplot(data = plotting_data) +
  geom_bar(aes(x = settlement_type, y = value, fill = result), 
           stat = "identity", position = "stack") +
  geom_text(aes(x = settlement_type, y = value, label = paste(percentage, "(%)")),  
            color = "black",
            size = 3.5, size = 3.5, nudge_y = 10) +
  facet_wrap(~Ward, ncol = 2)+
  scale_fill_manual(values = c("negative" = "#FFE7E7", "positive" = "#944E63")) +
  labs(title = "Malaria test results by settlement type",
       x = "Settlement Type",
       y = "Number of people tested for malaria",
       fill = "malaria RDT result") +
  theme_bw(base_size = 12, base_family = "")


ggsave(file.path(results, metropolis_name, "kano_tpr_settlement_type_ward_02.pdf"), 
       dpi = 300, width = 12,
       height = 10)


ggsave(file.path(results, metropolis_name, "kano_tpr_settlement_type_ward_02.png"), 
       dpi = 400, width = 12,
       height = 10)



age_adjusted_tpr <- Ibadan_data_malaria_data %>%
  filter(settlement_type_new != "", rdt_test_result != "Undeterminate") %>% 
  # st_drop_geometry() %>%
  mutate(malaria_test = ifelse(rdt_test_result == "POSITIVE", 1, 0)) %>%
  group_by(settlement_type_new, agebin ) %>% 
  summarise(positive = sum(malaria_test), 
            total = n(),
            negative = total - positive,
            tpr = round(sum((malaria_test * overall_hh_weight), na.rm = T) / sum(overall_hh_weight,na.rm = T) * 100, 3),
            compliment = 100 - tpr)


new_ward_data <- age_adjusted_tpr %>% 
  dplyr::select(agebin, settlement_type_new, positive, negative) %>% 
  reshape2::melt(id = c("settlement_type_new", "agebin"))


names(new_ward_data) <- c("settlement_type", "agebin", "result", "value")



labels_ward_new_data <- age_adjusted_tpr %>% 
  dplyr::select(settlement_type_new, agebin, tpr, compliment) %>% 
  reshape2::melt(id = c("settlement_type_new", "agebin")) %>% 
  mutate(variable = ifelse(variable == "tpr", "positive", "negative"))

names(labels_ward_new_data) <- c("settlement_type", "agebin", "result", "percentage")

plotting_data <- inner_join(new_ward_data, labels_ward_new_data) %>% 
  mutate(plot_position = cumsum(value) - ( value))%>% 
  mutate(age_bin = factor(agebin, levels = c("[0,5]", "(5,10]", "(10,17]", "(17,30]", "(30,122]")))



ggplot(data = plotting_data) +
  geom_bar(aes(x = age_bin, y = value, fill = result), 
           stat = "identity", position = "stack") +
  geom_text(aes(x = age_bin, y = value, label = paste(round(percentage, 1), "(%)")),  
            color = "black",
            size = 3.5,  nudge_y = 10) +
  facet_wrap(~settlement_type)+
  scale_fill_manual(values = c("negative" = "#FFE7E7", "positive" = "#944E63")) +
  labs(x = "age groups",
       y = "number of people tested for malaria",
       fill = "") +
  theme_bw(base_size = 12, base_family = "")


ggsave(file.path(results, metropolis_name, "malaria_burden_age_and_settlement_type.pdf"), 
       dpi = 300, width = 12,
       height = 8)


ggsave(file.path(results, metropolis_name, "malaria_burden_age_and_settlement_type.png"), 
       dpi = 400, width = 12,
       height = 8)







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
,                        "4" = "Fagge 2", "5" = "Gobirawa", "6" = "Others")))+
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
,,                    "3" = "slums")))+
  theme_minimal() +
  scale_fill_manual(values = c("1" = "#FFE7E7", "2" = "#B47B84", "3" = "#944E63"), 
                    labels = c("positive", "negative", "undeterminate"))+
  scale_x_discrete(labels = c("1"= "Zango", "2" = "Dorayi", # "3" = "Tundun Wazurchi", 
,    "4" = "Fagge 2", "5" = "Gobirawa", "6" = "Giginyu"))+
  labs(title = "Kano age distribution by settlement type", 
       x = "age group", y = "Frequency", fill = "test result")


# extract the covariates from Kano raster file 
# plot the data collection points on the respective shape files 
# create a geospatial model for the data area level and one that incoporates Krigging 
# fits a smooth surface over the data points 
