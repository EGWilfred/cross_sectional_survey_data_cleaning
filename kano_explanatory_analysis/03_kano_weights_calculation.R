rm(list=ls())

metropolis_name <- "Kano"

source("load_paths.R")

###Household Listed into 
kano_hh_listed_00<- read_excel(file.path(dropbox, "Kano_with_new_ward_2023-12-01.xlsx"))
kano_hh_listed_01 <- read_excel(file.path(dropbox, "Kano_HH_Listing_2023-12-01 (1).xlsx"))


kano_hh_listed <- rbind(kano_hh_listed_00, kano_hh_listed_01)



# View(kano_hh_listed %>% 
#        group_by(`EA Serial Number`) %>% 
#        transmute(Ward, total = n()))


# hh_listed <- kano_hh_listed %>% 
#   dplyr::select(`Enumeration Area`, `EA Serial Number`, `EA Serial Number`, 
#                 Ward) %>%
# distinct()


kano_hh_sampled <- read_excel(file.path(dropbox, "KN_sampled_HHs_2024.xlsx"))


# hh-sampled <- kano_hh_sampled %>% 
#   dplyr::select(settlement, ward, enumerationarea, 
#                 easerialnumber) %>%
#   mutate(new_ea_name = paste0(enumerationarea, "/", easerialnumber))
#   distinct()



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
  group_by(ea_serial_number, ward, hh_serial_number, structure_serial_number) %>%
  distinct() %>% 
  mutate(total_hh_listed_structure = n())


selected_household <- kano_hh_sampled %>% 
  mutate(ea_name_new  = paste0(ward, "/", easerialnumber),
    prob_selected_eas_settlement = case_when(
      ward == "Zango" & settlement == "Informal" ~ 1,
      ward == "Zango" & settlement == "Slum" ~ 1,
      ward == "Zango" & settlement == "Formal" ~ 1,
      
      ward == "Gobirawa" & settlement == "Formal" ~ 3/14,
      ward == "Gobirawa" & settlement == "Informal" ~ 37/228,
      ward == "Gobirawa" & settlement == "Slum" ~ 0,
      
      ward == "Giginyu" & settlement == "Formal" ~ 1,
      ward == "Giginyu" & settlement == "Informal" ~ 1,
      ward == "Giginyu" & settlement == "Slum" ~ 0,
      
      ward == "Fagge D2" & settlement == "Formal" ~ 1,
      ward == "Fagge D2" & settlement == "Informal" ~ 1,
      ward == "Fagge D2" & settlement == "Slum" ~ 0,
      
      ward == "Dorayi" & settlement == "Formal" ~ 1,
      ward == "Dorayi" & settlement == "Informal" ~ 27/29,
      ward == "Dorayi" & settlement == "Slum" ~ 0,
      
      TRUE ~ NA_real_  # If none of the conditions match, keep the original 'Value'
    )
  ) %>%
  group_by(enumerationarea, easerialnumber, ward, ea_name_new ,serialnumberofstructure) %>% 
  dplyr::distinct() %>% 
  mutate(total_hh_selected_structure = n(), ) %>% 
  ungroup()


all_eas <- selected_household %>% 
  dplyr::select(ward, enumerationarea, easerialnumber, settlement) %>% 
  group_by(ward, enumerationarea, easerialnumber, settlement) %>% 
  summarise(total = n(), 
            ea_name = paste0(enumerationarea[1], "/", easerialnumber[1]))
  
# write.csv(all_eas,
#           file.path(cleaned_data, metropolis_name,
#                     "duplicated_data",
#                     "all_eas_96.csv"))

# check <- read.csv(file.path(cleaned_data, metropolis_name,
#                     "duplicated_data",
#                     "check.csv"))
          

# View(all_eas)

# correct up to this points


all_selected_hh <- inner_join(listed_households, 
                              selected_household, 
                              by = c( "index" = "_index")) %>% 
  mutate(prob_selected_hh_structure = total_hh_selected_structure/total_hh_listed_structure)
# need clarity on the probability I am getting some are greater than one.


# View(all_selected_hh %>% dplyr::select(total_hh_selected_structure, total_hh_in_structure, 
#                                        total_hh_listed_structure,total_hh_in_structure,
#                                        prob_selected_hh_structure))



weights_data <- all_selected_hh %>% 
  mutate(enumeration_area = paste0(toupper(enumerationarea), "/", easerialnumber), 
         enumeration_area = str_replace_all(enumeration_area, "[ ,]", "")) %>% 
  inner_join(kano_hh_sampled_eanames) %>% 
  dplyr::select(longitude, latitude, ward = ward.x, index,
                enumeration_area, ea_names, ea_serial_number, hh_serial_number, 
                structure_serial_number,prob_selected_ward, 
                prob_selected_eas_settlement,
                prob_selected_hh_structure)



write.csv(weights_data, file.path(dropbox, "kano_weights_data.csv"))

write.csv(all_selected_hh, file.path(dropbox, "kano_all_selected_hh.csv"))





kano_hh_sampled00 <- kano_hh_sampled %>% 
  mutate(enumeration_area = paste0(toupper(enumerationarea), "/", easerialnumber), 
         enumeration_area = str_replace_all(enumeration_area, "[ ,]", ""))
# , 
#          enumeration_area = str_replace_all(manipulate_enumaration_area, "[']", ""))

ea_names <- unique(kano_hh_sampled00$enumeration_area)

write.csv(ea_names, file.path(dropbox, "ea_names.csv"))

corrected_EAS = read.csv("C:/Users/lml6626/Downloads/corrections_done (1).csv")

kano_hh_sampled_eanames <- kano_hh_sampled00 %>% 
  dplyr::select(easerialnumber, enumerationarea, enumeration_area) %>% 
  inner_join(corrected_EAS, by = c("easerialnumber" = "cluster_number")) %>% 
  distinct()




write.csv(ib_hh_sampled_eas, file.path(NuDir, "ib_hh_sampled_eas.csv"))




