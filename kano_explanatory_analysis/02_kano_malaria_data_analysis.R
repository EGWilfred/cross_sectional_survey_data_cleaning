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

  

# Figure out the weird EA name and correct them 

weird_ea_name <- kano_all_data %>% 
  dplyr::select(enumaration_area, settlement_type,lga, ward) %>% 
  group_by( EA, settlement_type,lga, ward) %>% 
  summarise(total = n()) %>%
  mutate(manipulate_enumaration_area = enumaration_area,
         manipulate_enumaration_area00 = enumaration_area,
         manipulate_enumaration_area = str_replace_all(enumaration_area, "[ ,]", ""), 
         manipulate_enumaration_area = str_replace_all(manipulate_enumaration_area, "[']", "")
  ) %>%  #remove space and commas 
  separate(col = manipulate_enumaration_area00, into = c("ea_name00", "cluster_number00"),
           sep = "/", #|(?<=[a-zA-Z])(?=[0-9])|(?<=[0-9])(?=[a-zA-Z])"
           extra = "merge") %>%
  mutate(ea_name = toupper(str_extract(manipulate_enumaration_area, "[A-Za-z]+")),
         cluster_number = str_extract(manipulate_enumaration_area, "[0-9]+"), 
         ea_name  = ifelse(ea_name == "ZONGO"|ea_name == "ZAGO", "ZANGO", 
                           ifelse(ea_name == "FORRESTREE", "FORESTRY", ea_name)), 
         ea_name =  if_else(str_detect(ea_name, "^TR"), "TRIUMPH", ea_name)) %>% 
  inner_join(corrected_ea_names, by = c("ea_name" = "oldeas_names")) %>% 
  mutate(ea_names = paste0(new_eas_names, "/", cluster_number)) %>% 
  filter(ward != 3)


write.csv(weird_ea_name, file.path(cleaned_data_path, metropolis_name, "weird_ea_name.csv"), row.names = F)


old_eas_names <- c(NA, "AFORESTRY", "ALHAJIALI", "BABADAWALAYOUT", "BABANGWARI", "BADAWA", "BADAWALAYOUT",                    
                   "BURHANA", "CHIKAL", "CHIKALA", "CHIKALAROAD", "CIKINGARI", "DARMANAWA", "DORAYI",
                   "DORAYIBABBA", "DORAYIKARAMA", "DORAYIYAMADAWA", "DUKAWA", "DUNIYARYANGARUWA",                
                   "EMEKA", "FAGGED", "FILIN", "FILINDURUMI", "FILINGIDI", "FILINIDI","FORESTER", 
                   "FORESTRY", "FORRESTER", "FORSTRY", "G" ,"GDUKAWA", "GDUKAWAL" ,"GIDANBABANGORI", 
                   "GIDANBABANGWARI", "GIGINYU" ,"GIGINYUB","GIGINYUC",  "GIGINYUNC","GOBARAWA","GOBERAWA" ,
                   "GOBIRAWA", "GOBIRAWAA", "GOBIRAWAB", "GOBIRAWAKURNA" ,"GOBIRAWAMAIUNGUWASALISU" , 
                   "HAJHAUWA","HAJIYAHAUWAMAI", "HAJIYAHAUWAMAISAKA","HAJIYAHAUWAMAISAKAROAD", "HAJIYAHAUWAMESAKA",
                   "HAJIYAHAUWAMESAQA", "HAJIYAHAUWMESAKA", "HAURE",   "HAUSAWA", "HAUWAMAISAKA", "HOTORO",  "JAENJIGAWA",
                   "JIGAWAJAEN", "KAFARMATADYEPITS", "KAMADA","KASUWARMATA","KASUWARMATA", "KAWO", "KAWOAREWA"  , "KAWOCIKI","KAWOCIKIMAIM", 
                   "KAWOCIKIN", "KAWOCIKINGARI" ,"KAWOKUDU","KAWOMAIGARI" , "KAWONMAIGARI" ,"KAWONMEGARI", "KOFARMATA"  ,
                   "KOFARMATADYEPITS", "KURNAA",  "LAYINALHAJIALI"  ,"LAYINALHAJIALINOCASE", "LAYINALHAJINOCASE", 
                   "LAYINDABINAI", "LAYINDANUJILE", "LAYINDANWANZAN", "LAYINHAJIYAHAUWAMESAKA", "LAYINMAKERA",
                   "LAYINMEUNGUWA", "LAYINTANKOMAIMAI", "LOKONMAKEA" ,"LOKONMAKERA","M",  "MAILIKAFA","MAILIKKAFA",
                   "MAKERA","MALUFAI", "MLIKAFA", "MMSH",  "MURTALA", "MURTALAMUHAMMAD" , "MURTALAMUHAMMADHOSPITAL" ,
                   "MURTALAMUHAMMADSOCIALISTHOSPITAL",  "MURTALAMUHAMMADSPECIALHOSPITAL",   "MURTALAMUHAMMADSPECIALISTHOSPITAL",
                   "NASSARAWA", "NASSARAWAG"  , "NASSARAWAGRA" , "R",       "SALLARBABBA" , "SALLARI", "SHAGO",   "SHAGOTARA","SHAWUCI",
                   "SHIGOTARA","T",       "TRIUMPH", "TUDUNBOJUWA","TUDUNBUJUWA","U",  "UNGUWARJAKADA","UNGUWARMATA" , "UNGUWARWAMBAI",                    
                   "UNGUWAWABAI" , "WAPA",    "YAMADAWA", "YAMAWADA","YANALAWA","YANALEWA", "YANGANDA","YANGANDU","YANMUDUBI", 
                   "ZANGO",   "ZANGOGURGAMA")


all_eas_names <- c(NA, "FORESTRY", "ALHAJIALI", "BADAWALAYOUT", "BABANGWARI", "BADAWA", "BADAWALAYOUT",                    
                   "BURHANA", "CHIKALA", "CHIKALA", "CHIKALA", "CIKINGARI", "DARMANAWA", "DORAYI",
                   "DORAYIBABBA", "DORAYIKARAMA", "YAMADAWA", "DUKAWA", "DUNIYARYANGARUWA",                
                   "EMEKA", "FAGGED", "FILINIDI", "FILINDURUMI", "FILINIDI", "FILINIDI","FORESTRY", 
                   "FORESTRY", "FORESTRY", "FORESTRY", "G" ,"G", "G" ,"GIDANBABANGORI", 
                   "GIDANBABANGWARI", "GIGINYU" ,"GIGINYUB","GIGINYUC",  "GIGINYUNC","GOBIRAWA","GOBIRAWA" ,
                   "GOBIRAWA", "GOBIRAWAA", "GOBIRAWAB", "GOBIRAWAKURNA" ,"GOBIRAWAMAIUNGUWASALISU" , 
                   "HAJIYAHAUWAMAI","HAJIYAHAUWAMAI", "HAJIYAHAUWAMAI","HAJIYAHAUWAMAI", "HAJIYAHAUWAMAI",
                   "HAJIYAHAUWAMAI", "HAJIYAHAUWAMAI", "HAURE",   "HAUSAWA", "HAJIYAHAUWAMAI", "HOTORO",  "JAENJIGAWA",
                   "JAENJIGAWA", "KOFARMATADYEPITS", "KAMADA","KASUWARMATA", "KASUWARMATA", "KAWO", "KAWO"  , "KAWO","KAWO", 
                   "KAWO", "KAWO" ,"KAWO","KAWO" , "KAWO" ,"KAWO", "KOFARMATADYEPITS"  ,
                   "KOFARMATADYEPITS", "KURNAA",  "LAYINALHAJIALI"  ,"LAYINALHAJIALI", "LAYINALHAJIALI", 
                   "LAYINDABINAI", "LAYINDANUJILE", "LAYINDANWANZAN", "LAYINHAJIYAHAUWAMESAKA", "LAYINMAKERA",
                   "LAYINMEUNGUWA", "LAYINTANKOMAIMAI", "LOKONMAKEA" ,"LOKONMAKERA","M",  "MAILIKAFA","MAILIKAFA",
                   "MAKERA","MALUFAI", "MLIKAFA", "MMSH",  "MURTALA", "MURTALAMUHAMMAD" , "MURTALAMUHAMMAD" ,
                   "MURTALAMUHAMMAD",  "MURTALAMUHAMMAD",   "MURTALAMUHAMMAD", "NASSARAWA", "NASSARAWAG"  , 
                   "NASSARAWAGRA" , "R",       "SALLARBABBA" , "SALLARI", "SHAGOTARA",   "SHAGOTARA","SHAWUCI",
                   "SHAGOTARA","T", "TRIUMPH", "TUDUNBUJUWA","TUDUNBUJUWA","U",  "UNGUWARJAKADA","UNGUWARMATA" , "UNGUWARWAMBAI",                    
                   "UNGUWARWAMBAI" , "WAPA",    "YAMADAWA", "YAMAWADA","YANALAWA","YANALAWA", "YANGANDA","YANGANDA","YANMUDUBI", 
                   "ZANGO",   "ZANGOGURGAMA")





corrected_ea_names = data.frame(oldeas_names = old_eas_names, 
                                new_eas_names = all_eas_names)



kano_hh_listed_01 <- read.csv("C:/Users/lml6626/Downloads/weird_ea_name_Yusuf_Draft (1).csv")


View(kano_hh_listed_01)






View(kano_hh_listed_01 %>% 
  group_by(ea_names) %>% 
  summarise(total = sum(total)))




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

kano_data_malaria_data <-  kano_all_data %>% 
  group_by(Ward, settlement_type_new, ea_numbers_new, hh_number, agebin) %>%
  mutate(ind_total = n(),
         prob_ind_hh = 1/ind_total, 
         ind_weights_hh = 1/prob_ind_hh, 
         overall_hh_weight  = ind_weights_hh * ward_weight *
           ea_settlement_weight * hhs_weights) %>% 
  ungroup() 



# write.csv(kano_data_malaria_data, file.path(cleaned_data_path, metropolis_name,"spatial_data_analysis.csv")) 



duplicates <- kano_data_malaria_data %>% 
  # duplicated unique ids at this point are repeated line numbers
  group_by(unique_id, agebin) %>% 
  dplyr::select(ward, settlement_type_new, ea_number, ea_numbers_new) %>% 
  summarise(count = n()) %>% 
  filter(count>1)

# Checked
# serial_nums <- duplicates$unique_id  
# 
# 
# ind_dup <- Ibadan_data_malaria_data %>%
#   filter(unique_id %in% serial_nums)



# Ibadan_data_malaria_data <- Ibadan_data_malaria_data %>% 
#   filter(!unique_id %in% serial_nums)



weight_adjusted_tpr <- kano_data_malaria_data %>%
  filter(settlement_type_new != "", rdt_test_result != "Undeterminate") %>% 
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

EA_weight_adjusted_tpr <- Ibadan_data_malaria_data %>%
  filter(settlement_type_new != "", rdt_test_result != "Undeterminate") %>% 
  # st_drop_geometry() %>%
  mutate(malaria_test = ifelse(rdt_test_result == "POSITIVE", 1, 0)) %>%
  group_by(settlement_type_new, ea_numbers_new, members_tested_ea ) %>% 
  summarise(positive = sum(malaria_test), 
            total = n(),
            negative = total - positive,
            tpr = round(sum((malaria_test * overall_hh_weight), na.rm = T) / sum(overall_hh_weight,na.rm = T) * 100, 3),
            compliment = 100 - tpr)




# write.csv(EA_weight_adjusted_tpr, file.path(cleaned_data_path, metropolis_name,"EA_weight_adjusted_tpr.csv"), row.names = F)  



ggplot(EA_weight_adjusted_tpr, aes(x = settlement_type_new, y = tpr),  fill = settlement_type_new ) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(color = settlement_type_new, size = members_tested_ea), width = 0.08, alpha = 0.5)+
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
