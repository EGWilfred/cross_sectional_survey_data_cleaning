rm(list=ls())

metropolis_name <- "Ibadan"

source("load_paths.R")


malaria_cleaned <- read.csv(file.path(cleaned_data_path, metropolis_name,"all_malaria_data.csv"))


Ibadan_data_malaria_data <- malaria_cleaned %>% 
  dplyr::select(serial_number, unique_id, repeat_instrument,
         repeat_instance, request_consent,  
         longitude, latitude, Ward,
         household_residents, 
         relatioship_head_household,
         gender, agebin , dob, age,
         mother_present, marital_status,
         rdt_eligibility, 
         ward, settlement_type_new,
         community_name, 
         hh_number, hhs_weights, name_household_head,
         consent_rdt = consent_rdt, rdt_test_result = rdt_test_result,   
         dried_blood_sample = dried_blood_sample,
         dbs_code = dbs_code, ward_weight, ea_settlement_weight,
         prob_ind_hh, ind_weights_hh, 
         hhs_weights, ea_number, ea_numbers_new) %>% 
  group_by(Ward, settlement_type_new,  hh_number, ea_numbers_new) %>% 
  mutate(members_tested_hh = n()) %>% 
  ungroup() %>% 
  group_by(settlement_type_new, ea_numbers_new) %>% 
  mutate(members_tested_ea = n()) %>% 
  distinct()






Ibadan_data_malaria_data <-  Ibadan_data_malaria_data %>% 
  group_by(Ward, settlement_type_new, ea_numbers_new, hh_number, agebin) %>%
  mutate(ind_total = n(),
         prob_ind_hh = 1/ind_total, 
         ind_weights_hh = 1/prob_ind_hh, 
         overall_hh_weight  = ind_weights_hh * ward_weight *
           ea_settlement_weight * hhs_weights) %>% 
  ungroup() 


 # write.csv(Ibadan_data_malaria_data, file.path(cleaned_data_path, metropolis_name,"spatial_data_analysis.csv")) 


duplicates <- Ibadan_data_malaria_data %>% 
  # duplicated unique ids at this point are repeated line numbers
  group_by(unique_id, agebin) %>% 
  dplyr::select(ward, settlement_type_new, ea_number, ea_numbers_new) %>% 
  summarise(count = n()) %>% 
  filter(count > 1)

# Checked
# serial_nums <- duplicates$unique_id  
# 
# 
# ind_dup <- Ibadan_data_malaria_data %>%
#   filter(unique_id %in% serial_nums)



# Ibadan_data_malaria_data <- Ibadan_data_malaria_data %>% 
#   filter(!unique_id %in% serial_nums)


weight_adjusted_tpr <- Ibadan_data_malaria_data %>%
  filter(settlement_type_new != "", rdt_test_result != "Undeterminate") %>% 
  mutate(malaria_test = ifelse(rdt_test_result == "POSITIVE", 1, 0)) %>%
  group_by(settlement_type_new) %>% 
  summarise(positive = sum(malaria_test), 
            total = n(), negative = total - positive,
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


ggsave(file.path(results, metropolis_name, "ibadan_tpr_settlement_type_02.pdf"), 
       dpi = 400, width = 15,
       height = 10,)


ggsave(file.path(results, metropolis_name, "ibadan_tpr_settlement_type_02.png"), 
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


less_than_5 = EA_weight_adjusted_tpr %>% 
  mutate(target = ifelse(tpr < 5, "less than 5%", 
                         "greater than 5%")) %>% 
  group_by(target) %>% 
  summarise(totals = sum(total))



less_than_1 = EA_weight_adjusted_tpr %>% 
  mutate(target = ifelse(tpr < 1, "less than 1%", 
                         "greater than 1%")) %>% 
  group_by(target) %>% 
  summarise(totals = sum(total))



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



ggsave(file.path(results, metropolis_name, "ibadan_tpr_wardlevel00.pdf"), 
       dpi = 300, width = 12,
       height = 10,)

ggsave(file.path(results, metropolis_name, "ibadan_tpr_wardlevel00.png"), 
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



less_than_5 = weight_adjusted_ward_tpr %>% 
  filter(tpr < 5)



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


ggsave(file.path(results, metropolis_name, "ibadan_tpr_settlement_type_ward_02.pdf"), 
       dpi = 300, width = 12,
       height = 10)


ggsave(file.path(results, metropolis_name, "ibadan_tpr_settlement_type_ward_02.png"), 
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


#########################################################

household_tested <- Ibadan_data_malaria_data %>%
  filter(settlement_type_new != "", rdt_test_result != "Undeterminate") %>% 
  group_by(settlement_type_new, Ward, serial_number, ea_numbers_new) %>% 
  summarise(total = n()) %>%
  group_by(settlement_type_new, ea_numbers_new, Ward) %>%
  summarise(total2 = n())

write.csv(household_tested, file.path(cleaned_data_path, metropolis_name,"ibadan_summary_household_tested.csv")) 

#############################################
