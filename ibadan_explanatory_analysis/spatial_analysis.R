rm(list=ls())


#library(viridis); library(glm2)

metropolis_name <- "Ibadan"

source("~/urban_malaria/cross_sectional_survey_data_cleaning/load_paths.R")



malaria_cleaned <- read.csv(file.path(cleaned_data_path, metropolis_name,"all_malaria_data.csv"))
ibadan_covariates <- read.csv(file.path(cleaned_data_path, metropolis_name,"ibadan_enviromental_covariates.csv"))


ibadan_env_covariates <-ibadan_covariates %>% 
  dplyr::select(Ward, longitude, latitude, 
         popcount_100m, avgEVI_2023, elevation)



combined_data <- merge(malaria_cleaned,  ibadan_env_covariates)


analysis_data <- combined_data %>% 
  dplyr::select(Ward, longitude, latitude, unique_id,
         settlement_type_new, community_name,
         ea_numbers_new, hh_number, hh_total, 
         gender, age, agebin, gender,
         
         # breeding sites availability
         ceiling_presence, leaves_open, 
         stagnant_water_nearby, vessels_with_stagnant_water, 
         bushes_nearby, dumpsite_nearby, overgrown_vegetation, 
         open_drainages, clogged_open_drainage, garden_farm_incompound,
         
         # ITNS data availability
         itn_presence, why_no_itn, distribution_campaign, anc,
         immunization_visit, gvt_health_facility, pvt_health_facility,
         pharmacy, shop_market, community_healthy_worker, religious_institution,
         school, other_itn_source, itn_not_known, mother_present,
         
         # ITN_usage
         net_use_frequencey, 
         other_net_use_frequencey, 
         permanently_hung_itn,
         itn_inspection_consent,
         
         # ind level weight accounting for sampling strategy
         overall_hh_weight,
         
         # Malaria tesrdt_test_result
         
         rdt_test_result,
         
         #environmental covariates 
         popcount_100m, avgEVI_2023, elevation, road_type
         )
  

recode_values <- function(x) {
  #data re-coding across many columns 
  case_when(
    x == "Checked" ~ 1,
    x == "Unchecked" ~ 0,
    TRUE ~ NA_real_
  )
}


recode_values_00 <- function(x) {
  #data re-coding across many columns 
  case_when(
    x == "Yes" ~ 1,
    x == "No" ~ 0,
    x == "" ~0,
    TRUE ~ NA_real_
  )
}


recode_values_01 <- function(x) {
  #data re-coding across many columns 
  case_when(
    x == "Daily" ~ 1,
    x== "Occasionally" ~ 1,
    x == "Others" ~ 0,
    x == "Never"~ 0,
    x == "Rarely" ~ 0,
    x == "" ~ 0,
    TRUE ~ NA_real_
  )
}



malaria_cases <- analysis_data %>%
  mutate(across(.cols =c(distribution_campaign, anc,
                         immunization_visit, gvt_health_facility, pvt_health_facility,
                         pharmacy, shop_market, community_healthy_worker, religious_institution,
                         school, other_itn_source, itn_not_known), .fns = recode_values), 
         across(.cols =c(itn_presence, ceiling_presence, leaves_open, 
                         stagnant_water_nearby, vessels_with_stagnant_water, 
                         bushes_nearby, dumpsite_nearby, overgrown_vegetation, 
                         open_drainages,clogged_open_drainage, garden_farm_incompound, mother_present), .fns = recode_values_00), 
         across(.cols =c(net_use_frequencey), .fns = recode_values_01),
         road_type = ifelse(road_type == "Tarred", 1, 0),
         rdt_test_result = ifelse(rdt_test_result == "POSITIVE", 1, 
                                  ifelse(rdt_test_result == "NEGATIVE", 0, NA)))


write.csv(malaria_cases, file.path(cleaned_data_path, metropolis_name,"ibadan_enviromental_covariates_coded.csv"))


coordinates <- malaria_cases[, c("longitude", "latitude")]
malaria_cases$location <- paste(malaria_cases$latitude, malaria_cases$longitude, sep = "-")



malaria_cases_new <- malaria_cases %>%
  select(Ward, 
         unique_id,
         settlement_type_new,
         agebin,
         ea_numbers_new, 
         hh_number,
         longitude, 
         latitude, 
         gender,
         itn_presence,
         elevation, 
         popcount_100m,
         avgEVI_2023,
         ceiling_presence, 
         leaves_open, 
         stagnant_water_nearby, 
         vessels_with_stagnant_water, 
         bushes_nearby, 
         dumpsite_nearby, 
         overgrown_vegetation, 
         open_drainages, 
         clogged_open_drainage,
         garden_farm_incompound, 
         test_result = rdt_test_result)


malaria_cases_new <- split(malaria_cases_new,
                           malaria_cases_new$Ward)

ward_names <- names(malaria_cases_new)

ward_models <- list()


for (index in seq_along(malaria_cases_new)){

# Basic logistic regression model
  
  prevalence_data  = malaria_cases_new[[index]]

  ward_models[[index]] <- glm(test_result ~  1 + 
                                
                                # demographic and population dynamics 
                                I(agebin)+
                                I(settlement_type_new) +
                                I(gender) +
                                I(popcount_100m) +
                                
                                # environmental around the hse (summerise into one)
                                I(stagnant_water_nearby) + 
                                I(vessels_with_stagnant_water) + 
                                I(bushes_nearby)+ 
                                I(dumpsite_nearby) + 
                                I(overgrown_vegetation) + 
                                I(open_drainages) +
                                I(clogged_open_drainage) +
                                I(garden_farm_incompound) +
                                
                                # raster files data  
                                I(elevation) +
                                I(avgEVI_2023) 
                              
                                # health seeking behavior/ case management
                                # vector control (itns distribution)
                                # migration /travel frequency malaria report after travel
                                # +
                                # I(wealth_index) # already summarised 
                              ,
                              family = binomial(link = "logit"), 
                              data = prevalence_data)


}


lapply(seq_along(ward_models), function(x) summary(ward_models[[x]]))




tab <- lapply(seq_along(ward_models), function(x) summary(ward_models[[x]])$coef)


odds_ratio <- data.table::rbindlist(lapply(seq_along(ward_models), 
                     function(x) data.frame(ward = ward_names[x],
                                            variable = rownames(tab[[x]]),
                                            oddsratio = round(exp(tab[[x]][,1]), 3),
                                            ci_low = round(exp(tab[[x]][,1] - 1.96 * tab[[x]][,2]), 3),
                                            ci_high = round(exp(tab[[x]][,1] + 1.96 * tab[[x]][,2]), 3),
                                            pval = scales::pvalue(tab[[x]][,4]),
                                            row.names = NULL)[-1,])) 




ggplot(odds_ratio %>% filter(ward == "AGUGU"), aes(x = oddsratio, y = variable)) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = ci_high, xmin = ci_low), size = .5, height =
                   .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  facet_wrap(~ward, ncol = 2)+
  theme_bw() +
  theme(panel.grid.minor = element_blank()) 


################################################################################################################
# the spatial model
################################################################################################################

# demographic breakdown 

newdat <- malaria_cases %>%
  # select(settlement_type_new, Ward, overgrown_vegetation) %>% 
  group_by(gender, settlement_type_new, agebin) %>% 
  summarise(value = sum(rdt_test_result, na.rm = T ),
            total = n(), 
            tpr = value/total)%>% 
  ungroup() %>% 
  group_by(agebin, settlement_type_new) %>%
  mutate(percentage = total/sum(total)) %>% 
  # ungroup() %>% 
  mutate(plot_position = ifelse(gender == "Female",  0.75*sum(percentage), 0.25)) %>% 
  #arrange(agebin, sort_variable) %>% 
  mutate(age_bin = factor(agebin, levels = c("[0,5]", "(5,10]", "(10,17]", "(17,30]", "(30,122]")))

       



ggplot(newdat, aes(fill=gender, y = total, x= age_bin)) + 
  geom_bar(position="fill", stat="identity")+
  geom_text(aes(x = agebin, y = plot_position, label = round(percentage*100, 1)), color = "black") +
  facet_wrap(~ settlement_type_new) + labs(x = "age group", y = "number of participats as a percentage")+
  scale_fill_manual(values = c("#fbb4ae", "#b3cde3")) +
  theme_bw(base_size = 20, base_family = "") +
  theme(panel.grid.minor = element_blank(),
        text = element_text(size = 20))


ggsave(file.path(results, metropolis_name, "demographic_breakdown.pdf"),
       dpi = 300, width = 15,
       height = 8,)

ggsave(file.path(results, metropolis_name, "demographic_breakdown.png"),
       dpi = 400, width = 15,
       height = 8,)



#exploring the variables and answers of the questionnaires



newdata_stagnant_water <- malaria_cases %>%
  # select(settlement_type_new, Ward, overgrown_vegetation) %>% 
  group_by(settlement_type_new, Ward) %>% 
  summarise(value = sum(stagnant_water_nearby, na.rm = T ),
            total = n(),
            ceiling = value/total)%>% 
  group_by() %>% 
  group_by(agebin, Ward) %>%
  # mutate(plot_position = cumsum(total) - total)
  mutate(plot_position = ifelse(gender == "Female", sum(total), 0)) 




demographic_pop <- malaria_cases %>% 
  group_by(Ward, settlement_type_new, 
           gender) %>% 
  summarise(totals = n())


demographic_pop %>%
  filter(settlement_type_new != "") %>% # only 9 data points removed
  ggplot(aes(x = agebin, fill = totals))+
  geom_bar() 
