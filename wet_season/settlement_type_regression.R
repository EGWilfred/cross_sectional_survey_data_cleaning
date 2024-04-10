
rm(list=ls())


library(viridis); library(glm2);
library(broom)

metropolis_name <- "Ibadan"

source("~/urban_malaria/cross_sectional_survey_data_cleaning/load_paths.R")


malaria_cases <- read.csv(file.path(cleaned_data_path, metropolis_name,"ibadan_enviromental_covariates_coded.csv"))


coordinates <- malaria_cases[, c("longitude", "latitude")]
malaria_cases$location <- paste(malaria_cases$latitude, malaria_cases$longitude, sep = "-")



malaria_cases_new <- malaria_cases %>%
  select(settlement_type_new, age,
         agebin,
         # ea_numbers_new, 
         # hh_number,
         # longitude, 
         # latitude, 
         gender,
         itn_presence,
         elevation, 
         popcount_100m,
         avgEVI_2023,
         #ceiling_presence, 
         #leaves_open, 
         stagnant_water_nearby, 
         vessels_with_stagnant_water, 
         bushes_nearby, 
         dumpsite_nearby, 
         overgrown_vegetation, 
         open_drainages, 
         #clogged_open_drainage,
         garden_farm_incompound, 
         #overall_hh_weight,
         test_result = rdt_test_result)

predictor_vars <- setdiff(names(malaria_cases_new), "test_result")



malaria_cases_new <- split(malaria_cases_new,
                           malaria_cases_new$settlement_type_new)




 columns <- c(2:14)


#######################################################################################
# univariate logistic regression model
#######################################################################################
model_results <- list()
ward_models <- list()


for (index in seq_along(malaria_cases_new)){
  
  model_results <- list()
  
  for(vari in columns){
    
    malaria_cases_new$agebin <- factor(malaria_cases_new$agebin, 
                                       levels = c("[0,5]", "(5,10]", "(10,17]", 
                                                  "(17,30]", "(30,122]"))
    
    prevalence_data  = malaria_cases_new[[index]]
    variable  = names(prevalence_data)[vari]
    
    formula <- as.formula(paste("test_result ~", variable))
    
    model <- glm(formula, family = binomial(link = "logit"),
                 data = prevalence_data)
    
    model_results[[vari]] <- broom::tidy(model) %>% 
      mutate(ward = names(malaria_cases_new)[index],
             oddsratio = round(exp(estimate), 3),
             ci_low = round(exp(estimate - (1.96 * std.error)), 3),
             ci_high = round(exp(estimate + (1.96 * std.error)), 3))
    
    
  }
  
  ward_models[[index]] <- data.table::rbindlist(model_results[!sapply(model_results, is.null)])
  
}




unlisted_files <- data.table::rbindlist(ward_models) %>%
  filter(term != "(Intercept)")



ggplot(unlisted_files, aes(x = oddsratio, y = term)) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = ci_high, xmin = ci_low), size = .5, height =
                   .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  facet_wrap(~ward)+
  theme_bw() +
  theme(panel.grid.minor = element_blank()) 


#######################################################################################
# multivariate logistic regression analysis 
#######################################################################################


multivariable <- c("vessels_with_stagnant_water + settlement_type_new + gender + agebin + itn_presence", 
                   "stagnant_water_nearby + dumpsite_nearby + agebin + gender +vessels_with_stagnant_water",
                   "agebin + dumpsite_nearby", 
                   "vessels_with_stagnant_water + settlement_type_new + agebin + itn_presence")




ward_multivariate_models <- list()

multivariate_models <- list()


wards <- names(malaria_cases_new)

for (index in seq_along(malaria_cases_new)){
  
  
  prevalence_data  = malaria_cases_new[[index]]
  variable  = multivariable[index]
  
  formula <- as.formula(paste("test_result ~", variable))
  
  model <- glm(formula, family = binomial(link = "logit"), data = prevalence_data)
  
  multivariate_models[[index]] <- model
  
  ward_multivariate_models[[index]] <- broom::tidy(model) %>% 
    mutate(ward = wards[index],
           oddsratio = round(exp(estimate), 3),
           ci_low = round(exp(estimate - 1.96 * std.error), 3),
           ci_high = round(exp(estimate + 1.96 * std.error), 3))
}


multivariate_unlisted_files <- data.table::rbindlist(ward_multivariate_models) 
# %>% 
#   filter(term != "(Intercept)")


ggplot(multivariate_unlisted_files, aes(x = oddsratio, y = term)) +
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") +
  geom_errorbarh(aes(xmax = ci_high, xmin = ci_low), size = .5, height =
                   .2, color = "gray50") +
  geom_point(size = 3.5, color = "orange") +
  facet_wrap(~ward, ncol = 2)+
  theme_bw() +
  theme(panel.grid.minor = element_blank()) 

