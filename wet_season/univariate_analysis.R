
rm(list=ls())


library(viridis); library(glm2);
library(broom)

metropolis_name <- "Ibadan"

source("~/urban_malaria/cross_sectional_survey_data_cleaning/load_paths.R")


malaria_cases <- read.csv(file.path(cleaned_data_path, metropolis_name,"ibadan_enviromental_covariates_coded.csv"))


coordinates <- malaria_cases[, c("longitude", "latitude")]
malaria_cases$location <- paste(malaria_cases$latitude, malaria_cases$longitude, sep = "-")



malaria_cases_new <- malaria_cases %>%
  select(Ward, 
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

predictor_vars <- setdiff(names(malaria_cases_new), "test_result")


malaria_cases_new <- split(malaria_cases_new,
                           malaria_cases_new$Ward)




columns <- c(2:3, 8:12,15:22)

ward_models <- list()

#######################################################################################
# univariate logistic regression model
#######################################################################################

for (index in seq_along(malaria_cases_new)){
  
  model_results <- list()
  
  for(vari in columns){
  
    prevalence_data  = malaria_cases_new[[index]]
    variable  = names(prevalence_data)[vari]
    
    formula <- as.formula(paste("test_result ~", variable))
    
    model <- glm(formula, family = binomial(link = "logit"), data = prevalence_data)
    
    model_results[[vari]] <- broom::tidy(model) %>% 
      mutate(ward = names(malaria_cases_new)[index],
             oddsratio = round(exp(estimate), 3),
             ci_low = round(exp(estimate - 1.96 * std.error), 3),
             ci_high = round(exp(estimate + 1.96 * std.error), 3))
  
  
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
  facet_wrap(~ward, ncol = 2)+
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


#######################################################################################
# variogram
#######################################################################################

plots <- list()

residuals <- lapply(seq_along(multivariate_models), function (x) residuals(multivariate_models[[x]]))


for (index in seq_along(malaria_cases_new)){

  # 
  malaria_cases_new[[index]]$residuals <- residuals[[index]]
  
  sp::coordinates(malaria_cases_new[[index]]) <- ~ longitude + latitude 
  
  emp_variogram <- gstat::variogram(residuals ~ 1, data = malaria_cases_new[[index]])
 
  initial_model <- gstat::vgm(psill = 0.5, model = "Sph", range = 100, nugget = 0.3)
  
  fit <- gstat::fit.variogram(emp_variogram, model = initial_model)
  
  plots[[index]] <- plot(emp_variogram, model = fit)
  
} 








variogram_model <- fit.variogram(emp_variogram, model = vgm(psill = 1, "Sph", range = 1, nugget = 0))


kriging_model <- gstat(id = "variable_name", formula = variable_name ~ 1, data = malaria_cases_new[[index]], model = variogram_model)


grd <- expand.grid(x = seq(min(coordinates(malaria_cases_new[[index]])[,1]), max(coordinates(malaria_cases_new[[index]])[,1]), by = 0.1),
                   y = seq(min(coordinates(malaria_cases_new[[index]])[,2]), max(coordinates(malaria_cases_new[[index]])[,2]), by = 0.1))

coordinates(grd) <- ~ x + y
gridded(grd) <- TRUE 


kriging_result <- gstat::krige(value ~ 1, data, newdata = grd, model = variogram_model)

kriging_result <- predict(kriging_model, newdata = grd)


spplot(kriging_result["var1.pred"], main = "Kriging Predictions")
spplot(kriging_result["var1.var"], main = "Kriging Variance")



