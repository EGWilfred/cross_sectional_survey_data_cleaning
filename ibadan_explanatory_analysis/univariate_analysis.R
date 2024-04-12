
rm(list=ls())


library(viridis); library(glm2);
library(broom)

metropolis_name <- "Ibadan"

source("~/urban_malaria/cross_sectional_survey_data_cleaning/load_paths.R")


malaria_cases <- read.csv(file.path(cleaned_data_path, metropolis_name,"ibadan_enviromental_covariates_coded.csv"))


coordinates <- malaria_cases[, c("longitude", "latitude")]
malaria_cases$location <- paste(malaria_cases$latitude, malaria_cases$longitude, sep = "-")



malaria_cases_new <- malaria_cases %>%
  dplyr::select(Ward, 
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

predictions <- lapply(seq_along(multivariate_models), function (x) predict(multivariate_models[[x]], type = "response"))

for (index in seq_along(malaria_cases_new)){

  # 
  # Generate predicted probabilities
  
  malaria_cases_new[[index]]$residuals <- residuals[[index]]
  malaria_cases_new[[index]]$predictions <- predictions[[index]]
  
  sp::coordinates(malaria_cases_new[[index]]) <- ~ longitude + latitude 
  
  emp_variogram <- gstat::variogram(residuals ~ 1, data = malaria_cases_new[[index]])
 
  initial_model <- gstat::vgm(psill = 0.5, model = "Sph", range = 100, nugget = 0.7)
  
  fit <- gstat::fit.variogram(emp_variogram, model = initial_model)
  
  plots[[index]] <- plot(emp_variogram, model = fit)
  
} 




Ibadan_shapefile <- sf::read_sf(shapefile) %>% 
  filter(WardName == "Agugu")

boundary_coords <- sf::st_coordinates(sf::st_boundary(Ibadan_shapefile[1,]))

grid <- expand.grid(x = seq(min(boundary_coords[,1]), max(boundary_coords[,1]), by = 0.0001),
                   y = seq(min(boundary_coords[,2]), max(boundary_coords[,2]), by = 0.0001))


df <- malaria_cases_new[[index]]%>% 
  dplyr::select(longitude, latitude, predictions, 
                test_result, vessels_with_stagnant_water, settlement_type_new , 
                  gender, agebin,itn_presence)

locations <- sf::st_as_sf(df, coords = c("longitude", "latitude"), crs = 4326)
grid <- locations # sf::st_as_sf(grid, coords = c("x", "y"), crs = 4326)

  
kriging_result <- krige(formula = test_result ~ vessels_with_stagnant_water + settlement_type_new + 
                        gender + agebin + itn_presence,  
                        locations, newdata = grid, model = initial_model, 
                        beta = coef(multivariate_models[[index]])[1:length(coef(multivariate_models[[index]]))])


plot(kriging_result)


ggplot() +
  geom_tile(data = as.data.frame(kriging_result), aes(x = lon, y = lat, fill = var1.pred)) + 
  geom_point(data = as.data.frame(locations), aes(x = lon, y = lat), color = "red") +
  scale_fill_gradient(low = "yellow", high = "red") +
  labs(title = "Predicted Malaria Prevalence", fill = "Prevalence") +
  coord_fixed()


variogram_model <- fit.variogram(emp_variogram, model = vgm(psill = 1, "Sph", range = 1, nugget = 0))


kriging_model <- gstat(id = "variable_name", formula = variable_name ~ 1, data = malaria_cases_new[[index]], model = variogram_model)


grd <- expand.grid(x = seq(min(coordinates(malaria_cases_new[[index]])[,1]), max(coordinates(malaria_cases_new[[index]])[,1]), by = 0.1),
                   y = seq(min(coordinates(malaria_cases_new[[index]])[,2]), max(coordinates(malaria_cases_new[[index]])[,2]), by = 0.1))

coordinates(grd) <- ~ x + y
gridded(grd) <- TRUE 


kriging_result <- gstat::krige(value ~ 1, malaria_cases_new[[index]], newdata = grd, model = variogram_model)

kriging_result <- krige(model_residuals ~ 1, locations, newdata = locations, model = v.model)



kriging_result <- predict(kriging_model, newdata = grd)


spplot(kriging_result["var1.pred"], main = "Kriging Predictions")
spplot(kriging_result["var1.var"], main = "Kriging Variance")



Ibadan_shapefile <- sf::read_sf(shapefile) %>% 
  mutate(sampled = ifelse(WardName == "Agugu"|WardName == "Bashorun"|
                            WardName == "Challenge"|WardName == "Olopomewa", 
                          "sampled", "unsampled"),
         labeled = ifelse(WardName == "Agugu"|WardName == "Bashorun"|
                          WardName == "Challenge"|WardName == "Olopomewa", 
                        WardName, ""))




ggplot(Ibadan_shapefile)+
  geom_sf(aes(fill = sampled)) +
  ggrepel::geom_text_repel( data = Ibadan_shapefile,aes(label =  labeled, geometry = geometry) ,color ='black',
                            stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  scale_fill_manual(values = c(sampled = "plum3" , unsampled = "white"))+
  guides(size = FALSE)+
  theme(legend.text = element_text(size = 24)) +
  labs(fill = "", x = "", y = "")+
  # theme(panel.grid.minor = element_blank()) +
  map_theme()

  
 
  
  
  ggsave(file.path(results, metropolis_name, "sampled_wards.pdf"),
         dpi = 400, width = 10,
         height = 8)
  
  ggsave(file.path(results, metropolis_name, "sampled_wards.png"),
         dpi = 600, width = 10,
         height = 8)
  