rm(list=ls())

metropolis_name <- "Ibadan"

source("load_paths.R")

Ibadan_data_malaria_screening <- read.csv(file.path(cleaned_data_path, metropolis_name, "ibadan_malaria_individual_hh_individual_information.csv"))



Ibadan_data_malaria_data <- Ibadan_data_malaria_screening %>% 
  select(serial_number, unique_id, repeat_instrument = repeat_instrument.x, 
         repeat_instance,request_consent,  household_residents, relatioship_head_household,
         gender, age, dob, mother_present, marital_status, rdt_eligibility, 
         ward, settlement_type,community_name, enumaration_area, 
         hh_number, longitude, latitude, name_household_head,
         consent_rdt, rdt_test_result ,   
         dried_blood_sample ,dbs_code) 

# no duplicates at this point 


##########################################################################################################
# ANALYSIS
##########################################################################################################

newdata <- Ibadan_data_malaria_data %>%  
  mutate(ward = ifelse(ward == "Basorun", "Bashorun", ward),
         settlement_type = ifelse(settlement_type == "Formal" & ward == "Agugu", 
                                  "Informal", settlement_type), 
         ward = ifelse(ward == "Challenge" & grepl("^BA", enumaration_area)| grepl("^Ba", enumaration_area) | grepl("^ BA", enumaration_area), "Bashorun",
                       ifelse(ward == "Bashorun" & grepl("^CH", enumaration_area)|grepl("^ CH", enumaration_area), "Challenge", 
                              ifelse(ward == "Bashorun" & grepl("^AG", enumaration_area)|grepl("^ AG", enumaration_area), "Agugu", 
                                     ifelse(ward == "", "Challenge", ward))))) %>% # no formal settlements in Bashorun
   mutate(agebin = cut(age, c(0,5,10,20,30,40,50, 60, 70, 122), include.lowest = T))

ggplot(newdata, aes(x = agebin, fill = gender))+
  geom_bar() +
  theme_minimal() +
  scale_fill_manual(values = c("Male" = "#FFE7E7", "Female" = "#944E63"))+
  labs(title = "Ibadan age sex distribution", 
       x = "age group", y = "Frequency", fill = "gender")


newdata %>% 
  filter(settlement_type != "") %>% # only 9 data points removed 
  ggplot(aes(x = agebin, fill = settlement_type))+
  geom_bar() +
  theme_minimal() +
  scale_fill_manual(values = c("Formal" = "#FFE7E7", "Informal" = "#B47B84", "Slum" = "#944E63"))+
  labs(title = "Ibadan age distribution by settlement type", 
       x = "age group", y = "Frequency", fill = "settlement type")


newdata %>% 
  filter(!is.na(ward), ward != "") %>% # only 4 data points that are uncounted for 
  ggplot(aes(x = agebin, fill = settlement_type))+ 
  geom_bar() +
  facet_wrap(~ward)+
  theme_minimal() +
  scale_fill_manual(values = c("Formal" = "#FFE7E7", "Informal" = "#B47B84", "Slum" = "#944E63"))+
  labs(title = "Ibadan age distribution by settlement type in the respective wards", 
       x = "age group", y = "Frequency", fill = "settlement type")



# newdata$ward <- factor(newdata$ward, levels = c("1", "2", "3", "4", "5", "6"))


newdata %>% 
  filter(settlement_type != "", rdt_test_result != "Undeterminate") %>%
  ggplot(aes(x = ward, fill = rdt_test_result))+
  geom_bar() +
  facet_wrap(~settlement_type)+
  theme_minimal() +
  scale_fill_manual(values = c("NEGATIVE" = "#FFE7E7",  "POSITIVE" = "#944E63"))+
  labs(title = "breakdown of positive/negative counts by ward and settlement type", 
       x = "ward", y = "Frequency", fill = "test result")





# newdata %>% 
# raw tpr calculation
#   #filter(rdt_test_result != "Undeterminate") %>%
#   # group_by() %>% 
#   summarise(total_positive = sum(ifelse(rdt_test_result == "POSITIVE", 1, 0)), 
#             total_tested = n())


# plot the data collection points on the respective shape files 


dhsDir <- file.path(DriveDir, "data")

nigeria_data <-  file.path(DriveDir,"data","nigeria")

shapefile <- file.path( nigeria_data, "kano_ibadan/kano_ibadan_shape_files/Ibadan_metro_ward_fiveLGAs/Ibadan_metro_fiveLGAs.shp")


Ibadan_shapefile <- sf::read_sf(shapefile)

wards_interest <- unique(newdata$ward)[-5]

sf_newdata = sf::st_as_sf(newdata, 
                          coords = c( "longitude", "latitude"),  
                          crs = 4326)

plots <- list()


for (index in seq_along(wards_interest)){
  
   wards = wards_interest[index]

   shapefile_interest <- Ibadan_shapefile %>% 
      filter(WardName == wards)
    
    ward_hh_data <- sf_newdata %>% 
      filter(ward == wards)
    
    sf::st_crs(shapefile_interest) <- 4326
    sf::st_crs(ward_hh_data) <- 4326
    
    coordinate_intersection <- sf::st_intersection(shapefile_interest, ward_hh_data)
    
    
   plott <-  ggplot(shapefile_interest)+
      geom_sf(fill = NA) +
      geom_point(data = coordinate_intersection,  aes(geometry = geometry, size = 1.0, 
                                                      col = settlement_type), stat= "sf_coordinates")+
      scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
      # geom_text_repel(
      #   data = coordinate_intersection,
      #   aes(label =  ea_code, geometry = geometry),color ='black',
      #   stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
      guides(size = FALSE)+
      map_theme()+ 
      ylab("")+
      xlab("")+
      labs(title= paste(wards_interest[index]), 
           color = "settlement type")+
      coord_sf()
    
   plots[[index]] <-  plott

  
}


write.csv(newdata, file.path(cleaned_data_path, metropolis_name, "ibadan_analysis_data.csv"))
#remember to remove the names in these column onces data has been verified 


combined_plot <- cowplot::plot_grid(plots[[1]], plots[[2]],
                           plots[[3]], plots[[4]],
                           ncol = 2, nrow = 2)


print(combined_plot)

# ggsave("combined_plot.png", combined_plot, width = 10, height = 8)





# create a geospatial model for the data area level and one that incoporates Krigging 
# fits a smooth surface over the data points 