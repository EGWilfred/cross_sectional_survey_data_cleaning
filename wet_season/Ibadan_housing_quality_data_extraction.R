rm(list=ls())

metropolis_name <- "Ibadan"

source("load_paths.R")

newdata <- read.csv(newdata, file.path(cleaned_data_path, metropolis_name, "ibadan_analysis_data.csv"))


raster_files <- file.path(dhsDir, "/nigeria/Raster_files")

setlement_tpye = sf::st_read((dhsDir, "nigeria_settlement_classification/blocks_V1.1/Nigeria_Blocks_V1.shp")) %>% 
  filter(state == 'Oyo', landuse =='Residential')


settlement_data = sf::st_join(ilorin_shapefile, 
                              setlement_tpye, 
                              join = sf::st_overlaps)


files_names  = list.files(file.path(raster_files, "/EVI/EVI_nigeria_2020"), 
                          pattern = ".tif", full.names = TRUE)

raster_filenames = lapply(seq_along(files_names), 
                          function(x) raster::raster(files_names[[x]]))


coordinates_sf <- lapply(seq_along(raster_filenames), 
                         function(x) sf::st_as_sf(newdata, 
                                                  coords = c( "longitude", "latitude"),
                                                  crs = sf::st_crs(raster_filenames[[x]])))



extracted_values <- lapply(seq_along(coordinates_sf),
                           function (x) raster::extract(raster_filenames[[x]], 
                                                        coordinates_sf[[x]],
                                                        #buffer = raster::buffer,
                                                        fun = mean, df =TRUE)) %>% 
  purrr::reduce(left_join, by = c("ID"))



newdata$average_EVI <- rowMeans(extracted_values[ , c(2:13)], na.rm=TRUE)

# Plot evi 

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
                                                    col = average_EVI), stat= "sf_coordinates")+
    # scale_color_manual(values = c(Formal = "#00A08A", Informal = "#F2A6A2" , Slum = "#923159"))+
    # geom_text_repel(
    #   data = coordinate_intersection,
    #   aes(label =  ea_code, geometry = geometry),color ='black',
    #   stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1, max.overlaps = Inf)+
    guides(size = FALSE)+
    map_theme()+ 
    ylab("")+
    xlab("")+
    labs(title= paste(wards_interest[index]), 
         color = "EVI")+
    coord_sf()
  
  plots[[index]] <-  plott
  
  
}


combined_plot <- cowplot::plot_grid(plots[[1]], plots[[2]],
                                    plots[[3]], plots[[4]],
                                    ncol = 2, nrow = 2)


print(combined_plot)
