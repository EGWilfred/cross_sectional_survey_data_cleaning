### Updated COVARIATES for NIGERIA

source("load_paths.R", echo=FALSE) 



nigeria_shp <- st_read(file.path(NigeriadataDir, 
                                 "nigeria_shapefiles", 
                                 "nigeria_polbnda_admin_0_unsalb", 
                                 "Admin_0", "NGA_cnty_admin0", 
                                 "nga_polbnda_adm0_1m.shp"))


ggplot(data= nigeria_shp)+
  geom_sf(color = "black", fill = "#e79a9a")+
  map_theme()


ibadanloc <- read.csv(file.path(IbadanDir, "coordinates.csv"))

ibadanloc_sf <- st_as_sf(ibadanloc, coords = c("longitude", "latitude"), crs = 4326)


##population count
nga_pop <- raster::raster(file.path(pop_count, "NGA_population_v2_0_gridded.tif"))

ibadan_popcount <- raster::extract(nga_pop, ibadanloc_sf,df = TRUE)

ibadanloc_sf$popcount_100m <- ibadan_popcount$NGA_population_v2_0_gridded

###rainfall 
nga_rain <- list.files(file.path(rainfall_rasters), 
                       pattern = ".tif", full.names = TRUE)[6:8] # extract june - August

ngarain_data <- lapply(seq_along(nga_rain),  
                      function(x) raster::raster(nga_rain[[x]]))

ibadanrain_data <- ngarain_data %>%
  purrr::map(~raster::extract(., ibadanloc_sf, df =TRUE))%>%
  purrr::reduce(left_join, by = c("ID"))

ibadanloc_sf$avgRAIN_m <- rowMeans(ibadanrain_data[, -1], na.rm=TRUE)


##temperature
nga_temp <- list.files(file.path(temperature_rasters), 
                       pattern = ".tif", full.names = TRUE)[6:8]

ngatempdata <- lapply(seq_along(nga_temp), 
                       function(x) raster::raster(nga_temp[[x]]))

ibadantemp_data <- ngatempdata %>%
  purrr::map(~raster::extract(., ibadanloc_sf, df =TRUE))%>%
  purrr::reduce(left_join, by = c("ID"))

ibadanloc_sf$avgTEMP_K <- rowMeans(ibadantemp_data[, -1], na.rm=TRUE)

##NDVI

nga_ndvi <- list.files(file.path(ndvi_rasters), 
                       pattern = ".tif", full.names = TRUE)[11:16]

ngandvi_data <- lapply(seq_along(nga_ndvi), 
                       function(x) raster::raster(nga_ndvi[[x]]))

ibadanndvi_data <- ngandvi_data %>%
  purrr::map(~raster::extract(., ibadanloc_sf, df =TRUE))%>%
  purrr::reduce(left_join, by = c("ID"))

ibadanloc_sf$avgNDVI_2023 <- rowMeans(ibadanndvi_data[, -1], na.rm=TRUE)

##EVI
nga_evi <- list.files(file.path(evi_rasters), 
                       pattern = ".tif", full.names = TRUE)[11:16]

ngaevi_data <- lapply(seq_along(nga_evi), 
                       function(x) raster::raster(nga_ndvi[[x]]))

ibadanevi_data <- ngaevi_data %>%
  purrr::map(~raster::extract(., ibadanloc_sf, df =TRUE))%>%
  purrr::reduce(left_join, by = c("ID"))

ibadanloc_sf$avgEVI_2023 <- rowMeans(ibadanevi_data[, -1], na.rm=TRUE)

##dew temperature/relativehumidity

nga_dewtemp <- list.files(file.path(dewtemp_rasters), 
                       pattern = ".tif", full.names = TRUE)[6:8]

ngadewtempdata <- lapply(seq_along(nga_dewtemp), 
                      function(x) raster::raster(nga_dewtemp[[x]]))

ibadandewtemp_data <- ngadewtempdata %>%
  purrr::map(~raster::extract(., ibadanloc_sf, df =TRUE))%>%
  purrr::reduce(left_join, by = c("ID"))

#convert K to C, calculate r humidity

ibadanloc_sf$avgTEMP_K <- rowMeans(ibadantemp_data[, -1], na.rm=TRUE)



## Elevation

elevation <- list.files(file.path(elevation_rasters), 
                          pattern = ".tif", full.names = TRUE)[1]

elevation_data <- lapply(seq_along(elevation), 
                         function(x) raster::raster(elevation[[x]]))

elevationdata <- elevation_data %>%
  purrr::map(~raster::extract(., ibadanloc_sf, df =TRUE))%>%
  purrr::reduce(left_join, by = c("ID"))


ibadanloc_sf$elevation <- elevationdata[, -1]


##combine
ibadan_data <- left_join(ibadanloc_sf, ibadanloc, by = c("X" = "X", "Ward" = "Ward"))

ibadan_data <- ibadan_data %>%
  st_drop_geometry()

output_file <- file.path(cleaned_data_path, metropolis_name, "ibadan_enviromental_covariates.csv")

write.csv(ibadan_data, file = output_file, row.names = FALSE)





