### Updated COVARIATES for NIGERIA

source("load_path.R", echo=FALSE) 

Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", gsub("OneDrive", "", Sys.getenv("HOME")))))
Drive <- file.path(gsub("[//]", "/", Drive))
DriveDir <- file.path(Drive, "Urban Malaria Proj Dropbox", "urban_malaria")
DataDir <- file.path(DriveDir, "data")
NigeriadataDir <- file.path(DataDir, "Nigeria")
updated_var <- file.path(NigeriadataDir, "Updated_Covariates")
IbadanDir <- file.path(DataDir, "nigeria", "kano_ibadan_epi", "Field data", "cleaned_data", "Ibadan")

rainfall_rasters <-file.path(updated_var, "monthlyrainfallera5")
temperature_rasters <-file.path(updated_var, "monthlytempera5")
dewtemp_rasters <-file.path(updated_var, "monthlydewtempera5")
evi_rasters <-file.path(updated_var, "2023_EVI_MOD13A1")
ndvi_rasters <-file.path(updated_var, "2023_NDVI_MOD13A1")
pop_count <- file.path(updated_var, "NGA_population_v2_0_gridded")

nigeria_shp <- st_read(file.path(NigeriadataDir, "nigeria_shapefiles", "nigeria_polbnda_admin_0_unsalb", "Admin_0", "NGA_cnty_admin0", "nga_polbnda_adm0_1m.shp"))
ggplot(data= nigeria_shp)+
  geom_sf(color = "black", fill = "#e79a9a")+
  map_theme()

ibadanloc <- read.csv(file.path(IbadanDir, "coordinates.csv"))
ibadanloc_sf <- st_as_sf(ibadanloc, coords = c("longitude", "latitude"), crs = 4326)
view(ibadanloc_sf)

##population count
nga_pop <- raster::raster(file.path(pop_count, "NGA_population_v2_0_gridded.tif"))
ibadan_popcount <- raster::extract(nga_pop, ibadanloc_sf,df = TRUE)

ibadanloc_sf$popcount_100m <- ibadan_popcount$NGA_population_v2_0_gridded

###rainfall 
nga_rain <- list.files(file.path(rainfall_rasters), 
                       pattern = ".tif", full.names = TRUE)

ngarain_data <- lapply(seq_along(nga_rain), 
                      function(x) raster::raster(nga_rain[[x]]))

ibadanrain_data <- ngarain_data %>%
  purrr::map(~raster::extract(., ibadanloc_sf, df =TRUE))%>%
  purrr::reduce(left_join, by = c("ID"))

ibadanloc_sf$avgRAIN_m <- rowMeans(ibadanrain_data[, 2:18], na.rm=TRUE)

##temperature
nga_temp <- list.files(file.path(temperature_rasters), 
                       pattern = ".tif", full.names = TRUE)

ngatempdata <- lapply(seq_along(nga_temp), 
                       function(x) raster::raster(nga_temp[[x]]))

ibadantemp_data <- ngatempdata %>%
  purrr::map(~raster::extract(., ibadanloc_sf, df =TRUE))%>%
  purrr::reduce(left_join, by = c("ID"))

ibadanloc_sf$avgTEMP_K <- rowMeans(ibadantemp_data[, 2:18], na.rm=TRUE)

##NDVI

nga_ndvi <- list.files(file.path(ndvi_rasters), 
                       pattern = ".tif", full.names = TRUE)

ngandvi_data <- lapply(seq_along(nga_ndvi), 
                       function(x) raster::raster(nga_ndvi[[x]]))

ibadanndvi_data <- ngandvi_data %>%
  purrr::map(~raster::extract(., ibadanloc_sf, df =TRUE))%>%
  purrr::reduce(left_join, by = c("ID"))

ibadanloc_sf$avgNDVI_2023 <- rowMeans(ibadanndvi_data[, 2:23], na.rm=TRUE)

##EVI
nga_evi <- list.files(file.path(evi_rasters), 
                       pattern = ".tif", full.names = TRUE)

ngaevi_data <- lapply(seq_along(nga_evi), 
                       function(x) raster::raster(nga_ndvi[[x]]))

ibadanevi_data <- ngaevi_data %>%
  purrr::map(~raster::extract(., ibadanloc_sf, df =TRUE))%>%
  purrr::reduce(left_join, by = c("ID"))

ibadanloc_sf$avgEVI_2023 <- rowMeans(ibadanevi_data[, 2:23], na.rm=TRUE)

##dew temperature/relativehumidity
nga_dewtemp <- list.files(file.path(dewtemp_rasters), 
                       pattern = ".tif", full.names = TRUE)

ngadewtempdata <- lapply(seq_along(nga_dewtemp), 
                      function(x) raster::raster(nga_dewtemp[[x]]))

ibadandewtemp_data <- ngadewtempdata %>%
  purrr::map(~raster::extract(., ibadanloc_sf, df =TRUE))%>%
  purrr::reduce(left_join, by = c("ID"))

#convert K to C, calculate r humidity

ibadanloc_sf$avgTEMP_K <- rowMeans(ibadantemp_data[, 2:18], na.rm=TRUE)

##combine
ibadan_data <- left_join(ibadanloc_sf, ibadanloc, by = c("X" = "X", "Ward" = "Ward"))
ibadan_data <- ibadan_data %>%
  st_drop_geometry()

output_file <- file.path(updated_var, "ibadan_variables.csv")
write.csv(ibadan_data, file = output_file, row.names = FALSE)





