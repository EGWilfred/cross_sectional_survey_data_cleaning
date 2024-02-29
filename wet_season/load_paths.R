

#directory path to dropbox
Drive <- gsub("Documents", "", Sys.getenv("HOME"))
DriveDir <- file.path(Drive, "Urban Malaria Proj Dropbox", "urban_malaria") 

 
dhsDir <- file.path(DriveDir, "data")
cleaned_data <- file.path(dhsDir, "nigeria/kano_ibadan_epi/Field data/cleaned_data")

 # only Kano and Ibadan

if (metropolis_name == "Kano"){
  
  dropbox <-  file.path(DriveDir,"data", "nigeria/kano_ibadan_epi/Field data/Kano data/Household Survey")
  cleaned_data_path <- file.path(DriveDir, "data/nigeria/kano_ibadan_epi/Field data/cleaned_data")
  Kano_data <- read.csv(file.path(dropbox, "UrbanMalariaHousehol-DataUpdate2_DATA_LABELS_2024-01-14_1648.csv"))
  new_names <- read.csv(file.path(dropbox, "data_dictionary.csv"))

  }else{
    
  dropbox <-  file.path(DriveDir,"data", "nigeria/kano_ibadan_epi/Field data/Ibadan_data/Household Survey")
  cleaned_data_path <- file.path(DriveDir, "data/nigeria/kano_ibadan_epi/Field data/cleaned_data")
  Ibadan_data <- read.csv(file.path(dropbox, "UrbanMalariaHousehol-ExportedData_DATA_LABELS_2024-01-05_1921.csv"))
  new_names <- read.csv(file.path(dropbox, "ibadan_data_dictionary.csv"))
}





# packges to use 
list_of_packages <- c("stringr","ggplot2", "dplyr", "purrr", "haven")

read_install_pacakges <- function(packages = list_of_packages
){
  new_packages <- packages[!(list_of_packages %in% installed.packages()[,"Package"])]
  if(length(new.packages)) install.packages(new_packages)
  return(sapply(list_of_packages, require, character.only = TRUE))
}

read_install_pacakges()




map_theme <- function(){
  theme(axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        rect = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(fill = "white", colour = NA),
        plot.title = element_text(hjust = 0.5),
        legend.title.align=0.5,
        legend.title=element_text(size=8, colour = 'black'),
        legend.text =element_text(size = 8, colour = 'black'),
        legend.key.height = unit(0.65, "cm"))
}
