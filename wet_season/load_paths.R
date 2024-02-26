rm(list=ls())

#directory path to dropbox
Drive <- gsub("Documents", "", Sys.getenv("HOME"))
DriveDir <- file.path(Drive, "Urban Malaria Proj Dropbox", "urban_malaria") 

 
dhsDir <- file.path(DriveDir, "data")
cleaned_data <- file.path(dhsDir, "nigeria/kano_ibadan_epi/Field data/cleaned_data")

metropolis_name <- "Kano" # only Kano and Ibadan

if (metropolis_name == "Kano"){
  
  dropbox <-  file.path(DriveDir,"data", "nigeria/kano_ibadan_epi/Field data/Kano data/Household Survey")
  Kano_data <- read.csv(file.path(dropbox, "UrbanMalariaHousehol-DataUpdate2_DATA_LABELS_2024-01-14_1648.csv"))
  new_names <- read.csv(file.path(dropbox, "data_dictionary.csv"))

  }else{
    
  dropbox <-  file.path(DriveDir,"data", "nigeria/kano_ibadan_epi/Field data/Ibadan_data/Household Survey")
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