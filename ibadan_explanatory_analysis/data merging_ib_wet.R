##read and manipulate data

convert_labels_to_factors <- function(df) {
  df %>%
    mutate(across(where(is.labelled), as_factor))
}

#ea, settlement and ward data
clean <- read.csv("/Users/Mac/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/IB_wet coordinate, EAs and Settlement.csv")%>%
  rename(
    `Serial Number` = sn,
    Ward = Ward.y,
    `Settlement Type` = settlement.y
  )%>%
  mutate(`Settlement Type` = tolower(`Settlement Type`))


#weight data
weight <- read_dta("/Users/Mac/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/240922_Ibadan_latest_data/wetseason_household_members_with_weights.dta")%>%
  dplyr::select(sn, hl1, overall_hh_weight)

weight_transform <- convert_labels_to_factors(weight) %>%
  mutate(across(everything(), as.character))%>%
  rename(
    `Serial Number` = sn,
    `Line Number` = hl1
  )%>%
  mutate(unique_id = paste(`Serial Number`, `Line Number`, sep = "_"))


#net ownership
net_ownership <- read_dta("/Users/Mac/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/240922_Ibadan_latest_data/IB Wet season household data_edited_220924 rev.dta")%>%
  dplyr::select(nh101a, sn, nh105)

net_ownership_transform <- convert_labels_to_factors(hh) %>%
  mutate(across(everything(), as.character))%>%
  rename(
    `Serial Number` = sn,
    `Does your household own a mosquitoe net?` = nh101a,
    `How often does anyone sleep in this mosquitoe net?` = nh105
  )


#rdt
rdt <- read_dta("/Users/Mac/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/240922_Ibadan_latest_data/IB Wet season household members RDT_160924.dta")%>%
  dplyr::select(sn, hl1, hl4, hl5, q302)%>%
  mutate(unique_id = paste(sn, hl1, sep = "_"))

rdt_transform <- convert_labels_to_factors(rdt) %>%
  mutate(across(everything(), as.character))%>%
  rename(
    `Serial Number` = sn,
    `Line Number` = hl1,
    Gender = hl4,
    Age = hl5,
    `Test Result` = q302
  )


#net usage
net_inspection <- read_dta("/Users/Mac/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/new_field_data/240922_Ibadan_latest_data/IB Wet season hhold net inspection.dta")%>%
  dplyr::select(sn, nh113, nh114a)

net_inspection_transform <- convert_labels_to_factors(net_inspection) %>%
  mutate(across(everything(), as.character)) %>%
  separate_rows(nh114a, sep = "[&/,\\s+and]+") %>%
  mutate(
    nh114a = gsub("0", "", trimws(nh114a)),
    nh114a = gsub("[A-Za-z]", "", nh114a)
  ) %>%
  rename(
    `Serial Number` = sn,
    `Did anyone sleep in this net last night?` = nh113,
    `Line number of the person who slept in this net last night` = nh114a
  )%>%
  mutate(unique_id = paste(`Serial Number`, `Line number of the person who slept in this net last night`, sep = "_"))




##merge data
# Merge net_inspection_transform, rdt_transform, and weight_transform by 'unique_id'
merged_by_unique_id <- net_inspection_transform %>%
  full_join(rdt_transform, by = "unique_id") %>%
  full_join(weight_transform, by = "unique_id")

#Merge net_ownership and clean by 'Serial Number'
merged_by_serial_number <- net_ownership_transform %>%
  mutate(`Serial Number` = as.character(`Serial Number`)) %>%
  full_join(
    clean %>% mutate(`Serial Number` = as.character(`Serial Number`)),
    by = "Serial Number"
  )

#Merge the two datasets (from step 1 and step 2) by 'Serial Number'
final_merged_data <- merged_by_serial_number %>%
  full_join(merged_by_unique_id, by = "Serial Number")%>%
  dplyr::select(`Serial Number`, unique_id, Gender, Age, overall_hh_weight, `Test Result`,
                `Did anyone sleep in this net last night?`,
                `Settlement Type`, enumeration_area, Ward, Longitude,
                Latitude, `How often does anyone sleep in this mosquitoe net?`, `Does your household own a mosquitoe net?`)%>%
  filter(!duplicated(unique_id), !is.na(`Serial Number`))

# View the final merged data
view(final_merged_data)
