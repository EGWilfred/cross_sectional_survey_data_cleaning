rm(list=ls())

metropolis_name <- "Ibadan"

source("~/urban_malaria/cross_sectional_survey_data_cleaning/load_paths.R", echo = F)

# malaria_cases <- read.csv('all_malaria_data.csv')
malaria_cases <- read.csv(file.path(cleaned_data_path, metropolis_name,"all_malaria_data.csv"))

names(malaria_cases)

unique(malaria_cases$drinking_water_source)

##recoding drinking water
malaria_cases <- malaria_cases %>%
  mutate(drinking_water_source_code = case_when(
    drinking_water_source %in% c(
      "Piped into dwelling/yard/plot",
      "Piped to neighbour",
      "Public tap/standpipe",
      "Tube well or borehole",
      "Protected dug well",
      "Protected spring",
      "Sachet water",
      "Tanker truck/cart with small tank",
      "Bottled water"
    ) ~ 1,  # Code 1 for improved sources
    drinking_water_source %in% c(
      "Unprotected dug well",
      "Unprotected spring",
      "Surface water (River, Pond)",
      "Rainwater"
    ) ~ 0,  # Code 0 for unimproved sources
    TRUE ~ NA_integer_  # Handle other cases (if any)
  ))



malaria_cases <- malaria_cases %>%
  mutate(recoded_other_drinking_water_source = ifelse(
    is.na(other_drinking_water_source), NA,                             
    ifelse(grepl("rain", other_drinking_water_source, ignore.case = TRUE), 0, 1)
  ))


#merge drinking water and others, specify
malaria_cases <- malaria_cases %>%
  mutate(final_drinking_water_source_code = coalesce(drinking_water_source_code, 
                                                     recoded_other_drinking_water_source))





##recoding water for household chores
malaria_cases <- malaria_cases %>%
  mutate(basic_hh_tasks_water_source_code = case_when(
    basic_hh_tasks_water_source %in% c(
      "Piped into dwelling/yard/plot",
      "Piped to neighbour",
      "Public tap/standpipe",
      "Tube well or borehole",
      "Protected dug well",
      "Protected spring",
      "Sachet water",
      "Tanker truck/cart with small tank",
      "Bottled water"
    ) ~ 1,  # Code 1 for improved sources
    basic_hh_tasks_water_source %in% c(
      "Unprotected dug well",
      "Unprotected spring",
      "Surface water (River, Pond)",
      "Rainwater"
    ) ~ 0,  # Code 0 for unimproved sources
    TRUE ~ NA_integer_  # Handle other cases (if any)
  ))

##drinking water- if other, specify
malaria_cases <- malaria_cases %>%
  mutate(recoded_other_basic_hh_tasks_water_source = ifelse(
    is.na(other_basic_hh_tasks_water_source),  # Check if the value is NA (blank)
    NA,                                   # If it's NA, keep it as NA in the recoded column
    ifelse(grepl("rain", other_basic_hh_tasks_water_source, ignore.case = TRUE), 0, 1)
  ))


#merge basic water and others, specify
malaria_cases <- malaria_cases %>%
  mutate(final_basic_hh_tasks_water_source = coalesce(basic_hh_tasks_water_source_code, recoded_other_basic_hh_tasks_water_source))




#recoding toilet facility
malaria_cases <- malaria_cases %>%
  mutate(toilet_facility_code = case_when(
    toilet_facility %in% c(
      "Flush toilet",
      "Ventilated improved pit (VIP) latrine",
      "Pit latrine with slab",
      "Composting toilet"
    ) ~ 1,  # Code 1 for improved sources
    toilet_facility %in% c(
      "Pit latrine without slab/open pit",
      "Bucket",
      "Hanging toilet/hanging latrine",
      "Open defecation (no facility/bush/field)"
    ) ~ 0,  # Code 0 for unimproved sources
    TRUE ~ NA_integer_  # Handle other cases (if any)
  ))

##toilet facility- if other, specify
malaria_cases <- malaria_cases %>%
  mutate(
    recoded_other_toilet_facility = case_when(
      is.na(other_toilet_facility) ~ NA_real_,  # If it's NA, keep it as NA in the recoded column
      other_toilet_facility %in% c("FLUSH AND PIT", "POTTY") ~ 0,  # Recode "FLUSH AND PIT" and "POTTY" as 0
      TRUE ~ 1  # For all other values, assign 1
    )
  )


#merge drinking water and others, specify
malaria_cases <- malaria_cases %>%
  mutate(final_toilet_facility = coalesce(toilet_facility_code, recoded_other_toilet_facility)) %>%
  replace_na(list(final_toilet_facility = 0))




##recoding other WASH Variables
malaria_cases <- malaria_cases %>%
  mutate(
    stagnant_water_nearby = recode(stagnant_water_nearby, "Yes" = 0, "No" = 1),
    vessels_with_stagnant_water = recode(vessels_with_stagnant_water, "Yes" = 0, "No" = 1),
    bushes_nearby = recode(bushes_nearby, "Yes" = 0, "No" = 1),
    open_drainages = recode(open_drainages, "Yes" = 0, "No" = 1),
    overgrown_vegetation = recode(overgrown_vegetation, "Yes" = 0, "No" = 1),
    open_drainages = recode(open_drainages, "Yes" = 0, "No" = 1),
    clogged_open_drainage = recode(clogged_open_drainage, "Yes" = 0, "No" = 1),
    dumpsite_nearby = recode(dumpsite_nearby, "Yes" = 0, "No" = 1)
  )


ibadan_household_data <- malaria_cases %>% 
  dplyr::select(serial_number, unique_id, Ward, longitude, latitude, settlement_type_new, unique_id,#leaves_open,
         stagnant_water_nearby, vessels_with_stagnant_water,
         bushes_nearby,  open_drainages, overgrown_vegetation,
         overgrown_vegetation, open_drainages, final_drinking_water_source_code,
         open_drainages, dumpsite_nearby, final_toilet_facility, final_basic_hh_tasks_water_source) %>% 
  tidyr::drop_na()

# write.csv(ibadan_household_data, "ibadan_household_data.csv", row.names = FALSE)



pca_result <- prcomp(ibadan_household_data[,7:14], center = TRUE, scale. = TRUE)

#view the PCA output
pc_scores <- pca_result$x 

plot(pc_scores[,1], pc_scores[,2], xlab="PC1", ylab="PC2", main="PCA of Household Data",
     pch=19, col=rainbow(nrow(ibadan_household_data)))
text(pc_scores[,1], pc_scores[,2], labels = row.names(ibadan_household_data), pos=4)

ibadan_household_data$pca= pc_scores[,1]

var_explained <- pca_result$sdev^2 / sum(pca_result$sdev^2)
plot(var_explained, xlab="Principal Component", ylab="Proportion of Variance Explained",
     type='b', pch=19, col="blue", main="Variance Explained by Each Principal Component")


##box plots
ggplot(ibadan_household_data, aes(x = settlement_type_new, y = pca),  fill = settlement_type_new ) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "",
       x = "Settlement Type",
       y = " household WASH performance index" ) +
  theme(legend.position = "none") +
  theme_bw(base_size = 20, base_family = "") 


ggsave(file.path(results, metropolis_name, "household_wash_index.pdf"),
       dpi = 300, width = 12,
       height = 8,)

ggsave(file.path(results, metropolis_name, "household_wash_index.png"),
       dpi = 300, width = 12,
       height = 8)



write.csv(ibadan_household_data, file.path(cleaned_data, metropolis_name, "household_wash_index.csv"), row.names = F)


