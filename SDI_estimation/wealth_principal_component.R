
package_name <- "factoextra"


if (!require(package_name, character.only = TRUE)) {
 
    install.packages(package_name)
    library(package_name, character.only = TRUE)
  } else {
    message(sprintf("Package '%s' is already installed.", package_name))
    library(package_name, character.only = TRUE)
    }



source("~/urban_malaria/cross_sectional_survey_data_cleaning/load_paths.R", echo = F)

malaria_cases <- read.csv(file.path(cleaned_data_path, metropolis_name,"all_malaria_data.csv"))



names(malaria_cases)

ibadan_household_data <- malaria_cases %>% 
  select(serial_number, 
         # variables commented out because they are common in 
         # all households and affected PCA out put. Current out put 
         # only explains 20% of the variation seen in the data will look  
         # into the details later  
         Ward, 
         longitude, 
         latitude, 
         settlement_type_new, 
         ea_numbers_new, 
         # radio,
         # television, 
         # cellphone, 
         laptop, 
         # phone, 
         # desktop,
         refrigerator,
         # table, 
         # chair, 
         # bed,  
         # cupboard,
         air_conditioner,
         electric_iron, 
         generator, 
         # fan, 
        # own_livestock,
         home_ownership,house_type, 
         #shared_compound, 
         number_hh_sharing_compound, 
         number_bedrooms, 
         shared_toilets, 
         bathroom_location, 
         basic_hh_tasks_energy_source, 
         # other_basic_hh_tasks_energy_source, 
         ceiling_presence) %>% 
  tidyr::drop_na()




recode_values <- function(x) {
  #data re-coding across many columns 
  case_when(
    x == "Checked" ~ 1,
    x == "Unchecked" ~ 0,
    TRUE ~ NA_real_
  )
}


recode_values_00 <- function(x) {
  #data re-coding across many columns 
  case_when(
    x == "Yes" ~ 1,
    x == "No" ~ 0,
    TRUE ~ NA_real_
  )
}




malaria_cases_coded <- ibadan_household_data %>%
  mutate(across(.cols =c(laptop:refrigerator, cupboard:fan), .fns = recode_values), 
         across(.cols =c(own_livestock), .fns = recode_values_00), 
         number_hh_sharing_compound = ifelse(number_hh_sharing_compound <=2, 1, 0), 
         number_bedrooms = ifelse(number_bedrooms >=2, 1, 0), 
         home_ownership = ifelse(home_ownership == "Owned by household"| home_ownership == "Owned by family/parents", 1, 0),
         house_type = ifelse(house_type == "Single Family Bungalow"| house_type == "Single Family Duplex" |
                               house_type == "Single family home (3 or more flats)", 1, 0), 
         basic_hh_tasks_energy_source  = ifelse(basic_hh_tasks_energy_source == "Electricity", 1, 0),
         ceiling_presence = ifelse(ceiling_presence == "Yes, Complete and sealed", 1, 0), 
         bathroom_location =  ifelse(bathroom_location == "Inside the house", 1, 0), 
         shared_toilets = ifelse(shared_toilets == "Have own toilet", 1, 0))


# melted_malaria_cases_coded <- reshape2::melt(malaria_cases_coded,
#                                              id.vars = c("serial_number", "Ward","longitude",
#                                                          "latitude", "settlement_type_new",
#                                                          "ea_numbers_new")) %>%
#   group_by(variable,settlement_type_new ,value) %>%
#   summarise(total = n())
# 
# 
#   ggplot(data = melted_malaria_cases_coded %>% filter(settlement_type_new  == "Formal"),
#          aes(x = variable, y = total, fill = as.factor(value)))+
#     geom_bar(stat = "identity", position = "stack")+
#    theme(axis.text.x = element_text(angle = 90))



ibadan_pca_result <- prcomp(malaria_cases_coded[,-(1:6)], center = TRUE, scale. = TRUE)



print(summary(ibadan_pca_result))

fviz_pca_ind(ibadan_pca_result,
             geom.ind = "point", 
             col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             legend.title = "Cos2"
)




pc_scores <- ibadan_pca_result$x 

malaria_cases_coded$pca <- pc_scores[,1]

plot(pc_scores[,1], pc_scores[,2], xlab="PC1", ylab="PC2", main="PCA of Household Data",
     pch=19, col=rainbow(nrow(malaria_cases_coded[,-(1:6)])))
text(pc_scores[,1], pc_scores[,2], labels = row.names(malaria_cases_coded[,-(1:6)]), pos=4)



var_explained <- ibadan_pca_result$sdev^2 / sum(ibadan_pca_result$sdev^2)
plot(var_explained, xlab="Principal Component", ylab="Proportion of Variance Explained",
     type='b', pch=19, col="blue", main="Variance Explained by Each Principal Component")




ggplot(malaria_cases_coded, aes(x = settlement_type_new, y = pca),  fill = settlement_type_new ) +
  geom_boxplot(outlier.shape = NA) +
  labs(title = "Distribution of household wealth index ",
       x = "Settlement type",
       y = "household wealth  index") +
  theme(legend.position = "none") +
  theme_bw(base_size = 12, base_family = "")


ggsave(file.path(results, metropolis_name, "household_wealth_index.pdf"),
       dpi = 300, width = 12,
       height = 10,)



write.csv(malaria_cases_coded, file.path(cleaned_data, metropolis_name, "household_wealth_index.csv"), row.names = F)

