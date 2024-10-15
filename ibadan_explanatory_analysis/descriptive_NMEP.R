install.packages("survey")
library(survey)

# Convert character columns to factors, excluding specified numeric columns
final_merged_data <- final_merged_data%>%
  filter(enumeration_area != "" & !is.na(enumeration_area))%>%
  mutate_if(is.character, as.factor) %>%
  mutate(
    overall_hh_weight = as.numeric(overall_hh_weight),  
    Longitude = as.numeric(Longitude),  
    Latitude = as.numeric(Latitude) 
)




#unweighted
settlement_ward <- final_merged_data %>%
  filter(!is.na(Ward), !is.na(`Settlement Type`))%>%
  group_by(Ward, `Settlement Type`) %>%
  summarise(count_settlements = n()) %>%
  arrange(Ward)

# Plotting the bar chart
ggplot(settlement_ward, aes(x = Ward, y = count_settlements, fill = `Settlement Type`)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Number of Individuals Surveyed per Settlement And Ward in Ibadan Metropolis",
       x = "Ward", y = "Number", fill = "Settlement Type") +
  theme_minimal() +
  theme(plot.title = element_text(size = 30, face = "bold"),         
        axis.title.x = element_text(size = 20, face = "bold"),       
        axis.title.y = element_text(size = 20, face = "bold"),       
        axis.text.x = element_text(size = 18, face = "bold"),        
        axis.text.y = element_text(size = 18, face = "bold"),        
        strip.text = element_text(size = 20, face = "bold")
  )


#weighted
survey_design <- svydesign(ids = ~enumeration_area, 
                           weights = ~overall_hh_weight, 
                           data = final_merged_data)

settlement_ward_weighted <- svyby(
  ~enumeration_area, 
  ~Ward + `Settlement Type`, 
  survey_design, 
  svytotal, 
  na.rm = TRUE
)

# Convert to a data frame for plotting
settlement_ward_weighted_df <- as.data.frame(settlement_ward_weighted)

names(settlement_ward_weighted_df)[names(settlement_ward_weighted_df) == "(enumeration_area)"] <- "count_settlements"

# Plot the bar chart with weighted counts
ggplot(settlement_ward_weighted_df, aes(x = Ward, y = count_settlements, fill = `Settlement Type`)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Weighted Number of Individuals Surveyed per Settlement and Ward in Ibadan Metropolis",
       x = "Ward", y = "Weighted Number", fill = "Settlement Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))













#unweighted prevalence by ward 
prevalence_data1 <- final_merged_data %>%
  filter(!is.na(`Test Result`))%>%
  group_by(Ward, `Test Result`) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(Ward) %>%
  mutate(proportion = count / sum(count))

# Plotting the stacked bar chart by proportion, labeling with counts
ggplot(prevalence_data1, aes(x = Ward, y = proportion, fill = `Test Result`)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = count), 
            position = position_stack(vjust = 0.5), size = 3) +
  scale_y_continuous(labels = scales::percent) +  # Show proportions as percentages
  scale_fill_manual(values = c("POSITIVE" = "red", "NEGATIVE" = "lightblue")) +  # Custom colors
  labs(title = "Proportion of Malaria Test Results by Ward in Ibadan Metropolis",
       x = "Ward", y = "Proportion of Results", fill = "Test Result") +
  theme_minimal() +
  theme(plot.title = element_text(size = 30, face = "bold"),         
        axis.title.x = element_text(size = 20, face = "bold"),       
        axis.title.y = element_text(size = 20, face = "bold"),       
        axis.text.x = element_text(size = 18, face = "bold"),        
        axis.text.y = element_text(size = 18, face = "bold"),        
        strip.text = element_text(size = 20, face = "bold"))






# Using the svyby function to calculate weighted counts by Ward and Test Result
prevalence_weighted <- svyby(
  ~ I(`Test Result` == "POSITIVE") + I(`Test Result` == "NEGATIVE"), 
  ~ Ward, 
  survey_design, 
  svytotal, 
  na.rm = TRUE
)

# Converting to a dataframe for visualization
prevalence_weighted_df <- prevalence_weighted %>%
  as.data.frame() %>%
  pivot_longer(
    cols = c(`I(\`Test Result\` == "POSITIVE")TRUE`, 
             `I(\`Test Result\` == "NEGATIVE")TRUE`),
    names_to = "Test Result",
    values_to = "count"
  ) %>%
  mutate(
    `Test Result` = recode(`Test Result`, 
                           `I(\`Test Result\` == "POSITIVE")TRUE` = "POSITIVE", 
                           `I(\`Test Result\` == "NEGATIVE")TRUE` = "NEGATIVE"),
    proportion = count / ave(count, Ward, FUN = sum)
  )




# Plotting the weighted prevalence stacked bar chart
ggplot(prevalence_weighted_df, aes(x = Ward, y = proportion, fill = `Test Result`)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = scales::percent) +  # Show proportions as percentages
  scale_fill_manual(values = c("POSITIVE" = "red", "NEGATIVE" = "lightblue", "Undeterminate" = "gray")) +  # Custom colors
  labs(title = "Weighted Proportion of Malaria Test Results by Ward in Ibadan Metropolis",
       x = "Ward", y = "Proportion of Results", fill = "Test Result") +
  theme_minimal() +
  theme(plot.title = element_text(size = 25, face = "bold"),         
        axis.title.x = element_text(size = 20, face = "bold"),       
        axis.title.y = element_text(size = 20, face = "bold"),       
        axis.text.x = element_text(size = 18, face = "bold"),        
        axis.text.y = element_text(size = 18, face = "bold"),        
        strip.text = element_text(size = 20, face = "bold"))










##net ownership
# Calculating weighted counts for net ownership
net_access_weighted <- svyby(
  ~`Does your household own a mosquitoe net?`, 
  ~Ward + `Settlement Type`, 
  survey_design, 
  svytotal, 
  na.rm = TRUE
)


# Converting to a dataframe for visualization
net_access_weighted_df <- net_access_weighted %>%
  as.data.frame() %>%
  pivot_longer(
    cols = c("`Does your household own a mosquitoe net?`1. Yes", 
             "`Does your household own a mosquitoe net?`2. No"), 
    names_to = "ITN Ownership",
    values_to = "Weighted Count"
  ) %>%
  mutate(
    ITN_Ownership = recode(`ITN Ownership`,
                           "`Does your household own a mosquitoe net?`1. Yes" = "Yes", 
                           "`Does your household own a mosquitoe net?`2. No" = "No")
  )

# Plotting the count of net availability responses per settlement and ward
ggplot(net_access_weighted_df, aes(x = `Settlement Type`, y = `Weighted Count`, fill = ITN_Ownership)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Ward, scales = "free_x") +  # Facet by Ward
  labs(title = "Weighted Net Ownership in Ibadan Metropolis",
       x = "Settlement Type", y = "Count of Responses", fill = "ITN Ownership Response") +
  theme_minimal() +
  theme(plot.title = element_text(size = 25, face = "bold"),         
         axis.title.x = element_text(size = 20, face = "bold"),       
         axis.title.y = element_text(size = 20, face = "bold"),       
         axis.text.x = element_text(size = 18, face = "bold"),        
         axis.text.y = element_text(size = 18, face = "bold"),        
         strip.text = element_text(size = 20, face = "bold"))

# Calculate proportion
net_access_weighted_df <- net_access_weighted_df %>%
  group_by(Ward, `Settlement Type`) %>%
  mutate(proportion = `Weighted Count` / sum(`Weighted Count`))

# Plotting the proportion of ITN ownership by Settlement and Ward
ggplot(net_access_weighted_df, aes(x = Ward, y = proportion, fill = ITN_Ownership)) +
  geom_bar(stat = "identity", position = "fill") +  # Use "fill" to make bars on the same range
  scale_y_continuous(labels = scales::percent) +  # Show proportions as percentages
  scale_fill_manual(values = c("No" = "red", "Yes" = "lightblue")) +
  labs(title = "Proportion of Weighted ITN Ownership by Ward in Ibadan Metropolis",
       x = "Ward", y = "Proportion", fill = "ITN Ownership") +
  theme_minimal() +
  theme(plot.title = element_text(size = 25, face = "bold"),         
        axis.title.x = element_text(size = 20, face = "bold"),       
        axis.title.y = element_text(size = 20, face = "bold"),       
        axis.text.x = element_text(size = 18, face = "bold", angle = 45, hjust = 1),  # Rotate x-axis labels for clarity
        axis.text.y = element_text(size = 18, face = "bold"),        
        strip.text = element_text(size = 20, face = "bold"))



#unweighted net ownership
# Calculating the counts and proportion of net availability responses
net_access_data <- final_merged_data %>%
  filter(!is.na(Ward), !is.na(`Does your household own a mosquitoe net?`), !is.na(`Settlement Type`))%>%
  group_by(Ward, `Settlement Type`, `Does your household own a mosquitoe net?`) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  group_by(Ward, `Settlement Type`)%>%
  mutate(proportion = count / sum(count))

# Plotting the count of net availability responses per settlement and ward
ggplot(net_access_data, aes(x = `Settlement Type`, y = count, fill = `Does your household own a mosquitoe net?`)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ Ward, scales = "free_x") +  # Facet by Ward
  labs(title = "Net Ownership in Ibadan Metropolis",
       x = "Settlement Type", y = "Count of Responses", fill = "ITN Ownership Response") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#proportion of net access by ward and settlement
ggplot(net_access_data, aes(x = `Settlement Type`, y = proportion, fill = `Does your household own a mosquitoe net?`)) +
  geom_bar(stat = "identity", position = "stack") +
  geom_text(aes(label = count), 
            position = position_stack(vjust = 0.5), size = 3) +  # Adding count labels
  facet_wrap(~ Ward, scales = "free_x") +
  scale_y_continuous(labels = scales::percent) +  # Show proportions as percentages
  scale_fill_manual(values = c("2. No" = "red", "1. Yes" = "lightblue")) +
  labs(title = "Proportion of ITN Ownership by Settlement and Ward in Ibadan Metropolis",
       x = "Settlement Type", y = "Proportion", fill = "ITN Ownership") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))






##net use

final_merged_data <- final_merged_data %>%
  mutate(`Did anyone sleep in this net last night?` = 
           if_else(is.na(`Did anyone sleep in this net last night?`), 
                   "No", 
                   `Did anyone sleep in this net last night?`))


# Calculating weighted counts for net ownership
net_use_weighted <- svyby(
  ~`Did anyone sleep in this net last night?`, 
  ~Ward, 
  survey_design, 
  svytotal, 
  na.rm = TRUE
)

# Converting to a dataframe for visualization
net_use_weighted_df <- net_use_weighted %>%
  as.data.frame() %>%
  pivot_longer(
    cols = c("`Did anyone sleep in this net last night?`Yes", 
             "`Did anyone sleep in this net last night?`No"), 
    names_to = "ITN_use",
    values_to = "Weighted Count"
  )%>%
  mutate(
    ITN_Use = recode(`ITN_use`,
                           "`Did anyone sleep in this net last night?`Yes" = "Yes", 
                           "`Did anyone sleep in this net last night?`No" = "No")
  )

# Plotting the count of net availability responses per settlement and ward
ggplot(net_use_weighted_df, aes(x = Ward, y = `Weighted Count`, fill = ITN_Use)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Weighted Net Use in Ibadan Metropolis",
       x = "Ward", y = "Number of Responses", fill = "ITN_Use") +
  theme_minimal() +
  theme(plot.title = element_text(size = 25, face = "bold"),         
        axis.title.x = element_text(size = 20, face = "bold"),       
        axis.title.y = element_text(size = 20, face = "bold"),       
        axis.text.x = element_text(size = 18, face = "bold", angle = 45, hjust = 1),  # Rotate x-axis labels for clarity
        axis.text.y = element_text(size = 18, face = "bold"),        
        strip.text = element_text(size = 20, face = "bold"))

# Calculate proportion
net_use_weighted_df <- net_use_weighted_df %>%
  group_by(Ward) %>%
  mutate(proportion = `Weighted Count` / sum(`Weighted Count`))

# Plotting the proportion of ITN ownership by Settlement and Ward
ggplot(net_use_weighted_df, aes(x = Ward, y = proportion, fill = ITN_Use)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_y_continuous(labels = scales::percent) +  # Show proportions as percentages
  scale_fill_manual(values = c("No" = "red", "Yes" = "lightblue")) +
  labs(title = "Proportion of Weighted ITN Use by Ward in Ibadan Metropolis",
       x = "Ward", y = "Proportion", fill = "ITN_Use") +
  theme_minimal() +
  theme(plot.title = element_text(size = 25, face = "bold"),         
        axis.title.x = element_text(size = 20, face = "bold"),       
        axis.title.y = element_text(size = 20, face = "bold"),       
        axis.text.x = element_text(size = 18, face = "bold", angle = 45, hjust = 1),  # Rotate x-axis labels for clarity
        axis.text.y = element_text(size = 18, face = "bold"),        
        strip.text = element_text(size = 20, face = "bold"))








#unweighted net use
net_use <- final_merged_data %>%
  filter(!is.na(`Did anyone sleep in this net last night?`))%>%
  group_by(Ward, `Settlement Type`) %>%
  summarise(count_net_use = n()) %>%
  arrange(Ward)

# Plotting the bar chart
ggplot(net_use, aes(x = Ward, y = count_net_use, fill = `Settlement Type`)) +
  geom_bar(stat = "identity", position = "dodge") + 
  labs(title = "Number of Individuals who used ITN per Settlement And Ward in Ibadan Metropolis",
       x = "Ward", y = "Number", fill = "Settlement Type") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



# Calculate the count of "Yes" and total responses (including NAs)
net_use_tpr <- final_merged_data %>%
  group_by(Ward) %>%
  summarise(
    Count_Yes = sum(`Did anyone sleep in this net last night?` == "Yes", na.rm = TRUE),
    Count_No = sum(`Did anyone sleep in this net last night?` == "No", na.rm = TRUE),
    Total_Count = Count_Yes + Count_No,
    Count_Positive = sum(`Test Result` == "POSITIVE", na.rm = TRUE),
    Total_Test_Count = sum(!is.na(`Test Result`)),  # Total number of tests performed (excluding NAs)
    Proportion_Positive = Count_Positive / Total_Test_Count
  ) %>%
  mutate(
    Proportion_Yes = Count_Yes / Total_Count,
    Proportion_No = Count_No / Total_Count
  )

# Reshape the data to have both Proportion_Yes and Proportion_No in one column
net_use_tpr_long <- net_use_tpr %>%
  pivot_longer(cols = c(Proportion_Yes, Proportion_No), 
               names_to = "Net_Use", 
               values_to = "Proportion") %>%
  mutate(Net_Use = if_else(Net_Use == "Proportion_Yes", "Yes", "No"))

# Create the plot
ggplot(net_use_tpr, aes(x = Proportion_Yes, y = Proportion_Positive, color = Ward)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add a trend line
  labs(title = "Scattered plot of Net Use and Test Positivity",
       x = "Proportion of Net Use",
       y = "Proportion of Positive Test Results") +
  theme_minimal() +
  theme(plot.title = element_text(size = 18, face = "bold"),         
        axis.title.x = element_text(size = 14, face = "bold"),       
        axis.title.y = element_text(size = 14, face = "bold"),       
        axis.text.x = element_text(size = 16, face = "bold"),        
        axis.text.y = element_text(size = 16, face = "bold"),        
        strip.text = element_text(size = 14, face = "bold") ) +
  scale_color_manual(values = c("agugu" = "blue", "bashorun" = "green", "challenge" = "orange", "olopomewa" = "purple"))




ggplot(net_use_tpr_long, aes(x = Proportion, y = Proportion_Positive, color = Ward, shape = Net_Use)) +
  geom_point(size = 3) +
  geom_smooth(aes(group = 1), method = "lm", se = FALSE, color = "red") +  # Add a single trend line for all points
  labs(title = "Scatter plot of Net Use and Test Positivity",
       x = "Proportion of Net Use",
       y = "Proportion of Positive Test Results") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = c("agugu" = "blue", "bashorun" = "green", "challenge" = "orange", "olopomewa" = "purple")) +
  scale_shape_manual(values = c("Yes" = 16, "No" = 17))  # Different shapes for Yes and No

#weighted
net_use_tpr_weighted <- final_merged_data %>%
  group_by(Ward) %>%
  summarise(
    Count_Yes = sum(`Did anyone sleep in this net last night?` == "Yes", na.rm = TRUE),
    Total_Weight = sum(overall_hh_weight, na.rm = TRUE),
    Weighted_Net_Use = sum(ifelse(`Did anyone sleep in this net last night?` == "Yes", overall_hh_weight, 0), na.rm = TRUE) / Total_Weight,
    Count_Positive = sum(`Test Result` == "POSITIVE", na.rm = TRUE),
    Total_Test_Weight = sum(overall_hh_weight, na.rm = TRUE),
    Weighted_Positive = sum(ifelse(`Test Result` == "POSITIVE", overall_hh_weight, 0), na.rm = TRUE) / Total_Test_Weight
  )


# Plot the weighted scatter plot
ggplot(net_use_tpr_weighted, aes(x = Weighted_Net_Use, y = Weighted_Positive, color = Ward)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add a trend line
  labs(title = "Weighted Scattered Plot Net Use and Test Positivity by Ward",
       x = "Weighted Proportion of Net Use",
       y = "Weighted Proportion of Positive Test Results") +
  theme_minimal() +
  theme(
  plot.title = element_text(size = 18, face = "bold"),         
  axis.title.x = element_text(size = 14, face = "bold"),       
  axis.title.y = element_text(size = 14, face = "bold"),       
  axis.text.x = element_text(size = 16, face = "bold"),        
  axis.text.y = element_text(size = 16, face = "bold"),        
  strip.text = element_text(size = 14, face = "bold")          
)












#weighted net access and TPR
net_access_tpr_weighted <- final_merged_data %>%
  group_by(Ward) %>%
  summarise(
    Count_Yes = sum(`Does your household own a mosquitoe net?` == "1. Yes", na.rm = TRUE),
    Total_Weight = sum(overall_hh_weight, na.rm = TRUE),
    Weighted_Net_Acess = sum(ifelse(`Does your household own a mosquitoe net?` == "1. Yes", overall_hh_weight, 0), na.rm = TRUE) / Total_Weight,
    Count_Positive = sum(`Test Result` == "POSITIVE", na.rm = TRUE),
    Total_Test_Weight = sum(overall_hh_weight, na.rm = TRUE),
    Weighted_Positive = sum(ifelse(`Test Result` == "POSITIVE", overall_hh_weight, 0), na.rm = TRUE) / Total_Test_Weight
  )


# Plot the weighted scatter plot
ggplot(net_access_tpr_weighted, aes(x = Weighted_Net_Acess, y = Weighted_Positive, color = Ward)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add a trend line
  labs(title = "Weighted Relationship between Net Access and Test Positivity by Ward",
       x = "Weighted Proportion of Net Access",
       y = "Weighted Proportion of Positive Test Results") +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 18, face = "bold"),         
    axis.title.x = element_text(size = 14, face = "bold"),       
    axis.title.y = element_text(size = 14, face = "bold"),       
    axis.text.x = element_text(size = 16, face = "bold"),        
    axis.text.y = element_text(size = 16, face = "bold"),        
    strip.text = element_text(size = 14, face = "bold")          
  )
