#net use
# Step 1: Clean the data
df_clean <- final_merged_data %>%
  # Replace NA values in `Did anyone sleep in this net last night?` with "No"
  mutate(`Did anyone sleep in this net last night?` = ifelse(is.na(`Did anyone sleep in this net last night?`), 
                                                             "No", 
                                                             "Yes")) %>%
  # Filter relevant cases and drop NAs
  filter(`Test Result` %in% c("POSITIVE", "Undeterminate", "NEGATIVE"),
         `Does your household own a mosquitoe net?` == "1. Yes") %>%
  drop_na(`Test Result`, `Did anyone sleep in this net last night?`) %>%
  
  # Convert prevalence to binary and create a net use factor
  mutate(prevalence_binary = ifelse(`Test Result` == "POSITIVE", 1, 0),
         net_use = factor(`Did anyone sleep in this net last night?`, 
                          levels = c("Yes", "No"))) 


# Step 3: Create a survey design object
survey_design <- svydesign(id = ~enumeration_area, data = df_clean, weights = ~overall_hh_weight)

# Step 2: Apply logistic regression for each ward, handling errors
ward_models <- df_clean %>%
  group_by(Ward) %>%
  group_modify(~ {
    tryCatch({
      # Create a subset of the survey design for the specific ward
      ward_design <- subset(survey_design, Ward == .y$Ward)
      # Fit the logistic model using svyglm
      model <- svyglm(prevalence_binary ~ net_use, design = ward_design, family = binomial)
      # Extract model statistics if the model is successful
      model_stats <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
      model_stats  # Return the model statistics
    }, error = function(e) {
      # If there is an error, return an empty tibble
      tibble()
    })
  }) %>%
  ungroup()


# Step 3: Plot the odds ratios and confidence intervals, faceted by Ward
ggplot(ward_models, aes(x = term, y = estimate)) +
  geom_point(size = 3, color = "blue") +  # Plot the OR estimates
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "red") +  # Plot the CI
  labs(
    title = "Relationship Between Net Use and Malaria by Ward in Ibadan Metropolis (Weighted)",
    x = "Net Use Category",
    y = "Odds Ratio"
  ) +
  theme_minimal() +
  coord_flip() +
  facet_wrap(~ Ward, scales = "free_y") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),         
    axis.title.x = element_text(size = 14, face = "bold"),       
    axis.title.y = element_text(size = 14, face = "bold"),       
    axis.text.x = element_text(size = 12, face = "bold"),        
    axis.text.y = element_text(size = 12, face = "bold"),        
    strip.text = element_text(size = 14, face = "bold")          
  )





#net use
# Step 1: Clean the data
df_clean_access <- final_merged_data %>%
  # Keep only "POSITIVE", "Undeterminate", and "NEGATIVE" cases for binary logistic regression
  filter(`Test Result` %in% c("POSITIVE", "Undeterminate", "NEGATIVE")) %>%
  # Remove rows with NA values in relevant columns (if any left)
  drop_na(`Test Result`, `Does your household own a mosquitoe net?`) %>%
  # Convert prevalence to binary: 1 = positive, 0 = negative
  mutate(prevalence_binary = ifelse(`Test Result` == "POSITIVE", 1, 0),
         net_access = factor(`Does your household own a mosquitoe net?`))

# Step 3: Create a survey design object
survey_design <- svydesign(id = ~enumeration_area, data = df_clean_access, weights = ~overall_hh_weight)

# Step 2: Apply logistic regression for each ward, handling errors
ward_models1 <- df_clean_access %>%
  group_by(Ward) %>%
  group_modify(~ {
    tryCatch({
      # Create a subset of the survey design for the specific ward
      ward_design <- subset(survey_design, Ward == .y$Ward)
      # Fit the logistic model using svyglm
      model <- svyglm(prevalence_binary ~ net_access, design = ward_design, family = binomial)
      # Extract model statistics if the model is successful
      model_stats <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
      model_stats  # Return the model statistics
    }, error = function(e) {
      # If there is an error, return an empty tibble
      tibble()
    })
  }) %>%
  ungroup()


# Step 3: Plot the odds ratios and confidence intervals, faceted by Ward
ggplot(ward_models1, aes(x = term, y = estimate)) +
  geom_point(size = 3, color = "blue") +  # Plot the OR estimates
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "red") +  # Plot the CI
  labs(
    title = "Relationship Between Net Access and Malaria by Ward in Ibadan Metropolis (Weighted)",
    x = "Net Access Category",
    y = "Odds Ratio"
  ) +
  theme_minimal() +
  coord_flip() +
  facet_wrap(~ Ward, scales = "free_y") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),         
    axis.title.x = element_text(size = 14, face = "bold"),       
    axis.title.y = element_text(size = 14, face = "bold"),       
    axis.text.x = element_text(size = 12, face = "bold"),        
    axis.text.y = element_text(size = 12, face = "bold"),        
    strip.text = element_text(size = 14, face = "bold")          
  )





#adjusted

# Step 1: Clean the data
df_clean_combined2 <- final_merged_data %>%
  # Replace NA values in `Did anyone sleep in this net last night?` with "No"
  mutate(`Did anyone sleep in this net last night?` = ifelse(is.na(`Did anyone sleep in this net last night?`), 
                                                             "No", 
                                                             `Did anyone sleep in this net last night?`)) %>%
  # Filter relevant cases and drop NAs
  filter(`Test Result` %in% c("POSITIVE", "Undeterminate", "NEGATIVE")) %>%
  drop_na(`Test Result`, `Did anyone sleep in this net last night?`, `Does your household own a mosquitoe net?`) %>%
  # Convert prevalence to binary, create net use and net access factors
  mutate(
    prevalence_binary = ifelse(`Test Result` == "POSITIVE", 1, 0),
    net_use = factor(`Did anyone sleep in this net last night?`, levels = c("Yes", "No")),
    net_access = factor(`Does your household own a mosquitoe net?`, levels = c("1. Yes", "2. No"))
  )

# Step 2: Create a survey design object
survey_design_combined <- svydesign(id = ~enumeration_area, data = df_clean_combined, weights = ~overall_hh_weight)

# Step 3: Apply logistic regression for each ward, handling errors
ward_models_combined <- df_clean_combined %>%
  group_by(Ward) %>%
  group_modify(~ {
    tryCatch({
      # Create a subset of the survey design for the specific ward
      ward_design <- subset(survey_design_combined, Ward == .y$Ward)
      # Fit the logistic model using svyglm with both net_use and net_access
      model <- svyglm(prevalence_binary ~ net_use + net_access, design = ward_design, family = binomial)
      # Extract model statistics if the model is successful
      model_stats <- tidy(model, exponentiate = TRUE, conf.int = TRUE)
      model_stats  # Return the model statistics
    }, error = function(e) {
      # If there is an error, return an empty tibble
      tibble()
    })
  }) %>%
  ungroup()


# Step 4: Plot the odds ratios and confidence intervals, faceted by Ward
ggplot(ward_models_combined, aes(x = term, y = estimate)) +
  geom_point(size = 3, color = "blue") +  # Plot the OR estimates
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, color = "red") +  # Plot the CI
  labs(
    title = "Adjusted Relationship Between Net Use, Net Access, and Malaria by Ward in Ibadan Metropolis (Weighted)",
    x = "Predictor",
    y = "Odds Ratio"
  ) +
  theme_minimal() +
  coord_flip() +
  facet_wrap(~ Ward, scales = "free_y") +
  theme(
    plot.title = element_text(size = 18, face = "bold"),         
    axis.title.x = element_text(size = 14, face = "bold"),       
    axis.title.y = element_text(size = 14, face = "bold"),       
    axis.text.x = element_text(size = 16, face = "bold"),        
    axis.text.y = element_text(size = 16, face = "bold"),        
    strip.text = element_text(size = 14, face = "bold")          
  )
