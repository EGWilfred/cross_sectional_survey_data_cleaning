library(dplyr)
library(ggplot2)
library(forcats)

ibadan_data <- read.csv("C:/Users/lml6626/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/Field data/Ibadan_data/UrbanMalariaHousehol-HouseholdSummary_DATA_LABELS_2024-01-05_1908.csv")




perform_univariate_analysis <- function(data) {
  data_summary <- data %>%
    summarise_all(function(x) {
      if (is.numeric(x)) {
        list(Min = min(x, na.rm = TRUE),
             Max = max(x, na.rm = TRUE),
             Mean = mean(x, na.rm = TRUE),
             Median = median(x, na.rm = TRUE),
             '1st Qu' = quantile(x, 0.25, na.rm = TRUE),
             '3rd Qu' = quantile(x, 0.75, na.rm = TRUE),
             SD = sd(x, na.rm = TRUE))
      } else {
        list(Unique = length(unique(x)),
             'Most Common' = names(sort(table(x), decreasing = TRUE)[1]),
             Frequency = sort(table(x), decreasing = TRUE)[1])
      }
    })
  
  print(data_summary)
  
  for (column_name in names(data)) {
    column_data <- data[[column_name]]
    if (is.numeric(column_data)) {
      ggplot(data, aes_string(x = column_name)) +
        geom_histogram(binwidth = diff(range(column_data, na.rm = TRUE))/30, fill = "skyblue", color = "black") +
        theme_minimal() +
        ggtitle(paste("Distribution of", column_name))
    } else {
      ggplot(data, aes_string(x = column_name)) +
        geom_bar(fill = "skyblue", color = "black") +
        theme_minimal() +
        ggtitle(paste("Frequency of", column_name))
    }
    print(ggplot2::last_plot())
  }
}


ibadan_data_summary <- ibadan_data %>% 
  group_by(Ward) %>% 
  count()


Nigeria_stats <- data.frame(country  = c("Nigeria", "Others", "Nigeria", "Others"),
                            percentage = c(26.8, 100 - 26.8, 31.1, 100 - 31.1),
                            outcome = c("malaria cases", "malaria cases", "malaria deaths", "malaria deaths"))%>%
  mutate(country = fct_reorder(country, percentage))%>%
  group_by(outcome) %>% 
  mutate(label_position = cumsum(percentage) - (0.5 * percentage))

Nigeria_stats$country <- factor(Nigeria_stats$country, levels = rev(levels(Nigeria_stats$country)))







# Your ggplot code
ggplot(Nigeria_stats, aes(fill = country, y = percentage, x = outcome)) + 
  geom_bar(position = 'stack', stat = 'identity') +
  geom_text(aes(y = label_position, label = percentage), vjust = 0.5) +
  scale_fill_manual(values = c("#fbb4ae", "#b3cde3")) +
  theme_minimal() +
  theme(panel.background = element_blank(),
        legend.title = element_blank(),
        text = element_text(size = 24), 
        legend.position = "bottom")




ggsave("C:/Users/lml6626/Urban Malaria Proj Dropbox/Laurette Mhlanga/proposed papers/Pictures/burden.png",  dpi = 350, width = 8, height = 6)
