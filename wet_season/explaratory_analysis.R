library(dplyr)
library(ggplot2)
library(forcats)

ibadan_data <- read.csv("C:/Users/lml6626/Urban Malaria Proj Dropbox/urban_malaria/data/nigeria/kano_ibadan_epi/Field data/Ibadan_data/UrbanMalariaHousehol-HouseholdSummary_DATA_LABELS_2024-01-05_1908.csv")


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
