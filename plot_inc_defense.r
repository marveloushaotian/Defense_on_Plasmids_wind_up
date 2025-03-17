# Load required libraries
library(ggplot2)
library(dplyr)

# Read the CSV file
data <- read.csv("Collect/scatter_muti_df_muti_inc.csv")

# Count Inc_Family frequencies for each Defense_Type
freq_data <- data %>%
  group_by(Defense_Type, Inc_Family) %>%
  summarise(count = n(), .groups = 'drop')

# Create stacked bar plot
ggplot(freq_data, aes(x = Defense_Type, y = count, fill = Inc_Family)) +
geom_bar(stat = "identity", position = "stack") +
  theme_minimal() +
  labs(title = "Distribution of Inc_Family by Defense_Type",
       x = "Defense Type",
       y = "Count",
       fill = "Inc Family") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("Collect/inc_defense_plot.png", width = 300, height = 300, dpi = 300, limitsize = FALSE)