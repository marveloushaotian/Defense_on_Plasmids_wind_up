# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Read the main table from CSV file
master_table <- read.csv("ptu_def_length.csv")

# Data preprocessing
pcn_data <- master_table %>%
  drop_na(NUCCORE_Length) %>%
  filter(NUCCORE_Length > 0) %>%  # Remove invalid NUCCORE_Length values
  mutate(
    num_def_sys = ifelse(is.na(num_def_sys), 0, num_def_sys),
    log10NUCCORE_Length = log10(NUCCORE_Length)
  )

# Calculate the median log10NUCCORE_Length for each PTU
mean_log10NUCCORE_Length <- pcn_data %>%
  group_by(`PTU_sHSBM_10`) %>%
  summarise(median_log10NUCCORE_Length = median(log10NUCCORE_Length, na.rm = TRUE)) %>%
  arrange(desc(median_log10NUCCORE_Length))

# Get categories sorted by median
sorted_categories <- mean_log10NUCCORE_Length$`PTU_sHSBM_10`

# Ensure consistent category order
pcn_data <- pcn_data %>%
  mutate(PTU_sHSBM_10 = factor(PTU_sHSBM_10, levels = sorted_categories))

# Save processed data for plotting
write.csv(pcn_data, "intermediate_data_for_log_plasmid_length_plot.csv", row.names = FALSE)

# Plot data
p <- ggplot() +
  # Boxplot with light blue color
  geom_boxplot(data = pcn_data, 
               aes(x = log10NUCCORE_Length, y = `PTU_sHSBM_10`),
               width = 0.7, 
               fill = "lightblue",  # Set fill color to light blue
               color = "black", 
               outlier.size = 0.5) +
  # Axis settings
  scale_x_continuous(
    name = "log10 Plasmid Length"
  ) +
  # Other plot settings
  labs(
    y = "PTU Classification",
    title = "Boxplot of log10 Plasmid Length"
  ) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  ) +
  # Reverse the y-axis to flip the plot vertically
  scale_y_discrete(limits = rev(levels(pcn_data$PTU_sHSBM_10)))

# Save plot
ggsave("log_plasmid_length.pdf", p, width = 6, height = 80, units = "in", dpi = 300, limitsize = FALSE)
