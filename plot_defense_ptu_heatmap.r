# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Read the data, ensuring proper handling of missing values
data <- read.csv("Collect/master_extract_defense_ptu.csv", stringsAsFactors = FALSE, na.strings = c("", "NA"))

# Remove rows with any NA values in the relevant columns
data <- data %>% drop_na(Type, Subtype, PTU)

# Create a count table for the first plot (Type vs PTU)
count_table_type_ptu <- data %>%
  group_by(Type, PTU) %>%
  summarise(count = n()) %>%
  ungroup()

# Plot the first plot using ggplot2 with circles and size representing the count
plot_type_ptu <- ggplot(count_table_type_ptu, aes(x = Type, y = PTU, size = count)) +
  geom_point(shape = 21, fill = "#434d91", color = "black") +
  scale_size_continuous(name = "Count", breaks = seq(0, max(count_table_type_ptu$count), by = 100)) + # More detailed legend
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_blank())

# Save the first plot
ggsave("Results/PTU_Group/plot_type_ptu.pdf", plot = plot_type_ptu, width = 25, height = 25)

# Create a count table for the second plot (Subtype vs PTU)
count_table_subtype_ptu <- data %>%
  group_by(Subtype, PTU) %>%
  summarise(count = n()) %>%
  ungroup()

# Plot the second plot using ggplot2 with circles and size representing the count
plot_subtype_ptu <- ggplot(count_table_subtype_ptu, aes(x = Subtype, y = PTU, size = count)) +
  geom_point(shape = 21, fill = "#434d91", color = "black") +
  scale_size_continuous(name = "Count", breaks = seq(0, max(count_table_subtype_ptu$count), by = 100)) + # More detailed legend
  theme_bw() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
        axis.text.y = element_text(face = "bold"),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title = element_blank())

# Save the second plot
ggsave("Results/PTU_Group/plot_subtype_ptu.pdf", plot = plot_subtype_ptu, width = 25, height = 25)

# Print a message indicating the script has completed
cat("Plots have been generated and saved as 'Results/PTU_Group/plot_type_ptu.pdf' and 'Results/PTU_Group/plot_subtype_ptu.pdf'.\n")
