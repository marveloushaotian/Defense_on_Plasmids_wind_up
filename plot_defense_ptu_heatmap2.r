# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)

# Read the data, ensuring proper handling of missing values
data <- read.csv("Collect/master_extract_defense_ptu.csv", stringsAsFactors = FALSE, na.strings = c("", "NA"))

# Remove rows with any NA values in the relevant columns
data <- data %>% drop_na(Type, Subtype, PTU)

# Calculate total counts for each Type to determine sorting order
type_totals <- data %>%
  group_by(Type) %>%
  summarise(total_count = n()) %>%
  arrange(desc(total_count))

# Create a count table for the first plot (Type vs PTU)
count_table_type_ptu <- data %>% 
  group_by(Type, PTU) %>% 
  summarise(count = n()) %>% 
  ungroup()

# Create a complete grid of all Type and PTU combinations
all_types <- unique(data$Type)
all_ptus <- unique(data$PTU)
complete_grid_type <- expand.grid(Type = all_types, PTU = all_ptus)

# Join with the count data, filling NA with 0
count_table_type_ptu_complete <- left_join(complete_grid_type, count_table_type_ptu, by = c("Type", "PTU")) %>%
  mutate(count = ifelse(is.na(count), 0, count))

# Convert Type to a factor with levels ordered by total count
count_table_type_ptu_complete$Type <- factor(count_table_type_ptu_complete$Type, 
                                            levels = type_totals$Type)

# Plot the first plot using ggplot2 with heatmap tiles
plot_type_ptu <- ggplot(count_table_type_ptu_complete, aes(x = Type, y = PTU, fill = count)) + 
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradientn(
    name = "Count",
    colors = c("#eaeeea", "#d5c3d5", "#ceb7ce", "#c6a9c6", "#be9bbc", 
               "#b58db3", "#af82ac", "#aa7aa7", "#a775a4", "#a673a3"),
    values = scales::rescale(c(0, 1, 5, 10, 50, 100, 200, 500, 1000, 2000)),
    na.value = "#eaeeea"
  ) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(fill = NA, color = "white", linewidth = 1),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.15, "cm")
  )

# Save the first plot
ggsave("Results/PTU_Group/plot_type_ptu.pdf", plot = plot_type_ptu, width = 15, height = 25)

# Calculate total counts for each Subtype to determine sorting order
subtype_totals <- data %>%
  group_by(Subtype) %>%
  summarise(total_count = n()) %>%
  arrange(desc(total_count))

# Create a count table for the second plot (Subtype vs PTU)
count_table_subtype_ptu <- data %>% 
  group_by(Subtype, PTU) %>% 
  summarise(count = n()) %>% 
  ungroup()

# Create a complete grid of all Subtype and PTU combinations
all_subtypes <- unique(data$Subtype)
complete_grid_subtype <- expand.grid(Subtype = all_subtypes, PTU = all_ptus)

# Join with the count data, filling NA with 0
count_table_subtype_ptu_complete <- left_join(complete_grid_subtype, count_table_subtype_ptu, by = c("Subtype", "PTU")) %>%
  mutate(count = ifelse(is.na(count), 0, count))

# Convert Subtype to a factor with levels ordered by total count
count_table_subtype_ptu_complete$Subtype <- factor(count_table_subtype_ptu_complete$Subtype, 
                                                 levels = subtype_totals$Subtype)

# Plot the second plot using ggplot2 with heatmap tiles
plot_subtype_ptu <- ggplot(count_table_subtype_ptu_complete, aes(x = Subtype, y = PTU, fill = count)) + 
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradientn(
    name = "Count",
    colors = c("#eaeeea", "#d5c3d5", "#ceb7ce", "#c6a9c6", "#be9bbc", 
               "#b58db3", "#af82ac", "#aa7aa7", "#a775a4", "#a673a3"),
    values = scales::rescale(c(0, 1, 5, 10, 50, 100, 200, 500, 1000, 2000)),
    na.value = "#eaeeea"
  ) + 
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "bold"),
    axis.text.y = element_text(face = "bold"),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(fill = NA, color = "white", linewidth = 1),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.15, "cm")
  )

# Save the second plot
ggsave("Results/PTU_Group/plot_subtype_ptu.pdf", plot = plot_subtype_ptu, width = 17, height = 25)

# Print a message indicating the script has completed
cat("Plots have been generated and saved as 'Results/PTU_Group/plot_type_ptu.pdf' and 'Results/PTU_Group/plot_subtype_ptu.pdf'.\n")