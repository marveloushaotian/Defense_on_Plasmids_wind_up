# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

# Define the directory containing the CSV files
directory <- "Collect/Split_by_Defense_Type/ANI_all_plasmid"

# List all CSV files in the directory
file_list <- list.files(directory, pattern = "*.csv", full.names = TRUE)

# Initialize an empty list to store data frames
data_list <- list()

# Read each file and store the data frame in the list
for (file in file_list) {
  data <- read_csv(file, col_types = cols(ANI = col_double()))  # Ensure ANI is read as double
  data$file_name <- tools::file_path_sans_ext(basename(file))  # Add a column for the file name without extension
  data_list[[file]] <- data
}

# Combine all data frames into one
combined_data <- bind_rows(data_list)

# Save the combined data for plotting
write_csv(combined_data, "Results/Defense_Type_Group/intermediate_data_for_plasmid_ANI_plot.csv")

# Create the violin plot with swapped axes
p <- ggplot(combined_data, aes(x = ANI, y = file_name)) +
  geom_violin(trim = FALSE, fill = "lightblue", bw = 0.1, scale = "width") +
  scale_x_continuous(limits = c(80, 100)) +  # Set x-axis limits to 80 to 100
  # geom_jitter(height = 0.3, width = 0.0, alpha = 0.5, size = 0.03) +
  geom_boxplot(width = 0.15, notch = FALSE, outlier.size = 1, color = "black") +
  stat_summary(fun = median, geom = "point", shape = 20, size = 2, color = "#dc5772") +
  labs(
    x = "ANI Distance",
    y = "Defense Type",
    title = "Pairwise Distance of Plasmid ANI"
  ) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10, face = "bold"),
    axis.text.y = element_text(size = 10 , face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )

# Save the plot
ggsave("Results/Defense_Type_Group/pairwise_ANI_of_plasmid.pdf", p, width = 10, height = length(file_list) * 0.3, units = "in", dpi = 300, limitsize = FALSE)
