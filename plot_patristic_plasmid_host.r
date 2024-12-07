library(ggplot2)
library(dplyr)

# 1. Get the list of files
file_list <- list.files(path = "Plasmid_Host_Breath/Patristic_distance_0", pattern = "*.csv", full.names = TRUE)

# 2. Initialize an empty data frame to store all data
all_data <- data.frame()

# 3. Process each file in the list
for (file in file_list) {
  tryCatch({
    # 3.1 Read the CSV file
    data <- read.csv(file)
    
    # 3.2 Check if the key column exists
    if (!"NUCCORE_ACC_updated_pair" %in% colnames(data)) {
      message(sprintf("Skipping file %s: NUCCORE_ACC_updated_pair column not found", file))
      next
    }
    
    # 3.3 Filter data and convert types
    data_filtered <- data %>%
      filter(!is.na(Distance), !is.na(NUCCORE_ACC_updated_pair)) %>%
      mutate(NUCCORE_ACC_updated_pair = as.character(NUCCORE_ACC_updated_pair))
    
    # 3.4 Extract the number of unique species
    unique_species <- data_filtered %>%
      pull(NUCCORE_ACC_updated_pair) %>%
      strsplit(split = "-") %>%
      unlist() %>%
      unique() %>%
      length()
    
    # 3.5 If the number of species is greater than 10, add to all_data
    if (unique_species > 10) {
      # Add a column for the file name with unique species count to distinguish data
      data_filtered <- data_filtered %>%
        mutate(File = paste0(sub(".*distance_(.*)\\.csv", "\\1", basename(file)), " (n=", unique_species, ")"))
      
      # Append to the all_data data frame
      all_data <- bind_rows(all_data, data_filtered)
    }
  }, error = function(e) {
    message(sprintf("Error processing file %s: %s", file, e$message))
  })
}

all_data$File <- factor(all_data$File)

# 4. Save the combined data for plotting
if (nrow(all_data) > 0) {
  write.csv(all_data, "intermediate_data_for_pairwise_patristic_distance_of_plasmid_host_plot.csv", row.names = FALSE)
}

# 5. Plot all data together if all_data is not empty
if (nrow(all_data) > 0) {
  p <- ggplot(all_data, aes(x = Distance, y = File)) +
    geom_violin(trim = FALSE, fill = "lightblue", bw = 0.1, scale = "width") +
    scale_x_continuous(limits = c(0, NA)) +
    # geom_jitter(height = 0.3, width = 0.0, alpha = 0.5, size = 0.1) +
    geom_boxplot(width = 0.15, notch = FALSE, outlier.size = 1, color = "black") +
    stat_summary(fun = median, geom = "point", shape = 20, size = 2, color = "#dc5772") +
    labs(
      x = "Patristic Distance",
      y = "PTU Classification",
      title = "Pairwise Distance of Plasmid Host"
    ) +
    theme_bw() +
    theme(axis.title.y = element_text(size = 12, face = "bold"),
          axis.text.y = element_text(size = 14, face = "bold"),
          axis.title.x = element_text(size = 14, face = "bold"),
          axis.text.x = element_text(size = 10, face = "bold"),
          plot.title = element_text(hjust = 0.5, size = 14, face = "bold"))
  
  # 6. Save the combined plot as a vertical PNG
  ggsave(filename = "pairwise_patristic_distance_of_plasmid_host.pdf", 
       plot = p, 
       width = 8,
       height = 100,
       dpi = 600,
       limitsize = FALSE)
}
