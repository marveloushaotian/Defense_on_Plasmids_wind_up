library(ggplot2)
library(dplyr)
library(purrr)

input_dir <- "Split_by_PTU/Cosine"

files <- list.files(input_dir, full.names = TRUE)
print(paste("Found", length(files), "files"))

all_data <- list()

for (file in files) {
  file_name <- basename(file)
  plot_title <- tools::file_path_sans_ext(file_name)
  plot_title <- sub("cosine_matrix_", "", plot_title)
  print(paste("Processing file:", file_name))

  mat <- tryCatch(
    {
      df <- read.csv(file, header = TRUE, row.names = NULL)
      if(all(grepl("^\\d+$", df[,1]))) {
        df <- df[,-1]
      }
      as.matrix(df)
    },
    error = function(e) {
      warning(paste("Failed to read file:", file, "-", e$message))
      return(NULL)
    }
  )

  if (is.null(mat) || nrow(mat) == 0 || ncol(mat) == 0) {
    warning(paste("Skipping empty or invalid file:", file))
    next
  }

  upper_indices <- which(upper.tri(mat, diag = FALSE), arr.ind = TRUE)
  cosine_values <- mat[upper_indices]

  cosine_values <- cosine_values[cosine_values != 0]

  if (length(cosine_values) == 0) {
    warning(paste("No non-zero values in file:", file))
    next
  }

  all_data[[plot_title]] <- data.frame(
    filename = plot_title,
    Value = cosine_values
  )
}

combined_data <- do.call(rbind, all_data)

write.csv(combined_data, "intermediate_data_for_cosine_plot.csv", row.names = FALSE)

print("Data range:")
print(summary(combined_data$Value))

combined_data_filtered <- combined_data %>%
  filter(Value >= 0 & Value <= 1)

print(paste("Original data points:", nrow(combined_data)))
print(paste("Filtered data points:", nrow(combined_data_filtered)))

data_counts <- combined_data_filtered %>%
  group_by(filename) %>%
  summarise(
    count = n(),
    median_val = median(Value)
  ) %>%
  arrange(desc(median_val))

combined_data_filtered$filename <- factor(combined_data_filtered$filename, 
                                        levels = data_counts$filename)

p <- ggplot(combined_data_filtered, aes(x = Value, y = filename)) +
  geom_violin(fill = "lightblue", color = "black", alpha = 0.7, scale = "width") +
  scale_x_continuous(limits = c(0, 1), 
                    breaks = seq(0, 1, 0.2), 
                    expand = c(0.01, 0)) +
  geom_boxplot(width = 0.15, notch = FALSE, outlier.size = 1, color = "black", alpha = 0.7) +
  stat_summary(fun = median, geom = "point", shape = 20, size = 2, color = "#dc5772") +
  labs(
    x = "Cosine Similarity",
    y = "PTU Classification",
    title = "Pairwise Cosine Similarities of Defense Systems Harbored"
  ) +
  theme_bw() +
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold"),
    axis.text.x = element_text(size = 10, face = "bold"),
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    plot.margin = margin(t = 20, r = 20, b = 20, l = 20, unit = "pt")
  )

n_files <- length(unique(combined_data_filtered$filename))
plot_height <- max(8, n_files * 0.3)

output_file <- file.path("Results/PTU_Group/pairwise_cosine_similarities_of_defense_systems_harbored.pdf")
print(paste("Saving combined plot to:", output_file))
ggsave(output_file, plot = p, width = 10, height = plot_height, limitsize = FALSE)

print("Combined plot has been generated and saved!")