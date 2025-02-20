library(ggplot2)
library(dplyr)
library(tidyr)
library(readr)

directory <- "Collect/Defense_Subtype_Group/ANI_all_plasmid"

file_list <- list.files(directory, pattern = "*.csv", full.names = TRUE)

data_list <- list()

for (file in file_list) {
  data <- read_csv(file, col_types = cols(ANI = col_double()))
  data$file_name <- tools::file_path_sans_ext(basename(file))
  data_list[[file]] <- data
}

combined_data <- bind_rows(data_list)

write_csv(combined_data, "Collect/Defense_Subtype_Group/intermediate_data_for_plasmid_ANI_plot.csv")

p <- ggplot(combined_data, aes(x = ANI, y = file_name)) +
  geom_violin(trim = FALSE, fill = "lightblue", bw = 0.1, scale = "width") +
  scale_x_continuous(limits = c(80, 100)) +
  geom_boxplot(width = 0.15, notch = FALSE, outlier.size = 1, color = "black") +
  stat_summary(fun = median, geom = "point", shape = 20, size = 2, color = "#dc5772") +
  labs(
    x = "ANI Distance",
    y = "Defense Subtype",
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

ggsave("Results/Defense_Subtype_Group/pairwise_ANI_of_plasmid.pdf", p, width = 10, height = length(file_list) * 0.3, units = "in", dpi = 300, limitsize = FALSE)
