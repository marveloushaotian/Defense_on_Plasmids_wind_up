library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)

data <- read.csv("Collect/all_imtermediate_files.csv")

complete_y <- unique(data$PTU_Group)

generate_plot <- function(value_group, x_label, x_limits, title) {
  plot_data <- data %>% filter(Value_Group == value_group)
  
  if (nrow(plot_data) == 0) {
    return(NULL)
  }
  
  plot_data <- plot_data %>%
    complete(PTU_Group = complete_y, fill = list(Value = NA))
  
  p <- ggplot(plot_data, aes(x = Value, y = PTU_Group)) +
    geom_violin(trim = FALSE, fill = "lightblue", bw = 0.1, scale = "width", na.rm = TRUE) +
    geom_boxplot(width = 0.15, notch = FALSE, outlier.size = 1, color = "black", na.rm = TRUE) +
    stat_summary(fun = median, geom = "point", shape = 20, size = 2, color = "#dc5772", na.rm = TRUE) +
    scale_x_continuous(limits = x_limits, expand = c(0, 0)) +
    labs(
      x = x_label,
      y = NULL,
      title = title
    ) +
    theme_bw() +
    theme(
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(size = 10, face = "bold"),
      axis.text.y = element_text(size = 10, face = "bold"),
      plot.title = element_blank()
    )
  return(p)
}

generate_defense_table <- function() {
  defense_data <- data %>% filter(Value_Group == "Defense_Percentage_Origin")
  
  if (nrow(defense_data) == 0) {
    return(NULL)
  }
  
  defense_data <- defense_data %>%
    complete(PTU_Group = complete_y, fill = list(Value = 0))
  
  p <- ggplot(defense_data, aes(x = Value, y = PTU_Group)) +
    geom_bar(stat = "identity", fill = "lightgray", color = "black", alpha = 0.7) +
    labs(
      x = "% Defense",
      y = NULL,
      title = "Defense Systems Percentage"
    ) +
    theme_bw() +
    theme(
      axis.title.x = element_text(size = 12, face = "bold"),
      axis.text.x = element_text(size = 10, face = "bold"),
      axis.text.y = element_text(size = 10, face = "bold"),
      plot.title = element_blank()
    )
  return(p)
}

plot_list <- list(
  generate_plot("Host_ANI", "Host ANI", c(80, 100), "Pairwise Distance of Host ANI"),
  generate_plot("Plasmid_ANI", "Plasmid ANI", c(80, 100), "Pairwise Distance of Plasmid ANI"),
  generate_plot("Cosine_Similarity", "Cosine", c(0, 1), "Pairwise Cosine Similarities"),
  generate_plot("Plasmid_Length", "Log pLength", c(NA, NA), "log10 Plasmid Length"),
  generate_plot("PCN", "Log PCN", c(NA, NA), "log10 Plasmid Copy Number"),
  generate_plot("Patristic_Distance", "Patristic", c(0, NA), "Pairwise Distance of Plasmid Host"),
  generate_defense_table()
)

plot_list <- plot_list[!sapply(plot_list, is.null)]

output_pdf <- "combined_plots_with_defense.pdf"

pdf(output_pdf, width = 16, height = 100)
grid.arrange(grobs = plot_list, ncol = length(plot_list))
dev.off()

cat("PDF saved to", output_pdf, "\n")