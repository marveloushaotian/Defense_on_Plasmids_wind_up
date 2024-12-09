library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)

data <- read.csv("all_imtermediate_files.csv")

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

plot_list <- list(
  generate_plot("Host_ANI", "ANI Distance", c(80, 100), "Pairwise Distance of Host ANI"),
  generate_plot("Plasmid_ANI", "ANI Distance", c(80, 100), "Pairwise Distance of Plasmid ANI"),
  generate_plot("Cosine_Similarity", "Cosine Similarity", c(0, 1), "Pairwise Cosine Similarities"),
  generate_plot("Plasmid_Length", "log10 Plasmid Length", c(NA, NA), "log10 Plasmid Length"),
  generate_plot("PCN", "log10 Plasmid Copy Number", c(NA, NA), "log10 Plasmid Copy Number"),
  generate_plot("Patristic_Distance", "Patristic Distance", c(0, NA), "Pairwise Distance of Plasmid Host")
)

plot_list <- plot_list[!sapply(plot_list, is.null)]

output_pdf <- "combined_plots.pdf"

pdf(output_pdf, width = 16, height = 100)
grid.arrange(grobs = plot_list, ncol = length(plot_list))
dev.off()

cat("PDF saved to", output_pdf, "\n")
