library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)

# 读取新文件
data <- read.csv("Collect/all_intermediate_files_by_defense_subtype.csv")

# 重新获取唯一的 Subtype_Group
complete_y <- unique(data$Subtype_Group)

# 生成绘图函数
generate_plot <- function(value_group, x_label, x_limits, title) {
  plot_data <- data %>% filter(Value_Group == value_group)
  
  if (nrow(plot_data) == 0) {
    return(NULL)
  }
  
  plot_data <- plot_data %>%
    complete(Subtype_Group = complete_y, fill = list(Value = NA))
  
  p <- ggplot(plot_data, aes(x = Value, y = Subtype_Group)) +
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

# 生成绘图列表（去除余弦相似度和防御系统比例）
plot_list <- list(
  generate_plot("Host_ANI", "Host ANI", c(80, 100), "Pairwise Distance of Host ANI"),
  generate_plot("Plasmid_ANI", "Plasmid ANI", c(80, 100), "Pairwise Distance of Plasmid ANI"),
  generate_plot("Plasmid_Length", "Log pLength", c(NA, NA), "log10 Plasmid Length"),
  generate_plot("PCN", "Log PCN", c(NA, NA), "log10 Plasmid Copy Number"),
  generate_plot("Patristic_Distance", "Patristic", c(0, NA), "Pairwise Distance of Plasmid Host")
)

# 过滤掉 NULL 值的绘图
plot_list <- plot_list[!sapply(plot_list, is.null)]

# 保存为 PDF
output_pdf <- "updated_combined_plots_by_defense_subtype.pdf"

pdf(output_pdf, width = 16, height = 100)
grid.arrange(grobs = plot_list, ncol = length(plot_list))
dev.off()

cat("PDF saved to", output_pdf, "\n")