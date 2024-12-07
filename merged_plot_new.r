# 加载所需的库
library(ggplot2)
library(gridExtra)
library(dplyr)
library(tidyr)

# 读取数据
data <- read.csv("all_imtermediate_files.csv")

# 创建一个完整的 y 坐标 (PTU_Group) 分类
complete_y <- unique(data$PTU_Group)

# 创建一个函数生成每种 Value_Group 的图
generate_plot <- function(value_group, x_label, x_limits, title) {
  # 筛选对应的 Value_Group 数据
  plot_data <- data %>% filter(Value_Group == value_group)
  
  # 如果该 Value_Group 没有数据，则返回空白图
  if (nrow(plot_data) == 0) {
    return(NULL)
  }
  
  # 补充完整的 y 坐标分类
  plot_data <- plot_data %>%
    complete(PTU_Group = complete_y, fill = list(Value = NA))
  
  # 绘图
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

# 针对 Defense_Percentage_Origin 生成表格图
generate_defense_table <- function() {
  # 筛选数据
  defense_data <- data %>% filter(Value_Group == "Defense_Percentage_Origin")
  
  # 如果没有数据，则返回空白图
  if (nrow(defense_data) == 0) {
    return(NULL)
  }
  
  # 补充完整的 y 坐标分类
  defense_data <- defense_data %>%
    complete(PTU_Group = complete_y, fill = list(Value = 0))  # 没有数据的填充为 0
  
  # 绘制柱状图
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

# 分别生成每种 Value_Group 的图
plot_list <- list(
  generate_plot("Host_ANI", "Host ANI", c(80, 100), "Pairwise Distance of Host ANI"),
  generate_plot("Plasmid_ANI", "Plasmid ANI", c(80, 100), "Pairwise Distance of Plasmid ANI"),
  generate_plot("Cosine_Similarity", "Cosine", c(0, 1), "Pairwise Cosine Similarities"),
  generate_plot("Plasmid_Length", "Log pLength", c(NA, NA), "log10 Plasmid Length"),
  generate_plot("PCN", "Log PCN", c(NA, NA), "log10 Plasmid Copy Number"),
  generate_plot("Patristic_Distance", "Patristic", c(0, NA), "Pairwise Distance of Plasmid Host"),
  generate_defense_table() # 添加 Defense_Percentage 的柱状图
)

# 过滤掉空图
plot_list <- plot_list[!sapply(plot_list, is.null)]

# 保存为PDF
output_pdf <- "combined_plots_with_defense.pdf"

pdf(output_pdf, width = 16, height = 100) # 设置PDF文件的宽度和高度
grid.arrange(grobs = plot_list, ncol = length(plot_list)) # 横向排列所有图
dev.off() # 关闭设备，保存PDF文件

cat("PDF saved to", output_pdf, "\n")