# 加载必要库
library(ggplot2)
library(dplyr)

# 数据处理和绘制热图函数
plot_heatmap <- function(input_file, output_pdf) {
  # 读取数据
  data <- read.csv(input_file)
  
  # 1. 添加显著性标志列
  data <- data %>%
    mutate(Significance = case_when(
      P_value < 0.001 ~ "***",
      P_value < 0.01 ~ "**",
      P_value < 0.05 ~ "*",
      TRUE ~ ""
    ))
  
  # 2. 根据显著性和 Cooccurrence_Type 添加颜色和透明度
  data <- data %>%
    mutate(Relationship = case_when(
      Cooccurrence_Type == "Negative" & P_value < 0.05 ~ "Negative",
      Cooccurrence_Type == "Positive" & P_value < 0.05 ~ "Positive",
      TRUE ~ "Neutral"
    ),
    Alpha = Alpha_MLE / max(Alpha_MLE, na.rm = TRUE))  # 归一化 Alpha_MLE 用于透明度
  
  # 3. 绘制热图
  p <- ggplot(data, aes(x = AMR_Type, y = Defense_Type, fill = Relationship, alpha = Alpha)) +
    geom_tile() +  # 使用透明度
    scale_fill_manual(values = c("Positive" = "#dc5772", "Negative" = "#75b989", "Neutral" = "grey")) +  # 自定义颜色
    scale_alpha_continuous(range = c(0.5, 1)) +  # 设置透明度范围
    theme_bw() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10,  face = "bold"),
          axis.text.y = element_text(size = 10, face = "bold"),
          axis.title.x = element_blank(),
          axis.title.y = element_blank(), 
          legend.position = "right") +
    labs(fill = "Cooccurrence Type")  # 添加图例标签
  
  # 4. 保存为 PDF
  ggsave(output_pdf, plot = p, width = 50, height = 15, limitsize = FALSE ) 
   
  print("热图已生成！")
}

# 输入和输出文件路径
input_file <- "Results/Co-Occ/significant_cooccurrences_defense_type_vs_amr.csv"  # 输入文件路径
output_pdf <- "defense_type_amr_heatmap.pdf"  # 输出PDF文件路径

# 调用函数
plot_heatmap(input_file, output_pdf)
