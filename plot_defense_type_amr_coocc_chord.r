# 加载必要库
library(dplyr)
library(circlize)

# 数据处理和绘图函数
process_and_plot <- function(input_file, output_filtered_csv, output_pdf) {
  # 读取数据
  data <- read.csv(input_file)
  
  # 0. 筛选 Cooccurrence_Type 为 "Negative" 的行
  data <- data %>%
    filter(Cooccurrence_Type == "Negative")
  
  # 1. 计算 Defense_Type 的总和，获取前10项
  top_defense <- data %>%
    group_by(Defense_Type) %>%
    summarise(Total = sum(Alpha_MLE, na.rm = TRUE)) %>%
    arrange(desc(Total)) %>%
    slice_head(n = 10) %>%
    pull(Defense_Type)
  
  # 2. 计算 AMR_Type 的总和，获取前10项
  top_amr <- data %>%
    group_by(AMR_Type) %>%
    summarise(Total = sum(Alpha_MLE, na.rm = TRUE)) %>%
    arrange(desc(Total)) %>%
    slice_head(n = 10) %>%
    pull(AMR_Type)
  
  # 3. 筛选数据：仅保留 Defense_Type 在前10 和 AMR_Type 在前10 的组合
  filtered_data <- data %>%
    filter(Defense_Type %in% top_defense & AMR_Type %in% top_amr)
  
  # 4. 保存筛选后的数据
  write.csv(filtered_data, output_filtered_csv, row.names = FALSE)
  
  # 5. 创建矩阵
  matrix_data <- with(filtered_data, tapply(Alpha_MLE, 
                                            list(Defense_Type, AMR_Type), 
                                            sum, na.rm = TRUE))
  matrix_data[is.na(matrix_data)] <- 0  # 替换 NA 为 0
  
  # 6. 定义颜色
  custom_colors <- c("#434d91","#6566aa","#9b7baa","#c6a4c5","#c6f0ec",
                     "#8fced1","#53a4a6","#d0cab7","#c0dbe6","#509d95",
                     "#75b989","#92ca77","#d6ecc1","#e7ee9f","#f7ded5")
  defense_colors <- custom_colors[1:length(unique(filtered_data$Defense_Type))]
  amr_colors <- custom_colors[1:length(unique(filtered_data$AMR_Type))]
  names(defense_colors) <- unique(filtered_data$Defense_Type)
  names(amr_colors) <- unique(filtered_data$AMR_Type)
  grid_colors <- c(defense_colors, amr_colors)
  
  # 7. 绘制弦图
  circos.clear()
  circos.par(cell.padding = c(0.02, 0.02, 0.02, 0.02),
             track.margin = c(0.01, 0.01),
             start.degree = 90,
             gap.degree = 1)
  
  # 保存为 PDF
  pdf(output_pdf, width = 16, height = 16)
  chordDiagram(matrix_data, grid.col = grid_colors, directional = TRUE, annotationTrack = "grid",
               preAllocateTracks = list(track.height = 0.05))
  
  # 在弦图上添加 Defense_Type 和 AMR_Type 的名字
  circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
    sector.name <- get.cell.meta.data("sector.index")
    circos.text(CELL_META$xcenter, CELL_META$ylim[1], sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5))
  }, bg.border = NA)
  
  dev.off()
  
  print("数据处理完成，图表已生成！")
}

# 设置输入和输出文件路径
input_file <- "Collect/Co-Occurrence/significant_cooccurrences_defense_type_vs_amr.csv"  # 输入文件路径
output_filtered_csv <- "Collect/Co-Occurrence/defense_type_amr_coocc_filtered_negative.csv"  # 筛选后的数据保存路径
output_pdf <- "Results/Co-Occurrence/defense_type_amr_coocc_filtered_negative.pdf"  # 图表 PDF 文件保存路径

# 调用函数
process_and_plot(input_file, output_filtered_csv, output_pdf)
 