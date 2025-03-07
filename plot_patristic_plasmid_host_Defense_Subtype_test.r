library(data.table)
library(ggplot2)

# 获取所有CSV文件
file_list <- list.files(path = "Collect/Plasmid_Host_Breath/Subtype_Patristic_distance_0", pattern = "*.csv", full.names = TRUE)

all_data <- list()  # 用于存储所有数据
average_values <- list()  # 用于存储每个defense的平均值

for (file in file_list) {
  tryCatch({
    # 1. 读取数据（更快的 fread）
    data <- fread(file, na.strings = c("", "NA"))[!is.na(Distance) & !is.na(NUCCORE_ACC_updated_pair)]
    
    # 2. 检查必要列
    if (!"NUCCORE_ACC_updated_pair" %in% colnames(data)) {
      message(sprintf("Skipping file %s: NUCCORE_ACC_updated_pair column not found", file))
      next
    }
    
    # 3. 计算唯一物种数
    species_list <- tstrsplit(data$NUCCORE_ACC_updated_pair, "-", fixed = TRUE)
    unique_species <- length(unique(unlist(species_list)))
    
    if (unique_species > 10) {
      # 4. 生成 File 分类信息
      defense_name <- sub(".*distance_(.*)\\.csv", "\\1", basename(file))
      file_label <- paste0(defense_name, " (n=", unique_species, ")")
      data[, File := file_label]
      
      # 5. 计算当前文件的非NA数值列的均值
      avg_distance <- mean(data$Distance, na.rm = TRUE)
      average_values[[length(average_values) + 1]] <- data.table(Defense = file_label, Average_Distance = avg_distance)

      # 6. 存入列表
      all_data[[length(all_data) + 1]] <- data
    }
  }, error = function(e) {
    message(sprintf("Error processing file %s: %s", file, e$message))
  })
}

# 7. 合并数据并写入文件
if (length(all_data) > 0) {
  all_data <- rbindlist(all_data, fill = TRUE)
  
  # 7.1 保存合并的数据
  fwrite(all_data, "Collect/Defense_Subtype_Group/intermediate_data_for_pairwise_patristic_distance_of_plasmid_host_plot.csv.gz", compress = "gzip")
  
  # 7.2 计算并保存 defense 平均值数据
  if (length(average_values) > 0) {
    average_values <- rbindlist(average_values, fill = TRUE)
    fwrite(average_values, "Collect/Defense_Subtype_Group/average_values_per_defense_subtype.csv")
  }
  
  # 8. 绘图
  p <- ggplot(all_data, aes(x = Distance, y = File)) +
    geom_violin(trim = FALSE, fill = "lightblue", bw = 0.1, scale = "width") +
    scale_x_continuous(limits = c(0, NA)) +
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

  # 9. 计算动态高度并保存图像
  height_val <- max(10, length(unique(all_data$File)) * 0.5)
  ggsave(
    filename = "Results/Defense_Subtype_Group/pairwise_patristic_distance_of_plasmid_host.pdf",
    plot = p,
    width = 8,
    height = height_val,
    dpi = 600,
    limitsize = FALSE
  )
}