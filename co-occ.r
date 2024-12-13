# 加载必要的包
library(tidyr)
library(dplyr)
library(CooccurrenceAffinity)

# 读取数据，确保正确处理空值
data <- read.csv("Collect/plasmid_defense_amr.csv", stringsAsFactors = FALSE, na.strings = c("", "NA"))

# 函数：将分号分隔的字符串转换为唯一的特征列表
split_features <- function(x) {
  if(is.na(x) || x == "") return(character(0))
  # 分割并清理特征
  features <- strsplit(x, ";")[[1]]
  features <- trimws(features)  # 移除空格
  features <- unique(features)  # 移除重复
  return(features[features != ""])
}

# 打印原始数据的一些信息
cat("数据总行数:", nrow(data), "\n")
cat("Defense不为空的行数:", sum(!is.na(data$Defense_Type)), "\n")
cat("AMR不为空的行数:", sum(!is.na(data$AMR_Type)), "\n\n")

# 获取所有unique的特征
all_defense <- unique(unlist(lapply(data$Defense_Type[!is.na(data$Defense_Type)], split_features)))
all_amr <- unique(unlist(lapply(data$AMR_Type[!is.na(data$AMR_Type)], split_features)))

# 移除可能的NA
all_defense <- all_defense[!is.na(all_defense)]
all_amr <- all_amr[!is.na(all_amr)]

cat("唯一Defense类型数:", length(all_defense), "\n")
cat("唯一AMR类型数:", length(all_amr), "\n")
print("Defense类型:")
print(all_defense)
print("AMR类型:")
print(all_amr)

# 合并所有特征
all_features <- unique(c(all_defense, all_amr))
all_features <- all_features[!is.na(all_features) & all_features != ""]

# 创建存在矩阵
presence_matrix <- matrix(0, 
                         nrow = length(all_features), 
                         ncol = nrow(data))
rownames(presence_matrix) <- all_features
colnames(presence_matrix) <- data$Plasmid_ID

# 填充矩阵
for(i in 1:nrow(data)) {
  # 处理Defense
  if(!is.na(data$Defense_Type[i])) {
    defense_types <- split_features(data$Defense_Type[i])
    if(length(defense_types) > 0) {
      valid_defense <- defense_types[defense_types %in% rownames(presence_matrix)]
      if(length(valid_defense) > 0) {
        presence_matrix[valid_defense, i] <- 1
      }
    }
  }
  
  # 处理AMR
  if(!is.na(data$AMR_Type[i])) {
    amr_types <- split_features(data$AMR_Type[i])
    if(length(amr_types) > 0) {
      valid_amr <- amr_types[amr_types %in% rownames(presence_matrix)]
      if(length(valid_amr) > 0) {
        presence_matrix[valid_amr, i] <- 1
      }
    }
  }
}

# 计算每个特征的出现频率
feature_frequencies <- rowSums(presence_matrix) / ncol(presence_matrix) * 100

# 设置最小频率阈值（这里设为5%）
min_freq_threshold <- 5

# 打印频率信息
cat("\n特征出现频率：\n")
print(sort(feature_frequencies, decreasing = TRUE))

# 过滤低频特征
frequent_features <- names(feature_frequencies[feature_frequencies >= min_freq_threshold])
presence_matrix <- presence_matrix[frequent_features, ]

cat("\n过滤后保留的特征数:", length(frequent_features), "\n")
print("保留的特征：")
print(frequent_features)

# 计算共现亲和力
if(nrow(presence_matrix) > 1) {  # 确保至少有2个特征
  affinity_result <- affinity(data = presence_matrix, 
                             row.or.col = "row", 
                             squarematrix = c("all"))
  
  # 提取显著结果
  significant_pairs <- data.frame(
    Feature1 = character(),
    Feature2 = character(),
    Alpha_MLE = numeric(),
    P_value = numeric(),
    stringsAsFactors = FALSE
  )
  
  n_features <- nrow(presence_matrix)
  for(i in 1:(n_features-1)) {
    for(j in (i+1):n_features) {
      feature1 <- rownames(presence_matrix)[i]
      feature2 <- rownames(presence_matrix)[j]
      
      x <- sum(presence_matrix[i,] & presence_matrix[j,])
      n1 <- sum(presence_matrix[i,])
      n2 <- sum(presence_matrix[j,])
      n <- ncol(presence_matrix)
      
      if(x > 0 && x < min(n1, n2)) {
        ml_result <- try(ML.Alpha(x, c(n1, n2, n)), silent = TRUE)
        if(!inherits(ml_result, "try-error")) {
          alpha_mle <- ml_result$est
          alpha_ints <- try(AlphInts(x, c(n1, n2, n)), silent = TRUE)
          if(!inherits(alpha_ints, "try-error")) {
            p_value <- alpha_ints$pval
            
            if(p_value < 0.05) {
              significant_pairs <- rbind(significant_pairs,
                                       data.frame(Feature1 = feature1,
                                                Feature2 = feature2,
                                                Alpha_MLE = alpha_mle,
                                                P_value = p_value,
                                                stringsAsFactors = FALSE))
            }
          }
        }
      }
    }
  }
  
  if(nrow(significant_pairs) > 0) {
    # 添加共现类型
    significant_pairs$Cooccurrence_Type <- ifelse(significant_pairs$Alpha_MLE > 1, 
                                                "Positive", "Negative")
    
    # 按p值排序
    significant_pairs <- significant_pairs[order(significant_pairs$P_value), ]
    
    # 保存结果
    write.csv(significant_pairs, "significant_cooccurrences_affinity.csv", 
              row.names = FALSE)
    
    # 打印结果摘要
    cat("\n显著共现对总数:", nrow(significant_pairs), "\n")
    cat("正共现对数量:", sum(significant_pairs$Cooccurrence_Type == "Positive"), "\n")
    cat("负共现对数量:", sum(significant_pairs$Cooccurrence_Type == "Negative"), "\n")
    
    # 显示前几个最显著的结果
    cat("\n最显著的共现对：\n")
    print(head(significant_pairs))
    
    # 可视化结果
    plotgg(data = affinity_result, 
           variable = "alpha_mle", 
           legendlimit = "datarange")
  } else {
    cat("\n未找到显著的共现对。\n")
  }
} else {
  cat("\n过滤后特征数太少，无法进行共现分析。\n")
}