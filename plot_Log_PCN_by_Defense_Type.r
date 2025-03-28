library(ggplot2)
library(dplyr)
library(tidyr)

# 1. 读取数据
master_table <- read.csv("Collect/pcn_type_def_length_columns.csv")

# 2. 数据清洗与转换
pcn_data <- master_table %>%
  drop_na(PCN) %>%
  filter(PCN > 0) %>%
  mutate(log10PCN = log10(PCN))

# 3. 计算每个Type的样本数量并按log10PCN中位数排序
mean_log10PCN <- pcn_data %>%
  group_by(Type) %>%
  summarise(
    median_log10PCN = median(log10PCN, na.rm = TRUE),
    count = n()
  ) %>%
  arrange(desc(median_log10PCN)) %>%
  mutate(Type_with_count = paste0(Type, " (n=", count, ")"))

sorted_types <- rev(mean_log10PCN$Type)  # Reverse the order
type_labels <- mean_log10PCN$Type_with_count
names(type_labels) <- mean_log10PCN$Type  # Keep original order for labels

pcn_data <- pcn_data %>%
  mutate(Type = factor(Type, levels = sorted_types))

# 反转标签顺序，使y轴的显示顺序上下翻转
type_labels <- rev(type_labels)

# 4. 绘制小提琴+箱线图
p <- ggplot(pcn_data, aes(x = log10PCN, y = Type)) +
  geom_violin(trim = FALSE, fill = "lightblue", scale = "width", bw = 0.1) +  # 小提琴图
  geom_boxplot(width = 0.15, notch = FALSE, outlier.size = 1, color = "black") +  # 叠加箱线图
  stat_summary(fun = median, geom = "point", shape = 20, size = 2, color = "#dc5772") +  # 标记中位数
  scale_x_continuous(name = "log10 Plasmid Copy Number") +
  scale_y_discrete(labels = type_labels) +  # 反转后的标签顺序
  labs(y = "Type Classification", title = "Plasmid Copy Number Distribution by Type") +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 14, face = "bold"),  # 调整 y 轴字体大小
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )

# 5. 保存图像
ggsave("Results/Defense_Type_Group/log_PCN_violinplot_by_type.pdf", p, width = 8, height = 50, units = "in", dpi = 600, limitsize = FALSE)

# 6. 输出中间数据
write.csv(pcn_data, "Collect/Defense_Type_Group/intermediate_data_for_PCN_violinplot_by_type.csv", row.names = FALSE)