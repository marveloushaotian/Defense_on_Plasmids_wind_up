library(ggplot2)
library(dplyr)
library(tidyr)

# 1. 读取数据
master_table <- read.csv("Collect/pcn_subtype_def_length_columns.csv")

# 2. 数据清洗与转换
pcn_data <- master_table %>%
  drop_na(PCN) %>%
  filter(PCN > 0) %>%
  mutate(log10PCN = log10(PCN))

# 3. 计算每个Subtype的样本数量并按log10PCN中位数排序
mean_log10PCN <- pcn_data %>%
  group_by(Subtype) %>%
  summarise(
    median_log10PCN = median(log10PCN, na.rm = TRUE),
    count = n()
  ) %>%
  arrange(desc(median_log10PCN)) %>%
  mutate(Subtype_with_count = paste0(Subtype, " (n=", count, ")"))

sorted_subtypes <- rev(mean_log10PCN$Subtype)  # Reverse the order
subtype_labels <- mean_log10PCN$Subtype_with_count
names(subtype_labels) <- mean_log10PCN$Subtype  # Keep original order for labels

pcn_data <- pcn_data %>%
  mutate(Subtype = factor(Subtype, levels = sorted_subtypes))

# 反转标签顺序，使y轴的显示顺序上下翻转
subtype_labels <- rev(subtype_labels)

# 4. 绘制小提琴+箱线图
p <- ggplot(pcn_data, aes(x = log10PCN, y = Subtype)) +
  geom_violin(trim = FALSE, fill = "lightblue", scale = "width", bw = 0.1) +  # 小提琴图
  geom_boxplot(width = 0.15, notch = FALSE, outlier.size = 1, color = "black") +  # 叠加箱线图
  stat_summary(fun = median, geom = "point", shape = 20, size = 2, color = "#dc5772") +  # 标记中位数
  scale_x_continuous(name = "log10 Plasmid Copy Number") +
  scale_y_discrete(labels = subtype_labels) +  # 反转后的标签顺序
  labs(y = "Subtype Classification", title = "Plasmid Copy Number Distribution by Subtype") +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 14, face = "bold"),  # 调整 y 轴字体大小
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )

# 5. 保存图像
ggsave("Results/Defense_Subtype_Group/log_PCN_violinplot_by_subtype.pdf", p, width = 8, height = 50, units = "in", dpi = 600, limitsize = FALSE)

# 6. 输出中间数据
write.csv(pcn_data, "Collect/Defense_Subtype_Group/intermediate_data_for_PCN_violinplot_by_subtype.csv", row.names = FALSE)