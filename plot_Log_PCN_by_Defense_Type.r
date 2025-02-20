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

# 3. 确保Type为因子类型，并按log10PCN中位数排序
mean_log10PCN <- pcn_data %>%
  group_by(Type) %>%
  summarise(median_log10PCN = median(log10PCN, na.rm = TRUE)) %>%
  arrange(desc(median_log10PCN))

sorted_types <- mean_log10PCN$Type

pcn_data <- pcn_data %>%
  mutate(Type = factor(Type, levels = sorted_types))

# 4. 绘制箱线图
p <- ggplot(pcn_data, aes(x = log10PCN, y = Type)) +
  geom_boxplot(width = 0.7, fill = "lightblue", color = "black", outlier.size = 0.5) +
  scale_x_continuous(name = "log10 Plasmid Copy Number") +
  labs(y = "Type Classification", title = "Plasmid Copy Number Distribution by Type") +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  )

# 5. 保存图像
ggsave("Results/log_PCN_boxplot_by_type.pdf", p, width = 6, height = 20, units = "in", dpi = 300, limitsize = FALSE)

# 6. 输出中间数据
write.csv(pcn_data, "intermediate_data_for_PCN_boxplot_by_type.csv", row.names = FALSE)