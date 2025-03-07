# Load required libraries
library(ggplot2)
library(dplyr)
library(readr)

# 1. Read data
data <- read_csv("Collect/Defense_Subtype_Mob_Type.csv",
                 na = c("", "NA", "N/A"),
                 guess_max = 10000)

# 移除所有 NA 值
data <- data %>% 
  filter(!is.na(Unified_subtype_name) & !is.na(predicted_mobility))

# 如果需要计算比例
if(!"proportion" %in% colnames(data)) {
  data <- data %>%
    group_by(Unified_subtype_name, predicted_mobility) %>%
    summarise(count = n(), .groups = 'drop') %>%
    group_by(Unified_subtype_name) %>%
    mutate(proportion = count/sum(count))
}

# 获取所有子类型
all_subtypes <- unique(data$Unified_subtype_name)

# 计算每个子类型的总数量
subtype_counts <- data %>%
  group_by(Unified_subtype_name) %>%
  summarise(total = sum(count)) %>%
  pull(total, name = Unified_subtype_name)

# 仅保留总数大于10的子类型
data <- data %>%
  filter(Unified_subtype_name %in% names(subtype_counts[subtype_counts > 20]))

# 为子类型名称添加计数标签
data$Unified_subtype_name_with_count <- paste0(data$Unified_subtype_name, "\n(n=", subtype_counts[data$Unified_subtype_name], ")")

# 获取有 conjugative 类型的子类型顺序
conj_subtypes <- data %>%
  filter(predicted_mobility == "conjugative") %>%
  arrange(desc(proportion)) %>%
  pull(Unified_subtype_name)

# 获取没有 conjugative 类型的子类型
non_conj_subtypes <- setdiff(unique(data$Unified_subtype_name), conj_subtypes)

# 将两部分合并为最终顺序
subtype_order <- c(conj_subtypes, non_conj_subtypes)
subtype_order_with_count <- paste0(subtype_order, "\n(n=", subtype_counts[subtype_order], ")")

# 转换为有序因子
data$Unified_subtype_name_with_count <- factor(data$Unified_subtype_name_with_count, levels = subtype_order_with_count)

# 2. 创建堆叠条形图
p <- ggplot(data, aes(x = Unified_subtype_name_with_count, y = proportion, fill = predicted_mobility)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(values = c("#6566aa", "#8fced1", "#f07e40")) +
  # Customize theme and labels
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    axis.title = element_text(size = 12, face = "bold"),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    plot.margin = margin(t = 30, r = 30, b = 30, l = 30, unit = "pt")  # 添加边距
  ) +
  labs(
    x = "Defense System Subtype",
    y = "Proportion", 
    fill = "Predicted Mobility",
    title = "Distribution of Predicted Mobility by Defense System Subtype"
  )

# 3. Save plot
ggsave(
  "Results/defense_system_mobility_distribution.pdf",
  plot = p,
  width = 60,
  height = 8,
  dpi = 300,
  limitsize = FALSE
)