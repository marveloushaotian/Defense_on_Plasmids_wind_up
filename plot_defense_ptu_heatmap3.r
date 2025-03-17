# Load necessary libraries
library(ggplot2)
library(dplyr)
library(tidyr)
library(grid)
library(gridExtra)
library(cowplot) # 用于更精确的图形拼接

# Read the data, ensuring proper handling of missing values
data <- read.csv("Collect/master_extract_defense_ptu_muti.csv", stringsAsFactors = FALSE, na.strings = c("", "NA"))

# Read the PTU Hrange data
ptu_hrange <- read.csv("Collect/PTU_HRange_Old.csv", stringsAsFactors = FALSE)
# Assuming the columns are named 'PTU' and 'Hrange'
# If they have different names, adjust accordingly
colnames(ptu_hrange) <- c("PTU", "Hrange")

# 假设Acc列是用于唯一标识记录的列，如果不是，请替换为正确的列名
# 首先计算每个PTU中有多少不同的Acc以及它们中有Type/Subtype的比例
ptu_acc_coverage <- data %>%
  group_by(PTU) %>%
  summarise(
    total_acc = n_distinct(Acc),  # 不同Acc的总数
    acc_with_type = n_distinct(Acc[!is.na(Type)]),  # 有Type值的Acc数量
    acc_with_subtype = n_distinct(Acc[!is.na(Subtype)]),  # 有Subtype值的Acc数量
    type_percentage = round(acc_with_type / total_acc * 100, 1),  # Type覆盖率
    subtype_percentage = round(acc_with_subtype / total_acc * 100, 1)  # Subtype覆盖率
  )

# 移除数据中任何具有NA值的PTU行
data <- data %>% filter(!is.na(PTU))

# 计算每个PTU的记录总数
ptu_record_counts <- data %>%
  group_by(PTU) %>%
  summarise(record_count = n())

# 过滤掉记录数小于10的PTU
ptu_to_keep <- ptu_record_counts %>% 
  filter(record_count >= 20) %>% 
  pull(PTU)

# 过滤数据，只保留记录数大于等于10的PTU
data <- data %>% filter(PTU %in% ptu_to_keep)

# Calculate total counts for each Type to determine sorting order
type_totals <- data %>%
  filter(!is.na(Type)) %>%
  group_by(Type) %>%
  summarise(total_count = n()) %>%
  arrange(desc(total_count))

# Create a count table for the first plot (Type vs PTU)
count_table_type_ptu <- data %>% 
  filter(!is.na(Type)) %>%
  group_by(Type, PTU) %>% 
  summarise(count = n()) %>% 
  ungroup()

# Create a complete grid of all Type and PTU combinations
all_types <- unique(data$Type[!is.na(data$Type)])
all_ptus <- unique(data$PTU)
complete_grid_type <- expand.grid(Type = all_types, PTU = all_ptus)

# Join with the count data, filling NA with 0
count_table_type_ptu_complete <- left_join(complete_grid_type, count_table_type_ptu, by = c("Type", "PTU")) %>%
  mutate(count = ifelse(is.na(count), 0, count))

# Convert Type to a factor with levels ordered by total count
count_table_type_ptu_complete$Type <- factor(count_table_type_ptu_complete$Type, 
                                            levels = type_totals$Type)

# Add Hrange information to the count table
count_table_type_ptu_complete <- left_join(count_table_type_ptu_complete, ptu_hrange, by = "PTU")

# Define colors for Hrange groups
hrange_colors <- c(
  "Unknown" = "#f7f7f7", # 最浅色
  "I" = "#d8e7ff",
  "II" = "#a6c9ff",
  "III" = "#74abff",
  "IV" = "#428cff",
  "V" = "#106eff",
  "VI" = "#0050d6"  # 最深色
)

# Convert Hrange to factor with ordered levels
hrange_levels <- c("Unknown", "I", "II", "III", "IV", "V", "VI")
count_table_type_ptu_complete$Hrange <- factor(count_table_type_ptu_complete$Hrange, 
                                             levels = hrange_levels)

# 更新过滤后的PTU记录总数
ptu_record_counts <- data %>%
  group_by(PTU) %>%
  summarise(record_count = n())

# 确保PTU顺序一致
ptu_order <- unique(count_table_type_ptu_complete$PTU)
ptu_acc_coverage <- ptu_acc_coverage %>% 
  filter(PTU %in% ptu_to_keep) %>%
  mutate(PTU = factor(PTU, levels = ptu_order))

# 创建带有记录数的PTU标签
ptu_labels <- left_join(data.frame(PTU = ptu_order), ptu_record_counts, by = "PTU") %>%
  mutate(PTU_label = paste0(PTU, " (n=", record_count, ")"))

# 创建PTU标签与原始PTU的映射
ptu_label_mapping <- setNames(ptu_labels$PTU_label, ptu_labels$PTU)

# Custom theme for plots with Hrange labels
theme_with_hrange <- function() {
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5, face = "bold"),
    axis.text.y = element_text(face = "bold", margin = margin(r = 25)),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    plot.title = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_rect(fill = NA, color = "white", linewidth = 1),
    axis.ticks = element_line(color = "black"),
    axis.ticks.length = unit(0.15, "cm")
  )
}

# 创建Hrange分组图
hrange_data <- count_table_type_ptu_complete %>% 
  select(PTU, Hrange) %>% 
  distinct() %>%
  mutate(PTU_label = ptu_label_mapping[PTU])

hrange_plot <- ggplot(hrange_data, aes(x = 1, y = PTU_label, fill = Hrange)) +
  geom_tile() +
  scale_fill_manual(values = hrange_colors, name = "Hrange",
                    na.value = "#f7f7f7") +
  theme_void() +
  theme(
    legend.position = "right",
    axis.text.y = element_text(hjust = 1, face = "bold")
  )

# 创建Type热图
type_heatmap_data <- count_table_type_ptu_complete %>%
  mutate(PTU_label = ptu_label_mapping[PTU])

type_heatmap <- ggplot(type_heatmap_data, aes(x = Type, y = PTU_label, fill = count)) + 
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradientn(
    name = "Count",
    colors = c("#eaeeea", "#d5c3d5", "#ceb7ce", "#c6a9c6", "#be9bbc", 
               "#b58db3", "#af82ac", "#aa7aa7", "#a775a4", "#a673a3"),
    values = scales::rescale(c(0, 1, 5, 10, 50, 100, 200, 500, 1000, 2000)),
    na.value = "#eaeeea"
  ) + 
  theme_with_hrange()

# 创建Type百分比条形图 - 使用新的基于Acc的Type百分比
type_percentage_data <- ptu_acc_coverage %>%
  mutate(PTU_label = ptu_label_mapping[PTU])

type_percentage_barplot <- ggplot(type_percentage_data, aes(x = type_percentage, y = PTU_label, fill = type_percentage)) + 
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_gradient(low = "#a1d99b", high = "#31a354", name = "Type %") +
  geom_text(aes(label = paste0(type_percentage, "%")), 
            hjust = -0.2, size = 3, fontface = "bold") +
  labs(x = "Acc with Type (%)") +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "none"
  ) +
  scale_x_continuous(limits = c(0, max(ptu_acc_coverage$type_percentage) * 1.2))

# 组合为一个图 - Type
combined_type_plot <- cowplot::plot_grid(
  hrange_plot, type_heatmap, type_percentage_barplot, 
  ncol = 3, 
  align = "h",
  rel_widths = c(0.15, 0.65, 0.2)
)

# 保存Type组合图
ggsave("Results/PTU_Group/plot_type_ptu_with_hrange_barplot.pdf", 
       plot = combined_type_plot, width = 20, height = 30, limitsize = FALSE)

# ================ Subtype部分 ================

# Calculate total counts for each Subtype to determine sorting order
subtype_totals <- data %>%
  filter(!is.na(Subtype)) %>%
  group_by(Subtype) %>%
  summarise(total_count = n()) %>%
  arrange(desc(total_count))

# Create a count table for the second plot (Subtype vs PTU)
count_table_subtype_ptu <- data %>% 
  filter(!is.na(Subtype)) %>%
  group_by(Subtype, PTU) %>% 
  summarise(count = n()) %>% 
  ungroup()

# Create a complete grid of all Subtype and PTU combinations
all_subtypes <- unique(data$Subtype[!is.na(data$Subtype)])
complete_grid_subtype <- expand.grid(Subtype = all_subtypes, PTU = all_ptus)

# Join with the count data, filling NA with 0
count_table_subtype_ptu_complete <- left_join(complete_grid_subtype, count_table_subtype_ptu, by = c("Subtype", "PTU")) %>%
  mutate(count = ifelse(is.na(count), 0, count))

# Convert Subtype to a factor with levels ordered by total count
count_table_subtype_ptu_complete$Subtype <- factor(count_table_subtype_ptu_complete$Subtype, 
                                                 levels = subtype_totals$Subtype)

# Add Hrange information to the subtype count table
count_table_subtype_ptu_complete <- left_join(count_table_subtype_ptu_complete, ptu_hrange, by = "PTU")

# Convert Hrange to factor with ordered levels
count_table_subtype_ptu_complete$Hrange <- factor(count_table_subtype_ptu_complete$Hrange, 
                                                levels = hrange_levels)

# 创建Subtype热图
subtype_heatmap_data <- count_table_subtype_ptu_complete %>%
  mutate(PTU_label = ptu_label_mapping[PTU])

subtype_heatmap <- ggplot(subtype_heatmap_data, aes(x = Subtype, y = PTU_label, fill = count)) + 
  geom_tile(color = "white", linewidth = 0.5) +
  scale_fill_gradientn(
    name = "Count",
    colors = c("#eaeeea", "#d5c3d5", "#ceb7ce", "#c6a9c6", "#be9bbc", 
               "#b58db3", "#af82ac", "#aa7aa7", "#a775a4", "#a673a3"),
    values = scales::rescale(c(0, 1, 5, 10, 50, 100, 200, 500, 1000, 2000)),
    na.value = "#eaeeea"
  ) + 
  theme_with_hrange()

# 创建Subtype百分比条形图 - 使用新的基于Acc的Subtype百分比
subtype_percentage_data <- ptu_acc_coverage %>%
  mutate(PTU_label = ptu_label_mapping[PTU])

subtype_percentage_barplot <- ggplot(subtype_percentage_data, aes(x = subtype_percentage, y = PTU_label, fill = subtype_percentage)) + 
  geom_bar(stat = "identity", width = 0.7) +
  scale_fill_gradient(low = "#a1d99b", high = "#31a354", name = "Subtype %") +
  geom_text(aes(label = paste0(subtype_percentage, "%")), 
            hjust = -0.2, size = 3, fontface = "bold") +
  labs(x = "Acc with Subtype (%)") +
  theme_minimal() +
  theme(
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    legend.position = "none"
  ) +
  scale_x_continuous(limits = c(0, max(ptu_acc_coverage$subtype_percentage) * 1.2))

# 组合为一个图 - Subtype
combined_subtype_plot <- cowplot::plot_grid(
  hrange_plot, subtype_heatmap, subtype_percentage_barplot, 
  ncol = 3, 
  align = "h",
  rel_widths = c(0.15, 0.65, 0.2)
)

# 保存Subtype组合图
ggsave("Results/PTU_Group/plot_subtype_ptu_with_hrange_barplot.pdf", 
       plot = combined_subtype_plot, width = 27, height = 30, limitsize = FALSE)

# Print a message indicating the script has completed
cat("添加了Hrange分组和基于Acc的百分比条形图的热图已生成并保存为:\n")
cat("'Results/PTU_Group/plot_type_ptu_with_hrange_barplot.pdf' 和\n")
cat("'Results/PTU_Group/plot_subtype_ptu_with_hrange_barplot.pdf'.\n")
cat("已过滤掉记录数小于10的PTU\n")