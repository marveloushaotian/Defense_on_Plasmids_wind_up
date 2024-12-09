library(ggplot2)
library(dplyr)
library(tidyr)

master_table <- read.csv("Collect/pcn_ptu_def_length_columns.csv")

pcn_data <- master_table %>%
  drop_na(PCN) %>%
  filter(PCN > 0) %>%
  mutate(
    num_def_sys = ifelse(is.na(num_def_sys), 0, num_def_sys),
    log10PCN = log10(PCN)
  )

mean_log10PCN <- pcn_data %>%
  group_by(`PTU_sHSBM_10`) %>%
  summarise(median_log10PCN = median(log10PCN, na.rm = TRUE)) %>%
  arrange(desc(median_log10PCN))

sorted_categories <- mean_log10PCN$`PTU_sHSBM_10`

percent_def_sys <- pcn_data %>%
  filter(!is.na(PTU_sHSBM_10)) %>%
  group_by(`PTU_sHSBM_10`) %>%
  summarise(
    def_sys_count = sum(num_def_sys > 0, na.rm = TRUE),
    total_count = n(),
    percent_def_sys = (def_sys_count / total_count) * 100
  )

log10PCN_range <- range(pcn_data$log10PCN, na.rm = TRUE)

percent_def_sys <- percent_def_sys %>%
  mutate(
    PTU_sHSBM_10 = factor(PTU_sHSBM_10, levels = sorted_categories),
    log10PCN_position = log10PCN_range[1] + (percent_def_sys / 100) * (log10PCN_range[2] - log10PCN_range[1])
  )

pcn_data <- pcn_data %>%
  mutate(PTU_sHSBM_10 = factor(PTU_sHSBM_10, levels = sorted_categories))

print("Data range check:")
print(paste("log10PCN range:", paste(range(pcn_data$log10PCN, na.rm = TRUE), collapse = " to ")))
print(paste("log10PCN_position range:", paste(range(percent_def_sys$log10PCN_position, na.rm = TRUE), collapse = " to ")))

plot_range <- range(c(pcn_data$log10PCN, percent_def_sys$log10PCN_position), na.rm = TRUE)

p <- ggplot() +
  geom_bar(data = percent_def_sys, 
           aes(x = log10PCN_position, y = `PTU_sHSBM_10`),
           stat = "identity", 
           fill = "lightgray", 
           alpha = 0.5) +
  geom_boxplot(data = pcn_data, 
               aes(x = log10PCN, y = `PTU_sHSBM_10`),
               width = 0.7, 
               fill = "lightblue",
               color = "black", 
               outlier.size = 0.5) +
  scale_x_continuous(
    name = "log10 Plasmid Copy Number", 
    sec.axis = sec_axis(~ (.-log10PCN_range[1]) / (log10PCN_range[2] - log10PCN_range[1]) * 100,
                        name = "Percentage of Plasmids with Defense Systems > 0",
                        breaks = seq(0, 100, by = 10))
  ) +
  labs(
    y = "PTU Classification",
    title = ""
  ) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  ) +
  coord_cartesian(xlim = plot_range) +
  scale_y_discrete(limits = rev(levels(pcn_data$PTU_sHSBM_10)))

ggsave("Results/PTU_Group/log_PCN_and_percentage_of_plasmid_with_defense_systems.pdf", p, width = 6, height = 40, units = "in", dpi = 300, limitsize = FALSE)

write.csv(pcn_data, "intermediate_data_for_PCN_plot.csv", row.names = FALSE)
write.csv(percent_def_sys, "intermediate_data_for_percentage_of_plasmid_with_defense_systems_plot.csv", row.names = FALSE)
