library(ggplot2)
library(dplyr)
library(tidyr)

master_table <- read.csv("Collect/subtype_def_length.csv")

pcn_data <- master_table %>%
  drop_na(NUCCORE_Length) %>%
  filter(NUCCORE_Length > 0) %>%
  mutate(
    num_def_sys = ifelse(is.na(num_def_sys), 0, num_def_sys),
    log10NUCCORE_Length = log10(NUCCORE_Length)
  )

# Calculate counts for each subtype
subtype_counts <- pcn_data %>%
  group_by(Subtype) %>%
  summarise(count = n()) %>%
  mutate(Subtype_label = paste0(Subtype, " (n=", count, ")"))

mean_log10NUCCORE_Length <- pcn_data %>%
  group_by(`Subtype`) %>%
  summarise(median_log10NUCCORE_Length = median(log10NUCCORE_Length, na.rm = TRUE)) %>%
  arrange(desc(median_log10NUCCORE_Length))

sorted_categories <- mean_log10NUCCORE_Length$`Subtype`

# Create a named vector for label mapping
label_mapping <- setNames(subtype_counts$Subtype_label, subtype_counts$Subtype)

pcn_data <- pcn_data %>%
  mutate(Subtype = factor(Subtype, levels = sorted_categories))

write.csv(pcn_data, "Collect/Defense_Subtype_Group/intermediate_data_for_log_plasmid_length_plot.csv", row.names = FALSE)

p <- ggplot() +
  geom_boxplot(data = pcn_data, 
               aes(x = log10NUCCORE_Length, y = `Subtype`),
               width = 0.7, 
               fill = "lightblue",
               color = "black", 
               outlier.size = 0.5) +
  scale_x_continuous(
    name = "log10 Plasmid Length"
  ) +
  labs(
    y = "Defense Subtype",
    title = "Boxplot of log10 Plasmid Length"
  ) +
  theme_bw() +
  theme(
    axis.title.x = element_text(size = 12, face = "bold"),
    axis.text.x = element_text(size = 10, face = "bold"),
    axis.title.y = element_text(size = 12, face = "bold"),
    axis.text.y = element_text(size = 10, face = "bold"),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold")
  ) +
  scale_y_discrete(limits = rev(levels(pcn_data$Subtype)),
                   labels = function(x) label_mapping[x])

ggsave("Results/Defense_Subtype_Group/log_plasmid_length.pdf", p, width = 6, height = 40, units = "in", dpi = 300, limitsize = FALSE)
