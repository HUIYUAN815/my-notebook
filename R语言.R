###不同分类防御系统和反防御系统占比情况
# =========================================================================
# 1. 加载必要的依赖包
# =========================================================================
my_lib <- "D:/R_library"
if (dir.exists(my_lib)) .libPaths(c(my_lib, .libPaths()))

library(dplyr)
library(ggplot2)
library(tidyr)
library(RColorBrewer)
library(patchwork)

# =========================================================================
# 2. 设置工作路径并读取数据
# =========================================================================
setwd("D:/课题/data")

# 读取合并后的主文件
raw_df <- read.delim("merged_phage_systems_meta.tsv", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

# =========================================================================
# ⭐ 3. 数据清洗与 Taxonomy 重命名
# =========================================================================
df_processed <- raw_df %>%
  mutate(
    Taxonomy = trimws(Taxonomy),
    Taxonomy = case_when(
      Taxonomy %in% c("NCLDV", "Retrovirales", "CressDNAParvo", "", "NA") | is.na(Taxonomy) ~ "Unknown",
      TRUE ~ Taxonomy
    )
  )

# 按 type 类型区分 Defense 与 Anti-Defense
def_df <- df_processed %>% 
  filter(!grepl("^Anti", type, ignore.case = TRUE))

anti_df <- df_processed %>% 
  filter(grepl("^Anti", type, ignore.case = TRUE))

# =========================================================================
# ⭐ 4. 防御系统数据整理
# =========================================================================
def_cleaned <- def_df %>%
  filter(type != "" & !is.na(type)) %>%
  group_by(Taxonomy, type) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Taxonomy) %>%
  mutate(clean_type = case_when(
    Taxonomy == "Caudovirales" ~ {
      r <- min_rank(desc(n))
      ifelse(r <= 6, type, "Other Defense-systems")
    },
    Taxonomy == "Unknown" ~ {
      r <- min_rank(desc(n))
      ifelse(r <= 5, type, "Other Defense-systems")
    },
    TRUE ~ type
  )) %>%
  group_by(Taxonomy, clean_type) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  group_by(Taxonomy) %>%
  mutate(Percentage = n / sum(n)) %>% 
  ungroup() %>%
  mutate(Facet_Group = "Defense Systems")

all_def_types <- unique(def_cleaned$clean_type)
def_no_summary <- all_def_types[all_def_types != "Other Defense-systems"]
def_cleaned$clean_type <- factor(def_cleaned$clean_type, levels = c(def_no_summary, "Other Defense-systems"))

# =========================================================================
# ⭐ 5. 反防御系统数据整理
# =========================================================================
anti_cleaned <- anti_df %>%
  filter(type != "" & !is.na(type)) %>%
  group_by(Taxonomy, type) %>%
  summarise(n = n(), .groups = "drop") %>%
  group_by(Taxonomy) %>%
  mutate(clean_type = case_when(
    Taxonomy == "Caudovirales" ~ {
      r <- min_rank(desc(n))
      ifelse(r <= 6, type, "Other Anti-defense systems")
    },
    Taxonomy == "Unknown" ~ {
      r <- min_rank(desc(n))
      ifelse(r <= 3, type, "Other Anti-defense systems")
    },
    TRUE ~ type
  )) %>%
  group_by(Taxonomy, clean_type) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  group_by(Taxonomy) %>%
  mutate(Percentage = n / sum(n)) %>% 
  ungroup() %>%
  mutate(Facet_Group = "Anti-Defense Systems")

all_anti_types <- unique(anti_cleaned$clean_type)
anti_no_summary <- all_anti_types[all_anti_types != "Other Anti-defense systems"]
anti_cleaned$clean_type <- factor(anti_cleaned$clean_type, levels = c(anti_no_summary, "Other Anti-defense systems"))

# =========================================================================
# 6. 生成调色盘与坐标轴宽度分配
# =========================================================================
palette_def  <- colorRampPalette(brewer.pal(12, "Set3"))(length(levels(def_cleaned$clean_type)))
palette_anti <- colorRampPalette(brewer.pal(12, "Pastel1"))(length(levels(anti_cleaned$clean_type)))

num_def_x  <- length(unique(def_cleaned$Taxonomy))
num_anti_x <- length(unique(anti_cleaned$Taxonomy))

# =========================================================================
# ⭐ 7. 绘制左侧：Defense Systems（控制柱体变纤细 width = 0.5）
# =========================================================================
p_left <- ggplot(def_cleaned, aes(x = Taxonomy, y = Percentage, fill = clean_type)) +
  geom_col(position = position_fill(), width = 0.5, color = "black", linewidth = 0.3) + # ⭐ width 改为 0.5
  geom_text(
    aes(label = ifelse(Percentage > 0.015, paste0(round(Percentage * 100, 1), "%"), "")), # 略加门槛避免小标签挤在一起
    position = position_fill(vjust = 0.5), size = 9 / .pt, color = "black"
  ) +
  facet_wrap(~ Facet_Group, strip.position = "bottom") +
  scale_fill_manual(values = palette_def) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  labs(x = NULL, y = "Proportion of System Types", fill = NULL) + 
  theme_minimal() +
  theme(
    text = element_text(color = "black", size = 10),                                    
    axis.text.x = element_text(color = "black", size = 10, angle = 45, hjust = 1, vjust = 1), 
    axis.text.y = element_blank(),                                               
    axis.title.y = element_text(color = "black", size = 10),
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    strip.background = element_blank(), 
    strip.text = element_text(size = 10, face = "bold", margin = margin(t = 4, b = 2)), 
    strip.placement = "outside", 
    legend.text = element_text(size = 9),
    legend.box.margin = margin(t = -2, b = 0, r = 0, l = 0), 
    legend.position = "bottom"
  ) +
  guides(fill = guide_legend(ncol = 3, byrow = TRUE))

# =========================================================================
# ⭐ 8. 绘制右侧：Anti-Defense Systems（控制柱体变纤细 width = 0.5）
# =========================================================================
p_right <- ggplot(anti_cleaned, aes(x = Taxonomy, y = Percentage, fill = clean_type)) +
  geom_col(position = position_fill(), width = 0.5, color = "black", linewidth = 0.3) + # ⭐ width 改为 0.5
  geom_text(
    aes(label = ifelse(Percentage > 0.015, paste0(round(Percentage * 100, 1), "%"), "")),
    position = position_fill(vjust = 0.5), size = 9 / .pt, color = "black"
  ) +
  facet_wrap(~ Facet_Group, strip.position = "bottom") +
  scale_fill_manual(values = palette_anti) + 
  scale_y_continuous(expand = c(0, 0), limits = c(0, 1)) +
  labs(x = NULL, y = NULL, fill = NULL) + 
  theme_minimal() +
  theme(
    text = element_text(color = "black", size = 10),                                    
    axis.text.x = element_text(color = "black", size = 10, angle = 45, hjust = 1, vjust = 1), 
    axis.text.y = element_blank(),                                               
    panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    axis.line.y = element_blank(), 
    strip.background = element_blank(), 
    strip.text = element_text(size = 10, face = "bold", margin = margin(t = 4, b = 2)), 
    strip.placement = "outside", 
    legend.text = element_text(size = 9),
    legend.box.margin = margin(t = -2, b = 0, r = 0, l = 0), 
    legend.position = "bottom"
  ) +
  guides(fill = guide_legend(ncol = 3, byrow = TRUE))

# =========================================================================
# ⭐ 9. 拼接输出（调整宽度比例使图表整体更挺拔）
# =========================================================================
p_final <- p_left + p_right

p_final <- p_final + 
  plot_layout(widths = c(num_def_x, num_anti_x)) & 
  theme(
    strip.placement = "outside"
  )

# 在 RStudio 中展示
print(p_final)
