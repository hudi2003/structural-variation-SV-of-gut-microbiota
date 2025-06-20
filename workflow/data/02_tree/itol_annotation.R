# 加载必要的库
library(dplyr)
library(stringr)

# 导入数据
data <- read.csv("workflow/data/02_tree/gtdbtk.bac120.summary.tsv", sep = "\t", stringsAsFactors = FALSE)

# 从 classification 列提取门水平分类 (p__)
data$phylum <- str_extract(data$classification, "p__[^;]+")

# 获取唯一门列表
phylum_unique <- unique(data$phylum[!is.na(data$phylum)])

# 动态生成颜色（使用 RColorBrewer 或随机颜色）
if (length(phylum_unique) <= 12) {
  # 如果门数 <= 12，使用 RColorBrewer 的 Set3 调色板
  library(RColorBrewer)
  colors <- brewer.pal(max(3, length(phylum_unique)), "Set3")[1:length(phylum_unique)]
} else {
  # 如果门数 > 12，生成随机颜色
  set.seed(123) # 确保颜色可重复
  colors <- sprintf("#%06X", sample(0:16777215, length(phylum_unique)))
}

# 创建门与颜色的映射
color_map <- data.frame(phylum = phylum_unique, color = colors)

# 合并颜色到数据框
data <- left_join(data, color_map, by = "phylum")

# 生成 iTOL DATASET_COLORSTRIP 注释文件
itol_colorstrip <- data %>%
  dplyr::filter(!is.na(phylum)) %>% # 移除 phylum 为 NA 的行
  select(user_genome, phylum, color) %>%
  mutate(label = paste(user_genome, phylum, sep = "|")) %>%
  select(user_genome, color, label)

phylum_color_map <- itol_colorstrip %>%              # 原始数据框
  mutate(phylum = str_extract(label, "(?<=\\|p__)[^|]+")) %>%
  distinct(phylum, color)                            # 相同门只保留一行

print(phylum_color_map)

writeLines(c(
  "DATASET_COLORSTRIP",
  "SEPARATOR TAB",
  "DATASET_LABEL\tPhylum_Colors",
  "COLOR\t#ffffff",
  "STRIP_WIDTH\t25",
  "DATA",
  paste(itol_colorstrip$user_genome, itol_colorstrip$color, itol_colorstrip$label, sep = "\t")
), "workflow/data/02_tree/itol_phylum_colorstrip.txt")

# 生成 iTOL TREE_COLORS 注释文件（用于分支着色）
itol_treecolors <- data %>%
  dplyr::filter(!is.na(phylum)) %>% # 移除 phylum 为 NA 的行
  select(user_genome, color) %>%
  mutate(type = "branch", style = "normal", size = "1")

writeLines(c(
  "TREE_COLORS",
  "SEPARATOR TAB",
  "DATA",
  paste(itol_treecolors$user_genome, itol_treecolors$type, itol_treecolors$color, itol_treecolors$style, itol_treecolors$size, sep = "\t")
), "workflow/data/02_tree/itol_phylum_treecolors.txt")

# 输出提示
cat("iTOL 颜色条注释文件已生成：itol_phylum_colorstrip.txt\n")
cat("iTOL 分支颜色注释文件已生成：itol_phylum_treecolors.txt\n")
cat("共生成", length(phylum_unique), "种门颜色的注释\n")
