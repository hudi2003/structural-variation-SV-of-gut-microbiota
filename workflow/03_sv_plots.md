03_sv_plots
================
Compiled at 2025-06-20 08:36:53 UTC

``` r
here::i_am(paste0(params$name, ".Rmd"), uuid = "8088b640-f804-4c6f-8adf-e1c77196ead3")
```

The purpose of this document is …

``` r
library("conflicted")
library(readr)     # 高效读取 TSV
library(dplyr)     # 数据整理
library(stringr)   # 字符串处理
```

    ## Warning: 程辑包'stringr'是用R版本4.3.3 来建造的

``` r
library(ggplot2)   # 可视化
```

    ## Warning: 程辑包'ggplot2'是用R版本4.3.3 来建造的

``` r
# create or *empty* the target directory, used to write this file's data: 
projthis::proj_create_dir_target(params$name, clean = F)

# function to get path to target directory: path_target("sample.csv")
path_target <- projthis::proj_path_target(params$name)

# function to get path to previous data: path_source("00-import", "sample.csv")
path_source <- projthis::proj_path_source(params$name)
```

## Tasks

The first task is …

## Files written

These files have been written to the target directory,
`data/03_sv_plots`:

``` r
projthis::proj_dir_info(path_target())
```

    ## # A tibble: 1 × 4
    ##   path       type         size modification_time  
    ##   <fs::path> <fct> <fs::bytes> <dttm>             
    ## 1 all.tsv    file         451K 2025-06-20 08:36:37

``` r
sv <- read.csv("data/03_sv_plots/all.tsv", sep = " ")

## ── 3.  Derive sample_id and filter ───────────────────────────────────────
sv_clean <- sv %>%
  mutate(sample_id = str_extract(ref_chr, "^[^.]+")) %>%  # extract prefix before first "."
  dplyr::filter(sample_id != "ref_chr")                          # exclude header artefacts

## ── 4.  Summarise counts per sample and SV_type ───────────────────────────
sv_summary <- sv_clean %>%
  count(sample_id, SV_type, name = "count")

## ── 5.  Visualise with new palette + rectangular border ───────────────────
p <- ggplot(sv_summary,
       aes(x = sample_id, y = count, fill = SV_type)) +
  geom_col(width = 0.7) +                        # keep bar borders
  scale_fill_brewer(palette = "Set1") +                            # new colour scheme
  labs(x = "Sample ID",
       y = "Number of SV events",
       fill = "SV type") +
  theme_classic(base_size = 10) +
  theme(
    axis.text.x   = element_text(angle = 35, hjust = 1, size = 6),
    axis.line.x    = element_blank(), 
    axis.line.y    = element_blank(),  
    panel.border  = element_rect(colour = "black", fill = NA,      # rectangular frame
                                 linewidth = 0.8),
    plot.margin   = margin(10, 10, 10, 50, unit = "pt")               # neat padding
  )

ggsave(filename = "data/03_sv_plots/SV_plot.svg",
       plot     = p,
       device   = "svg",   # uses svglite backend
       width    = 14,      # in centimetres; adjust as needed
       height   = 10,
       units    = "cm")
```
