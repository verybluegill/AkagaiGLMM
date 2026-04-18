# =========================================
# 01_make_extracted_data.R
# 図から抽出したCSVを、解析しやすい形に整形する
# 修正版：NofDatat.csv の日付割当を修正
# =========================================

library(tidyverse)

input_dir <- file.path("PrepareDummyData", "DataFromFig")

# -----------------------------------------
# 出力フォルダ
# -----------------------------------------
dir.create("data_processed", showWarnings = FALSE, recursive = TRUE)

# -----------------------------------------
# 2列CSVを安全に読む関数
# 1行目も実データなので header = FALSE にする
# -----------------------------------------
read_xy_csv <- function(path, x_name = "x", y_name = "y") {
  df <- read.csv(path, header = FALSE, stringsAsFactors = FALSE)
  names(df) <- c(x_name, y_name)
  df |>
    mutate(
      across(everything(), as.numeric)
    )
}

# -----------------------------------------
# 1. 全サイズ合計の年別漁獲効率
# -----------------------------------------
qt_allsize <- read_xy_csv(file.path(input_dir, "qt_allsize.csv"), x_name = "year_raw", y_name = "cpue_total") |>
  mutate(
    year = round(year_raw)
  ) |>
  select(year, cpue_total)

print(qt_allsize)

write_csv(qt_allsize, "data_processed/year_total_cpue_extracted.csv")

# -----------------------------------------
# 2. サイズ別の年別漁獲効率
# -----------------------------------------
qt_medium <- read_xy_csv(file.path(input_dir, "qt_medium.csv"), x_name = "year_raw", y_name = "cpue") |>
  mutate(
    year = round(year_raw),
    size_class = "chu"
  ) |>
  select(year, size_class, cpue)

qt_dai <- read_xy_csv(file.path(input_dir, "qt_dai.csv"), x_name = "year_raw", y_name = "cpue") |>
  mutate(
    year = round(year_raw),
    size_class = "dai"
  ) |>
  select(year, size_class, cpue)

qt_toku <- read_xy_csv(file.path(input_dir, "qt_toku.csv"), x_name = "year_raw", y_name = "cpue") |>
  mutate(
    year = round(year_raw),
    size_class = "toku"
  ) |>
  select(year, size_class, cpue)

qt_tokudai <- read_xy_csv(file.path(input_dir, "qt_tokudai.csv"), x_name = "year_raw", y_name = "cpue") |>
  mutate(
    year = round(year_raw),
    size_class = "tokudai"
  ) |>
  select(year, size_class, cpue)

year_size_cpue <- bind_rows(
  qt_medium,
  qt_dai,
  qt_toku,
  qt_tokudai
) |>
  arrange(size_class, year)

print(year_size_cpue)

write_csv(year_size_cpue, "data_processed/year_size_cpue_extracted.csv")

# -----------------------------------------
# 3. 年月ごとのデータ数
# 修正版：
# NofDatat.csv の x は実年月ではなく、棒の並び順として扱う
# 青グラフの正しい年月順を手で与えて対応づける
# -----------------------------------------
n_month_raw <- read_xy_csv(file.path(input_dir, "NofDatat.csv"), x_name = "x_raw", y_name = "n_tow")

# 青グラフの正しい年月順
ym_order <- c(
  "2020-09","2020-10","2020-11","2020-12",
  "2021-01","2021-03","2021-04","2021-05",
  "2021-09","2021-10","2021-11","2021-12",
  "2022-01","2022-02","2022-03","2022-04","2022-05",
  "2022-08","2022-09","2022-10","2022-11","2022-12",
  "2023-01","2023-02","2023-03","2023-04","2023-05","2023-06",
  "2023-09","2023-10","2023-11","2023-12",
  "2024-01","2024-02","2024-03","2024-04","2024-05","2024-06"
)

if (nrow(n_month_raw) != length(ym_order)) {
  stop(
    "Row mismatch: NofDatat.csv has ", nrow(n_month_raw),
    " rows, but ym_order has ", length(ym_order), " labels."
  )
}

ym_counts <- tibble(
  ym = ym_order,
  n_tow = n_month_raw$n_tow
) |>
  mutate(
    year = as.integer(substr(ym, 1, 4)),
    month = as.integer(substr(ym, 6, 7)),
    ym = factor(ym, levels = ym_order)
  )

print(ym_counts, n = nrow(ym_counts))

write_csv(
  ym_counts |>
    mutate(ym = as.character(ym)),
  "data_processed/ym_counts_extracted.csv"
)

# -----------------------------------------
# 5. 確認用プロット
# -----------------------------------------

# 全サイズ
p1 <- ggplot(qt_allsize, aes(x = factor(year), y = cpue_total, group = 1)) +
  geom_line() +
  geom_point(size = 2) +
  labs(
    x = "Year",
    y = "Catch efficiency"
  )

print(p1)

# サイズ別
p2 <- ggplot(year_size_cpue, aes(x = factor(year), y = cpue, color = size_class, group = size_class)) +
  geom_line() +
  geom_point(size = 2) +
  labs(
    x = "Year",
    y = "Catch efficiency",
    color = "Size class"
  )

print(p2)

# 年月ごとのデータ数
p3 <- ggplot(ym_counts, aes(x = ym, y = n_tow)) +
  geom_col() +
  labs(
    x = "Year-Month",
    y = "Number of data"
  ) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

print(p3)

cat("\n=== saved files ===\n")
cat("data_processed/year_total_cpue_extracted.csv\n")
cat("data_processed/year_size_cpue_extracted.csv\n")
cat("data_processed/ym_counts_extracted.csv\n")
cat("data_processed/extracted_figure_data.rds\n")