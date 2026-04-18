# =========================================
# 01_make_extracted_data.R
# 図から抽出したCSVを、解析しやすい形に整形する
# =========================================

library(tidyverse)

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
qt_allsize <- read_xy_csv("qt_allsize.csv", x_name = "year_raw", y_name = "cpue_total") |>
  mutate(
    year = round(year_raw)
  ) |>
  select(year, cpue_total)

print(qt_allsize)

write_csv(qt_allsize, "data_processed/year_total_cpue_extracted.csv")

# -----------------------------------------
# 2. サイズ別の年別漁獲効率
# -----------------------------------------
qt_medium <- read_xy_csv("qt_medium.csv", x_name = "year_raw", y_name = "cpue") |>
  mutate(
    year = round(year_raw),
    size_class = "chu"
  ) |>
  select(year, size_class, cpue)

qt_dai <- read_xy_csv("qt_dai.csv", x_name = "year_raw", y_name = "cpue") |>
  mutate(
    year = round(year_raw),
    size_class = "dai"
  ) |>
  select(year, size_class, cpue)

qt_toku <- read_xy_csv("qt_toku.csv", x_name = "year_raw", y_name = "cpue") |>
  mutate(
    year = round(year_raw),
    size_class = "toku"
  ) |>
  select(year, size_class, cpue)

qt_tokudai <- read_xy_csv("qt_tokudai.csv", x_name = "year_raw", y_name = "cpue") |>
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
# NofDatat.csv の x は decimal year なので Year-Month に直す
# 例:
#   2022.75 なら 2022年10月ごろ
# -----------------------------------------
decimal_year_to_ym <- function(x) {
  yr <- floor(x)
  frac <- x - yr
  
  # 0-11の月番号に変換してから +1
  mon <- floor(frac * 12) + 1
  
  # 念のため範囲補正
  mon[mon < 1] <- 1
  mon[mon > 12] <- 12
  
  tibble(
    year = yr,
    month = mon,
    ym = sprintf("%04d-%02d", yr, mon)
  )
}

n_month_raw <- read_xy_csv("NofDatat.csv", x_name = "decimal_year", y_name = "n_tow")

ym_tbl <- decimal_year_to_ym(n_month_raw$decimal_year)

ym_counts <- bind_cols(ym_tbl, n_month_raw |> select(n_tow)) |>
  group_by(year, month, ym) |>
  summarise(
    n_tow = mean(n_tow, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(year, month)

print(ym_counts, n = nrow(ym_counts))

write_csv(ym_counts, "data_processed/ym_counts_extracted.csv")

# -----------------------------------------
# 4. 解析用の最低限オブジェクトをまとめる
# 後続の解析Rで読みやすくする
# -----------------------------------------
saveRDS(
  list(
    year_total_cpue = qt_allsize,
    year_size_cpue = year_size_cpue,
    ym_counts = ym_counts
  ),
  file = "data_processed/extracted_figure_data.rds"
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