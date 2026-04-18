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
  n_tow = as.integer(round(n_month_raw$n_tow))
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
# 4. 1行=1曳網の擬似フルデータ作成
# -----------------------------------------
set.seed(123)

vessel_candidates <- c("稲荷丸", "大成丸", "第八正利丸", "清竜丸", "海幸丸")
area_candidates <- c("151", "152", "161", "162", "172", "201", "202", "203", "212", "213", "214", "223", "224")

year_effect_tbl <- tibble(
  year = c(2020L, 2021L, 2022L, 2023L, 2024L),
  chu_mult = c(0.85, 0.95, 1.00, 1.15, 1.00),
  dai_mult = c(0.85, 0.95, 1.00, 1.20, 1.05),
  toku_mult = c(0.80, 0.95, 1.00, 1.20, 1.05),
  tokudai_mult = c(0.75, 0.90, 1.00, 1.10, 1.30),
  ware_mult = c(0.95, 1.00, 1.00, 1.05, 1.00),
  sho_mult = c(1.00, 1.00, 1.00, 1.05, 0.95),
  shosho_mult = c(1.00, 1.00, 1.00, 1.05, 0.95)
)

akagai_dummy_tows <- ym_counts |>
  mutate(ym = as.character(ym)) |>
  rowwise() |>
  do({
    n_tow_i <- .$n_tow
    year_i <- .$year
    month_i <- .$month
    ym_i <- .$ym
    day_i <- sample(1:28, size = n_tow_i, replace = TRUE)
    start_minute_i <- sample(seq(6 * 60, 14 * 60, by = 5), size = n_tow_i, replace = TRUE)
    duration_i <- sample(40:120, size = n_tow_i, replace = TRUE)
    end_minute_i <- start_minute_i + duration_i
    depth_mid_i <- runif(n_tow_i, min = 16, max = 43)
    depth_min_i <- floor(depth_mid_i - runif(n_tow_i, min = 0, max = 3))
    depth_max_i <- ceiling(depth_mid_i + runif(n_tow_i, min = 0, max = 3))

    tibble(
      year = year_i,
      month = month_i,
      ym = ym_i,
      vessel = sample(vessel_candidates, size = n_tow_i, replace = TRUE),
      date = sprintf("%04d-%02d-%02d", year_i, month_i, day_i),
      tow_no = seq_len(n_tow_i),
      start_time = sprintf("%02d:%02d", start_minute_i %/% 60, start_minute_i %% 60),
      end_time = sprintf("%02d:%02d", end_minute_i %/% 60, end_minute_i %% 60),
      area = sample(area_candidates, size = n_tow_i, replace = TRUE),
      depth_mid = depth_mid_i,
      depth_min = pmin(depth_min_i, depth_max_i),
      depth_max = pmax(depth_min_i, depth_max_i),
      duration_min = duration_i,
      speed_knot = pmin(pmax(rnorm(n_tow_i, mean = 3.1, sd = 0.2), 2.5), 3.5)
    )
  }) |>
  ungroup() |>
  left_join(year_effect_tbl, by = "year") |>
  left_join(
    year_size_cpue |>
      select(year, size_class, cpue) |>
      pivot_wider(names_from = size_class, values_from = cpue),
    by = "year"
  ) |>
  mutate(
    chu_base = chu,
    dai_base = dai,
    toku_base = toku,
    tokudai_base = tokudai,
    chu = rpois(n(), lambda = pmax(chu_base * chu_mult, 0.1)),
    dai = rpois(n(), lambda = pmax(dai_base * dai_mult, 0.1)),
    toku = rpois(n(), lambda = pmax(toku_base * toku_mult, 0.1)),
    tokudai = rpois(n(), lambda = pmax(tokudai_base * tokudai_mult, 0.1)),
    ware = rpois(n(), lambda = pmax((dai_base * 0.18 + toku_base * 0.12) * ware_mult, 0.1)),
    sho = rpois(n(), lambda = pmax((chu_base * 0.45 + 1.2) * sho_mult, 0.1)),
    shosho = rpois(n(), lambda = pmax((chu_base * 0.25 + 0.8) * shosho_mult, 0.1)),
    count_total = chu + dai + toku + tokudai + ware + sho + shosho
  ) |>
  select(
    vessel, date, tow_no, start_time, end_time, area, depth_min, depth_max, speed_knot,
    chu, dai, toku, tokudai, ware, sho, shosho,
    year, month, ym, duration_min, depth_mid, count_total
  )

write_csv(akagai_dummy_tows, "data_processed/akagai_dummy_tows.csv")

print(head(akagai_dummy_tows))
str(akagai_dummy_tows)
print(summary(akagai_dummy_tows))
print(table(format(as.Date(akagai_dummy_tows$date), "%Y-%m")))
print(table(akagai_dummy_tows$vessel))

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
cat("data_processed/akagai_dummy_tows.csv\n")
cat("data_processed/extracted_figure_data.rds\n")
