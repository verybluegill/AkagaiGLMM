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
dir.create(file.path("PrepareDummyData", "data_processed"), showWarnings = FALSE, recursive = TRUE)

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

# -----------------------------------------
# 4. 1行=1曳網の擬似フルデータ作成
# -----------------------------------------
set.seed(123)

vessel_candidates <- c("稲荷丸", "大成丸", "第八正利丸", "清竜丸", "海幸丸")
area_candidates <- c("151", "152", "161", "162", "172", "201", "202", "203", "212", "213", "214", "223", "224")
vessel_code_tbl <- tibble(
  vessel = vessel_candidates,
  shipCode = c(1L, 2L, 3L, 4L, 5L)
)

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

# サイズ別の水深-CPUE 2点から直線効果を作る
depth_line_tbl <- tibble(
  size_class = c("chu", "dai", "toku", "tokudai"),
  x1 = c(16.53543307086614, 16.35270420587438, 16.554146675805345, 16.449430676490273),
  y1 = c(0.23459567554055738, 0.23638878032648858, 0.15924862919808191, 0.044879437374414355),
  x2 = c(42.67716535433071, 42.70871538947177, 43.05003427004798, 43.08841259209645),
  y2 = c(-0.3087114110736153, -0.1494453296455518, -0.027416038382453323, 0.28033824514400596)
) |>
  mutate(
    b = (y2 - y1) / (x2 - x1),
    a = y1 - b * x1
  )

eps <- 0.02
chu_count_scale <- 150
dai_count_scale <- 150
toku_count_scale <- 126
tokudai_count_scale <- 126
ware_count_scale <- 30
sho_count_scale <- 18
shosho_count_scale <- 11.3

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
      tow_round = sample(c(1L, 2L, 3L), size = n_tow_i, replace = TRUE, prob = c(0.62, 0.29, 0.09)),
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
    chu_base = chu * chu_count_scale,
    dai_base = dai * dai_count_scale,
    toku_base = toku * toku_count_scale,
    tokudai_base = tokudai * tokudai_count_scale,
    # 水深効果は負値を避けるため下限を設け、年平均を壊しにくいよう平均1に正規化する
    depth_eff_chu_raw = pmax(eps, depth_line_tbl$a[depth_line_tbl$size_class == "chu"] + depth_line_tbl$b[depth_line_tbl$size_class == "chu"] * depth_mid),
    depth_eff_dai_raw = pmax(eps, depth_line_tbl$a[depth_line_tbl$size_class == "dai"] + depth_line_tbl$b[depth_line_tbl$size_class == "dai"] * depth_mid),
    depth_eff_toku_raw = pmax(eps, depth_line_tbl$a[depth_line_tbl$size_class == "toku"] + depth_line_tbl$b[depth_line_tbl$size_class == "toku"] * depth_mid),
    depth_eff_tokudai_raw = pmax(eps, depth_line_tbl$a[depth_line_tbl$size_class == "tokudai"] + depth_line_tbl$b[depth_line_tbl$size_class == "tokudai"] * depth_mid),
    depth_eff_chu = depth_eff_chu_raw / mean(depth_eff_chu_raw, na.rm = TRUE),
    depth_eff_dai = depth_eff_dai_raw / mean(depth_eff_dai_raw, na.rm = TRUE),
    depth_eff_toku = depth_eff_toku_raw / mean(depth_eff_toku_raw, na.rm = TRUE),
    depth_eff_tokudai = depth_eff_tokudai_raw / mean(depth_eff_tokudai_raw, na.rm = TRUE),
    chu = rpois(n(), lambda = pmax(chu_base * chu_mult * depth_eff_chu, 0.1)),
    dai = rpois(n(), lambda = pmax(dai_base * dai_mult * depth_eff_dai, 0.1)),
    toku = rpois(n(), lambda = pmax(toku_base * toku_mult * depth_eff_toku, 0.1)),
    tokudai = rpois(n(), lambda = pmax(tokudai_base * tokudai_mult * depth_eff_tokudai, 0.1)),
    ware = rpois(n(), lambda = pmax((dai * 0.06 + toku * 0.04) * ware_mult, 0.1)),
    sho = rpois(n(), lambda = pmax((chu_base * 0.45 + 1.2) * sho_mult * sho_count_scale / chu_count_scale, 0.1)),
    shosho = rpois(n(), lambda = pmax((chu_base * 0.25 + 0.8) * shosho_mult * shosho_count_scale / chu_count_scale, 0.1)),
    count_total = chu + dai + toku + tokudai + ware + sho + shosho,
    shipCode = vessel_code_tbl$shipCode[match(vessel, vessel_code_tbl$vessel)],
    year_reiwa = year - 2018L,
    day = as.integer(substr(date, 9, 10)),
    duration_time = sprintf("%02d:%02d", duration_min %/% 60, duration_min %% 60),
    area_start = area,
    area_end = area
  ) |>
  (\(x) {
    print(summary(x$depth_mid))
    print(summary(x$depth_eff_chu))
    print(summary(x$depth_eff_dai))
    print(summary(x$depth_eff_toku))
    print(summary(x$depth_eff_tokudai))
    x
  })() |>
  select(
    shipCode, year_reiwa, month, day, area, tow_round,
    start_time, end_time, duration_time, speed_knot, area_start, area_end,
    depth_min, depth_max, count_total, chu, dai, toku, tokudai, ware, sho, shosho
  )

write_csv(akagai_dummy_tows, "PrepareDummyData/data_processed/akagai_dummy_tows.csv")

print(head(akagai_dummy_tows))
str(akagai_dummy_tows)
print(summary(akagai_dummy_tows))

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
cat("PrepareDummyData/data_processed/akagai_dummy_tows.csv\n")
