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
sigma_area_common <- 0.24
sigma_area_dev <- 0.07
sigma_vessel_common <- 0.14
sigma_vessel_dev <- 0.05
nb_size_chu <- 6
nb_size_dai <- 6
nb_size_toku <- 6
nb_size_tokudai <- 5
nb_size_sho <- 5
nb_size_shosho <- 5
nb_size_ware <- 5

month_eff_tbl <- tibble(
  month = 1:12,
  month_eff_common = c(0.03, 0.05, 0.02, 0.00, -0.02, -0.03, -0.02, 0.01, 0.04, 0.06, 0.03, 0.01),
  month_eff_chu = month_eff_common + c(0.01, 0.01, 0.00, 0.00, -0.01, -0.02, -0.01, 0.00, 0.01, 0.02, 0.01, 0.00),
  month_eff_dai = month_eff_common + c(0.00, 0.01, 0.01, 0.00, 0.00, -0.01, -0.01, 0.00, 0.01, 0.01, 0.00, 0.00),
  month_eff_toku = month_eff_common + c(-0.01, 0.00, 0.01, 0.01, 0.00, 0.00, -0.01, -0.01, 0.00, 0.01, 0.01, 0.00),
  month_eff_tokudai = month_eff_common + c(-0.01, -0.01, 0.00, 0.01, 0.01, 0.01, 0.00, -0.01, 0.00, 0.00, 0.01, 0.01),
  month_eff_sho = month_eff_common * 0.75 + c(0.02, 0.01, 0.01, 0.00, -0.01, -0.01, -0.01, 0.00, 0.02, 0.02, 0.01, 0.00),
  month_eff_shosho = month_eff_common * 0.65 + c(0.02, 0.02, 0.01, 0.00, 0.00, -0.01, -0.01, 0.00, 0.01, 0.02, 0.01, 0.00),
  month_eff_ware = month_eff_common * 0.70 + c(0.00, 0.01, 0.01, 0.00, -0.01, -0.01, -0.01, 0.00, 0.01, 0.01, 0.01, 0.00)
) |>
  select(-month_eff_common)

area_re_tbl <- tibble(
  area = area_candidates,
  area_re_common = rnorm(length(area_candidates), mean = 0, sd = sigma_area_common),
  area_re_chu = area_re_common + rnorm(length(area_candidates), mean = 0, sd = sigma_area_dev),
  area_re_dai = area_re_common + rnorm(length(area_candidates), mean = 0, sd = sigma_area_dev),
  area_re_toku = area_re_common + rnorm(length(area_candidates), mean = 0, sd = sigma_area_dev),
  area_re_tokudai = area_re_common + rnorm(length(area_candidates), mean = 0, sd = sigma_area_dev),
  area_re_sho = area_re_common * 0.8 + rnorm(length(area_candidates), mean = 0, sd = sigma_area_dev * 0.8),
  area_re_shosho = area_re_common * 0.7 + rnorm(length(area_candidates), mean = 0, sd = sigma_area_dev * 0.8),
  area_re_ware = area_re_common * 0.75 + rnorm(length(area_candidates), mean = 0, sd = sigma_area_dev * 0.8)
) |>
  select(-area_re_common)

vessel_re_tbl <- tibble(
  vessel = vessel_candidates,
  vessel_re_common = rnorm(length(vessel_candidates), mean = 0, sd = sigma_vessel_common),
  vessel_re_chu = vessel_re_common + rnorm(length(vessel_candidates), mean = 0, sd = sigma_vessel_dev),
  vessel_re_dai = vessel_re_common + rnorm(length(vessel_candidates), mean = 0, sd = sigma_vessel_dev),
  vessel_re_toku = vessel_re_common + rnorm(length(vessel_candidates), mean = 0, sd = sigma_vessel_dev),
  vessel_re_tokudai = vessel_re_common + rnorm(length(vessel_candidates), mean = 0, sd = sigma_vessel_dev),
  vessel_re_sho = vessel_re_common * 0.8 + rnorm(length(vessel_candidates), mean = 0, sd = sigma_vessel_dev * 0.8),
  vessel_re_shosho = vessel_re_common * 0.7 + rnorm(length(vessel_candidates), mean = 0, sd = sigma_vessel_dev * 0.8),
  vessel_re_ware = vessel_re_common * 0.75 + rnorm(length(vessel_candidates), mean = 0, sd = sigma_vessel_dev * 0.8)
) |>
  select(-vessel_re_common)

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
  left_join(
    year_size_cpue |>
      select(year, size_class, cpue) |>
      pivot_wider(names_from = size_class, values_from = cpue),
    by = "year"
  ) |>
  left_join(month_eff_tbl, by = "month") |>
  left_join(area_re_tbl, by = "area") |>
  left_join(vessel_re_tbl, by = "vessel") |>
  mutate(
    base_mean_chu = chu * chu_count_scale,
    base_mean_dai = dai * dai_count_scale,
    base_mean_toku = toku * toku_count_scale,
    base_mean_tokudai = tokudai * tokudai_count_scale,
    base_mean_sho = (base_mean_chu * 0.45 + 1.2) * sho_count_scale / chu_count_scale,
    base_mean_shosho = (base_mean_chu * 0.25 + 0.8) * shosho_count_scale / chu_count_scale,
    base_mean_ware = base_mean_dai * 0.06 + base_mean_toku * 0.04,
    # effort は曳網時間に相当する量として duration_min を利用する
    effort_raw = pmax(duration_min, 1e-6),
    # 水深効果は負値を避けるため下限を設け、年平均を壊しにくいよう平均1に正規化する
    depth_eff_chu_raw = pmax(eps, depth_line_tbl$a[depth_line_tbl$size_class == "chu"] + depth_line_tbl$b[depth_line_tbl$size_class == "chu"] * depth_mid),
    depth_eff_dai_raw = pmax(eps, depth_line_tbl$a[depth_line_tbl$size_class == "dai"] + depth_line_tbl$b[depth_line_tbl$size_class == "dai"] * depth_mid),
    depth_eff_toku_raw = pmax(eps, depth_line_tbl$a[depth_line_tbl$size_class == "toku"] + depth_line_tbl$b[depth_line_tbl$size_class == "toku"] * depth_mid),
    depth_eff_tokudai_raw = pmax(eps, depth_line_tbl$a[depth_line_tbl$size_class == "tokudai"] + depth_line_tbl$b[depth_line_tbl$size_class == "tokudai"] * depth_mid)
  ) |>
  group_by(year) |>
  mutate(
    # GLMM の offset(log(effort)) に対応させるため、平均1に正規化する
    effort = effort_raw / mean(effort_raw, na.rm = TRUE),
    depth_eff_chu = depth_eff_chu_raw / mean(depth_eff_chu_raw, na.rm = TRUE),
    depth_eff_dai = depth_eff_dai_raw / mean(depth_eff_dai_raw, na.rm = TRUE),
    depth_eff_toku = depth_eff_toku_raw / mean(depth_eff_toku_raw, na.rm = TRUE),
    depth_eff_tokudai = depth_eff_tokudai_raw / mean(depth_eff_tokudai_raw, na.rm = TRUE)
  ) |>
  ungroup() |>
  mutate(
    effort = pmax(effort, 1e-8),
    log_effort = log(pmax(effort, 1e-8)),
    depth_eff_sho = pmax(eps, 1 + 0.85 * (depth_eff_chu - 1)),
    depth_eff_shosho = pmax(eps, 1 + 0.75 * (depth_eff_chu - 1)),
    depth_eff_ware = pmax(eps, 1 + 0.90 * ((0.6 * depth_eff_dai + 0.4 * depth_eff_toku) - 1)),
    log_base_chu = log(pmax(base_mean_chu, 1e-8)),
    log_base_dai = log(pmax(base_mean_dai, 1e-8)),
    log_base_toku = log(pmax(base_mean_toku, 1e-8)),
    log_base_tokudai = log(pmax(base_mean_tokudai, 1e-8)),
    log_base_sho = log(pmax(base_mean_sho, 1e-8)),
    log_base_shosho = log(pmax(base_mean_shosho, 1e-8)),
    log_base_ware = log(pmax(base_mean_ware, 1e-8)),
    log_depth_eff_chu = log(pmax(depth_eff_chu, 1e-8)),
    log_depth_eff_dai = log(pmax(depth_eff_dai, 1e-8)),
    log_depth_eff_toku = log(pmax(depth_eff_toku, 1e-8)),
    log_depth_eff_tokudai = log(pmax(depth_eff_tokudai, 1e-8)),
    log_depth_eff_sho = log(pmax(depth_eff_sho, 1e-8)),
    log_depth_eff_shosho = log(pmax(depth_eff_shosho, 1e-8)),
    log_depth_eff_ware = log(pmax(depth_eff_ware, 1e-8)),
    eta_chu = log_base_chu + month_eff_chu + area_re_chu + vessel_re_chu + log_depth_eff_chu + log_effort,
    eta_dai = log_base_dai + month_eff_dai + area_re_dai + vessel_re_dai + log_depth_eff_dai + log_effort,
    eta_toku = log_base_toku + month_eff_toku + area_re_toku + vessel_re_toku + log_depth_eff_toku + log_effort,
    eta_tokudai = log_base_tokudai + month_eff_tokudai + area_re_tokudai + vessel_re_tokudai + log_depth_eff_tokudai + log_effort,
    eta_sho = log_base_sho + month_eff_sho + area_re_sho + vessel_re_sho + log_depth_eff_sho + log_effort,
    eta_shosho = log_base_shosho + month_eff_shosho + area_re_shosho + vessel_re_shosho + log_depth_eff_shosho + log_effort,
    eta_ware = log_base_ware + month_eff_ware + area_re_ware + vessel_re_ware + log_depth_eff_ware + log_effort,
    mu_chu = exp(eta_chu),
    mu_dai = exp(eta_dai),
    mu_toku = exp(eta_toku),
    mu_tokudai = exp(eta_tokudai),
    mu_sho = exp(eta_sho),
    mu_shosho = exp(eta_shosho),
    mu_ware = exp(eta_ware),
    chu = rnbinom(n(), mu = pmax(mu_chu, 1e-8), size = nb_size_chu),
    dai = rnbinom(n(), mu = pmax(mu_dai, 1e-8), size = nb_size_dai),
    toku = rnbinom(n(), mu = pmax(mu_toku, 1e-8), size = nb_size_toku),
    tokudai = rnbinom(n(), mu = pmax(mu_tokudai, 1e-8), size = nb_size_tokudai),
    sho = rnbinom(n(), mu = pmax(mu_sho, 1e-8), size = nb_size_sho),
    shosho = rnbinom(n(), mu = pmax(mu_shosho, 1e-8), size = nb_size_shosho),
    ware = rnbinom(n(), mu = pmax(mu_ware, 1e-8), size = nb_size_ware),
    # count_total は total GLMM の応答として使う全サイズ合計で、ware を含める
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
    date, year, month, vessel, area, shipCode, year_reiwa, day, duration_min, effort,
    tow_round,
    start_time, end_time, duration_time, speed_knot, area_start, area_end,
    depth_mid, depth_min, depth_max, depth_eff_chu, depth_eff_dai, depth_eff_toku, depth_eff_tokudai, depth_eff_sho, depth_eff_shosho, depth_eff_ware,
    count_total, chu, dai, toku, tokudai, ware, sho, shosho
  )

write_csv(akagai_dummy_tows, "PrepareDummyData/data_processed/akagai_dummy_tows.csv")

print(head(akagai_dummy_tows))
str(akagai_dummy_tows)
print(summary(akagai_dummy_tows))

cat("\n=== GLMM quick check: n rows ===\n")
cat(nrow(akagai_dummy_tows), "\n")

cat("\n=== GLMM quick check: rows by year ===\n")
akagai_dummy_tows |>
  count(year) |>
  print(n = Inf)

cat("\n=== GLMM quick check: rows by vessel ===\n")
akagai_dummy_tows |>
  count(vessel) |>
  print(n = Inf)

cat("\n=== GLMM quick check: rows by area ===\n")
akagai_dummy_tows |>
  count(area) |>
  print(n = Inf)

cat("\n=== GLMM quick check: mean(count_total) by year ===\n")
akagai_dummy_tows |>
  group_by(year) |>
  summarise(mean_count_total = mean(count_total, na.rm = TRUE), .groups = "drop") |>
  print(n = Inf)

cat("\n=== GLMM quick check: mean(count_total) by year-month ===\n")
akagai_dummy_tows |>
  group_by(year, month) |>
  summarise(mean_count_total = mean(count_total, na.rm = TRUE), .groups = "drop") |>
  print(n = Inf)

cat("\n=== GLMM quick check: mean(count_total) by area ===\n")
akagai_dummy_tows |>
  group_by(area) |>
  summarise(mean_count_total = mean(count_total, na.rm = TRUE), .groups = "drop") |>
  print(n = Inf)

cat("\n=== GLMM quick check: mean(count_total) by vessel ===\n")
akagai_dummy_tows |>
  group_by(vessel) |>
  summarise(mean_count_total = mean(count_total, na.rm = TRUE), .groups = "drop") |>
  print(n = Inf)

cat("\n=== GLMM quick check: var/mean and zero proportion of count_total ===\n")
akagai_dummy_tows |>
  summarise(
    mean_count_total = mean(count_total, na.rm = TRUE),
    var_count_total = var(count_total, na.rm = TRUE),
    var_over_mean = var_count_total / pmax(mean_count_total, 1e-8),
    zero_prop_count_total = mean(count_total == 0, na.rm = TRUE)
  ) |>
  print()

cat("\n=== GLMM quick check: mean counts by year and size ===\n")
akagai_dummy_tows |>
  group_by(year) |>
  summarise(
    mean_chu = mean(chu, na.rm = TRUE),
    mean_dai = mean(dai, na.rm = TRUE),
    mean_toku = mean(toku, na.rm = TRUE),
    mean_tokudai = mean(tokudai, na.rm = TRUE),
    .groups = "drop"
  ) |>
  print(n = Inf)

cat("\n=== GLMM quick check: area random effect summary ===\n")
print(summary(area_re_tbl))

cat("\n=== GLMM quick check: vessel random effect summary ===\n")
print(summary(vessel_re_tbl))

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

# -----------------------------------------
# 6. 検証
# -----------------------------------------
count_scale_tbl <- tibble(
  size_class = c("chu", "dai", "toku", "tokudai"),
  count_scale = c(150, 150, 126, 126)
)

year_mean_check <- akagai_dummy_tows |>
  mutate(
    year = year_reiwa + 2018L
  ) |>
  pivot_longer(
    cols = c(chu, dai, toku, tokudai),
    names_to = "size_class",
    values_to = "count_obs"
  ) |>
  left_join(count_scale_tbl, by = "size_class") |>
  left_join(
    year_size_cpue |>
      rename(cpue_target = cpue),
    by = c("year", "size_class")
  ) |>
  mutate(
    cpue_backcalc = count_obs / count_scale
  ) |>
  group_by(year, size_class) |>
  summarise(
    n_tow = n(),
    mean_count_obs = mean(count_obs, na.rm = TRUE),
    mean_cpue_backcalc = mean(cpue_backcalc, na.rm = TRUE),
    sd_cpue_backcalc = sd(cpue_backcalc, na.rm = TRUE),
    cpue_target = mean(cpue_target, na.rm = TRUE),
    diff = mean_cpue_backcalc - cpue_target,
    rel_error = if_else(abs(cpue_target) > 1e-12, diff / cpue_target, NA_real_),
    rel_error_pct = 100 * rel_error,
    .groups = "drop"
  )

write_csv(year_mean_check, "PrepareDummyData/data_processed/year_mean_check.csv")

cat("\n=== Year-by-size comparison ===\n")
print(year_mean_check)

cat("\n=== Summary of absolute relative error (%) ===\n")
year_mean_check |>
  summarise(
    mean_abs_rel_error_pct = mean(abs(rel_error_pct), na.rm = TRUE),
    max_abs_rel_error_pct = max(abs(rel_error_pct), na.rm = TRUE)
  ) |>
  print()

cat("\n=== By size class: mean absolute relative error (%) ===\n")
year_mean_check |>
  group_by(size_class) |>
  summarise(
    mean_abs_rel_error_pct = mean(abs(rel_error_pct), na.rm = TRUE),
    max_abs_rel_error_pct = max(abs(rel_error_pct), na.rm = TRUE),
    .groups = "drop"
  ) |>
  print()

verify_scatter <- ggplot(year_mean_check, aes(x = cpue_target, y = mean_cpue_backcalc)) +
  geom_abline(slope = 1, intercept = 0, linetype = 2) +
  geom_point(size = 2) +
  facet_wrap(~ size_class, scales = "free") +
  labs(
    x = "Target CPUE",
    y = "Reconstructed mean CPUE"
  )

print(verify_scatter)

verify_year_series <- year_mean_check |>
  select(year, size_class, cpue_target, mean_cpue_backcalc) |>
  pivot_longer(
    cols = c(cpue_target, mean_cpue_backcalc),
    names_to = "series",
    values_to = "cpue"
  ) |>
  ggplot(aes(x = year, y = cpue, color = series, group = series)) +
  geom_line() +
  geom_point(size = 2) +
  facet_wrap(~ size_class, scales = "free_y") +
  labs(
    x = "Year",
    y = "CPUE",
    color = "Series"
  )

print(verify_year_series)

verify_relative_error <- ggplot(year_mean_check, aes(x = factor(year), y = rel_error_pct)) +
  geom_hline(yintercept = 0) +
  geom_col() +
  facet_wrap(~ size_class, scales = "free_y") +
  labs(
    x = "Year",
    y = "Relative error (%)"
  )

print(verify_relative_error)

if (all(c("depth_eff_chu", "depth_eff_dai", "depth_eff_toku", "depth_eff_tokudai") %in% names(akagai_dummy_tows))) {
  depth_effect_long <- akagai_dummy_tows |>
    mutate(
      year = year_reiwa + 2018L
    ) |>
    select(year, depth_eff_chu, depth_eff_dai, depth_eff_toku, depth_eff_tokudai) |>
    pivot_longer(
      cols = starts_with("depth_eff_"),
      names_to = "size_class",
      values_to = "depth_effect"
    ) |>
    mutate(
      size_class = sub("^depth_eff_", "", size_class)
    ) |>
    group_by(year, size_class) |>
    summarise(
      mean_depth_effect = mean(depth_effect, na.rm = TRUE),
      .groups = "drop"
    )

  verify_depth_effect <- ggplot(depth_effect_long, aes(x = year, y = mean_depth_effect, group = 1)) +
    geom_line() +
    geom_point(size = 2) +
    facet_wrap(~ size_class, scales = "free_y") +
    labs(
      x = "Year",
      y = "Mean depth effect"
    )

  print(verify_depth_effect)

  ggsave("PrepareDummyData/data_processed/verify_depth_effect_by_year.png", verify_depth_effect, width = 10, height = 6, dpi = 150)
}

ggsave("PrepareDummyData/data_processed/verify_target_vs_reconstructed.png", verify_scatter, width = 10, height = 6, dpi = 150)
ggsave("PrepareDummyData/data_processed/verify_year_series.png", verify_year_series, width = 10, height = 6, dpi = 150)
ggsave("PrepareDummyData/data_processed/verify_relative_error.png", verify_relative_error, width = 10, height = 6, dpi = 150)

cat("\n=== saved files ===\n")
cat("PrepareDummyData/data_processed/akagai_dummy_tows.csv\n")
cat("PrepareDummyData/data_processed/year_mean_check.csv\n")
cat("PrepareDummyData/data_processed/verify_target_vs_reconstructed.png\n")
cat("PrepareDummyData/data_processed/verify_year_series.png\n")
cat("PrepareDummyData/data_processed/verify_relative_error.png\n")
if (exists("verify_depth_effect")) {
  cat("PrepareDummyData/data_processed/verify_depth_effect_by_year.png\n")
}
