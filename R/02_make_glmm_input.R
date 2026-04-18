# =========================================
# 02_make_glmm_input.R
# ダミーデータまたは実データ相当の入力から GLMM-ready CSV を作成し、その直後に最低限の check を行う
# =========================================

source(file.path("R", "00_load_packages.R"))

load_project_packages(
  required_pkgs = c("tidyverse", "lubridate"),
  optional_pkgs = character()
)

ensure_project_dirs()

input_path <- file.path("PrepareDummyData", "data_processed", "akagai_dummy_tows.csv")
output_path <- file.path("data_processed", "akagai_glmm_input.csv")

if (!file.exists(input_path)) {
  stop("Input file not found: ", input_path)
}

raw_input <- readr::read_csv(
  input_path,
  col_types = readr::cols(.default = readr::col_character()),
  show_col_types = FALSE
)

pick_first_existing <- function(data, candidates) {
  matched <- candidates[candidates %in% names(data)]

  if (length(matched) == 0) {
    return(NULL)
  }

  data[[matched[[1]]]]
}

pick_first_existing_or_na <- function(data, candidates, n) {
  matched <- candidates[candidates %in% names(data)]

  if (length(matched) == 0) {
    return(rep(NA, n))
  }

  data[[matched[[1]]]]
}

coalesce_vector <- function(...) {
  candidates <- list(...)
  candidates <- Filter(Negate(is.null), candidates)

  if (length(candidates) == 0) {
    return(NULL)
  }

  out <- candidates[[1]]

  if (length(candidates) == 1) {
    return(out)
  }

  for (i in seq.int(2, length(candidates))) {
    replace_idx <- is.na(out) & !is.na(candidates[[i]])
    out[replace_idx] <- candidates[[i]][replace_idx]
  }

  out
}

parse_duration_min <- function(data) {
  duration_min <- pick_first_existing(data, c("duration_min"))

  if (!is.null(duration_min)) {
    return(as.numeric(duration_min))
  }

  duration_time <- pick_first_existing(data, c("duration_time"))

  if (is.null(duration_time)) {
    return(NULL)
  }

  duration_split <- stringr::str_split_fixed(as.character(duration_time), ":", 3)
  duration_hour <- suppressWarnings(as.numeric(duration_split[, 1]))
  duration_minute <- suppressWarnings(as.numeric(duration_split[, 2]))
  duration_second <- suppressWarnings(as.numeric(duration_split[, 3]))
  duration_second[is.na(duration_second)] <- 0

  duration_hour * 60 + duration_minute + duration_second / 60
}

get_numeric_or_zero <- function(data, column_name) {
  if (column_name %in% names(data)) {
    return(as.numeric(data[[column_name]]))
  }

  rep(0, nrow(data))
}

year_value <- pick_first_existing(raw_input, c("year"))

if (is.null(year_value) && "year_reiwa" %in% names(raw_input)) {
  year_value <- as.integer(raw_input$year_reiwa) + 2018L
}

month_value <- pick_first_existing(raw_input, c("month"))
day_value <- pick_first_existing(raw_input, c("day"))
area_value <- pick_first_existing(raw_input, c("area", "area_start", "area_end"))
vessel_value <- pick_first_existing(raw_input, c("vessel"))

if (is.null(vessel_value) && "shipCode" %in% names(raw_input)) {
  vessel_value <- paste0("ship_", raw_input$shipCode)
}

duration_min_value <- parse_duration_min(raw_input)
depth_value <- pick_first_existing(raw_input, c("depth", "depth_mid"))

if (is.null(depth_value) && all(c("depth_min", "depth_max") %in% names(raw_input))) {
  depth_value <- (as.numeric(raw_input$depth_min) + as.numeric(raw_input$depth_max)) / 2
}

date_value <- pick_first_existing(raw_input, c("date"))

if (!is.null(date_value)) {
  date_value <- as.Date(date_value)
}

if (is.null(date_value) && !is.null(year_value) && !is.null(month_value) && !is.null(day_value)) {
  date_value <- as.Date(sprintf(
    "%04d-%02d-%02d",
    as.integer(year_value),
    as.integer(month_value),
    as.integer(day_value)
  ))
}

if (is.null(year_value) || is.null(month_value) || is.null(area_value) || is.null(vessel_value) || is.null(date_value)) {
  stop("Failed to derive one of the required fields: date, year, month, area, vessel.")
}

effort_value <- duration_min_value

# effort は曳網時間が取れる場合は duration_min を使い、無い場合のみ 1 を使う
if (is.null(effort_value)) {
  effort_value <- rep(1, nrow(raw_input))
}

glmm_input <- raw_input |>
  mutate(
    tow_id = if ("tow_id" %in% names(raw_input)) raw_input$tow_id else seq_len(n()),
    date = date_value,
    year = as.integer(year_value),
    month = as.integer(month_value),
    area = as.character(area_value),
    vessel = as.character(vessel_value),
    duration_min = if (is.null(duration_min_value)) NA_real_ else as.numeric(duration_min_value),
    effort = as.numeric(effort_value),
    tokudai = get_numeric_or_zero(raw_input, "tokudai"),
    toku = get_numeric_or_zero(raw_input, "toku"),
    dai = get_numeric_or_zero(raw_input, "dai"),
    chu = get_numeric_or_zero(raw_input, "chu"),
    sho = get_numeric_or_zero(raw_input, "sho"),
    shosho = get_numeric_or_zero(raw_input, "shosho"),
    # count_total の定義はサイズ別個体数の合計に固定する
    count_total = tokudai + toku + dai + chu + sho + shosho,
    depth = if (is.null(depth_value)) NA_real_ else as.numeric(depth_value),
    shipCode = pick_first_existing_or_na(raw_input, c("shipCode"), n()),
    year_reiwa = coalesce_vector(
      pick_first_existing(raw_input, c("year_reiwa")),
      as.integer(year) - 2018L
    ),
    day = coalesce_vector(
      pick_first_existing(raw_input, c("day")),
      as.integer(lubridate::day(date))
    ),
    ware = pick_first_existing_or_na(raw_input, c("ware"), n()),
    original_area = as.character(area_value),
    original_vessel = as.character(vessel_value)
  ) |>
  select(
    date, year, month, area, vessel, effort, count_total,
    tokudai, toku, dai, chu, sho, shosho,
    tow_id, depth, duration_min, shipCode, year_reiwa, day, ware, original_area, original_vessel
  )

# required_cols は GLMM 実行前に最低限そろっているべき列
required_cols <- c(
  "date", "year", "month", "area", "vessel", "effort", "count_total",
  "tokudai", "toku", "dai", "chu", "sho", "shosho"
)

missing_cols <- setdiff(required_cols, names(glmm_input))

if (length(missing_cols) > 0) {
  cat("Missing required columns:\n")
  print(missing_cols)
  stop("Required columns are missing.")
}

if (any(is.na(glmm_input$date))) {
  stop("date contains NA.")
}

if (any(is.na(glmm_input$count_total))) {
  stop("count_total contains NA.")
}

if (any(is.na(glmm_input$effort)) || any(!is.finite(glmm_input$effort)) || any(glmm_input$effort <= 0)) {
  stop("effort must be finite and > 0.")
}

write_csv(glmm_input, output_path)

# -----------------------------------------
# check: 必須列・件数・分布の確認
# -----------------------------------------
cat("\n=== nrow ===\n")
print(nrow(glmm_input))

cat("\n=== table(year) ===\n")
print(table(glmm_input$year))

cat("\n=== table(vessel) ===\n")
print(table(glmm_input$vessel))

cat("\n=== zero_ratio_count_total ===\n")
print(mean(glmm_input$count_total == 0))

year_month_tbl <- glmm_input |>
  count(year, month, name = "n_tow") |>
  arrange(year, month)

cat("\n=== count(year, month) ===\n")
print(year_month_tbl, n = nrow(year_month_tbl))

write_csv(year_month_tbl, file.path("output", "tables", "check_n_year_month.csv"))

year_area_tbl <- glmm_input |>
  count(year, area, name = "n_tow") |>
  arrange(year, area)

cat("\n=== count(year, area) ===\n")
print(year_area_tbl, n = nrow(year_area_tbl))

write_csv(year_area_tbl, file.path("output", "tables", "check_n_year_area.csv"))

year_vessel_tbl <- glmm_input |>
  count(year, vessel, name = "n_tow") |>
  arrange(year, vessel)

cat("\n=== count(year, vessel) ===\n")
print(year_vessel_tbl, n = nrow(year_vessel_tbl))

write_csv(year_vessel_tbl, file.path("output", "tables", "check_n_year_vessel.csv"))

cat("\n=== summary(count_total) ===\n")
print(summary(glmm_input$count_total))

cat("\n=== mean_var_ratio(count_total) ===\n")
print(tibble(
  mean = mean(glmm_input$count_total),
  var = stats::var(glmm_input$count_total),
  var_over_mean = stats::var(glmm_input$count_total) / mean(glmm_input$count_total)
))

# -----------------------------------------
# check: 簡単な図の保存
# -----------------------------------------
check_effort_year_month <- ggplot(year_month_tbl, aes(x = month, y = n_tow, color = factor(year), group = year)) +
  geom_line() +
  geom_point(size = 2) +
  scale_x_continuous(breaks = sort(unique(year_month_tbl$month))) +
  labs(
    x = "Month",
    y = "Number of tows",
    color = "Year"
  )

ggsave(
  filename = file.path("output", "figures", "check_effort_year_month.png"),
  plot = check_effort_year_month,
  width = 10,
  height = 6,
  dpi = 150
)

check_hist_count_total <- ggplot(glmm_input, aes(x = count_total)) +
  geom_histogram(bins = 30, color = "white") +
  labs(
    x = "Count total",
    y = "Frequency"
  )

ggsave(
  filename = file.path("output", "figures", "check_hist_count_total.png"),
  plot = check_hist_count_total,
  width = 10,
  height = 6,
  dpi = 150
)

cat("\n=== saved files ===\n")
cat(output_path, "\n", sep = "")
cat(file.path("output", "tables", "check_n_year_month.csv"), "\n", sep = "")
cat(file.path("output", "tables", "check_n_year_area.csv"), "\n", sep = "")
cat(file.path("output", "tables", "check_n_year_vessel.csv"), "\n", sep = "")
cat(file.path("output", "figures", "check_effort_year_month.png"), "\n", sep = "")
cat(file.path("output", "figures", "check_hist_count_total.png"), "\n", sep = "")
