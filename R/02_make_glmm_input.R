# =========================================
# 02_make_glmm_input.R
# ダミーデータまたは実データ相当の入力から GLMM-ready CSV を作成し、その直後に最低限の check を行う
# =========================================

source(file.path("R", "00_load_packages.R"))

load_project_packages(
  required_pkgs = c("tidyverse", "lubridate", "readxl"),
  optional_pkgs = character()
)

ensure_project_dirs()

output_path <- file.path("data_processed", "akagai_glmm_input.csv")
input_excel_path <- c(
  file.path("data_raw", "Akagai_sheet.xlsx"),
  "Akagai_sheet.xlsx",
  file.path("ActualData", "Akagai_sheet.xlsx")
)
input_csv_path <- c(
  file.path("PrepareDummyData", "data_processed", "akagai_dummy_tows.csv"),
  file.path("data_processed", "akagai_glmm_input.csv")
)

pick_first_existing_path <- function(paths) {
  matched <- paths[file.exists(paths)]

  if (length(matched) == 0) {
    return(NULL)
  }

  matched[[1]]
}

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
  if (is.null(data)) {
    return(NULL)
  }

  # Excel の曳網時間は日単位の数値、POSIXt、hms、文字列 HH:MM のいずれでも来うるので、
  # ここで一度すべて「分」の numeric にそろえる
  if (inherits(data, "difftime")) {
    return(as.numeric(data, units = "mins"))
  }

  if (inherits(data, "POSIXt")) {
    data_chr <- format(data, "%H:%M:%S")
    duration_split <- stringr::str_split_fixed(data_chr, ":", 3)
    duration_hour <- suppressWarnings(as.numeric(duration_split[, 1]))
    duration_minute <- suppressWarnings(as.numeric(duration_split[, 2]))
    duration_second <- suppressWarnings(as.numeric(duration_split[, 3]))
    duration_second[is.na(duration_second)] <- 0

    return(duration_hour * 60 + duration_minute + duration_second / 60)
  }

  if (inherits(data, "hms")) {
    return(as.numeric(data) / 60)
  }

  if (is.numeric(data)) {
    out <- as.numeric(data)
    out[!is.na(out) & out < 1] <- out[!is.na(out) & out < 1] * 24 * 60
    return(out)
  }

  data_chr <- trimws(as.character(data))
  data_chr[data_chr %in% c("", "NA", "NaN")] <- NA_character_

  suppressWarnings(data_num <- as.numeric(data_chr))

  if (any(!is.na(data_num))) {
    out <- data_num
    out[!is.na(out) & out < 1] <- out[!is.na(out) & out < 1] * 24 * 60
    colon_idx <- stringr::str_detect(data_chr, ":")
    out[colon_idx] <- NA_real_

    if (all(!colon_idx | is.na(out))) {
      duration_split <- stringr::str_split_fixed(data_chr, ":", 3)
      duration_hour <- suppressWarnings(as.numeric(duration_split[, 1]))
      duration_minute <- suppressWarnings(as.numeric(duration_split[, 2]))
      duration_second <- suppressWarnings(as.numeric(duration_split[, 3]))
      duration_second[is.na(duration_second)] <- 0
      out[colon_idx] <- duration_hour[colon_idx] * 60 + duration_minute[colon_idx] + duration_second[colon_idx] / 60
    }

    return(out)
  }

  if (any(stringr::str_detect(data_chr, ":"))) {
    duration_split <- stringr::str_split_fixed(data_chr, ":", 3)
    duration_hour <- suppressWarnings(as.numeric(duration_split[, 1]))
    duration_minute <- suppressWarnings(as.numeric(duration_split[, 2]))
    duration_second <- suppressWarnings(as.numeric(duration_split[, 3]))
    duration_second[is.na(duration_second)] <- 0

    return(duration_hour * 60 + duration_minute + duration_second / 60)
  }

  rep(NA_real_, length(data_chr))
}

get_numeric_or_zero <- function(data, column_name) {
  if (column_name %in% names(data)) {
    out <- suppressWarnings(as.numeric(data[[column_name]]))
    out[is.na(out)] <- 0
    return(out)
  }

  rep(0, nrow(data))
}

find_name_by_pattern <- function(data, patterns) {
  nm <- names(data)

  for (pat in patterns) {
    matched <- nm[stringr::str_detect(nm, stringr::regex(pat))]

    if (length(matched) > 0) {
      return(matched[[1]])
    }
  }

  NULL
}

pick_by_pattern <- function(data, patterns) {
  matched_name <- find_name_by_pattern(data, patterns)

  if (is.null(matched_name)) {
    return(NULL)
  }

  data[[matched_name]]
}

pick_by_pattern_or_na <- function(data, patterns, n) {
  matched <- pick_by_pattern(data, patterns)

  if (is.null(matched)) {
    return(rep(NA, n))
  }

  matched
}

input_path <- pick_first_existing_path(input_excel_path)
input_type <- "excel"

if (is.null(input_path)) {
  input_path <- pick_first_existing_path(input_csv_path)
  input_type <- "csv"
}

if (is.null(input_path)) {
  stop("Input file not found in any configured Excel/CSV path.")
}

if (identical(input_type, "excel")) {
  excel_input <- readxl::read_excel(input_path, sheet = "sheet1")

  excel_fill_cols <- c(
    find_name_by_pattern(excel_input, c("^船名", "shipCode")),
    find_name_by_pattern(excel_input, c("^年（令和）$", "^年\\(令和\\)$", "令和")),
    find_name_by_pattern(excel_input, c("^月$")),
    find_name_by_pattern(excel_input, c("^日$"))
  )
  excel_fill_cols <- unique(excel_fill_cols[!is.na(excel_fill_cols)])

  if (length(excel_fill_cols) > 0) {
    excel_input <- excel_input |>
      tidyr::fill(dplyr::all_of(excel_fill_cols), .direction = "down")
  }

  ship_code_value <- suppressWarnings(as.integer(pick_by_pattern(excel_input, c("^船名", "shipCode"))))
  year_reiwa_value <- suppressWarnings(as.integer(pick_by_pattern(excel_input, c("^年（令和）$", "^年\\(令和\\)$", "令和"))))
  month_value_excel <- suppressWarnings(as.integer(pick_by_pattern(excel_input, c("^月$"))))
  day_value_excel <- suppressWarnings(as.integer(pick_by_pattern(excel_input, c("^日$"))))
  area_original_value <- pick_by_pattern(excel_input, c("^漁場1"))
  area_start_value <- suppressWarnings(as.numeric(pick_by_pattern(excel_input, c("^曳網を開始した漁区の番号$"))))
  area_end_value <- suppressWarnings(as.numeric(pick_by_pattern(excel_input, c("^曳網を終了した漁区の番号$"))))
  depth_min_value <- suppressWarnings(as.numeric(pick_by_pattern(excel_input, c("^水深\\(m\\)浅い側$"))))
  depth_max_value <- suppressWarnings(as.numeric(pick_by_pattern(excel_input, c("^水深\\(m\\)深い側$"))))
  duration_time_value <- pick_by_pattern(excel_input, c("^曳網時間$"))
  total_reported_value <- suppressWarnings(as.numeric(pick_by_pattern(excel_input, c("^漁獲個数（合計）$", "^漁獲個数\\(合計\\)$", "漁獲個数.*合計"))))

  # 実データの先頭列は見た目は船名だが実体は shipCode なので、ここでコード→船名へ固定変換する
  vessel_value_excel <- dplyr::case_when(
    ship_code_value == 1L ~ "\u7a32\u8377\u4e38",
    ship_code_value == 2L ~ "\u7b2c\u516b\u6b63\u5229\u4e38",
    ship_code_value == 3L ~ "\u6d77\u5e78\u4e38",
    ship_code_value == 4L ~ "\u6e05\u7adc\u4e38",
    ship_code_value == 5L ~ "\u5927\u6210\u4e38",
    TRUE ~ as.character(ship_code_value)
  )

  duration_min_value_excel <- parse_duration_min(duration_time_value)
  year_value_excel <- year_reiwa_value + 2018L
  depth_cleaning_value_excel <- dplyr::case_when(
    !is.na(depth_min_value) & !is.na(depth_max_value) ~ pmin(depth_min_value, depth_max_value),
    !is.na(depth_min_value) ~ depth_min_value,
    !is.na(depth_max_value) ~ depth_max_value,
    TRUE ~ NA_real_
  )

  raw_input <- tibble(
    tow_id = seq_len(nrow(excel_input)),
    date = as.Date(sprintf("%04d-%02d-%02d", year_value_excel, month_value_excel, day_value_excel)),
    year = year_value_excel,
    month = month_value_excel,
    area = as.character(area_start_value),
    vessel = vessel_value_excel,
    duration_min = duration_min_value_excel,
    effort = duration_min_value_excel,
    tokudai = suppressWarnings(as.numeric(pick_by_pattern_or_na(excel_input, c("^特大\\(8\\.6-\\)$", "^特大"), nrow(excel_input)))),
    toku = suppressWarnings(as.numeric(pick_by_pattern_or_na(excel_input, c("^特\\(8\\.1-8\\.5\\)$", "^特\\("), nrow(excel_input)))),
    dai = suppressWarnings(as.numeric(pick_by_pattern_or_na(excel_input, c("^大\\(7\\.6-8\\.0\\)$", "^大\\("), nrow(excel_input)))),
    chu = suppressWarnings(as.numeric(pick_by_pattern_or_na(excel_input, c("^中\\(7\\.1-7\\.5\\)$", "^中\\("), nrow(excel_input)))),
    ware = suppressWarnings(as.numeric(pick_by_pattern_or_na(excel_input, c("^割れ貝$"), nrow(excel_input)))),
    sho = suppressWarnings(as.numeric(pick_by_pattern_or_na(excel_input, c("^小\\(7\\.0-5\\.1\\)$", "^小\\("), nrow(excel_input)))),
    shosho = suppressWarnings(as.numeric(pick_by_pattern_or_na(excel_input, c("^小小\\(-5\\.0\\)$", "^小小"), nrow(excel_input)))),
    depth = dplyr::case_when(
      !is.na(depth_min_value) & !is.na(depth_max_value) ~ (depth_min_value + depth_max_value) / 2,
      !is.na(depth_min_value) ~ depth_min_value,
      !is.na(depth_max_value) ~ depth_max_value,
      TRUE ~ NA_real_
    ),
    depth_min_raw = depth_min_value,
    depth_max_raw = depth_max_value,
    depth_cleaning = depth_cleaning_value_excel,
    shipCode = ship_code_value,
    year_reiwa = year_reiwa_value,
    day = day_value_excel,
    original_area = as.character(area_original_value),
    original_vessel = as.character(ship_code_value),
    total_reported = total_reported_value,
    area_end = area_end_value
  ) |>
    mutate(
      effort = if_else(is.na(effort) | !is.finite(effort) | effort <= 0, 1, effort)
    )
} else {
  raw_input <- readr::read_csv(
    input_path,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )
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

duration_min_value <- coalesce_vector(
  parse_duration_min(pick_first_existing(raw_input, c("duration_min"))),
  parse_duration_min(pick_first_existing(raw_input, c("duration_time")))
)
depth_value <- pick_first_existing(raw_input, c("depth", "depth_mid"))
depth_cleaning_value <- pick_first_existing(raw_input, c("depth_cleaning"))
depth_min_raw_value <- pick_first_existing(raw_input, c("depth_min_raw", "depth_min"))
depth_max_raw_value <- pick_first_existing(raw_input, c("depth_max_raw", "depth_max"))

if (is.null(depth_value) && all(c("depth_min", "depth_max") %in% names(raw_input))) {
  depth_value <- (as.numeric(raw_input$depth_min) + as.numeric(raw_input$depth_max)) / 2
}

if (is.null(depth_cleaning_value) && all(c("depth_min", "depth_max") %in% names(raw_input))) {
  depth_cleaning_value <- pmin(as.numeric(raw_input$depth_min), as.numeric(raw_input$depth_max))
}

if (is.null(depth_cleaning_value)) {
  depth_cleaning_value <- depth_value
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

effort_raw_value <- duration_min_value
effort_value <- duration_min_value

# effort は曳網時間が取れる場合は duration_min を使い、無い場合のみ 1 を使う
if (is.null(effort_value)) {
  effort_value <- rep(NA_real_, nrow(raw_input))
}

effort_value[is.na(effort_value) | !is.finite(effort_value) | effort_value <= 0] <- NA_real_

# depth の GLMM 用妥当範囲。必要に応じて調整
# raw の depth は保持し、範囲外は depth_glmm で NA 化する
depth_min_glmm <- 0
depth_max_glmm <- 100

# duration_min の GLMM 用妥当範囲。0 分や極端に長い曳網は NA 化候補
# まずは保守的な範囲
duration_min_min_glmm <- 1
duration_min_max_glmm <- 300

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
    ware = get_numeric_or_zero(raw_input, "ware"),
    sho = get_numeric_or_zero(raw_input, "sho"),
    shosho = get_numeric_or_zero(raw_input, "shosho"),
    # count_total の定義はサイズ別個体数の合計に固定し、実データでは ware も含める
    count_total = tokudai + toku + dai + chu + ware + sho + shosho,
    depth = if (is.null(depth_value)) NA_real_ else as.numeric(depth_value),
    depth_min_raw = if (is.null(depth_min_raw_value)) NA_real_ else as.numeric(depth_min_raw_value),
    depth_max_raw = if (is.null(depth_max_raw_value)) NA_real_ else as.numeric(depth_max_raw_value),
    depth_cleaning_raw = if (is.null(depth_cleaning_value)) NA_real_ else as.numeric(depth_cleaning_value),
    shipCode = pick_first_existing_or_na(raw_input, c("shipCode"), n()),
    year_reiwa = coalesce_vector(
      pick_first_existing(raw_input, c("year_reiwa")),
      as.integer(year) - 2018L
    ),
    day = coalesce_vector(
      pick_first_existing(raw_input, c("day")),
      as.integer(lubridate::day(date))
    ),
    total_reported = suppressWarnings(as.numeric(pick_first_existing_or_na(raw_input, c("total_reported", "count_total_raw"), n()))),
    original_area = as.character(coalesce_vector(
      pick_first_existing(raw_input, c("original_area")),
      area_value
    )),
    original_vessel = as.character(coalesce_vector(
      pick_first_existing(raw_input, c("original_vessel")),
      vessel_value
    )),
    depth_raw = depth,
    # depth の異常値判定は浅い側深度を優先し、浅い側が無い場合のみ depth_raw にフォールバックする
    depth_flag_base = if_else(!is.na(depth_min_raw), depth_min_raw, depth_raw),
    flag_depth_missing = is.na(depth_flag_base),
    flag_depth_out_of_range = !is.na(depth_flag_base) & (depth_flag_base < depth_min_glmm | depth_flag_base > depth_max_glmm),
    flag_depth_bad = flag_depth_missing | flag_depth_out_of_range,
    depth_glmm = if_else(flag_depth_bad, NA_real_, as.numeric(depth_raw)),
    duration_min_raw = duration_min,
    flag_duration_missing = is.na(duration_min_raw),
    flag_duration_out_of_range = !is.na(duration_min_raw) & (duration_min_raw < duration_min_min_glmm | duration_min_raw > duration_min_max_glmm),
    flag_duration_bad = flag_duration_missing | flag_duration_out_of_range,
    duration_min_glmm = if_else(flag_duration_bad, NA_real_, as.numeric(duration_min_raw)),
    # effort_raw は未補正の raw 値
    effort_raw = if (is.null(effort_raw_value)) NA_real_ else as.numeric(effort_raw_value),
    # effort は既存 GLMM 互換のための補正済み値
    effort = as.numeric(effort),
    # effort_glmm は cleaned duration に基づく GLMM 用値
    effort_glmm = duration_min_glmm
  ) |>
  select(
    date, year, month, area, vessel, effort, count_total,
    tokudai, toku, dai, chu, sho, shosho,
    tow_id, depth, duration_min, shipCode, year_reiwa, day, ware, total_reported, original_area, original_vessel,
    depth_raw, depth_min_raw, depth_max_raw, depth_flag_base, depth_cleaning_raw, depth_glmm, duration_min_raw, duration_min_glmm, effort_raw, effort_glmm,
    flag_depth_missing, flag_depth_out_of_range, flag_depth_bad,
    flag_duration_missing, flag_duration_out_of_range, flag_duration_bad
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

# -----------------------------------------
# check: 必須列・件数・分布の確認
# -----------------------------------------
cat("\n=== nrow ===\n")
print(nrow(glmm_input))

cat("\n=== table(year) ===\n")
print(table(glmm_input$year))

cat("\n=== table(vessel) ===\n")
print(table(glmm_input$vessel))

cat("\n=== table(shipCode) ===\n")
print(table(glmm_input$shipCode, useNA = "ifany"))

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

cat("\n=== summary(duration_min) ===\n")
print(tibble(
  min = min(glmm_input$duration_min, na.rm = TRUE),
  median = stats::median(glmm_input$duration_min, na.rm = TRUE),
  mean = mean(glmm_input$duration_min, na.rm = TRUE),
  max = max(glmm_input$duration_min, na.rm = TRUE)
))

if ("total_reported" %in% names(glmm_input) && any(!is.na(glmm_input$total_reported))) {
  total_diff <- glmm_input$count_total - glmm_input$total_reported

  cat("\n=== reported_vs_recalculated_total ===\n")
  print(tibble(
    n_compared = sum(!is.na(glmm_input$total_reported)),
    n_diff = sum(total_diff != 0, na.rm = TRUE),
    match_rate = mean(total_diff == 0, na.rm = TRUE),
    diff_min = min(total_diff, na.rm = TRUE),
    diff_mean = mean(total_diff, na.rm = TRUE),
    diff_max = max(total_diff, na.rm = TRUE)
  ))
}

cat("\n=== mean_var_ratio(count_total) ===\n")
print(tibble(
  mean = mean(glmm_input$count_total),
  var = stats::var(glmm_input$count_total),
  var_over_mean = stats::var(glmm_input$count_total) / mean(glmm_input$count_total)
))

check_area_missing_path <- file.path("output", "tables", "check_area_missing_rows.csv")
check_non_integer_count_path <- file.path("output", "tables", "check_non_integer_count_rows.csv")
check_year_out_of_range_path <- file.path("output", "tables", "check_year_out_of_range_rows.csv")
check_numeric_range_summary_path <- file.path("output", "tables", "check_numeric_range_summary.csv")
check_depth_glmm_na_path <- file.path("output", "tables", "check_depth_glmm_na_rows.csv")
check_duration_glmm_na_path <- file.path("output", "tables", "check_duration_glmm_na_rows.csv")
check_main_glmm_dropped_rows_path <- file.path("output", "tables", "check_main_glmm_dropped_rows.csv")
count_check_cols <- c("chu", "dai", "toku", "tokudai", "ware", "sho", "shosho", "count_total")

glmm_input_check <- glmm_input |>
  mutate(
    flag_area_missing = is.na(area) | area == ""
  )

# area/year/non-integer は今回は flag のみ。将来の修正や補完候補として保持する

non_integer_matrix <- sapply(
  count_check_cols,
  function(col_name) {
    abs(glmm_input_check[[col_name]] - round(glmm_input_check[[col_name]])) > 1e-8
  }
)

if (is.null(dim(non_integer_matrix))) {
  non_integer_matrix <- matrix(non_integer_matrix, ncol = length(count_check_cols))
  colnames(non_integer_matrix) <- count_check_cols
}

non_integer_matrix[is.na(non_integer_matrix)] <- FALSE

glmm_input_check <- glmm_input_check |>
  mutate(
    flag_non_integer_count = apply(non_integer_matrix, 1, any),
    non_integer_cols = apply(
      non_integer_matrix,
      1,
      function(x) paste(count_check_cols[x], collapse = ",")
    ),
    flag_year_out_of_range = !(year %in% 2020:2024),
    flag_effort_glmm_bad = is.na(effort_glmm) | !is.finite(effort_glmm) | effort_glmm <= 0,
    flag_use_for_main_glmm = !flag_year_out_of_range &
      !flag_area_missing &
      !flag_effort_glmm_bad
  )

glmm_input <- glmm_input_check |>
  select(
    all_of(names(glmm_input)),
    flag_area_missing, flag_non_integer_count, flag_year_out_of_range,
    flag_effort_glmm_bad, flag_use_for_main_glmm
  )

write_csv(glmm_input, output_path)

area_missing_rows <- glmm_input_check |>
  filter(flag_area_missing) |>
  select(any_of(c(
    "tow_id", "date", "year", "month", "day", "vessel", "shipCode",
    "area", "original_area", "duration_min", "effort", "count_total"
  )))

cat("\n=== area missing check ===\n")
cat("n_area_missing = ", nrow(area_missing_rows), "\n", sep = "")

if (nrow(area_missing_rows) > 0) {
  print(head(area_missing_rows, 10))
}

write_csv(area_missing_rows, check_area_missing_path)

non_integer_count_rows <- glmm_input_check |>
  filter(flag_non_integer_count) |>
  select(any_of(c(
    "tow_id", "date", "year", "month", "vessel", "area", "count_total",
    "chu", "dai", "toku", "tokudai", "ware", "sho", "shosho", "non_integer_cols"
  )))

cat("\n=== non-integer count check ===\n")
cat("n_non_integer_count = ", nrow(non_integer_count_rows), "\n", sep = "")

if (nrow(non_integer_count_rows) > 0) {
  warning("non-integer count rows detected")
  print(head(non_integer_count_rows, 10))
}

write_csv(non_integer_count_rows, check_non_integer_count_path)

year_out_of_range_rows <- glmm_input_check |>
  filter(flag_year_out_of_range) |>
  select(any_of(c(
    "tow_id", "date", "year", "year_reiwa", "month", "day", "vessel", "shipCode", "area", "count_total"
  )))

cat("\n=== year out-of-range check ===\n")
cat("n_year_out_of_range = ", nrow(year_out_of_range_rows), "\n", sep = "")

if (nrow(year_out_of_range_rows) > 0) {
  cat("Years outside expected range 2020:2024 detected\n")
  print(year_out_of_range_rows |>
    count(year, name = "n_row") |>
    arrange(year), n = nrow(year_out_of_range_rows |>
      count(year, name = "n_row") |>
      arrange(year)))
  print(head(year_out_of_range_rows, 10))
}

write_csv(year_out_of_range_rows, check_year_out_of_range_path)

numeric_summary_cols <- c(
  "depth", "duration_min", "effort", "count_total",
  "chu", "dai", "toku", "tokudai", "ware", "sho", "shosho",
  "depth_raw", "depth_min_raw", "depth_max_raw", "depth_flag_base", "depth_glmm", "duration_min_raw", "duration_min_glmm", "effort_raw", "effort_glmm"
)

numeric_range_summary <- purrr::map_dfr(
  intersect(numeric_summary_cols, names(glmm_input_check)),
  function(col_name) {
    x <- suppressWarnings(as.numeric(glmm_input_check[[col_name]]))
    x_valid <- x[!is.na(x) & is.finite(x)]

    tibble(
      variable = col_name,
      n = length(x),
      n_missing = sum(is.na(x)),
      min = if (length(x_valid) == 0) NA_real_ else min(x_valid),
      median = if (length(x_valid) == 0) NA_real_ else stats::median(x_valid),
      mean = if (length(x_valid) == 0) NA_real_ else mean(x_valid),
      max = if (length(x_valid) == 0) NA_real_ else max(x_valid)
    )
  }
)

cat("\n=== numeric range summary ===\n")
print(numeric_range_summary)

write_csv(numeric_range_summary, check_numeric_range_summary_path)

glmm_numeric_range_summary <- numeric_range_summary |>
  filter(variable %in% c("depth_glmm", "duration_min_glmm", "effort_glmm"))

cat("\n=== GLMM numeric range summary ===\n")
print(glmm_numeric_range_summary)

for (col_name in c("depth", "duration_min", "effort", "count_total", "depth_flag_base")) {
  if (col_name %in% names(glmm_input_check)) {
    x <- suppressWarnings(as.numeric(glmm_input_check[[col_name]]))
    x_valid <- x[!is.na(x) & is.finite(x)]

    cat("\n=== ", col_name, " observed range ===\n", sep = "")
    print(tibble(
      !!paste0(col_name, "_min_observed") := if (length(x_valid) == 0) NA_real_ else min(x_valid),
      !!paste0(col_name, "_max_observed") := if (length(x_valid) == 0) NA_real_ else max(x_valid)
    ))
  }
}

depth_glmm_na_rows <- glmm_input_check |>
  filter(flag_depth_bad) |>
  select(any_of(c(
    "tow_id", "date", "year", "month", "day", "vessel", "shipCode",
    "area", "depth_raw", "depth_min_raw", "depth_max_raw", "depth_flag_base", "depth_glmm", "flag_depth_missing", "flag_depth_out_of_range", "count_total"
  )))

write_csv(depth_glmm_na_rows, check_depth_glmm_na_path)

cat("\n=== depth_glmm NA summary ===\n")
print(tibble(
  n_depth_glmm_na = nrow(depth_glmm_na_rows),
  n_depth_flag_base_missing = sum(glmm_input_check$flag_depth_missing),
  n_depth_out_of_range = sum(glmm_input_check$flag_depth_out_of_range),
  depth_glmm_min_observed = if (all(is.na(glmm_input_check$depth_glmm))) NA_real_ else min(glmm_input_check$depth_glmm, na.rm = TRUE),
  depth_glmm_max_observed = if (all(is.na(glmm_input_check$depth_glmm))) NA_real_ else max(glmm_input_check$depth_glmm, na.rm = TRUE)
))

duration_glmm_na_rows <- glmm_input_check |>
  filter(flag_duration_bad) |>
  select(any_of(c(
    "tow_id", "date", "year", "month", "day", "vessel", "shipCode",
    "area", "duration_min_raw", "duration_min_glmm", "effort_raw", "effort_glmm",
    "flag_duration_missing", "flag_duration_out_of_range", "count_total"
  )))

write_csv(duration_glmm_na_rows, check_duration_glmm_na_path)

main_glmm_dropped_rows <- glmm_input_check |>
  filter(!flag_use_for_main_glmm) |>
  select(any_of(c(
    "tow_id", "date", "year", "month", "day", "vessel", "shipCode",
    "area", "count_total", "duration_min_raw", "duration_min_glmm",
    "effort_raw", "effort_glmm",
    "flag_year_out_of_range", "flag_area_missing", "flag_effort_glmm_bad"
  )))

write_csv(main_glmm_dropped_rows, check_main_glmm_dropped_rows_path)

cat("\n=== main GLMM availability check ===\n")
cat("n_year_out_of_range = ", sum(glmm_input_check$flag_year_out_of_range), "\n", sep = "")
cat("n_area_missing = ", sum(glmm_input_check$flag_area_missing), "\n", sep = "")
cat("n_effort_glmm_bad = ", sum(glmm_input_check$flag_effort_glmm_bad), "\n", sep = "")
cat("n_main_glmm_dropped = ", nrow(main_glmm_dropped_rows), "\n", sep = "")

cat("\n=== data quality flags summary ===\n")
print(tibble(
  area_missing_rate = mean(glmm_input_check$flag_area_missing),
  non_integer_count_rate = mean(glmm_input_check$flag_non_integer_count),
  year_out_of_range_rate = mean(glmm_input_check$flag_year_out_of_range),
  effort_glmm_bad_rate = mean(glmm_input_check$flag_effort_glmm_bad),
  n_no_problem_rows = sum(!(glmm_input_check$flag_area_missing | glmm_input_check$flag_non_integer_count | glmm_input_check$flag_year_out_of_range | glmm_input_check$flag_effort_glmm_bad))
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
cat("input_path=", input_path, "\n", sep = "")
cat(output_path, "\n", sep = "")
cat(file.path("output", "tables", "check_n_year_month.csv"), "\n", sep = "")
cat(file.path("output", "tables", "check_n_year_area.csv"), "\n", sep = "")
cat(file.path("output", "tables", "check_n_year_vessel.csv"), "\n", sep = "")
cat(check_area_missing_path, "\n", sep = "")
cat(check_non_integer_count_path, "\n", sep = "")
cat(check_year_out_of_range_path, "\n", sep = "")
cat(check_numeric_range_summary_path, "\n", sep = "")
cat(check_depth_glmm_na_path, "\n", sep = "")
cat(check_duration_glmm_na_path, "\n", sep = "")
cat(check_main_glmm_dropped_rows_path, "\n", sep = "")
cat(file.path("output", "figures", "check_effort_year_month.png"), "\n", sep = "")
cat(file.path("output", "figures", "check_hist_count_total.png"), "\n", sep = "")
