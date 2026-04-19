# =========================================
# check_raw_data.R
# 生データを読み込み、クリーニング前チェック用の表と図を保存する
# =========================================

source(file.path("R", "00_load_packages.R"))

load_project_packages(
  required_pkgs = c("dplyr", "ggplot2", "lubridate", "readr", "readxl", "scales", "stringr", "tidyr", "tibble"),
  optional_pkgs = character()
)

dir.create(file.path("output", "check_tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path("output", "check_figures"), showWarnings = FALSE, recursive = TRUE)

excel_path <- file.path("ActualData", "Akagai_sheet.xlsx")

save_check_csv <- function(data, path) {
  readr::write_csv(data, path, na = "")
  cat("saved:", path, "\n")
}

parse_numeric_trim <- function(x) {
  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% c("", "NA", "NaN")] <- NA_character_
  suppressWarnings(as.numeric(x_chr))
}

parse_effort_hours <- function(x) {
  if (inherits(x, "difftime")) {
    return(as.numeric(x, units = "hours"))
  }

  if (inherits(x, "POSIXt")) {
    hour_value <- as.numeric(format(x, "%H"))
    minute_value <- as.numeric(format(x, "%M"))
    second_value <- as.numeric(format(x, "%S"))
    return(hour_value + minute_value / 60 + second_value / 3600)
  }

  if (is.numeric(x)) {
    out <- as.numeric(x)
    out[!is.na(out) & out < 1] <- out[!is.na(out) & out < 1] * 24
    return(out)
  }

  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% c("", "NA", "NaN")] <- NA_character_
  out <- suppressWarnings(as.numeric(x_chr))
  out[!is.na(out) & out < 1] <- out[!is.na(out) & out < 1] * 24

  colon_idx <- stringr::str_detect(x_chr, ":")
  if (any(colon_idx, na.rm = TRUE)) {
    split_mat <- stringr::str_split_fixed(x_chr[colon_idx], ":", 3)
    hour_value <- suppressWarnings(as.numeric(split_mat[, 1]))
    minute_value <- suppressWarnings(as.numeric(split_mat[, 2]))
    second_value <- suppressWarnings(as.numeric(split_mat[, 3]))
    second_value[is.na(second_value)] <- 0
    out[colon_idx] <- hour_value + minute_value / 60 + second_value / 3600
  }

  out
}

make_depth_use <- function(depth_raw_1, depth_raw_2) {
  dplyr::case_when(
    is.na(depth_raw_1) & is.na(depth_raw_2) ~ NA_real_,
    !is.na(depth_raw_1) & is.na(depth_raw_2) ~ depth_raw_1,
    is.na(depth_raw_1) & !is.na(depth_raw_2) ~ depth_raw_2,
    !is.na(depth_raw_1) & !is.na(depth_raw_2) & depth_raw_1 <= 50 & depth_raw_2 <= 50 ~ (depth_raw_1 + depth_raw_2) / 2,
    !is.na(depth_raw_1) & !is.na(depth_raw_2) & ((depth_raw_1 <= 50 & depth_raw_2 > 50) | (depth_raw_1 > 50 & depth_raw_2 <= 50)) ~ pmin(depth_raw_1, depth_raw_2),
    !is.na(depth_raw_1) & !is.na(depth_raw_2) & depth_raw_1 > 50 & depth_raw_2 > 50 ~ NA_real_,
    TRUE ~ NA_real_
  )
}

valid_area_codes <- c(151L, 152L, 161L, 162L, 171L, 172L, 181L, 182L, 191L, 192L, 201L, 202L, 203L, 212L, 213L, 214L, 223L, 224L)

make_date_year_check_plot <- function(raw_tbl, output_png) {
  date_tbl <- raw_tbl |>
    dplyr::mutate(
      year_group = dplyr::if_else(is.na(.data$year), "Out of scope / NA", as.character(.data$year)),
      year_group = factor(.data$year_group, levels = c(as.character(2020:2024), "Out of scope / NA"))
    ) |>
    dplyr::count(.data$year_group, name = "n")

  p <- ggplot2::ggplot(date_tbl, ggplot2::aes(x = .data$year_group, y = .data$n, fill = .data$year_group)) +
    ggplot2::geom_col(width = 0.7, show.legend = FALSE) +
    ggplot2::scale_fill_manual(values = c("2020" = "#4E79A7", "2021" = "#59A14F", "2022" = "#E15759", "2023" = "#F28E2B", "2024" = "#9C755F", "Out of scope / NA" = "#BAB0AC")) +
    ggplot2::labs(
      title = "Date and year check",
      x = "Year",
      y = "Rows"
    ) +
    ggplot2::theme_bw()

  ggplot2::ggsave(output_png, p, width = 8.5, height = 5.2, dpi = 300)
  cat("saved:", output_png, "\n")
}

make_depth_histogram_plot <- function(raw_tbl, output_png) {
  p <- ggplot2::ggplot(
    dplyr::filter(raw_tbl, !is.na(.data$depth_use_raw_rule)),
    ggplot2::aes(x = .data$depth_use_raw_rule)
  ) +
    ggplot2::geom_histogram(bins = 30, fill = "#4E79A7", color = "white") +
    ggplot2::labs(
      title = "Raw depth distribution",
      x = "Depth (m)",
      y = "Rows"
    ) +
    ggplot2::theme_bw()

  ggplot2::ggsave(output_png, p, width = 8.5, height = 5.2, dpi = 300)
  cat("saved:", output_png, "\n")
}

make_depth_by_year_plot <- function(raw_tbl, output_png) {
  p <- ggplot2::ggplot(
    dplyr::filter(raw_tbl, !is.na(.data$year), !is.na(.data$depth_use_raw_rule)),
    ggplot2::aes(x = factor(.data$year), y = .data$depth_use_raw_rule)
  ) +
    ggplot2::geom_boxplot(fill = "#59A14F", outlier.alpha = 0.25) +
    ggplot2::labs(
      title = "Raw depth by year",
      x = "Year",
      y = "Depth (m)"
    ) +
    ggplot2::theme_bw()

  ggplot2::ggsave(output_png, p, width = 8.5, height = 5.2, dpi = 300)
  cat("saved:", output_png, "\n")
}

make_depth_missing_plot <- function(raw_tbl, output_png) {
  heat_tbl <- raw_tbl |>
    dplyr::filter(!is.na(.data$year), !is.na(.data$area)) |>
    dplyr::group_by(.data$year, .data$area) |>
    dplyr::summarise(
      missing_rate = mean(is.na(.data$depth_use_raw_rule)),
      n = dplyr::n(),
      .groups = "drop"
    )

  p <- ggplot2::ggplot(heat_tbl, ggplot2::aes(x = factor(.data$year), y = .data$area, fill = .data$missing_rate)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.2) +
    ggplot2::scale_fill_gradient(low = "white", high = "#D73027", limits = c(0, 1)) +
    ggplot2::labs(
      title = "Depth missing rate by year and area",
      x = "Year",
      y = "Area",
      fill = "Missing rate"
    ) +
    ggplot2::theme_bw()

  ggplot2::ggsave(output_png, p, width = 10, height = 8, dpi = 300)
  cat("saved:", output_png, "\n")
}

raw_dat <- readxl::read_excel(excel_path, sheet = "sheet1")

vessel_col <- names(raw_dat)[[1]]
year_col <- names(raw_dat)[[2]]
month_col <- names(raw_dat)[[3]]
day_col <- names(raw_dat)[[4]]
effort_col <- names(raw_dat)[[9]]
speed_col <- names(raw_dat)[[10]]
area_start_col <- names(raw_dat)[[11]]
area_end_col <- names(raw_dat)[[12]]
depth_1_col <- names(raw_dat)[[13]]
depth_2_col <- names(raw_dat)[[14]]
catch_total_col <- names(raw_dat)[[15]]
catch_medium_col <- names(raw_dat)[[16]]
catch_dai_col <- names(raw_dat)[[17]]
catch_toku_col <- names(raw_dat)[[18]]
catch_tokudai_col <- names(raw_dat)[[19]]
catch_ware_col <- names(raw_dat)[[20]]
catch_sho_col <- names(raw_dat)[[21]]
catch_shosho_col <- names(raw_dat)[[22]]

raw_tbl <- tibble::as_tibble(raw_dat) |>
  dplyr::mutate(
    row_id = dplyr::row_number(),
    `年（令和）` = suppressWarnings(as.integer(.data[[year_col]])),
    month = suppressWarnings(as.integer(.data[[month_col]])),
    day = suppressWarnings(as.integer(.data[[day_col]])),
    year_raw = 2018L + `年（令和）`,
    ymd_reiwa = suppressWarnings(lubridate::ymd(sprintf("%04d-%02d-%02d", .data$year_raw, .data$month, .data$day))),
    flag_year_out_of_scope = is.na(.data$year_raw) | !(.data$year_raw %in% 2020:2024),
    ymd_reiwa = dplyr::if_else(.data$flag_year_out_of_scope, as.Date(NA), .data$ymd_reiwa),
    year = dplyr::if_else(.data$flag_year_out_of_scope, NA_integer_, .data$year_raw),
    vessel = as.character(.data[[vessel_col]]),
    area_start = as.character(.data[[area_start_col]]),
    area_end = as.character(.data[[area_end_col]]),
    area = dplyr::if_else(!is.na(.data$area_start) & .data$area_start != "", .data$area_start, .data$area_end),
    effort_hours = parse_effort_hours(.data[[effort_col]]),
    flag_effort_missing = is.na(.data$effort_hours),
    flag_effort_nonpositive = !is.na(.data$effort_hours) & .data$effort_hours <= 0,
    depth_raw_1 = suppressWarnings(as.numeric(.data[[depth_1_col]])),
    depth_raw_2 = suppressWarnings(as.numeric(.data[[depth_2_col]])),
    flag_depth_both_missing = is.na(.data$depth_raw_1) & is.na(.data$depth_raw_2),
    flag_depth_only_one = xor(is.na(.data$depth_raw_1), is.na(.data$depth_raw_2)),
    flag_depth_both_present = !is.na(.data$depth_raw_1) & !is.na(.data$depth_raw_2),
    flag_depth_both_le_50 = .data$flag_depth_both_present & .data$depth_raw_1 <= 50 & .data$depth_raw_2 <= 50,
    flag_depth_one_le_50_one_gt_50 = .data$flag_depth_both_present & ((.data$depth_raw_1 <= 50 & .data$depth_raw_2 > 50) | (.data$depth_raw_1 > 50 & .data$depth_raw_2 <= 50)),
    flag_depth_both_gt_50 = .data$flag_depth_both_present & .data$depth_raw_1 > 50 & .data$depth_raw_2 > 50,
    depth_use_raw_rule = make_depth_use(.data$depth_raw_1, .data$depth_raw_2),
    catch_total = suppressWarnings(as.numeric(.data[[catch_total_col]])),
    catch_medium = suppressWarnings(as.numeric(.data[[catch_medium_col]])),
    catch_dai = suppressWarnings(as.numeric(.data[[catch_dai_col]])),
    catch_toku = suppressWarnings(as.numeric(.data[[catch_toku_col]])),
    catch_tokudai = suppressWarnings(as.numeric(.data[[catch_tokudai_col]])),
    cpue_total_raw = dplyr::if_else(!is.na(.data$effort_hours) & .data$effort_hours > 0, .data$catch_total / .data$effort_hours, NA_real_)
  ) |>
  dplyr::select(
    "row_id",
    "年（令和）",
    "month",
    "day",
    "ymd_reiwa",
    "year",
    "flag_year_out_of_scope",
    "vessel",
    "area",
    "effort_hours",
    "flag_effort_missing",
    "flag_effort_nonpositive",
    "depth_raw_1",
    "depth_raw_2",
    "flag_depth_both_missing",
    "flag_depth_only_one",
    "flag_depth_both_present",
    "flag_depth_both_le_50",
    "flag_depth_one_le_50_one_gt_50",
    "flag_depth_both_gt_50",
    "depth_use_raw_rule",
    "catch_total",
    "catch_medium",
    "catch_dai",
    "catch_toku",
    "catch_tokudai",
    "cpue_total_raw"
  )

raw_tbl <- raw_tbl |>
  dplyr::mutate(
    year_reiwa_raw = suppressWarnings(as.integer(parse_numeric_trim(.env$raw_dat[[year_col]]))),
    month = suppressWarnings(as.integer(parse_numeric_trim(.env$raw_dat[[month_col]]))),
    day = suppressWarnings(as.integer(parse_numeric_trim(.env$raw_dat[[day_col]]))),
    month = dplyr::if_else(!is.na(.data$month) & .data$month %in% 1:12, .data$month, NA_integer_),
    day = dplyr::if_else(!is.na(.data$day) & .data$day %in% 1:31, .data$day, NA_integer_),
    year_raw = 2018L + .data$year_reiwa_raw,
    ymd_reiwa = suppressWarnings(lubridate::ymd(sprintf("%04d-%02d-%02d", .data$year_raw, .data$month, .data$day))),
    day = dplyr::if_else(!is.na(.data$year_raw) & !is.na(.data$month) & !is.na(.data$day) & is.na(.data$ymd_reiwa), NA_integer_, .data$day),
    ymd_reiwa = suppressWarnings(lubridate::ymd(sprintf("%04d-%02d-%02d", .data$year_raw, .data$month, .data$day))),
    flag_year_out_of_scope = is.na(.data$year_raw) | !(.data$year_raw %in% 2020:2024),
    ymd_reiwa = dplyr::if_else(.data$flag_year_out_of_scope, as.Date(NA), .data$ymd_reiwa),
    year = dplyr::if_else(.data$flag_year_out_of_scope, NA_integer_, .data$year_raw),
    vessel_raw = parse_numeric_trim(.env$raw_dat[[vessel_col]]),
    vessel = dplyr::if_else(!is.na(.data$vessel_raw) & .data$vessel_raw %in% 1:5, as.integer(.data$vessel_raw), NA_integer_),
    area_start = parse_numeric_trim(.env$raw_dat[[area_start_col]]),
    area_end = parse_numeric_trim(.env$raw_dat[[area_end_col]]),
    area = dplyr::case_when(
      !is.na(.data$area_start) & as.integer(.data$area_start) %in% valid_area_codes ~ as.integer(.data$area_start),
      (is.na(.data$area_start) | !(as.integer(.data$area_start) %in% valid_area_codes)) &
        !is.na(.data$area_end) & as.integer(.data$area_end) %in% valid_area_codes ~ as.integer(.data$area_end),
      TRUE ~ NA_integer_
    ),
    speed_kt = parse_numeric_trim(.env$raw_dat[[speed_col]]),
    catch_ware = suppressWarnings(as.numeric(.env$raw_dat[[catch_ware_col]])),
    catch_sho = suppressWarnings(as.numeric(.env$raw_dat[[catch_sho_col]])),
    catch_shosho = suppressWarnings(as.numeric(.env$raw_dat[[catch_shosho_col]])),
    count_total_raw = suppressWarnings(as.numeric(.env$raw_dat[[catch_total_col]]))
  ) |>
  dplyr::select(
    "row_id",
    "year_reiwa_raw",
    "month",
    "day",
    "ymd_reiwa",
    "year",
    "flag_year_out_of_scope",
    "vessel",
    "vessel_raw",
    "area_start",
    "area_end",
    "area",
    "effort_hours",
    "speed_kt",
    "flag_effort_missing",
    "flag_effort_nonpositive",
    "depth_raw_1",
    "depth_raw_2",
    "flag_depth_both_missing",
    "flag_depth_only_one",
    "flag_depth_both_present",
    "flag_depth_both_le_50",
    "flag_depth_one_le_50_one_gt_50",
    "flag_depth_both_gt_50",
    "depth_use_raw_rule",
    "catch_total",
    "catch_medium",
    "catch_dai",
    "catch_toku",
    "catch_tokudai",
    "catch_ware",
    "catch_sho",
    "catch_shosho",
    "count_total_raw",
    "cpue_total_raw"
  )

date_year_summary <- raw_tbl |>
  dplyr::mutate(year_label = dplyr::if_else(is.na(.data$year), "NA", as.character(.data$year))) |>
  dplyr::count(.data$year_label, .data$flag_year_out_of_scope, name = "n") |>
  dplyr::arrange(.data$year_label, .data$flag_year_out_of_scope)

depth_rule_check_summary <- tibble::tibble(
  metric = c(
    "depth_both_missing_n",
    "depth_only_one_n",
    "depth_both_present_n",
    "depth_both_le_50_n",
    "depth_one_le_50_one_gt_50_n",
    "depth_both_gt_50_n",
    "depth_use_raw_rule_missing_n"
  ),
  value = c(
    sum(raw_tbl$flag_depth_both_missing, na.rm = TRUE),
    sum(raw_tbl$flag_depth_only_one, na.rm = TRUE),
    sum(raw_tbl$flag_depth_both_present, na.rm = TRUE),
    sum(raw_tbl$flag_depth_both_le_50, na.rm = TRUE),
    sum(raw_tbl$flag_depth_one_le_50_one_gt_50, na.rm = TRUE),
    sum(raw_tbl$flag_depth_both_gt_50, na.rm = TRUE),
    sum(is.na(raw_tbl$depth_use_raw_rule), na.rm = TRUE)
  )
)

effort_check_summary <- tibble::tibble(
  metric = c("rows_total", "effort_missing_n", "effort_nonpositive_n", "effort_valid_n"),
  value = c(
    nrow(raw_tbl),
    sum(raw_tbl$flag_effort_missing, na.rm = TRUE),
    sum(raw_tbl$flag_effort_nonpositive, na.rm = TRUE),
    sum(!raw_tbl$flag_effort_missing & !raw_tbl$flag_effort_nonpositive, na.rm = TRUE)
  )
)

data_cleaning_candidates <- tibble::tibble(
  candidate = c(
    "Keep out-of-scope years as NA with flag",
    "Use effort_hours only",
    "Use depth_use final rule only",
    "Review rows with both depth > 50 m",
    "Review rows with one depth <= 50 and one > 50",
    "Review extreme CPUE candidates"
  ),
  reason = c(
    "Period outside 2020 to 2024 should be retained for checking",
    "A single effort variable simplifies later CPUE and offset handling",
    "A single depth variable simplifies later model comparison",
    "Both depths over 50 m are outside the intended shallow range",
    "Mixed shallow and deep values need explicit review but can still use the smaller depth",
    "Large CPUE values may reflect short effort or data entry issues"
  )
)

model_structure_candidates <- tibble::tibble(
  candidate = c(
    "Count model with log(effort_hours) offset",
    "CPUE summary for annual comparison",
    "Depth effect with depth_use",
    "Area effect with cleaned area-year CPUE"
  ),
  note = c(
    "Retain raw catch and effort for future GLMM",
    "Use annual ratio CPUE for descriptive figures",
    "Use one final depth variable instead of multiple variants",
    "Use cleaned data only when comparing spatial CPUE"
  )
)

depth_both_gt50_rows <- raw_tbl |>
  dplyr::filter(.data$flag_depth_both_gt_50)

depth_one_le50_one_gt50_rows <- raw_tbl |>
  dplyr::filter(.data$flag_depth_one_le_50_one_gt_50)

extreme_cpue_cutoff <- stats::quantile(raw_tbl$cpue_total_raw, probs = 0.99, na.rm = TRUE, names = FALSE)
if (!is.finite(extreme_cpue_cutoff)) {
  extreme_cpue_cutoff <- Inf
}

extreme_cpue_candidate_rows <- raw_tbl |>
  dplyr::filter(!is.na(.data$cpue_total_raw), .data$cpue_total_raw >= extreme_cpue_cutoff)

save_check_csv(raw_tbl, file.path("output", "check_tables", "raw_data_check.csv"))
save_check_csv(date_year_summary, file.path("output", "check_tables", "date_year_summary.csv"))
save_check_csv(depth_rule_check_summary, file.path("output", "check_tables", "depth_rule_check_summary.csv"))
save_check_csv(effort_check_summary, file.path("output", "check_tables", "effort_check_summary.csv"))
save_check_csv(data_cleaning_candidates, file.path("output", "check_tables", "data_cleaning_candidates.csv"))
save_check_csv(model_structure_candidates, file.path("output", "check_tables", "model_structure_candidates.csv"))
save_check_csv(depth_both_gt50_rows, file.path("output", "check_tables", "depth_both_gt50_rows.csv"))
save_check_csv(depth_one_le50_one_gt50_rows, file.path("output", "check_tables", "depth_one_le50_one_gt50_rows.csv"))
save_check_csv(extreme_cpue_candidate_rows, file.path("output", "check_tables", "extreme_cpue_candidate_rows.csv"))

make_date_year_check_plot(raw_tbl, file.path("output", "check_figures", "date_year_check.png"))
make_depth_histogram_plot(raw_tbl, file.path("output", "check_figures", "depth_histogram_raw.png"))
make_depth_by_year_plot(raw_tbl, file.path("output", "check_figures", "depth_by_year_raw.png"))
make_depth_missing_plot(raw_tbl, file.path("output", "check_figures", "depth_missing_by_year_area.png"))

cat("used date rule = ymd_reiwa\n")
cat("rows total =", nrow(raw_tbl), "\n")
cat("rows with year in 2020:2024 =", sum(!is.na(raw_tbl$year)), "\n")
cat("rows out of scope =", sum(raw_tbl$flag_year_out_of_scope, na.rm = TRUE), "\n")
cat("vessel_na_n=", sum(is.na(raw_tbl$vessel)), "\n", sep = "")
cat("area_na_n=", sum(is.na(raw_tbl$area)), "\n", sep = "")
cat("month_na_n=", sum(is.na(raw_tbl$month)), "\n", sep = "")
cat("day_na_n=", sum(is.na(raw_tbl$day)), "\n", sep = "")
cat("effort missing n =", sum(raw_tbl$flag_effort_missing, na.rm = TRUE), "\n")
cat("effort nonpositive n =", sum(raw_tbl$flag_effort_nonpositive, na.rm = TRUE), "\n")
cat("depth both missing n =", sum(raw_tbl$flag_depth_both_missing, na.rm = TRUE), "\n")
cat("depth only one n =", sum(raw_tbl$flag_depth_only_one, na.rm = TRUE), "\n")
cat("depth both <= 50 n =", sum(raw_tbl$flag_depth_both_le_50, na.rm = TRUE), "\n")
cat("depth one<=50 one>50 n =", sum(raw_tbl$flag_depth_one_le_50_one_gt_50, na.rm = TRUE), "\n")
cat("depth both > 50 n =", sum(raw_tbl$flag_depth_both_gt_50, na.rm = TRUE), "\n")
