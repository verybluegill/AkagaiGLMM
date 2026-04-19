# =========================================
# make_overview_figure.R
# Excel 直読みによるデータ概要図と診断用チェック出力を作成する
# =========================================

source(file.path("R", "00_load_packages.R"))

load_project_packages(
  required_pkgs = c("dplyr", "ggplot2", "lubridate", "readr", "readxl", "stringr", "tidyr", "tibble", "scales"),
  optional_pkgs = character()
)

ensure_project_dirs()
dir.create(file.path("output", "check_tables"), showWarnings = FALSE, recursive = TRUE)

get_akagai_default_paths <- function() {
  list(
    excel_path = file.path("ActualData", "Akagai_sheet.xlsx"),
    check_points_path = file.path("PrepareDummyData", "check_points.R")
  )
}

get_akagai_analysis_period <- function() {
  c(as.Date("2020-09-01"), as.Date("2024-06-30"))
}

`%||%` <- function(x, y) {
  if (is.null(x)) {
    return(y)
  }
  x
}

clean_name_for_display <- function(x) {
  x <- enc2utf8(as.character(x))
  x[is.na(x)] <- ""
  x <- gsub("[\r\n\t]+", " ", x, perl = TRUE)
  x <- gsub("[[:space:]]+", " ", x, perl = TRUE)
  trimws(x)
}

clean_name_for_match <- function(x) {
  x <- clean_name_for_display(x)
  x <- tolower(x)
  x <- gsub("[[:space:][:punct:]]+", "", x, perl = TRUE)
  x
}

make_clean_names_local <- function(x) {
  x <- clean_name_for_display(x)
  x <- tolower(x)
  x <- gsub("[[:space:]]+", "_", x, perl = TRUE)
  x <- gsub("[^[:alnum:]_]+", "_", x, perl = TRUE)
  x <- gsub("_+", "_", x, perl = TRUE)
  x <- gsub("^_|_$", "", x, perl = TRUE)
  x[x == ""] <- "blank"
  make.unique(x, sep = "_")
}

print_vector_diagnostic <- function(label, x) {
  cat(label, "=", paste(x, collapse = " | "), "\n")
}

save_check_table <- function(data, path) {
  readr::write_csv(data, path)
  cat("saved:", path, "\n")
}

find_column_by_regex <- function(data, patterns) {
  raw_names <- names(data)

  for (pat in patterns) {
    hit <- raw_names[stringr::str_detect(raw_names, stringr::regex(pat))]

    if (length(hit) > 0) {
      return(hit[[1]])
    }
  }

  NULL
}

size_id_to_label <- function(size_id) {
  dplyr::case_when(
    size_id == "tokudai" ~ "特大",
    size_id == "toku" ~ "特",
    size_id == "dai" ~ "大",
    size_id == "medium" ~ "中",
    size_id == "waregai" ~ "割貝",
    size_id == "sho" ~ "小",
    size_id == "shosho" ~ "小々",
    TRUE ~ as.character(size_id)
  )
}

find_size_columns <- function(data) {
  raw_names <- names(data)
  size_patterns <- c(
    tokudai = "特大|8\\.6",
    toku = "特\\(|特\\)|8\\.1|8\\.5",
    dai = "大\\(|大\\)|7\\.6|8\\.0",
    medium = "中\\(|中\\)|7\\.1|7\\.5",
    waregai = "割れ?貝|割貝",
    sho = "小\\(|7\\.0|5\\.1",
    shosho = "小小|小々|5\\.0"
  )

  matched <- lapply(names(size_patterns), function(size_id) {
    hit <- raw_names[stringr::str_detect(raw_names, stringr::regex(size_patterns[[size_id]]))]

    if (length(hit) == 0) {
      return(NULL)
    }

    tibble::tibble(size_id = size_id, size_col = hit[[1]])
  })

  dplyr::bind_rows(matched)
}

standardize_size_value <- function(x) {
  x_chr <- clean_name_for_display(x)

  dplyr::case_when(
    stringr::str_detect(x_chr, "特大|8\\.6") ~ "tokudai",
    stringr::str_detect(x_chr, "特|8\\.1|8\\.5") ~ "toku",
    stringr::str_detect(x_chr, "大|7\\.6|8\\.0") ~ "dai",
    stringr::str_detect(x_chr, "中|7\\.1|7\\.5") ~ "medium",
    stringr::str_detect(x_chr, "割れ?貝|割貝") ~ "waregai",
    stringr::str_detect(x_chr, "小小|小々|5\\.0") ~ "shosho",
    stringr::str_detect(x_chr, "小|7\\.0|5\\.1") ~ "sho",
    TRUE ~ NA_character_
  )
}

coerce_numeric_value <- function(x) {
  if (is.numeric(x)) {
    return(as.numeric(x))
  }

  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% c("", "NA", "NaN")] <- NA_character_
  x_chr <- gsub(",", "", x_chr, fixed = TRUE)
  suppressWarnings(as.numeric(x_chr))
}

parse_date_flexibly <- function(x) {
  if (inherits(x, "Date")) {
    return(as.Date(x))
  }

  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }

  if (is.numeric(x)) {
    return(suppressWarnings(as.Date(x, origin = "1899-12-30")))
  }

  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% c("", "NA", "NaN")] <- NA_character_
  out <- suppressWarnings(lubridate::ymd(x_chr, quiet = TRUE))

  need_retry <- is.na(out) & !is.na(x_chr)
  if (any(need_retry)) {
    out[need_retry] <- suppressWarnings(as.Date(lubridate::ymd_hms(x_chr[need_retry], quiet = TRUE)))
  }

  need_retry <- is.na(out) & !is.na(x_chr)
  if (any(need_retry)) {
    out[need_retry] <- suppressWarnings(as.Date(lubridate::parse_date_time(
      x_chr[need_retry],
      orders = c("ymd", "Ymd", "Y/m/d", "Y-m-d", "Ymd HMS", "Ymd HM", "ymd HMS", "ymd HM", "Y/m/d HMS", "Y/m/d HM"),
      quiet = TRUE
    )))
  }

  out
}

diagnose_date_candidate_column <- function(data, col_name) {
  x <- data[[col_name]]
  cat("date candidate =", clean_name_for_display(col_name), "\n")
  cat("class =", paste(class(x), collapse = " / "), "\n")
  cat("head =\n")
  print(utils::head(x))
  cat("summary =\n")
  print(summary(x))
  cat("NA count =", sum(is.na(x)), "\n")
}

convert_year_component <- function(year_value, mode = c("auto", "gregorian", "reiwa")) {
  mode <- match.arg(mode)
  year_num <- suppressWarnings(as.integer(year_value))

  if (mode == "gregorian") {
    return(year_num)
  }

  if (mode == "reiwa") {
    year_num[!is.na(year_num)] <- year_num[!is.na(year_num)] + 2018L
    return(year_num)
  }

  if (all(is.na(year_num))) {
    return(year_num)
  }

  if (max(year_num, na.rm = TRUE) <= 20) {
    year_num[!is.na(year_num)] <- year_num[!is.na(year_num)] + 2018L
  }

  year_num
}

make_date_from_components <- function(year_value, month_value, day_value, mode = c("auto", "gregorian", "reiwa")) {
  year_num <- convert_year_component(year_value, mode = mode[[1]])
  month_num <- suppressWarnings(as.integer(month_value))
  day_num <- suppressWarnings(as.integer(day_value))

  date_text <- sprintf(
    "%04d-%02d-%02d",
    year_num,
    ifelse(is.na(month_num), 1L, month_num),
    ifelse(is.na(day_num), 1L, day_num)
  )

  suppressWarnings(as.Date(date_text))
}

choose_best_date_vector <- function(candidate_list) {
  if (length(candidate_list) == 0) {
    return(list(name = NA_character_, date = rep(as.Date(NA), 0), success_rate = NA_real_))
  }

  score_tbl <- dplyr::bind_rows(lapply(names(candidate_list), function(name_i) {
    date_i <- candidate_list[[name_i]]
    year_i <- suppressWarnings(as.integer(format(date_i, "%Y")))
    tibble::tibble(
      candidate_name = name_i,
      success_rate = mean(!is.na(date_i)),
      in_range_rate = mean(year_i %in% 2020:2024, na.rm = TRUE),
      in_range_count = sum(year_i %in% 2020:2024, na.rm = TRUE)
    )
  }))

  score_tbl <- score_tbl |>
    dplyr::arrange(dplyr::desc(.data$in_range_count), dplyr::desc(.data$success_rate))

  best_name <- score_tbl$candidate_name[[1]]

  list(
    name = best_name,
    date = candidate_list[[best_name]],
    success_rate = score_tbl$success_rate[[1]],
    score_table = score_tbl
  )
}

plot_date_parsing_check <- function(date_check_tbl, output_png = file.path("output", "figures", "date_parsing_check.png")) {
  plot_tbl <- date_check_tbl |>
    dplyr::mutate(row_index = dplyr::row_number())

  p_raw <- ggplot2::ggplot(dplyr::filter(plot_tbl, !is.na(.data$raw_date_numeric)), ggplot2::aes(x = .data$row_index, y = .data$raw_date_numeric)) +
    ggplot2::geom_point(size = 0.8, alpha = 0.6, color = "#4E79A7") +
    ggplot2::labs(title = "Raw date candidate", x = "Row", y = "Raw value") +
    theme_akagai_report(base_size = 10)

  p_parsed <- ggplot2::ggplot(dplyr::filter(plot_tbl, !is.na(.data$parsed_date)), ggplot2::aes(x = .data$row_index, y = .data$parsed_date)) +
    ggplot2::geom_point(size = 0.8, alpha = 0.6, color = "#59A14F") +
    ggplot2::labs(title = "Parsed date", x = "Row", y = "Parsed date") +
    theme_akagai_report(base_size = 10)

  year_count_tbl <- plot_tbl |>
    dplyr::filter(!is.na(.data$parsed_year)) |>
    dplyr::count(.data$parsed_year, name = "n")

  p_year <- ggplot2::ggplot(year_count_tbl, ggplot2::aes(x = .data$parsed_year, y = .data$n)) +
    ggplot2::geom_col(fill = "#E15759") +
    ggplot2::scale_x_continuous(breaks = sort(unique(year_count_tbl$parsed_year))) +
    ggplot2::labs(title = "Parsed year counts", x = "Year", y = "Rows") +
    theme_akagai_report(base_size = 10)

  grDevices::png(output_png, width = 12, height = 10, units = "in", res = 300)
  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(3, 1)))
  print(p_raw, vp = grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
  print(p_parsed, vp = grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
  print(p_year, vp = grid::viewport(layout.pos.row = 3, layout.pos.col = 1))
  grDevices::dev.off()

  cat("saved:", output_png, "\n")
}

load_akagai_excel <- function(excel_path = get_akagai_default_paths()$excel_path, sheet = NULL) {
  cat("Excel file exists =", file.exists(excel_path), "\n")

  if (!file.exists(excel_path)) {
    return(list(data = NULL, sheet = NULL, diagnostics = list(excel_path = excel_path)))
  }

  available_sheets <- readxl::excel_sheets(excel_path)
  print_vector_diagnostic("available sheets", available_sheets)

  used_sheet <- sheet %||% available_sheets[[1]]
  cat("used sheet =", used_sheet, "\n")

  dat <- readxl::read_excel(excel_path, sheet = used_sheet)
  raw_names <- names(dat)
  cleaned_names <- make_clean_names_local(raw_names)

  print_vector_diagnostic("raw names", clean_name_for_display(raw_names))
  print_vector_diagnostic("cleaned names", cleaned_names)
  cat("loaded_rows =", nrow(dat), "\n")
  print(utils::head(dat))
  str(dat)

  list(
    data = dat,
    sheet = used_sheet,
    diagnostics = list(
      excel_path = excel_path,
      available_sheets = available_sheets,
      raw_names = raw_names,
      cleaned_names = cleaned_names
    )
  )
}

clean_akagai_data <- function(dat) {
  if (is.null(dat) || nrow(dat) == 0) {
    cat("clean_akagai_data: input data is empty.\n")
    return(tibble::tibble())
  }

  raw_names <- names(dat)
  cleaned_names <- make_clean_names_local(raw_names)
  dat <- tibble::as_tibble(dat) |>
    dplyr::mutate(row_id = dplyr::row_number())

  date_candidate_cols <- raw_names[stringr::str_detect(raw_names, stringr::regex("date|日付|操業日|曳航日|年|月|日"))]
  date_candidate_cols <- unique(date_candidate_cols)
  print_vector_diagnostic("date candidate columns", clean_name_for_display(date_candidate_cols))

  for (col_i in date_candidate_cols) {
    diagnose_date_candidate_column(dat, col_i)
  }

  detected_cols <- list(
    date = find_column_by_regex(dat, c("(^|[^[:alpha:]])date([^[:alpha:]]|$)", "日付", "操業日", "曳航日")),
    year = find_column_by_regex(dat, c("年.*令和", "^年$", "(^|[^[:alpha:]])year([^[:alpha:]]|$)")),
    month = find_column_by_regex(dat, c("^月$", "(^|[^[:alpha:]])month([^[:alpha:]]|$)")),
    day = find_column_by_regex(dat, c("^日$", "(^|[^[:alpha:]])day([^[:alpha:]]|$)")),
    area = find_column_by_regex(dat, c("開始.*漁区.*番号", "終了.*漁区.*番号", "漁区", "区画", "mesh", "漁場", "(^|[^[:alpha:]])area([^[:alpha:]]|$)")),
    size = find_column_by_regex(dat, c("サイズ", "(^|[^[:alpha:]])size([^[:alpha:]]|$)")),
    count = find_column_by_regex(dat, c("漁獲個数.*合計", "採取個体数", "個体数", "尾数", "(^|[^[:alpha:]])count([^[:alpha:]]|$)"))
  )
  size_col_tbl <- find_size_columns(dat)

  print_vector_diagnostic("candidate area columns", clean_name_for_display(raw_names[stringr::str_detect(raw_names, "漁区|漁場|区画|mesh")]))
  print_vector_diagnostic("candidate size columns", clean_name_for_display(size_col_tbl$size_col))
  print_vector_diagnostic("candidate count columns", clean_name_for_display(raw_names[stringr::str_detect(raw_names, "個体数|採取個体数|尾数|漁獲個数|count")]))
  cat("detected date column =", clean_name_for_display(detected_cols$date %||% NA_character_), "\n")
  cat("detected year column =", clean_name_for_display(detected_cols$year %||% NA_character_), "\n")
  cat("detected month column =", clean_name_for_display(detected_cols$month %||% NA_character_), "\n")
  cat("detected day column =", clean_name_for_display(detected_cols$day %||% NA_character_), "\n")
  cat("detected area column =", clean_name_for_display(detected_cols$area %||% NA_character_), "\n")
  cat("detected size column =", clean_name_for_display(detected_cols$size %||% NA_character_), "\n")
  cat("detected count column =", clean_name_for_display(detected_cols$count %||% NA_character_), "\n")

  date_candidates <- list()

  if (!is.null(detected_cols$date)) {
    date_candidates[[paste0("direct:", detected_cols$date)]] <- parse_date_flexibly(dat[[detected_cols$date]])
  }

  if (!is.null(detected_cols$year) && !is.null(detected_cols$month) && !is.null(detected_cols$day)) {
    date_candidates[["ymd_gregorian"]] <- make_date_from_components(
      dat[[detected_cols$year]],
      dat[[detected_cols$month]],
      dat[[detected_cols$day]],
      mode = "gregorian"
    )
    date_candidates[["ymd_reiwa"]] <- make_date_from_components(
      dat[[detected_cols$year]],
      dat[[detected_cols$month]],
      dat[[detected_cols$day]],
      mode = "reiwa"
    )
  }

  chosen_date <- choose_best_date_vector(date_candidates)
  parsed_date <- chosen_date$date
  parsed_year <- suppressWarnings(as.integer(format(parsed_date, "%Y")))
  parsed_month <- suppressWarnings(as.integer(format(parsed_date, "%m")))

  cat("date parse success rate =", round(chosen_date$success_rate, 4), "\n")
  if (!is.null(chosen_date$score_table)) {
    print(chosen_date$score_table)
  }
  if (all(is.na(parsed_date))) {
    cat("min(date) = NA\n")
    cat("max(date) = NA\n")
  } else {
    cat("min(date) =", format(min(parsed_date, na.rm = TRUE)), "\n")
    cat("max(date) =", format(max(parsed_date, na.rm = TRUE)), "\n")
  }
  cat("raw year table =\n")
  print(table(parsed_year, useNA = "ifany"))

  date_check_tbl <- tibble::tibble(
    row_id = dat$row_id,
    date_candidate_name = chosen_date$name,
    raw_date_value = if (!is.null(detected_cols$date)) as.character(dat[[detected_cols$date]]) else NA_character_,
    raw_date_numeric = if (!is.null(detected_cols$date)) coerce_numeric_value(dat[[detected_cols$date]]) else NA_real_,
    parsed_date = parsed_date,
    parsed_year = parsed_year
  )
  save_check_table(date_check_tbl, file.path("output", "check_tables", "date_parsing_check.csv"))
  plot_date_parsing_check(date_check_tbl)

  suspicious_year_rows <- dat |>
    dplyr::mutate(parsed_date = parsed_date, year = parsed_year) |>
    dplyr::filter(!is.na(.data$year), !(.data$year %in% 2020:2024))
  suspicious_years <- sort(unique(stats::na.omit(suspicious_year_rows$year)))
  print_vector_diagnostic("suspicious years", suspicious_years)
  save_check_table(suspicious_year_rows, file.path("output", "check_tables", "suspicious_year_rows.csv"))

  year_na_rows <- dat |>
    dplyr::mutate(parsed_date = parsed_date, year = parsed_year) |>
    dplyr::filter(is.na(.data$year))
  save_check_table(year_na_rows, file.path("output", "check_tables", "year_na_rows.csv"))

  analysis_period <- get_akagai_analysis_period()
  suspicious_date_rows <- dat |>
    dplyr::mutate(parsed_date = parsed_date, year = parsed_year) |>
    dplyr::filter(!is.na(.data$parsed_date), (.data$parsed_date < analysis_period[[1]] | .data$parsed_date > analysis_period[[2]]))
  save_check_table(suspicious_date_rows, file.path("output", "check_tables", "suspicious_date_rows.csv"))

  filtered_row_id <- dat |>
    dplyr::mutate(year = parsed_year, parsed_date = parsed_date) |>
    dplyr::filter(.data$year %in% 2020:2024, !is.na(.data$parsed_date), .data$parsed_date >= analysis_period[[1]], .data$parsed_date <= analysis_period[[2]]) |>
    dplyr::pull(.data$row_id)

  cat("filtered year table =\n")
  print(table(parsed_year[dat$row_id %in% filtered_row_id], useNA = "ifany"))

  area_vec <- if (!is.null(detected_cols$area)) clean_name_for_display(dat[[detected_cols$area]]) else rep(NA_character_, nrow(dat))
  area_vec[area_vec == ""] <- NA_character_

  size_structure <- if (!is.null(detected_cols$size) && !is.null(detected_cols$count)) "long" else if (nrow(size_col_tbl) > 0) "wide" else "unknown"
  cat("detected size structure =", size_structure, "\n")
  print_vector_diagnostic("size columns used", clean_name_for_display(size_col_tbl$size_col))

  if (identical(size_structure, "long")) {
    clean_dat_all <- dat |>
      dplyr::transmute(
        row_id = .data$row_id,
        date = parsed_date,
        year = parsed_year,
        month = parsed_month,
        area = area_vec,
        size = standardize_size_value(.data[[detected_cols$size]]),
        size_raw = clean_name_for_display(.data[[detected_cols$size]]),
        count = coerce_numeric_value(.data[[detected_cols$count]])
      )
  } else if (identical(size_structure, "wide")) {
    wide_before_total <- sum(vapply(size_col_tbl$size_col, function(col_i) sum(coerce_numeric_value(dat[[col_i]]), na.rm = TRUE), numeric(1)))
    cat("wide nrow before =", nrow(dat), "\n")
    cat("wide total before =", wide_before_total, "\n")

    long_dat <- dat |>
      dplyr::select(dplyr::all_of(c("row_id", size_col_tbl$size_col))) |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(size_col_tbl$size_col),
        names_to = "size_source_col",
        values_to = "count_raw",
        values_transform = list(count_raw = as.character)
      ) |>
      dplyr::left_join(size_col_tbl, by = c("size_source_col" = "size_col"))

    cat("wide nrow after pivot_longer =", nrow(long_dat), "\n")
    cat("wide total after =", sum(coerce_numeric_value(long_dat$count_raw), na.rm = TRUE), "\n")

    clean_dat_all <- long_dat |>
      dplyr::transmute(
        row_id = .data$row_id,
        date = parsed_date[match(.data$row_id, dat$row_id)],
        year = parsed_year[match(.data$row_id, dat$row_id)],
        month = parsed_month[match(.data$row_id, dat$row_id)],
        area = area_vec[match(.data$row_id, dat$row_id)],
        size = .data$size_id,
        size_raw = clean_name_for_display(.data$size_source_col),
        count = coerce_numeric_value(.data$count_raw)
      )
  } else {
    cat("サイズ構造を判定できませんでした。\n")
    clean_dat_all <- tibble::tibble(
      row_id = dat$row_id,
      date = parsed_date,
      year = parsed_year,
      month = parsed_month,
      area = area_vec,
      size = NA_character_,
      size_raw = NA_character_,
      count = NA_real_
    )
  }

  clean_dat_all <- clean_dat_all |>
    dplyr::mutate(
      count = dplyr::if_else(is.na(.data$count), 0, .data$count),
      size_label = size_id_to_label(.data$size)
    )

  size_year_totals_long <- clean_dat_all |>
    dplyr::filter(.data$row_id %in% filtered_row_id, !is.na(.data$size)) |>
    dplyr::group_by(.data$size, .data$size_label, .data$year) |>
    dplyr::summarise(total_count = sum(.data$count, na.rm = TRUE), .groups = "drop")
  save_check_table(size_year_totals_long, file.path("output", "check_tables", "size_year_totals_long.csv"))

  size_year_area_totals_long <- clean_dat_all |>
    dplyr::filter(.data$row_id %in% filtered_row_id, !is.na(.data$size), !is.na(.data$area)) |>
    dplyr::group_by(.data$size, .data$size_label, .data$year, .data$area) |>
    dplyr::summarise(total_count = sum(.data$count, na.rm = TRUE), .groups = "drop")
  save_check_table(size_year_area_totals_long, file.path("output", "check_tables", "size_year_area_totals_long.csv"))

  size_year_totals_wide <- size_year_totals_long |>
    dplyr::select("year", "size_label", "total_count") |>
    tidyr::pivot_wider(names_from = "size_label", values_from = "total_count")
  save_check_table(size_year_totals_wide, file.path("output", "check_tables", "size_year_totals_wide.csv"))

  cat("each size total =\n")
  print(
    clean_dat_all |>
      dplyr::filter(.data$row_id %in% filtered_row_id, !is.na(.data$size_label)) |>
      dplyr::group_by(.data$size_label) |>
      dplyr::summarise(total_count = sum(.data$count, na.rm = TRUE), .groups = "drop")
  )
  cat("year x size table =\n")
  print(size_year_totals_long)

  clean_dat <- clean_dat_all |>
    dplyr::filter(.data$row_id %in% filtered_row_id)

  date_range_text <- if (all(is.na(clean_dat$date))) "NA to NA" else paste(range(clean_dat$date, na.rm = TRUE), collapse = " to ")
  cat("date range =", date_range_text, "\n")
  cat("area unique count =", dplyr::n_distinct(stats::na.omit(clean_dat$area)), "\n")
  print_vector_diagnostic("detected size columns", clean_name_for_display(size_col_tbl$size_col))

  overview_size_totals <- clean_dat |>
    dplyr::filter(!is.na(.data$size)) |>
    dplyr::group_by(.data$size_label) |>
    dplyr::summarise(total_count = sum(.data$count, na.rm = TRUE), .groups = "drop")
  cat("overview_size_totals =\n")
  print(overview_size_totals)

  cat("year x area counts\n")
  print(
    clean_dat |>
      dplyr::filter(!is.na(.data$year), !is.na(.data$area)) |>
      dplyr::group_by(.data$year, .data$area) |>
      dplyr::summarise(total_count = sum(.data$count, na.rm = TRUE), .groups = "drop")
  )

  column_map <- tibble::tibble(
    internal_name = c("date", "year", "month", "area", "size", "count"),
    raw_column = c(
      chosen_date$name %||% NA_character_,
      detected_cols$year %||% NA_character_,
      detected_cols$month %||% NA_character_,
      detected_cols$area %||% NA_character_,
      detected_cols$size %||% paste(size_col_tbl$size_col, collapse = " | "),
      detected_cols$count %||% NA_character_
    )
  )

  attr(clean_dat, "column_map") <- column_map
  attr(clean_dat, "raw_names") <- raw_names
  attr(clean_dat, "cleaned_names") <- cleaned_names
  attr(clean_dat, "size_structure") <- size_structure
  clean_dat
}

theme_akagai_report <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", size = base_size + 1),
      legend.title = ggplot2::element_text(face = "bold"),
      axis.title = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(face = "bold", size = base_size + 3),
      plot.subtitle = ggplot2::element_text(size = base_size),
      panel.background = ggplot2::element_rect(fill = "white", color = NA),
      plot.background = ggplot2::element_rect(fill = "white", color = NA)
    )
}

make_overview_summary_data <- function(clean_dat) {
  size_order <- c("tokudai", "toku", "dai", "medium", "waregai", "sho", "shosho")

  summary_tbl <- tibble::tibble(size = size_order) |>
    dplyr::mutate(size_label = size_id_to_label(.data$size)) |>
    dplyr::left_join(
      clean_dat |>
        dplyr::filter(.data$size %in% size_order) |>
        dplyr::group_by(.data$size) |>
        dplyr::summarise(total_count = sum(.data$count, na.rm = TRUE), .groups = "drop"),
      by = "size"
    ) |>
    dplyr::mutate(
      total_count = dplyr::if_else(is.na(.data$total_count), 0, .data$total_count),
      total_count_label = scales::comma(.data$total_count)
    )

  period_text <- if (all(is.na(clean_dat$date))) {
    "NA"
  } else {
    paste(format(min(clean_dat$date, na.rm = TRUE), "%Y-%m-%d"), "to", format(max(clean_dat$date, na.rm = TRUE), "%Y-%m-%d"))
  }

  bullets <- c(
    paste0("Number of tows: ", scales::comma(dplyr::n_distinct(clean_dat$row_id))),
    paste0("Period: ", period_text),
    paste0("Fishing areas: ", scales::comma(dplyr::n_distinct(stats::na.omit(clean_dat$area)))),
    "Recorded variables: date / year / month / area / size / count"
  )

  list(
    bullets = bullets,
    summary_tbl = summary_tbl
  )
}

plot_overview_summary <- function(summary_data, output_png = file.path("output", "figures", "data_overview_summary.png")) {
  grDevices::png(
    filename = output_png,
    width = 13.5,
    height = 5.5,
    units = "in",
    res = 300
  )

  grid::grid.newpage()
  grid::pushViewport(grid::viewport(layout = grid::grid.layout(2, 1, heights = grid::unit(c(0.52, 0.48), "npc"))))

  grid::pushViewport(grid::viewport(layout.pos.row = 1, layout.pos.col = 1))
  grid::grid.rect(gp = grid::gpar(fill = "white", col = NA))
  grid::grid.text(
    "Akagai data overview",
    x = grid::unit(0.02, "npc"),
    y = grid::unit(0.92, "npc"),
    just = c("left", "top"),
    gp = grid::gpar(fontface = "bold", cex = 1.35)
  )

  bullet_y <- seq(0.72, 0.18, length.out = length(summary_data$bullets))

  for (i in seq_along(summary_data$bullets)) {
    grid::grid.text(
      paste0("\u2022 ", summary_data$bullets[[i]]),
      x = grid::unit(0.03, "npc"),
      y = grid::unit(bullet_y[[i]], "npc"),
      just = c("left", "center"),
      gp = grid::gpar(cex = 1.08)
    )
  }
  grid::popViewport()

  grid::pushViewport(grid::viewport(layout.pos.row = 2, layout.pos.col = 1))
  tbl <- summary_data$summary_tbl
  n_cols <- nrow(tbl)
  left_margin <- 0.03
  usable_width <- 0.94
  col_width <- usable_width / n_cols
  header_y <- 0.68
  value_y <- 0.32
  cell_h <- 0.34

  grid::grid.text(
    "Total sampled individuals by size class",
    x = grid::unit(0.03, "npc"),
    y = grid::unit(0.98, "npc"),
    just = c("left", "top"),
    gp = grid::gpar(fontface = "bold", cex = 1.05)
  )

  for (i in seq_len(n_cols)) {
    x_center <- left_margin + (i - 0.5) * col_width

    grid::grid.rect(
      x = grid::unit(x_center, "npc"),
      y = grid::unit(header_y, "npc"),
      width = grid::unit(col_width - 0.004, "npc"),
      height = grid::unit(cell_h, "npc"),
      gp = grid::gpar(fill = "#2F6DB2", col = "#4A4A4A", lwd = 1)
    )
    grid::grid.text(
      tbl$size_label[[i]],
      x = grid::unit(x_center, "npc"),
      y = grid::unit(header_y, "npc"),
      gp = grid::gpar(col = "white", fontface = "bold", cex = 1.0)
    )

    grid::grid.rect(
      x = grid::unit(x_center, "npc"),
      y = grid::unit(value_y, "npc"),
      width = grid::unit(col_width - 0.004, "npc"),
      height = grid::unit(cell_h, "npc"),
      gp = grid::gpar(fill = "#EFEFEF", col = "#4A4A4A", lwd = 1)
    )
    grid::grid.text(
      tbl$total_count_label[[i]],
      x = grid::unit(x_center, "npc"),
      y = grid::unit(value_y, "npc"),
      gp = grid::gpar(col = "black", cex = 1.0)
    )
  }

  grid::popViewport()
  grid::popViewport()
  grDevices::dev.off()

  cat("saved:", output_png, "\n")
}

run_make_overview_figure <- function() {
  paths <- get_akagai_default_paths()
  excel_obj <- load_akagai_excel(paths$excel_path)
  clean_dat <- clean_akagai_data(excel_obj$data)
  summary_data <- make_overview_summary_data(clean_dat)

  summary_csv <- summary_data$summary_tbl |>
    dplyr::select("size_label", "total_count")

  save_check_table(summary_csv, file.path("output", "tables", "data_overview_summary.csv"))
  plot_overview_summary(
    summary_data = summary_data,
    output_png = file.path("output", "figures", "data_overview_summary.png")
  )

  invisible(list(
    excel_sheet = excel_obj$sheet,
    column_map = attr(clean_dat, "column_map"),
    summary_data = summary_data
  ))
}

if (sys.nframe() == 0) {
  run_make_overview_figure()
}
