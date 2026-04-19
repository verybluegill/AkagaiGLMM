# =========================================
# make_overview_figure.R
# Excel 直読みによるデータ概要図と概要テーブルを作成する
# =========================================

source(file.path("R", "00_load_packages.R"))

load_project_packages(
  required_pkgs = c("dplyr", "ggplot2", "readr", "readxl", "stringr", "tidyr", "tibble", "scales"),
  optional_pkgs = character()
)

ensure_project_dirs()

get_akagai_default_paths <- function() {
  list(
    excel_path = file.path("ActualData", "Akagai_sheet.xlsx"),
    check_points_path = file.path("PrepareDummyData", "check_points.R")
  )
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

find_column_by_candidates <- function(data, candidates) {
  raw_names <- names(data)
  raw_display <- clean_name_for_display(raw_names)
  norm_names <- clean_name_for_match(raw_names)
  candidate_display <- clean_name_for_display(candidates)
  norm_candidates <- clean_name_for_match(candidates)

  exact_raw_idx <- match(candidate_display, raw_display, nomatch = 0L)
  exact_raw_idx <- exact_raw_idx[exact_raw_idx > 0]

  if (length(exact_raw_idx) > 0) {
    return(raw_names[[exact_raw_idx[[1]]]])
  }

  exact_idx <- match(norm_candidates, norm_names, nomatch = 0L)
  exact_idx <- exact_idx[exact_idx > 0]

  if (length(exact_idx) > 0) {
    return(raw_names[[exact_idx[[1]]]])
  }

  for (cand in candidate_display) {
    hit <- which(vapply(raw_display, function(nm) grepl(cand, nm, fixed = TRUE), logical(1)))

    if (length(hit) > 0) {
      return(raw_names[[hit[[1]]]])
    }
  }

  for (cand in norm_candidates) {
    hit <- which(stringr::str_detect(norm_names, stringr::fixed(cand)))

    if (length(hit) > 0) {
      return(raw_names[[hit[[1]]]])
    }
  }

  NULL
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

find_size_columns <- function(data) {
  raw_names <- names(data)

  size_patterns <- c(
    tokudai = "特大|8\\.6",
    toku = "特\\(|特\\)|8\\.1|8\\.5",
    dai = "大\\(|大\\)|7\\.6|8\\.0",
    medium = "中\\(|中\\)|7\\.1|7\\.5",
    waregai = "割れ?貝|割貝",
    sho = "小\\(|5\\.1|7\\.0",
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
  out <- dplyr::case_when(
    stringr::str_detect(x_chr, "特大|8\\.6") ~ "tokudai",
    stringr::str_detect(x_chr, "特|8\\.1|8\\.5") ~ "toku",
    stringr::str_detect(x_chr, "大|7\\.6|8\\.0") ~ "dai",
    stringr::str_detect(x_chr, "中|7\\.1|7\\.5") ~ "medium",
    stringr::str_detect(x_chr, "割れ?貝|割貝") ~ "waregai",
    stringr::str_detect(x_chr, "小小|小々|5\\.0") ~ "shosho",
    stringr::str_detect(x_chr, "小|5\\.1|7\\.0") ~ "sho",
    TRUE ~ NA_character_
  )
  out
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

coerce_excel_like_date <- function(x) {
  if (inherits(x, "Date")) {
    return(x)
  }

  if (inherits(x, "POSIXt")) {
    return(as.Date(x))
  }

  if (is.numeric(x)) {
    out <- suppressWarnings(as.Date(x, origin = "1899-12-30"))
    return(out)
  }

  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% c("", "NA", "NaN")] <- NA_character_
  out <- suppressWarnings(as.Date(x_chr))

  need_retry <- is.na(out) & !is.na(x_chr)

  if (any(need_retry)) {
    x_chr2 <- gsub("[./]", "-", x_chr[need_retry])
    out[need_retry] <- suppressWarnings(as.Date(x_chr2))
  }

  out
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

convert_reiwa_to_gregorian <- function(year_value, year_col_name = NULL) {
  year_num <- suppressWarnings(as.integer(year_value))

  if (all(is.na(year_num))) {
    return(year_num)
  }

  year_col_name <- clean_name_for_display(year_col_name %||% "")
  looks_reiwa <- stringr::str_detect(year_col_name, "令和|reiwa")

  if (looks_reiwa || max(year_num, na.rm = TRUE) <= 30) {
    year_num[!is.na(year_num)] <- year_num[!is.na(year_num)] + 2018L
  }

  year_num
}

`%||%` <- function(x, y) {
  if (is.null(x)) {
    return(y)
  }
  x
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

  print_vector_diagnostic("candidate date columns", clean_name_for_display(names(dat)[stringr::str_detect(names(dat), "date|日|操業|曳航")]))
  print_vector_diagnostic("candidate area columns", clean_name_for_display(names(dat)[stringr::str_detect(names(dat), "area|漁区|漁場|区画|mesh")]))
  print_vector_diagnostic("candidate size columns", clean_name_for_display(size_col_tbl$size_col))
  print_vector_diagnostic("candidate count columns", clean_name_for_display(names(dat)[stringr::str_detect(names(dat), "count|個体|採取|尾数|数")]))
  cat("detected date column =", clean_name_for_display(detected_cols$date %||% NA_character_), "\n")
  cat("detected year column =", clean_name_for_display(detected_cols$year %||% NA_character_), "\n")
  cat("detected month column =", clean_name_for_display(detected_cols$month %||% NA_character_), "\n")
  cat("detected day column =", clean_name_for_display(detected_cols$day %||% NA_character_), "\n")
  cat("detected area column =", clean_name_for_display(detected_cols$area %||% NA_character_), "\n")
  cat("detected size column =", clean_name_for_display(detected_cols$size %||% NA_character_), "\n")
  cat("detected count column =", clean_name_for_display(detected_cols$count %||% NA_character_), "\n")

  dat <- tibble::as_tibble(dat) |>
    dplyr::mutate(row_id = dplyr::row_number())

  if (!is.null(detected_cols$year)) {
    year_vec <- convert_reiwa_to_gregorian(dat[[detected_cols$year]], detected_cols$year)
  } else {
    year_vec <- rep(NA_integer_, nrow(dat))
  }

  month_vec <- if (!is.null(detected_cols$month)) suppressWarnings(as.integer(dat[[detected_cols$month]])) else rep(NA_integer_, nrow(dat))
  day_vec <- if (!is.null(detected_cols$day)) suppressWarnings(as.integer(dat[[detected_cols$day]])) else rep(NA_integer_, nrow(dat))
  date_vec <- if (!is.null(detected_cols$date)) coerce_excel_like_date(dat[[detected_cols$date]]) else rep(as.Date(NA), nrow(dat))

  if (all(is.na(date_vec)) && any(!is.na(year_vec))) {
    month_fill <- ifelse(is.na(month_vec), 1L, month_vec)
    day_fill <- ifelse(is.na(day_vec), 1L, day_vec)
    date_text <- sprintf("%04d-%02d-%02d", year_vec, month_fill, day_fill)
    date_vec <- suppressWarnings(as.Date(date_text))
  }

  if (all(is.na(year_vec)) && any(!is.na(date_vec))) {
    year_vec <- as.integer(format(date_vec, "%Y"))
  }

  if (all(is.na(month_vec)) && any(!is.na(date_vec))) {
    month_vec <- as.integer(format(date_vec, "%m"))
  }

  area_vec <- if (!is.null(detected_cols$area)) dat[[detected_cols$area]] else rep(NA, nrow(dat))
  area_vec <- clean_name_for_display(area_vec)
  area_vec[area_vec == ""] <- NA_character_

  if (!is.null(detected_cols$size) && !is.null(detected_cols$count)) {
    clean_dat <- dat |>
      dplyr::transmute(
        row_id = .data$row_id,
        date = date_vec,
        year = year_vec,
        month = month_vec,
        area = area_vec,
        size = standardize_size_value(.data[[detected_cols$size]]),
        size_raw = clean_name_for_display(.data[[detected_cols$size]]),
        count = coerce_numeric_value(.data[[detected_cols$count]])
      )
  } else if (nrow(size_col_tbl) > 0) {
    clean_dat <- dat |>
      dplyr::select(dplyr::all_of(c("row_id", size_col_tbl$size_col))) |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(size_col_tbl$size_col),
        names_to = "size_source_col",
        values_to = "count",
        values_transform = list(count = as.character)
      ) |>
      dplyr::left_join(
        size_col_tbl,
        by = c("size_source_col" = "size_col")
      ) |>
      dplyr::mutate(
        date = date_vec[match(.data$row_id, dat$row_id)],
        year = year_vec[match(.data$row_id, dat$row_id)],
        month = month_vec[match(.data$row_id, dat$row_id)],
        area = area_vec[match(.data$row_id, dat$row_id)],
        size_raw = clean_name_for_display(.data$size_source_col),
        count = coerce_numeric_value(.data$count)
      ) |>
      dplyr::transmute(
        row_id = .data$row_id,
        date = .data$date,
        year = .data$year,
        month = .data$month,
        area = .data$area,
        size = .data$size_id,
        size_raw = .data$size_raw,
        count = .data$count
      )
  } else {
    cat("想定列が不明なため、候補列名を再掲します。\n")
    print_vector_diagnostic("raw names", clean_name_for_display(raw_names))
    print(utils::head(dat))
    str(dat)

    clean_dat <- tibble::tibble(
      row_id = dat$row_id,
      date = date_vec,
      year = year_vec,
      month = month_vec,
      area = area_vec,
      size = NA_character_,
      size_raw = NA_character_,
      count = NA_real_
    )
  }

  clean_dat <- clean_dat |>
    dplyr::mutate(
      area = ifelse(is.na(.data$area) | .data$area == "", NA_character_, .data$area),
      count = dplyr::if_else(is.na(.data$count), 0, .data$count),
      size_label = size_id_to_label(.data$size)
    )

  column_map <- tibble::tibble(
    internal_name = c("date", "year", "month", "area", "size", "count"),
    raw_column = c(
      detected_cols$date %||% NA_character_,
      detected_cols$year %||% NA_character_,
      detected_cols$month %||% NA_character_,
      detected_cols$area %||% NA_character_,
      detected_cols$size %||% paste(size_col_tbl$size_col, collapse = " | "),
      detected_cols$count %||% NA_character_
    )
  )

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

  cat("year x size counts\n")
  print(
    clean_dat |>
      dplyr::filter(!is.na(.data$year), !is.na(.data$size)) |>
      dplyr::group_by(.data$year, .data$size_label) |>
      dplyr::summarise(total_count = sum(.data$count, na.rm = TRUE), .groups = "drop")
  )

  cat("year x area counts\n")
  print(
    clean_dat |>
      dplyr::filter(!is.na(.data$year), !is.na(.data$area)) |>
      dplyr::group_by(.data$year, .data$area) |>
      dplyr::summarise(total_count = sum(.data$count, na.rm = TRUE), .groups = "drop")
  )

  attr(clean_dat, "column_map") <- column_map
  attr(clean_dat, "raw_names") <- raw_names
  attr(clean_dat, "cleaned_names") <- cleaned_names
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

  readr::write_csv(summary_csv, file.path("output", "tables", "data_overview_summary.csv"))
  cat("saved:", file.path("output", "tables", "data_overview_summary.csv"), "\n")

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
