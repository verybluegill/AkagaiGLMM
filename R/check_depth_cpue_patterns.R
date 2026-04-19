# =========================================
# check_depth_cpue_patterns.R
# 水深とサイズ別 CPUE の診断図を作成する
# =========================================

source(file.path("R", "make_overview_figure.R"))

dir.create(file.path("output", "check_figures"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path("output", "check_tables"), showWarnings = FALSE, recursive = TRUE)

detect_depth_columns <- function(dat) {
  raw_names <- names(dat)
  idx <- stringr::str_detect(
    raw_names,
    stringr::regex("水深|depth|深度|開始水深|終了水深|上限|下限|浅い側|深い側")
  )
  raw_names[idx]
}

detect_depth_role_columns <- function(dat) {
  depth_cols <- detect_depth_columns(dat)
  shallow_col <- find_column_by_regex(
    dat,
    c("浅い側", "下限", "開始水深", "min_depth", "min", "shallow")
  )
  deep_col <- find_column_by_regex(
    dat,
    c("深い側", "上限", "終了水深", "max_depth", "max", "deep")
  )
  single_cols <- setdiff(depth_cols, c(shallow_col, deep_col))

  list(
    all_cols = depth_cols,
    shallow_col = shallow_col,
    deep_col = deep_col,
    single_cols = single_cols
  )
}

detect_effort_column <- function(dat) {
  find_column_by_regex(
    dat,
    c("曳網時間", "操業時間", "duration", "effort", "hours")
  )
}

detect_vessel_column <- function(dat) {
  find_column_by_regex(
    dat,
    c("船名", "ship", "vessel", "漁船")
  )
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

  colon_idx <- stringr::str_detect(x_chr, ":")
  out <- suppressWarnings(as.numeric(x_chr))
  out[!is.na(out) & out < 1] <- out[!is.na(out) & out < 1] * 24

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

extract_numeric_tokens <- function(x_chr) {
  if (is.na(x_chr) || x_chr == "") {
    return(numeric(0))
  }

  x_chr <- gsub("約", "", x_chr, fixed = TRUE)
  x_chr <- gsub("m", "", x_chr, ignore.case = TRUE)
  x_chr <- gsub("～|〜|−|―", "-", x_chr)
  x_chr <- gsub(",", "-", x_chr, fixed = TRUE)
  num_chr <- stringr::str_extract_all(x_chr, "[0-9]+\\.?[0-9]*")[[1]]
  suppressWarnings(as.numeric(num_chr))
}

parse_depth_vector <- function(x) {
  x_chr <- clean_name_for_display(x)

  out_tbl <- tibble::tibble(
    raw_value = x_chr,
    depth_min_raw = NA_real_,
    depth_max_raw = NA_real_,
    depth_mid_raw = NA_real_,
    parse_failed = FALSE
  )

  for (i in seq_along(x_chr)) {
    if (is.na(x_chr[[i]]) || x_chr[[i]] == "") {
      next
    }

    if (is.numeric(x)) {
      depth_value <- suppressWarnings(as.numeric(x[[i]]))
      if (!is.na(depth_value)) {
        out_tbl$depth_min_raw[[i]] <- depth_value
        out_tbl$depth_max_raw[[i]] <- depth_value
        out_tbl$depth_mid_raw[[i]] <- depth_value
      } else {
        out_tbl$parse_failed[[i]] <- TRUE
      }
      next
    }

    num_vec <- extract_numeric_tokens(x_chr[[i]])

    if (length(num_vec) == 0) {
      out_tbl$parse_failed[[i]] <- TRUE
    } else if (length(num_vec) == 1) {
      out_tbl$depth_min_raw[[i]] <- num_vec[[1]]
      out_tbl$depth_max_raw[[i]] <- num_vec[[1]]
      out_tbl$depth_mid_raw[[i]] <- num_vec[[1]]
    } else {
      out_tbl$depth_min_raw[[i]] <- num_vec[[1]]
      out_tbl$depth_max_raw[[i]] <- num_vec[[2]]
      out_tbl$depth_mid_raw[[i]] <- mean(num_vec[1:2], na.rm = TRUE)
    }
  }

  out_tbl
}

make_depth_numeric_summary <- function(x, label) {
  tibble::tibble(
    variable = label,
    statistic = c("n", "missing_n", "min", "q25", "median", "mean", "q75", "max"),
    value = c(
      length(x),
      sum(is.na(x)),
      if (all(is.na(x))) NA_real_ else min(x, na.rm = TRUE),
      if (all(is.na(x))) NA_real_ else as.numeric(stats::quantile(x, 0.25, na.rm = TRUE)),
      if (all(is.na(x))) NA_real_ else stats::median(x, na.rm = TRUE),
      if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE),
      if (all(is.na(x))) NA_real_ else as.numeric(stats::quantile(x, 0.75, na.rm = TRUE)),
      if (all(is.na(x))) NA_real_ else max(x, na.rm = TRUE)
    )
  )
}

build_depth_from_candidates <- function(dat, depth_roles) {
  candidate_tbl_list <- list()
  target_cols <- c(depth_roles$shallow_col, depth_roles$deep_col, depth_roles$single_cols)
  target_cols <- unique(stats::na.omit(target_cols))

  for (col_i in target_cols) {
    cat("depth candidate =", clean_name_for_display(col_i), "\n")
    cat("class =", paste(class(dat[[col_i]]), collapse = " / "), "\n")
    cat("head =\n")
    print(utils::head(dat[[col_i]], 10))
    cat("summary =\n")
    print(summary(dat[[col_i]]))
    cat("NA count =", sum(is.na(dat[[col_i]])), "\n")

    parsed_i <- parse_depth_vector(dat[[col_i]]) |>
      dplyr::mutate(source_col = col_i, row_id = dat$row_id)

    candidate_tbl_list[[col_i]] <- parsed_i
  }

  if (length(candidate_tbl_list) == 0) {
    return(tibble::tibble(
      row_id = dat$row_id,
      depth_source_col = NA_character_,
      raw_depth_value = NA_character_,
      raw_non_missing = FALSE,
      depth_parse_failed = FALSE,
      depth_shallow = NA_real_,
      depth_deep = NA_real_,
      depth_mid = NA_real_,
      depth_range = NA_real_,
      depth_m = NA_real_
    ))
  }

  candidate_tbl <- dplyr::bind_rows(candidate_tbl_list)
  single_source_tbl <- candidate_tbl |>
    dplyr::filter(.data$source_col %in% depth_roles$single_cols) |>
    dplyr::arrange(.data$row_id, .data$source_col) |>
    dplyr::group_by(.data$row_id) |>
    dplyr::summarise(
      single_source_col = dplyr::first(.data$source_col[!is.na(.data$depth_mid_raw)]),
      single_depth = dplyr::first(.data$depth_mid_raw[!is.na(.data$depth_mid_raw)]),
      single_raw_value = dplyr::first(.data$raw_value[!is.na(.data$raw_value) & .data$raw_value != ""]),
      single_parse_failed = any(.data$parse_failed),
      .groups = "drop"
    )

  shallow_tbl <- if (!is.null(depth_roles$shallow_col)) {
    candidate_tbl |>
      dplyr::filter(.data$source_col == depth_roles$shallow_col) |>
      dplyr::transmute(
        row_id = .data$row_id,
        shallow_source_col = .data$source_col,
        shallow_raw_value = .data$raw_value,
        depth_shallow = .data$depth_mid_raw,
        shallow_parse_failed = .data$parse_failed
      )
  } else {
    tibble::tibble(row_id = dat$row_id, shallow_source_col = NA_character_, shallow_raw_value = NA_character_, depth_shallow = NA_real_, shallow_parse_failed = FALSE)
  }

  deep_tbl <- if (!is.null(depth_roles$deep_col)) {
    candidate_tbl |>
      dplyr::filter(.data$source_col == depth_roles$deep_col) |>
      dplyr::transmute(
        row_id = .data$row_id,
        deep_source_col = .data$source_col,
        deep_raw_value = .data$raw_value,
        depth_deep = .data$depth_mid_raw,
        deep_parse_failed = .data$parse_failed
      )
  } else {
    tibble::tibble(row_id = dat$row_id, deep_source_col = NA_character_, deep_raw_value = NA_character_, depth_deep = NA_real_, deep_parse_failed = FALSE)
  }

  dat |>
    dplyr::select("row_id") |>
    dplyr::left_join(shallow_tbl, by = "row_id") |>
    dplyr::left_join(deep_tbl, by = "row_id") |>
    dplyr::left_join(single_source_tbl, by = "row_id") |>
    dplyr::rowwise() |>
    dplyr::mutate(
      shallow_parse_failed = dplyr::coalesce(.data$shallow_parse_failed, FALSE),
      deep_parse_failed = dplyr::coalesce(.data$deep_parse_failed, FALSE),
      single_parse_failed = dplyr::coalesce(.data$single_parse_failed, FALSE),
      depth_shallow = dplyr::coalesce(.data$depth_shallow, .data$single_depth, .data$depth_deep),
      depth_deep = dplyr::coalesce(.data$depth_deep, .data$single_depth, .data$depth_shallow),
      depth_mid = ifelse(!is.na(.data$depth_shallow) & !is.na(.data$depth_deep), (.data$depth_shallow + .data$depth_deep) / 2, NA_real_),
      depth_range = ifelse(!is.na(.data$depth_shallow) & !is.na(.data$depth_deep), .data$depth_deep - .data$depth_shallow, NA_real_),
      depth_source_col = paste(trimws(stats::na.omit(c(.data$shallow_source_col, .data$deep_source_col, .data$single_source_col)))[trimws(stats::na.omit(c(.data$shallow_source_col, .data$deep_source_col, .data$single_source_col))) != ""], collapse = " | "),
      raw_depth_value = paste(trimws(stats::na.omit(c(.data$shallow_raw_value, .data$deep_raw_value, .data$single_raw_value)))[trimws(stats::na.omit(c(.data$shallow_raw_value, .data$deep_raw_value, .data$single_raw_value))) != ""], collapse = " | "),
      raw_non_missing = !is.na(.data$raw_depth_value) & .data$raw_depth_value != "",
      depth_parse_failed = is.na(.data$depth_mid) & .data$raw_non_missing & (.data$shallow_parse_failed | .data$deep_parse_failed | .data$single_parse_failed),
      depth_m = .data$depth_mid,
      raw_depth_value = dplyr::na_if(.data$raw_depth_value, ""),
      depth_min_raw = .data$depth_shallow,
      depth_max_raw = .data$depth_deep,
      depth_mid_raw = .data$depth_mid
    ) |>
    dplyr::ungroup() |>
    dplyr::select("row_id", "depth_source_col", "raw_depth_value", "raw_non_missing", "depth_parse_failed", "depth_shallow", "depth_deep", "depth_mid", "depth_range", "depth_m", "depth_min_raw", "depth_max_raw", "depth_mid_raw")
}

make_depth_flags <- function(depth_tbl) {
  depth_tbl |>
    dplyr::mutate(
      flag_depth_missing = dplyr::coalesce(!.data$raw_non_missing, TRUE),
      flag_depth_parse_failed = dplyr::coalesce(.data$depth_parse_failed, FALSE),
      flag_depth_negative = !is.na(.data$depth_m) & .data$depth_m < 0,
      flag_depth_zero = !is.na(.data$depth_m) & .data$depth_m == 0,
      flag_depth_too_shallow = !is.na(.data$depth_m) & .data$depth_m < 5,
      flag_depth_too_deep = !is.na(.data$depth_m) & .data$depth_m > 100,
      flag_depth_range_inverted = !is.na(.data$depth_shallow) & !is.na(.data$depth_deep) & .data$depth_shallow > .data$depth_deep,
      flag_depth_shallow_gt_deep = .data$flag_depth_range_inverted,
      flag_depth_range_large = !is.na(.data$depth_range) & .data$depth_range > 100,
      flag_depth_mid_gt100 = !is.na(.data$depth_mid) & .data$depth_mid > 100,
      flag_depth_deep_gt100 = !is.na(.data$depth_deep) & .data$depth_deep > 100,
      flag_depth_suspicious = .data$flag_depth_parse_failed | .data$flag_depth_negative | .data$flag_depth_zero | .data$flag_depth_too_shallow | .data$flag_depth_too_deep | .data$flag_depth_range_inverted | .data$flag_depth_range_large | .data$flag_depth_mid_gt100 | .data$flag_depth_deep_gt100
    )
}

make_depth_size_long <- function(dat, analysis_row_ids, effort_col, vessel_col) {
  size_col_tbl <- find_size_columns(dat) |>
    dplyr::filter(.data$size_id %in% c("medium", "dai", "toku", "tokudai"))

  long_tbl <- dat |>
    dplyr::select(dplyr::all_of(c("row_id", size_col_tbl$size_col))) |>
    tidyr::pivot_longer(
      cols = dplyr::all_of(size_col_tbl$size_col),
      names_to = "size_source_col",
      values_to = "catch_raw",
      values_transform = list(catch_raw = as.character)
    ) |>
    dplyr::left_join(size_col_tbl, by = c("size_source_col" = "size_col")) |>
    dplyr::mutate(
      catch = coerce_numeric_value(.data$catch_raw),
      effort_hours = parse_effort_hours(dat[[effort_col]][match(.data$row_id, dat$row_id)]),
      vessel = if (!is.null(vessel_col)) clean_name_for_display(dat[[vessel_col]][match(.data$row_id, dat$row_id)]) else NA_character_
    ) |>
    dplyr::mutate(
      size_label = factor(
        dplyr::case_when(
          .data$size_id == "medium" ~ "medium",
          .data$size_id == "dai" ~ "large",
          .data$size_id == "toku" ~ "special",
          .data$size_id == "tokudai" ~ "extra_large",
          TRUE ~ as.character(.data$size_id)
        ),
        levels = c("medium", "large", "special", "extra_large")
      )
    ) |>
    dplyr::filter(.data$row_id %in% analysis_row_ids)

  long_tbl
}

plot_depth_histogram <- function(depth_tbl, depth_limit = NULL, output_png, plot_title) {
  plot_data <- depth_tbl |>
    dplyr::filter(!is.na(.data$depth_mid))

  if (!is.null(depth_limit)) {
    plot_data <- dplyr::filter(plot_data, .data$depth_mid <= depth_limit)
  }

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$depth_mid)) +
    ggplot2::geom_histogram(bins = 30, fill = "#4E79A7", color = "white") +
    ggplot2::labs(title = plot_title, x = "Depth (m)", y = "Count") +
    theme_akagai_report()

  ggplot2::ggsave(output_png, p, width = 9, height = 5.5, dpi = 300)
  cat("saved:", output_png, "\n")
}

plot_depth_by_year_boxplot <- function(depth_tbl, depth_limit = NULL, output_png, plot_title) {
  plot_data <- depth_tbl |>
    dplyr::filter(!is.na(.data$depth_mid), !is.na(.data$year))

  if (!is.null(depth_limit)) {
    plot_data <- dplyr::filter(plot_data, .data$depth_mid <= depth_limit)
  }

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = factor(.data$year), y = .data$depth_mid)) +
    ggplot2::geom_boxplot(fill = "#59A14F", outlier.alpha = 0.25) +
    ggplot2::labs(title = plot_title, x = "Year", y = "Depth (m)") +
    theme_akagai_report()

  ggplot2::ggsave(output_png, p, width = 9, height = 5.5, dpi = 300)
  cat("saved:", output_png, "\n")
}

plot_depth_missing_by_year_area <- function(depth_tbl) {
  heat_tbl <- depth_tbl |>
    dplyr::filter(!is.na(.data$year), !is.na(.data$area)) |>
    dplyr::group_by(.data$year, .data$area) |>
    dplyr::summarise(missing_rate = mean(.data$flag_depth_missing | .data$flag_depth_parse_failed), .groups = "drop")

  p <- ggplot2::ggplot(heat_tbl, ggplot2::aes(x = factor(.data$year), y = .data$area, fill = .data$missing_rate)) +
    ggplot2::geom_tile(color = "white", linewidth = 0.15) +
    ggplot2::scale_fill_gradient(low = "white", high = "#D73027", limits = c(0, 1)) +
    ggplot2::labs(title = "Depth missing rate by year and area", x = "Year", y = "Area", fill = "Missing rate") +
    theme_akagai_report(base_size = 10)

  ggplot2::ggsave(file.path("output", "check_figures", "depth_missing_by_year_area.png"), p, width = 10, height = 9, dpi = 300)
  cat("saved:", file.path("output", "check_figures", "depth_missing_by_year_area.png"), "\n")
}

plot_depth_cpue_scatter <- function(plot_tbl, output_png, plot_title, smooth_method = "lm", depth_limit = NULL) {
  plot_data <- plot_tbl

  if (!is.null(depth_limit)) {
    plot_data <- dplyr::filter(plot_data, .data$depth_mid <= depth_limit)
  }

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$depth_mid, y = .data$cpue)) +
    ggplot2::geom_point(alpha = 0.35, size = 1.2, color = "#2C7FB8") +
    ggplot2::geom_smooth(method = smooth_method, se = FALSE, color = "#D95F0E", linewidth = 0.9) +
    ggplot2::facet_wrap(~size_label, scales = "free_y") +
    ggplot2::labs(
      title = plot_title,
      subtitle = "Diagnostic plot only: year, area, and vessel effects are mixed",
      x = "Depth (m)",
      y = "CPUE (count/hour)"
    ) +
    theme_akagai_report()

  ggplot2::ggsave(output_png, p, width = 11, height = 8, dpi = 300)
  cat("saved:", output_png, "\n")
}

plot_depth_shallow_vs_deep_distribution <- function(depth_tbl) {
  plot_data <- depth_tbl |>
    dplyr::select("row_id", "depth_shallow", "depth_deep") |>
    tidyr::pivot_longer(
      cols = c("depth_shallow", "depth_deep"),
      names_to = "depth_type",
      values_to = "depth_value"
    ) |>
    dplyr::filter(!is.na(.data$depth_value)) |>
    dplyr::mutate(
      depth_type = factor(.data$depth_type, levels = c("depth_shallow", "depth_deep"), labels = c("shallow", "deep"))
    )

  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$depth_value, fill = .data$depth_type)) +
    ggplot2::geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
    ggplot2::labs(title = "Shallow and deep depth distributions", x = "Depth (m)", y = "Count", fill = "Depth side") +
    theme_akagai_report()

  output_png <- file.path("output", "check_figures", "depth_shallow_vs_deep_distribution.png")
  ggplot2::ggsave(output_png, p, width = 10, height = 5.5, dpi = 300)
  cat("saved:", output_png, "\n")
}

make_depth_correlations <- function(plot_tbl) {
  overall_tbl <- plot_tbl |>
    dplyr::group_by(.data$size_id, .data$size_label) |>
    dplyr::summarise(
      pearson = suppressWarnings(stats::cor(.data$depth_mid, .data$cpue, method = "pearson")),
      spearman = suppressWarnings(stats::cor(.data$depth_mid, .data$cpue, method = "spearman")),
      n = dplyr::n(),
      scope = "overall",
      .groups = "drop"
    )

  by_year_tbl <- plot_tbl |>
    dplyr::group_by(.data$size_id, .data$size_label, .data$year) |>
    dplyr::summarise(
      pearson = suppressWarnings(stats::cor(.data$depth_mid, .data$cpue, method = "pearson")),
      spearman = suppressWarnings(stats::cor(.data$depth_mid, .data$cpue, method = "spearman")),
      n = dplyr::n(),
      scope = "by_year",
      .groups = "drop"
    )

  dplyr::bind_rows(overall_tbl, by_year_tbl)
}

run_check_depth_cpue_patterns <- function() {
  paths <- get_akagai_default_paths()
  excel_obj <- load_akagai_excel(paths$excel_path)
  clean_dat <- clean_akagai_data(excel_obj$data)
  dat <- tibble::as_tibble(excel_obj$data) |>
    dplyr::mutate(row_id = dplyr::row_number())

  analysis_row_ids <- clean_dat$row_id
  depth_roles <- detect_depth_role_columns(dat)
  effort_col <- detect_effort_column(dat)
  vessel_col <- detect_vessel_column(dat)

  cat("detected depth column(s) =", paste(clean_name_for_display(depth_roles$all_cols), collapse = " | "), "\n")
  cat("detected shallow depth column =", clean_name_for_display(depth_roles$shallow_col %||% NA_character_), "\n")
  cat("detected deep depth column =", clean_name_for_display(depth_roles$deep_col %||% NA_character_), "\n")
  cat("detected effort column =", clean_name_for_display(effort_col %||% NA_character_), "\n")
  cat("detected vessel column =", clean_name_for_display(vessel_col %||% NA_character_), "\n")

  depth_tbl <- build_depth_from_candidates(dat, depth_roles)
  depth_tbl <- make_depth_flags(depth_tbl)

  parse_success_rate <- mean(!is.na(depth_tbl$depth_mid))
  cat("depth parse success rate =", round(parse_success_rate, 4), "\n")
  cat("raw depth examples =\n")
  print(utils::head(unique(depth_tbl$raw_depth_value), 20))
  cat("depth_shallow summary =\n")
  print(summary(depth_tbl$depth_shallow))
  cat("depth_deep summary =\n")
  print(summary(depth_tbl$depth_deep))
  cat("depth_mid summary =\n")
  print(summary(depth_tbl$depth_mid))
  cat("depth_range summary =\n")
  print(summary(depth_tbl$depth_range))

  depth_parse_failed_rows <- dat |>
    dplyr::left_join(depth_tbl, by = "row_id") |>
    dplyr::filter(.data$depth_parse_failed)
  save_check_table(depth_parse_failed_rows, file.path("output", "check_tables", "depth_parse_failed_rows.csv"))

  date_lookup <- clean_dat |>
    dplyr::select("row_id", "date", "year", "area")
  date_lookup <- dplyr::distinct(date_lookup, .data$row_id, .keep_all = TRUE)

  depth_master <- dat |>
    dplyr::select("row_id") |>
    dplyr::left_join(date_lookup, by = "row_id") |>
    dplyr::left_join(depth_tbl, by = "row_id") |>
    dplyr::mutate(
      vessel = if (!is.null(vessel_col)) clean_name_for_display(dat[[vessel_col]][match(.data$row_id, dat$row_id)]) else NA_character_
    ) |>
    dplyr::filter(.data$row_id %in% analysis_row_ids)

  flag_cols <- c(
    "flag_depth_missing",
    "flag_depth_parse_failed",
    "flag_depth_negative",
    "flag_depth_zero",
    "flag_depth_too_shallow",
    "flag_depth_too_deep",
    "flag_depth_range_inverted",
    "flag_depth_shallow_gt_deep",
    "flag_depth_range_large",
    "flag_depth_mid_gt100",
    "flag_depth_deep_gt100",
    "flag_depth_suspicious"
  )

  flag_count_tbl <- tibble::tibble(
    flag = flag_cols,
    n = vapply(flag_cols, function(col_i) sum(depth_master[[col_i]], na.rm = TRUE), numeric(1))
  )
  cat("depth flag counts =\n")
  print(flag_count_tbl)

  depth_candidate_summary <- dplyr::bind_rows(
    make_depth_numeric_summary(depth_master$depth_shallow, "depth_shallow"),
    make_depth_numeric_summary(depth_master$depth_deep, "depth_deep"),
    make_depth_numeric_summary(depth_master$depth_mid, "depth_mid"),
    make_depth_numeric_summary(depth_master$depth_range, "depth_range")
  )
  save_check_table(depth_candidate_summary, file.path("output", "check_tables", "depth_candidate_summary.csv"))

  depth_summary <- depth_master |>
    dplyr::summarise(
      n_rows = dplyr::n(),
      depth_parse_success_rate = mean(!is.na(.data$depth_mid)),
      depth_missing_rate = mean(.data$flag_depth_missing),
      depth_min = min(.data$depth_mid, na.rm = TRUE),
      depth_q25 = stats::quantile(.data$depth_mid, 0.25, na.rm = TRUE),
      depth_median = stats::median(.data$depth_mid, na.rm = TRUE),
      depth_mean = mean(.data$depth_mid, na.rm = TRUE),
      depth_q75 = stats::quantile(.data$depth_mid, 0.75, na.rm = TRUE),
      depth_max = max(.data$depth_mid, na.rm = TRUE)
    )
  save_check_table(depth_summary, file.path("output", "check_tables", "depth_summary.csv"))
  save_check_table(depth_master, file.path("output", "check_tables", "depth_cleaning_flags.csv"))

  depth_extreme_breakdown <- tibble::tibble(
    metric = c("shallow_gt_100", "deep_gt_100", "mid_gt_100", "both_shallow_and_deep_gt_100", "deep_only_gt_100", "shallow_only_gt_100", "range_gt_100"),
    n = c(
      sum(!is.na(depth_master$depth_shallow) & depth_master$depth_shallow > 100),
      sum(!is.na(depth_master$depth_deep) & depth_master$depth_deep > 100),
      sum(!is.na(depth_master$depth_mid) & depth_master$depth_mid > 100),
      sum(!is.na(depth_master$depth_shallow) & depth_master$depth_shallow > 100 & !is.na(depth_master$depth_deep) & depth_master$depth_deep > 100),
      sum((is.na(depth_master$depth_shallow) | depth_master$depth_shallow <= 100) & !is.na(depth_master$depth_deep) & depth_master$depth_deep > 100),
      sum(!is.na(depth_master$depth_shallow) & depth_master$depth_shallow > 100 & (is.na(depth_master$depth_deep) | depth_master$depth_deep <= 100)),
      sum(!is.na(depth_master$depth_range) & depth_master$depth_range > 100)
    )
  )
  cat("extreme depth breakdown =\n")
  print(depth_extreme_breakdown)
  save_check_table(depth_extreme_breakdown, file.path("output", "check_tables", "depth_extreme_breakdown.csv"))

  depth_missing_year <- depth_master |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(missing_rate = mean(.data$flag_depth_missing | .data$flag_depth_parse_failed), n = dplyr::n(), .groups = "drop") |>
    dplyr::mutate(group_type = "year", group_value = as.character(.data$year))
  depth_missing_area <- depth_master |>
    dplyr::group_by(.data$area) |>
    dplyr::summarise(missing_rate = mean(.data$flag_depth_missing | .data$flag_depth_parse_failed), n = dplyr::n(), .groups = "drop") |>
    dplyr::mutate(group_type = "area", group_value = as.character(.data$area))
  depth_missing_vessel <- depth_master |>
    dplyr::group_by(.data$vessel) |>
    dplyr::summarise(missing_rate = mean(.data$flag_depth_missing | .data$flag_depth_parse_failed), n = dplyr::n(), .groups = "drop") |>
    dplyr::mutate(group_type = "vessel", group_value = as.character(.data$vessel))

  cat("depth missing by year =\n")
  print(depth_missing_year)
  cat("depth missing by area =\n")
  print(utils::head(depth_missing_area, 20))
  cat("depth missing by vessel =\n")
  print(depth_missing_vessel)

  depth_missing_summary <- dplyr::bind_rows(depth_missing_year, depth_missing_area, depth_missing_vessel)
  save_check_table(depth_missing_summary, file.path("output", "check_tables", "depth_missing_summary.csv"))
  save_check_table(depth_missing_year, file.path("output", "check_tables", "depth_missing_by_year.csv"))
  save_check_table(depth_missing_area, file.path("output", "check_tables", "depth_missing_by_area.csv"))
  save_check_table(depth_missing_vessel, file.path("output", "check_tables", "depth_missing_by_vessel.csv"))

  depth_missing_year_area <- depth_master |>
    dplyr::filter(!is.na(.data$year), !is.na(.data$area)) |>
    dplyr::group_by(.data$year, .data$area) |>
    dplyr::summarise(missing_rate = mean(.data$flag_depth_missing | .data$flag_depth_parse_failed), n = dplyr::n(), .groups = "drop") |>
    dplyr::mutate(group_type = "year_area", group_value = paste(.data$year, .data$area, sep = " / "))
  depth_missing_year_vessel <- depth_master |>
    dplyr::filter(!is.na(.data$year), !is.na(.data$vessel), .data$vessel != "") |>
    dplyr::group_by(.data$year, .data$vessel) |>
    dplyr::summarise(missing_rate = mean(.data$flag_depth_missing | .data$flag_depth_parse_failed), n = dplyr::n(), .groups = "drop") |>
    dplyr::mutate(group_type = "year_vessel", group_value = paste(.data$year, .data$vessel, sep = " / "))
  depth_missing_high_cells <- dplyr::bind_rows(depth_missing_year_area, depth_missing_year_vessel) |>
    dplyr::filter(.data$missing_rate >= 0.5)
  save_check_table(depth_missing_high_cells, file.path("output", "check_tables", "depth_missing_high_cells.csv"))

  depth_outlier_rows <- depth_master |>
    dplyr::filter(.data$flag_depth_suspicious)
  save_check_table(depth_outlier_rows, file.path("output", "check_tables", "depth_outlier_rows.csv"))

  if (mean(depth_master$flag_depth_missing | depth_master$flag_depth_parse_failed) > 0.2) {
    warning("depth 欠損率が高いため、モデル投入前に欠損バイアスの確認が必要です。")
  }

  low_coverage_year <- depth_missing_year |>
    dplyr::filter(.data$missing_rate > 0.3)
  if (nrow(low_coverage_year) > 0) {
    cat("特定年で depth 欠損が高いです:\n")
    print(low_coverage_year)
  }

  if (is.null(effort_col)) {
    stop("effort 列を検出できませんでした。")
  }

  size_long_tbl <- make_depth_size_long(dat, analysis_row_ids, effort_col, vessel_col)
  size_long_tbl <- size_long_tbl |>
    dplyr::left_join(
      depth_master |>
        dplyr::select("row_id", "date", "year", "area", "vessel", "depth_shallow", "depth_deep", "depth_mid", "depth_range"),
      by = "row_id"
    ) |>
    dplyr::mutate(cpue = .data$catch / .data$effort_hours)

  cpue_main_cleaned <- size_long_tbl |>
    dplyr::filter(!is.na(.data$effort_hours), .data$effort_hours > 0, !is.na(.data$catch))
  cpue_depth_cleaned <- cpue_main_cleaned |>
    dplyr::filter(!is.na(.data$depth_mid))
  save_check_table(utils::head(cpue_main_cleaned, 500), file.path("output", "check_tables", "cpue_main_cleaned_preview.csv"))
  save_check_table(utils::head(cpue_depth_cleaned, 500), file.path("output", "check_tables", "cpue_depth_cleaned_preview.csv"))

  removed_missing_depth <- sum(is.na(size_long_tbl$depth_mid))
  removed_missing_effort <- sum(is.na(size_long_tbl$effort_hours))
  removed_nonpositive_effort <- sum(!is.na(size_long_tbl$effort_hours) & size_long_tbl$effort_hours <= 0)
  removed_missing_catch <- sum(is.na(size_long_tbl$catch))

  cat("depth-cpue plot rows =", nrow(size_long_tbl), "\n")
  cat("removed for missing depth =", removed_missing_depth, "\n")
  cat("removed for missing effort =", removed_missing_effort, "\n")
  cat("removed for nonpositive effort =", removed_nonpositive_effort, "\n")
  cat("removed for missing catch =", removed_missing_catch, "\n")

  plot_tbl <- cpue_depth_cleaned
  cat("depth + cpue cleaned rows =", nrow(plot_tbl), "\n")
  cat("plot preview =\n")
  print(utils::head(dplyr::distinct(plot_tbl, .data$size_label, .data$year, .data$area, .data$depth_mid, .data$cpue), 12))
  save_check_table(plot_tbl, file.path("output", "check_tables", "depth_cpue_plot_data.csv"))

  plot_depth_histogram(depth_master, depth_limit = NULL, output_png = file.path("output", "check_figures", "depth_histogram_full.png"), plot_title = "Depth distribution")
  plot_depth_histogram(depth_master, depth_limit = 100, output_png = file.path("output", "check_figures", "depth_histogram_trim100.png"), plot_title = "Depth distribution (<= 100 m)")
  plot_depth_histogram(depth_master, depth_limit = 60, output_png = file.path("output", "check_figures", "depth_histogram_trim60.png"), plot_title = "Depth distribution (<= 60 m)")
  plot_depth_by_year_boxplot(depth_master, depth_limit = NULL, output_png = file.path("output", "check_figures", "depth_by_year_boxplot_full.png"), plot_title = "Depth by year")
  plot_depth_by_year_boxplot(depth_master, depth_limit = 100, output_png = file.path("output", "check_figures", "depth_by_year_boxplot_trim100.png"), plot_title = "Depth by year (<= 100 m)")
  plot_depth_missing_by_year_area(depth_master)
  plot_depth_cpue_scatter(plot_tbl, output_png = file.path("output", "check_figures", "depth_cpue_by_size_scatter_full.png"), plot_title = "Depth and CPUE by size class", smooth_method = "lm", depth_limit = NULL)
  plot_depth_cpue_scatter(plot_tbl, output_png = file.path("output", "check_figures", "depth_cpue_by_size_scatter_trim100.png"), plot_title = "Depth and CPUE by size class (<= 100 m)", smooth_method = "lm", depth_limit = 100)
  plot_depth_cpue_scatter(plot_tbl, output_png = file.path("output", "check_figures", "depth_cpue_by_size_scatter_loess_trim100.png"), plot_title = "Depth and CPUE by size class (loess, <= 100 m)", smooth_method = "loess", depth_limit = 100)
  plot_depth_shallow_vs_deep_distribution(depth_master)

  corr_tbl <- make_depth_correlations(plot_tbl)
  save_check_table(corr_tbl, file.path("output", "check_tables", "depth_cpue_correlations.csv"))

  depth_missing_model_note <- tibble::tibble(
    item = c("overall_missing_rate", "high_missing_year_area_cells", "high_missing_year_vessel_cells", "interpretation"),
    value = c(
      round(mean(depth_master$flag_depth_missing | depth_master$flag_depth_parse_failed), 4),
      nrow(depth_missing_year_area |> dplyr::filter(.data$missing_rate >= 0.5)),
      nrow(depth_missing_year_vessel |> dplyr::filter(.data$missing_rate >= 0.5)),
      "Depth missingness should be checked before complete-case modeling because year/area/vessel imbalance may remain."
    )
  )
  save_check_table(depth_missing_model_note, file.path("output", "check_tables", "depth_missing_model_note.csv"))

  model_note_tbl <- tibble::tibble(
    candidate = c("depth_none", "depth_linear", "depth_quadratic", "depth_spline"),
    note = c(
      "欠損や外れ値が大きい場合の比較基準",
      "まずは fixed effect として depth_shallow または depth_mid を線形投入する候補",
      "深度と CPUE の曲線関係を疑う場合の 2 次項候補",
      "非線形が強そうなら spline/GAM 的に扱う候補"
    )
  )
  save_check_table(model_note_tbl, file.path("output", "check_tables", "depth_model_candidate_note.csv"))

  invisible(list(
    depth_summary = depth_summary,
    flag_count_tbl = flag_count_tbl,
    corr_tbl = corr_tbl
  ))
}

if (sys.nframe() == 0) {
  run_check_depth_cpue_patterns()
}
