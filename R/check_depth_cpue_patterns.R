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

build_depth_from_candidates <- function(dat, depth_cols) {
  candidate_tbl_list <- list()

  for (col_i in depth_cols) {
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

  candidate_tbl <- dplyr::bind_rows(candidate_tbl_list)

  selected_depth <- candidate_tbl |>
    dplyr::arrange(.data$row_id, .data$source_col) |>
    dplyr::group_by(.data$row_id) |>
    dplyr::summarise(
      depth_source_col = paste(unique(.data$source_col[!is.na(.data$depth_mid_raw)]), collapse = " | "),
      depth_min_raw = if (all(is.na(.data$depth_min_raw))) NA_real_ else min(.data$depth_min_raw, na.rm = TRUE),
      depth_max_raw = if (all(is.na(.data$depth_max_raw))) NA_real_ else max(.data$depth_max_raw, na.rm = TRUE),
      raw_depth_value = paste(unique(.data$raw_value[!is.na(.data$raw_value) & .data$raw_value != ""]), collapse = " | "),
      raw_non_missing = any(!is.na(.data$raw_value) & .data$raw_value != ""),
      parse_failed_any = any(.data$parse_failed),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      depth_mid_raw = ifelse(!is.na(.data$depth_min_raw) & !is.na(.data$depth_max_raw), (.data$depth_min_raw + .data$depth_max_raw) / 2, NA_real_)
    )

  selected_depth |>
    dplyr::mutate(
      depth_m = .data$depth_mid_raw,
      raw_depth_value = dplyr::na_if(.data$raw_depth_value, ""),
      depth_parse_failed = is.na(.data$depth_m) & .data$raw_non_missing & .data$parse_failed_any
    )
}

make_depth_flags <- function(depth_tbl) {
  depth_tbl |>
    dplyr::mutate(
      flag_depth_missing = is.na(.data$raw_depth_value) | .data$raw_depth_value == "",
      flag_depth_parse_failed = .data$depth_parse_failed,
      flag_depth_negative = !is.na(.data$depth_m) & .data$depth_m < 0,
      flag_depth_zero = !is.na(.data$depth_m) & .data$depth_m == 0,
      flag_depth_too_shallow = !is.na(.data$depth_m) & .data$depth_m < 5,
      flag_depth_too_deep = !is.na(.data$depth_m) & .data$depth_m > 100,
      flag_depth_range_inverted = !is.na(.data$depth_min_raw) & !is.na(.data$depth_max_raw) & .data$depth_min_raw > .data$depth_max_raw,
      flag_depth_suspicious = .data$flag_depth_parse_failed | .data$flag_depth_negative | .data$flag_depth_zero | .data$flag_depth_too_shallow | .data$flag_depth_too_deep | .data$flag_depth_range_inverted
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
        size_id_to_label(.data$size_id),
        levels = c("中", "大", "特", "特大")
      )
    ) |>
    dplyr::filter(.data$row_id %in% analysis_row_ids)

  long_tbl
}

plot_depth_histogram <- function(depth_tbl) {
  p <- ggplot2::ggplot(depth_tbl |> dplyr::filter(!is.na(.data$depth_m)), ggplot2::aes(x = .data$depth_m)) +
    ggplot2::geom_histogram(bins = 30, fill = "#4E79A7", color = "white") +
    ggplot2::labs(title = "Depth distribution", x = "Depth (m)", y = "Count") +
    theme_akagai_report()

  ggplot2::ggsave(file.path("output", "check_figures", "depth_histogram.png"), p, width = 9, height = 5.5, dpi = 300)
  cat("saved:", file.path("output", "check_figures", "depth_histogram.png"), "\n")
}

plot_depth_by_year_boxplot <- function(depth_tbl) {
  p <- ggplot2::ggplot(depth_tbl |> dplyr::filter(!is.na(.data$depth_m), !is.na(.data$year)), ggplot2::aes(x = factor(.data$year), y = .data$depth_m)) +
    ggplot2::geom_boxplot(fill = "#59A14F", outlier.alpha = 0.25) +
    ggplot2::labs(title = "Depth by year", x = "Year", y = "Depth (m)") +
    theme_akagai_report()

  ggplot2::ggsave(file.path("output", "check_figures", "depth_by_year_boxplot.png"), p, width = 9, height = 5.5, dpi = 300)
  cat("saved:", file.path("output", "check_figures", "depth_by_year_boxplot.png"), "\n")
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

plot_depth_cpue_scatter <- function(plot_tbl) {
  p <- ggplot2::ggplot(plot_tbl, ggplot2::aes(x = .data$depth_m, y = .data$cpue)) +
    ggplot2::geom_point(alpha = 0.35, size = 1.2, color = "#2C7FB8") +
    ggplot2::geom_smooth(method = "lm", se = FALSE, color = "#D95F0E", linewidth = 0.9) +
    ggplot2::facet_wrap(~size_label, scales = "free_y") +
    ggplot2::labs(
      title = "Depth and CPUE by size class",
      subtitle = "Diagnostic plot only: year, area, and vessel effects are mixed",
      x = "Depth (m)",
      y = "CPUE (count/hour)"
    ) +
    theme_akagai_report()

  ggplot2::ggsave(file.path("output", "check_figures", "depth_cpue_by_size_scatter.png"), p, width = 11, height = 8, dpi = 300)
  cat("saved:", file.path("output", "check_figures", "depth_cpue_by_size_scatter.png"), "\n")

  p_log <- p +
    ggplot2::scale_y_continuous(trans = "log1p") +
    ggplot2::labs(y = "CPUE log1p(count/hour)")

  ggplot2::ggsave(file.path("output", "check_figures", "depth_cpue_by_size_scatter_log1p.png"), p_log, width = 11, height = 8, dpi = 300)
  cat("saved:", file.path("output", "check_figures", "depth_cpue_by_size_scatter_log1p.png"), "\n")
}

make_depth_correlations <- function(plot_tbl) {
  overall_tbl <- plot_tbl |>
    dplyr::group_by(.data$size_id, .data$size_label) |>
    dplyr::summarise(
      pearson = suppressWarnings(stats::cor(.data$depth_m, .data$cpue, method = "pearson")),
      spearman = suppressWarnings(stats::cor(.data$depth_m, .data$cpue, method = "spearman")),
      n = dplyr::n(),
      scope = "overall",
      .groups = "drop"
    )

  by_year_tbl <- plot_tbl |>
    dplyr::group_by(.data$size_id, .data$size_label, .data$year) |>
    dplyr::summarise(
      pearson = suppressWarnings(stats::cor(.data$depth_m, .data$cpue, method = "pearson")),
      spearman = suppressWarnings(stats::cor(.data$depth_m, .data$cpue, method = "spearman")),
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
  depth_cols <- detect_depth_columns(dat)
  effort_col <- detect_effort_column(dat)
  vessel_col <- detect_vessel_column(dat)

  cat("detected depth column(s) =", paste(clean_name_for_display(depth_cols), collapse = " | "), "\n")
  cat("detected effort column =", clean_name_for_display(effort_col %||% NA_character_), "\n")
  cat("detected vessel column =", clean_name_for_display(vessel_col %||% NA_character_), "\n")

  depth_tbl <- build_depth_from_candidates(dat, depth_cols)
  depth_tbl <- make_depth_flags(depth_tbl)

  parse_success_rate <- mean(!is.na(depth_tbl$depth_m))
  cat("depth parse success rate =", round(parse_success_rate, 4), "\n")
  cat("raw depth examples =\n")
  print(utils::head(unique(depth_tbl$raw_depth_value), 20))
  cat("cleaned depth summary =\n")
  print(summary(depth_tbl$depth_m))

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
    "flag_depth_suspicious"
  )

  flag_count_tbl <- tibble::tibble(
    flag = flag_cols,
    n = vapply(flag_cols, function(col_i) sum(depth_master[[col_i]], na.rm = TRUE), numeric(1))
  )
  cat("depth flag counts =\n")
  print(flag_count_tbl)

  depth_summary <- depth_master |>
    dplyr::summarise(
      n_rows = dplyr::n(),
      depth_parse_success_rate = mean(!is.na(.data$depth_m)),
      depth_missing_rate = mean(.data$flag_depth_missing),
      depth_min = min(.data$depth_m, na.rm = TRUE),
      depth_q25 = stats::quantile(.data$depth_m, 0.25, na.rm = TRUE),
      depth_median = stats::median(.data$depth_m, na.rm = TRUE),
      depth_mean = mean(.data$depth_m, na.rm = TRUE),
      depth_q75 = stats::quantile(.data$depth_m, 0.75, na.rm = TRUE),
      depth_max = max(.data$depth_m, na.rm = TRUE)
    )
  save_check_table(depth_summary, file.path("output", "check_tables", "depth_summary.csv"))
  save_check_table(depth_master, file.path("output", "check_tables", "depth_cleaning_flags.csv"))

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
        dplyr::select("row_id", "date", "year", "area", "vessel", "depth_m"),
      by = "row_id"
    ) |>
    dplyr::mutate(cpue = .data$catch / .data$effort_hours)

  removed_missing_depth <- sum(is.na(size_long_tbl$depth_m))
  removed_missing_effort <- sum(is.na(size_long_tbl$effort_hours))
  removed_nonpositive_effort <- sum(!is.na(size_long_tbl$effort_hours) & size_long_tbl$effort_hours <= 0)
  removed_missing_catch <- sum(is.na(size_long_tbl$catch))

  cat("depth-cpue plot rows =", nrow(size_long_tbl), "\n")
  cat("removed for missing depth =", removed_missing_depth, "\n")
  cat("removed for missing effort =", removed_missing_effort, "\n")
  cat("removed for nonpositive effort =", removed_nonpositive_effort, "\n")
  cat("removed for missing catch =", removed_missing_catch, "\n")

  plot_tbl <- size_long_tbl |>
    dplyr::filter(!is.na(.data$depth_m), !is.na(.data$effort_hours), .data$effort_hours > 0, !is.na(.data$catch))
  save_check_table(plot_tbl, file.path("output", "check_tables", "depth_cpue_plot_data.csv"))

  plot_depth_histogram(depth_master)
  plot_depth_by_year_boxplot(depth_master)
  plot_depth_missing_by_year_area(depth_master)
  plot_depth_cpue_scatter(plot_tbl)

  corr_tbl <- make_depth_correlations(plot_tbl)
  save_check_table(corr_tbl, file.path("output", "check_tables", "depth_cpue_correlations.csv"))

  model_note_tbl <- tibble::tibble(
    candidate = c("depth_none", "depth_linear", "depth_quadratic", "depth_spline"),
    note = c(
      "欠損や外れ値が大きい場合の比較基準",
      "まずは fixed effect として depth_m を線形投入する候補",
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
