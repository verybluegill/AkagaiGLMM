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
      flag_depth_range_gt100 = .data$flag_depth_range_large,
      flag_depth_mid_gt100 = !is.na(.data$depth_mid) & .data$depth_mid > 100,
      flag_depth_shallow_gt100 = !is.na(.data$depth_shallow) & .data$depth_shallow > 100,
      flag_depth_deep_gt100 = !is.na(.data$depth_deep) & .data$depth_deep > 100,
      flag_depth_suspicious = .data$flag_depth_parse_failed | .data$flag_depth_negative | .data$flag_depth_zero | .data$flag_depth_too_shallow | .data$flag_depth_too_deep | .data$flag_depth_range_inverted | .data$flag_depth_range_large | .data$flag_depth_mid_gt100 | .data$flag_depth_shallow_gt100 | .data$flag_depth_deep_gt100
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

size_id_to_report_label <- function(size_id) {
  dplyr::case_when(
    size_id == "medium" ~ "Medium",
    size_id == "dai" ~ "Large",
    size_id == "toku" ~ "Special",
    size_id == "tokudai" ~ "Extra large",
    TRUE ~ as.character(size_id)
  )
}

prepare_depth_check_context <- function(dat, clean_dat, depth_roles, effort_col, vessel_col) {
  detected_cols <- list(
    year = find_column_by_regex(dat, c("年.*令和", "^年$", "(^|[^[:alpha:]])year([^[:alpha:]]|$)")),
    month = find_column_by_regex(dat, c("^月$", "(^|[^[:alpha:]])month([^[:alpha:]]|$)")),
    day = find_column_by_regex(dat, c("^日$", "(^|[^[:alpha:]])day([^[:alpha:]]|$)")),
    area = find_column_by_regex(dat, c("開始.*漁区.*番号", "終了.*漁区.*番号", "漁区", "区画", "mesh", "漁場", "(^|[^[:alpha:]])area([^[:alpha:]]|$)")),
    total_count = find_column_by_regex(dat, c("漁獲個数.*合計", "採取個体数", "個体数", "尾数", "(^|[^[:alpha:]])count([^[:alpha:]]|$)"))
  )

  year_raw <- if (!is.null(detected_cols$year)) suppressWarnings(as.integer(dat[[detected_cols$year]])) else rep(NA_integer_, nrow(dat))
  month_raw <- if (!is.null(detected_cols$month)) suppressWarnings(as.integer(dat[[detected_cols$month]])) else rep(NA_integer_, nrow(dat))
  day_raw <- if (!is.null(detected_cols$day)) suppressWarnings(as.integer(dat[[detected_cols$day]])) else rep(NA_integer_, nrow(dat))
  ymd_reiwa <- if (!is.null(detected_cols$year) && !is.null(detected_cols$month) && !is.null(detected_cols$day)) {
    make_date_from_components(dat[[detected_cols$year]], dat[[detected_cols$month]], dat[[detected_cols$day]], mode = "reiwa")
  } else {
    rep(as.Date(NA), nrow(dat))
  }

  area_value <- if (!is.null(detected_cols$area)) clean_name_for_display(dat[[detected_cols$area]]) else rep(NA_character_, nrow(dat))
  vessel_value <- if (!is.null(vessel_col)) clean_name_for_display(dat[[vessel_col]]) else rep(NA_character_, nrow(dat))
  effort_value <- if (!is.null(effort_col)) parse_effort_hours(dat[[effort_col]]) else rep(NA_real_, nrow(dat))
  effort_raw <- if (!is.null(effort_col)) as.character(dat[[effort_col]]) else rep(NA_character_, nrow(dat))

  size_col_tbl <- find_size_columns(dat)
  total_catch <- if (nrow(size_col_tbl) > 0) {
    rowSums(as.data.frame(lapply(size_col_tbl$size_col, function(col_i) coerce_numeric_value(dat[[col_i]]))), na.rm = TRUE)
  } else if (!is.null(detected_cols$total_count)) {
    coerce_numeric_value(dat[[detected_cols$total_count]])
  } else {
    rep(NA_real_, nrow(dat))
  }

  date_lookup <- clean_dat |>
    dplyr::select("row_id", "date", "year", "area") |>
    dplyr::distinct(.data$row_id, .keep_all = TRUE)

  dat |>
    dplyr::transmute(
      row_id = .data$row_id,
      year = suppressWarnings(as.integer(format(ymd_reiwa, "%Y"))),
      month = dplyr::coalesce(month_raw, suppressWarnings(as.integer(format(ymd_reiwa, "%m")))),
      day = dplyr::coalesce(day_raw, suppressWarnings(as.integer(format(ymd_reiwa, "%d")))),
      ymd_reiwa = ymd_reiwa,
      vessel = vessel_value,
      area = area_value,
      `水深(m)浅い側` = if (!is.null(depth_roles$shallow_col)) as.character(dat[[depth_roles$shallow_col]]) else NA_character_,
      `水深(m)深い側` = if (!is.null(depth_roles$deep_col)) as.character(dat[[depth_roles$deep_col]]) else NA_character_,
      `曳網時間` = effort_raw,
      effort_hours = effort_value,
      `total catch` = total_catch
    ) |>
    dplyr::left_join(date_lookup, by = "row_id", suffix = c("", "_clean")) |>
    dplyr::mutate(
      year = dplyr::coalesce(.data$year, .data$year_clean),
      area = dplyr::coalesce(.data$area, .data$area_clean)
    ) |>
    dplyr::select(-dplyr::any_of(c("date", "year_clean", "area_clean")))
}

save_flag_breakdowns <- function(flag_tbl, flag_col, output_stub) {
  year_tbl <- flag_tbl |>
    dplyr::filter(.data[[flag_col]]) |>
    dplyr::count(.data$year, name = "n", sort = TRUE)
  area_tbl <- flag_tbl |>
    dplyr::filter(.data[[flag_col]]) |>
    dplyr::count(.data$area, name = "n", sort = TRUE)
  vessel_tbl <- flag_tbl |>
    dplyr::filter(.data[[flag_col]]) |>
    dplyr::count(.data$vessel, name = "n", sort = TRUE)

  save_check_table(year_tbl, file.path("output", "check_tables", paste0(output_stub, "_year_counts.csv")))
  save_check_table(area_tbl, file.path("output", "check_tables", paste0(output_stub, "_area_counts.csv")))
  save_check_table(vessel_tbl, file.path("output", "check_tables", paste0(output_stub, "_vessel_counts.csv")))

  list(year = year_tbl, area = area_tbl, vessel = vessel_tbl)
}

print_flag_bias_if_needed <- function(label, tbl) {
  if (nrow(tbl) == 0 || sum(tbl$n, na.rm = TRUE) == 0) {
    return(invisible(NULL))
  }

  share_top <- max(tbl$n, na.rm = TRUE) / sum(tbl$n, na.rm = TRUE)
  if (is.finite(share_top) && share_top >= 0.5) {
    cat(label, "is strongly concentrated; top share =", round(share_top, 3), "\n")
  }
}

build_depth_rule_tables <- function(depth_master) {
  depth_rule_raw <- depth_master |>
    dplyr::transmute(
      row_id = .data$row_id,
      depth_rule = "depth_rule_raw",
      depth_shallow = .data$depth_shallow,
      depth_deep = .data$depth_deep,
      depth_mid = .data$depth_mid,
      depth_range = .data$depth_range
    )

  depth_rule_swapped <- depth_master |>
    dplyr::transmute(
      row_id = .data$row_id,
      depth_rule = "depth_rule_swapped",
      swapped_flag = .data$flag_depth_shallow_gt_deep,
      shallow_fixed = dplyr::if_else(.data$flag_depth_shallow_gt_deep, .data$depth_deep, .data$depth_shallow),
      deep_fixed = dplyr::if_else(.data$flag_depth_shallow_gt_deep, .data$depth_shallow, .data$depth_deep)
    ) |>
    dplyr::mutate(
      mid_fixed = ifelse(!is.na(.data$shallow_fixed) & !is.na(.data$deep_fixed), (.data$shallow_fixed + .data$deep_fixed) / 2, NA_real_),
      range_fixed = ifelse(!is.na(.data$shallow_fixed) & !is.na(.data$deep_fixed), .data$deep_fixed - .data$shallow_fixed, NA_real_)
    )

  depth_rule_trimmed <- depth_rule_swapped |>
    dplyr::mutate(
      trim_shallow_gt100 = !is.na(.data$shallow_fixed) & .data$shallow_fixed > 100,
      trim_deep_gt100 = !is.na(.data$deep_fixed) & .data$deep_fixed > 100,
      trim_mid_gt100 = !is.na(.data$mid_fixed) & .data$mid_fixed > 100,
      trim_range_gt100 = !is.na(.data$range_fixed) & .data$range_fixed > 100,
      shallow_trimmed = dplyr::if_else(.data$trim_shallow_gt100, NA_real_, .data$shallow_fixed),
      deep_trimmed = dplyr::if_else(.data$trim_deep_gt100, NA_real_, .data$deep_fixed),
      mid_trimmed = dplyr::if_else(.data$trim_mid_gt100 | .data$trim_range_gt100 | .data$trim_shallow_gt100 | .data$trim_deep_gt100, NA_real_, .data$mid_fixed),
      range_trimmed = dplyr::if_else(.data$trim_mid_gt100 | .data$trim_range_gt100 | .data$trim_shallow_gt100 | .data$trim_deep_gt100, NA_real_, .data$range_fixed),
      depth_rule = "depth_rule_trimmed"
    )

  list(
    raw = depth_rule_raw,
    swapped = depth_rule_swapped,
    trimmed = depth_rule_trimmed
  )
}

make_depth_rule_summary <- function(depth_master, rule_tbl, rule_name, shallow_col, deep_col, mid_col, range_col) {
  metric_tbl <- dplyr::bind_rows(
    make_depth_numeric_summary(rule_tbl[[shallow_col]], paste0(rule_name, ":depth_shallow")),
    make_depth_numeric_summary(rule_tbl[[deep_col]], paste0(rule_name, ":depth_deep")),
    make_depth_numeric_summary(rule_tbl[[mid_col]], paste0(rule_name, ":depth_mid")),
    make_depth_numeric_summary(rule_tbl[[range_col]], paste0(rule_name, ":depth_range"))
  ) |>
    dplyr::mutate(scope = "overall", group = NA_character_)

  year_tbl <- depth_master |>
    dplyr::select("row_id", "year") |>
    dplyr::left_join(rule_tbl, by = "row_id") |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(non_missing_n = sum(!is.na(.data[[mid_col]])), .groups = "drop") |>
    dplyr::transmute(variable = paste0(rule_name, ":depth_mid"), statistic = "non_missing_n", value = .data$non_missing_n, scope = "year", group = as.character(.data$year))

  area_tbl <- depth_master |>
    dplyr::select("row_id", "area") |>
    dplyr::left_join(rule_tbl, by = "row_id") |>
    dplyr::group_by(.data$area) |>
    dplyr::summarise(non_missing_n = sum(!is.na(.data[[mid_col]])), .groups = "drop") |>
    dplyr::transmute(variable = paste0(rule_name, ":depth_mid"), statistic = "non_missing_n", value = .data$non_missing_n, scope = "area", group = as.character(.data$area))

  vessel_tbl <- depth_master |>
    dplyr::select("row_id", "vessel") |>
    dplyr::left_join(rule_tbl, by = "row_id") |>
    dplyr::group_by(.data$vessel) |>
    dplyr::summarise(non_missing_n = sum(!is.na(.data[[mid_col]])), .groups = "drop") |>
    dplyr::transmute(variable = paste0(rule_name, ":depth_mid"), statistic = "non_missing_n", value = .data$non_missing_n, scope = "vessel", group = as.character(.data$vessel))

  dplyr::bind_rows(metric_tbl, year_tbl, area_tbl, vessel_tbl)
}

build_model_input_preview <- function(depth_master, size_long_tbl, rule_tbl, rule_type = c("raw", "swapped", "trimmed")) {
  rule_type <- match.arg(rule_type)

  base_tbl <- size_long_tbl |>
    dplyr::filter(!is.na(.data$effort_hours), .data$effort_hours > 0) |>
    dplyr::mutate(
      catch_total = .data$catch,
      log_effort = log(.data$effort_hours),
      cpue_total = .data$catch / .data$effort_hours
    )

  if (identical(rule_type, "raw")) {
    return(base_tbl |>
      dplyr::transmute(
        row_id = .data$row_id,
        catch_total = .data$catch_total,
        effort_hours = .data$effort_hours,
        log_effort = .data$log_effort,
        cpue_total = .data$cpue_total,
        year = .data$year,
        month = .data$month,
        area = .data$area,
        vessel = .data$vessel,
        depth_shallow = .data$depth_shallow,
        depth_deep = .data$depth_deep,
        depth_mid = .data$depth_mid,
        depth_range = .data$depth_range,
        swapped_depth_shallow = .data$depth_shallow,
        swapped_depth_deep = .data$depth_deep,
        swapped_depth_mid = .data$depth_mid,
        swapped_depth_range = .data$depth_range,
        trimmed_depth_shallow = .data$depth_shallow,
        trimmed_depth_deep = .data$depth_deep,
        trimmed_depth_mid = .data$depth_mid,
        trimmed_depth_range = .data$depth_range
      ))
  }

  if (identical(rule_type, "swapped")) {
    return(base_tbl |>
      dplyr::left_join(rule_tbl, by = "row_id") |>
      dplyr::transmute(
        row_id = .data$row_id,
        catch_total = .data$catch_total,
        effort_hours = .data$effort_hours,
        log_effort = .data$log_effort,
        cpue_total = .data$cpue_total,
        year = .data$year,
        month = .data$month,
        area = .data$area,
        vessel = .data$vessel,
        depth_shallow = .data$depth_shallow,
        depth_deep = .data$depth_deep,
        depth_mid = .data$depth_mid,
        depth_range = .data$depth_range,
        swapped_depth_shallow = .data$shallow_fixed,
        swapped_depth_deep = .data$deep_fixed,
        swapped_depth_mid = .data$mid_fixed,
        swapped_depth_range = .data$range_fixed,
        trimmed_depth_shallow = .data$shallow_fixed,
        trimmed_depth_deep = .data$deep_fixed,
        trimmed_depth_mid = .data$mid_fixed,
        trimmed_depth_range = .data$range_fixed
      ))
  }

  base_tbl |>
    dplyr::left_join(rule_tbl, by = "row_id") |>
    dplyr::transmute(
      row_id = .data$row_id,
      catch_total = .data$catch_total,
      effort_hours = .data$effort_hours,
      log_effort = .data$log_effort,
      cpue_total = .data$cpue_total,
      year = .data$year,
      month = .data$month,
      area = .data$area,
      vessel = .data$vessel,
      depth_shallow = .data$depth_shallow,
      depth_deep = .data$depth_deep,
      depth_mid = .data$depth_mid,
      depth_range = .data$depth_range,
      swapped_depth_shallow = .data$shallow_fixed,
      swapped_depth_deep = .data$deep_fixed,
      swapped_depth_mid = .data$mid_fixed,
      swapped_depth_range = .data$range_fixed,
      trimmed_depth_shallow = .data$shallow_trimmed,
      trimmed_depth_deep = .data$deep_trimmed,
      trimmed_depth_mid = .data$mid_trimmed,
      trimmed_depth_range = .data$range_trimmed
    )
}

plot_depth_cpue_scatter_diagnostic <- function(plot_tbl, output_png, plot_title, y_free = FALSE) {
  p <- ggplot2::ggplot(plot_tbl, ggplot2::aes(x = .data$depth_mid, y = .data$cpue)) +
    ggplot2::geom_point(alpha = 0.3, size = 1.1, color = "#2C7FB8") +
    ggplot2::facet_wrap(~size_label, scales = if (y_free) "free_y" else "fixed") +
    ggplot2::labs(title = plot_title, x = "Depth (m)", y = "CPUE (count/hour)") +
    theme_akagai_report()

  ggplot2::ggsave(output_png, p, width = 11, height = 8, dpi = 300)
  cat("saved:", output_png, "\n")
}

plot_report_depth_cpue <- function(summary_tbl, y_col, output_png, plot_title, ribbon = FALSE) {
  plot_tbl <- summary_tbl |>
    dplyr::filter(!is.na(.data[[y_col]])) |>
    dplyr::mutate(size_label = factor(size_id_to_report_label(.data$size_id), levels = c("Medium", "Large", "Special", "Extra large")))

  p <- ggplot2::ggplot(plot_tbl, ggplot2::aes(x = .data$depth_bin_center, y = .data[[y_col]])) +
    {if (ribbon) ggplot2::geom_ribbon(ggplot2::aes(ymin = .data$q25, ymax = .data$q75), fill = "#9ECAE1", alpha = 0.25) else ggplot2::geom_blank()} +
    ggplot2::geom_line(linewidth = 1.1, color = "#2C7FB8") +
    ggplot2::geom_point(size = 1.8, color = "#6BAED6") +
    ggplot2::facet_wrap(~size_label, ncol = 2, scales = "free_y") +
    ggplot2::labs(title = plot_title, x = "Depth (m)", y = "CPUE (count/hour)") +
    ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(color = "#E6E6E6", linewidth = 0.25),
      strip.background = ggplot2::element_rect(fill = "white", color = "white"),
      strip.text = ggplot2::element_text(face = "bold"),
      legend.position = "none",
      axis.title = ggplot2::element_text(face = "bold"),
      plot.title = ggplot2::element_text(face = "bold")
    )

  ggplot2::ggsave(output_png, p, width = 10, height = 8, dpi = 300)
  cat("saved:", output_png, "\n")
}

run_check_depth_cpue_patterns <- function() {
  paths <- get_akagai_default_paths()
  excel_obj <- load_akagai_excel(paths$excel_path)
  clean_dat <- clean_akagai_data(excel_obj$data)
  dat <- tibble::as_tibble(excel_obj$data) |>
    dplyr::mutate(row_id = dplyr::row_number())

  analysis_row_ids <- clean_dat$row_id
  date_out_of_scope_n <- sum(!(dat$row_id %in% analysis_row_ids))
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
  context_tbl <- prepare_depth_check_context(dat, clean_dat, depth_roles, effort_col, vessel_col)

  depth_master <- dat |>
    dplyr::select("row_id") |>
    dplyr::left_join(date_lookup, by = "row_id") |>
    dplyr::left_join(context_tbl, by = "row_id", suffix = c("", "_context")) |>
    dplyr::left_join(depth_tbl, by = "row_id") |>
    dplyr::mutate(
      year = dplyr::coalesce(.data$year, .data$year_context),
      area = dplyr::coalesce(.data$area, .data$area_context),
      vessel = .data$vessel
    ) |>
    dplyr::select(-dplyr::any_of(c("year_context", "area_context"))) |>
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
    "flag_depth_range_gt100",
    "flag_depth_range_large",
    "flag_depth_mid_gt100",
    "flag_depth_shallow_gt100",
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

  flag_export_specs <- list(
    list(flag = "flag_depth_shallow_gt_deep", rows = "depth_shallow_gt_deep_rows.csv", stub = "depth_shallow_gt_deep"),
    list(flag = "flag_depth_range_gt100", rows = "depth_range_gt100_rows.csv", stub = "depth_range_gt100"),
    list(flag = "flag_depth_mid_gt100", rows = "depth_mid_gt100_rows.csv", stub = "depth_mid_gt100"),
    list(flag = "flag_depth_shallow_gt100", rows = "depth_shallow_gt100_rows.csv", stub = "depth_shallow_gt100"),
    list(flag = "flag_depth_deep_gt100", rows = "depth_deep_gt100_rows.csv", stub = "depth_deep_gt100")
  )

  flag_breakdown_store <- list()
  for (spec_i in flag_export_specs) {
    rows_i <- depth_master |>
      dplyr::filter(.data[[spec_i$flag]]) |>
      dplyr::select("row_id", "year", "month", "day", "ymd_reiwa", "vessel", "area", "水深(m)浅い側", "水深(m)深い側", "depth_shallow", "depth_deep", "depth_mid", "depth_range", "曳網時間", "effort_hours", "total catch")
    save_check_table(rows_i, file.path("output", "check_tables", spec_i$rows))
    flag_breakdown_store[[spec_i$flag]] <- save_flag_breakdowns(depth_master, spec_i$flag, spec_i$stub)
  }

  cat("top years for shallow>deep =\n")
  print(utils::head(flag_breakdown_store[["flag_depth_shallow_gt_deep"]]$year, 10))
  cat("top areas for shallow>deep =\n")
  print(utils::head(flag_breakdown_store[["flag_depth_shallow_gt_deep"]]$area, 10))
  cat("top vessels for shallow>deep =\n")
  print(utils::head(flag_breakdown_store[["flag_depth_shallow_gt_deep"]]$vessel, 10))
  cat("top years for range>100 =\n")
  print(utils::head(flag_breakdown_store[["flag_depth_range_gt100"]]$year, 10))
  cat("top areas for range>100 =\n")
  print(utils::head(flag_breakdown_store[["flag_depth_range_gt100"]]$area, 10))
  cat("top vessels for range>100 =\n")
  print(utils::head(flag_breakdown_store[["flag_depth_range_gt100"]]$vessel, 10))

  print_flag_bias_if_needed("shallow>deep year", flag_breakdown_store[["flag_depth_shallow_gt_deep"]]$year)
  print_flag_bias_if_needed("shallow>deep area", flag_breakdown_store[["flag_depth_shallow_gt_deep"]]$area)
  print_flag_bias_if_needed("shallow>deep vessel", flag_breakdown_store[["flag_depth_shallow_gt_deep"]]$vessel)
  print_flag_bias_if_needed("range>100 year", flag_breakdown_store[["flag_depth_range_gt100"]]$year)
  print_flag_bias_if_needed("range>100 area", flag_breakdown_store[["flag_depth_range_gt100"]]$area)
  print_flag_bias_if_needed("range>100 vessel", flag_breakdown_store[["flag_depth_range_gt100"]]$vessel)

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
        dplyr::select("row_id", "date", "year", "month", "area", "depth_shallow", "depth_deep", "depth_mid", "depth_range"),
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

  depth_rules <- build_depth_rule_tables(depth_master)
  depth_rule_summary <- dplyr::bind_rows(
    make_depth_rule_summary(depth_master, depth_rules$raw, "depth_rule_raw", "depth_shallow", "depth_deep", "depth_mid", "depth_range"),
    make_depth_rule_summary(depth_master, depth_rules$swapped, "depth_rule_swapped", "shallow_fixed", "deep_fixed", "mid_fixed", "range_fixed"),
    make_depth_rule_summary(depth_master, depth_rules$trimmed, "depth_rule_trimmed", "shallow_trimmed", "deep_trimmed", "mid_trimmed", "range_trimmed")
  )
  save_check_table(depth_rule_summary, file.path("output", "check_tables", "depth_rule_comparison_summary.csv"))
  save_check_table(
    depth_master |>
      dplyr::select("row_id", "year", "month", "day", "ymd_reiwa", "vessel", "area", "depth_shallow", "depth_deep", "depth_mid", "depth_range") |>
      dplyr::left_join(depth_rules$swapped, by = "row_id") |>
      dplyr::filter(.data$swapped_flag),
    file.path("output", "check_tables", "depth_rule_swapped_preview.csv")
  )
  save_check_table(
    depth_master |>
      dplyr::select("row_id", "year", "month", "day", "ymd_reiwa", "vessel", "area", "depth_shallow", "depth_deep", "depth_mid", "depth_range") |>
      dplyr::left_join(depth_rules$trimmed, by = "row_id") |>
      dplyr::filter(.data$trim_shallow_gt100 | .data$trim_deep_gt100 | .data$trim_mid_gt100 | .data$trim_range_gt100),
    file.path("output", "check_tables", "depth_rule_trimmed_preview.csv")
  )

  model_input_raw <- build_model_input_preview(depth_master, size_long_tbl, depth_rules$raw, "raw")
  model_input_swapped <- build_model_input_preview(depth_master, size_long_tbl, depth_rules$swapped, "swapped")
  model_input_trimmed <- build_model_input_preview(depth_master, size_long_tbl, depth_rules$trimmed, "trimmed")
  save_check_table(model_input_raw, file.path("output", "check_tables", "model_input_preview_raw_depth.csv"))
  save_check_table(model_input_swapped, file.path("output", "check_tables", "model_input_preview_swapped_depth.csv"))
  save_check_table(model_input_trimmed, file.path("output", "check_tables", "model_input_preview_trimmed_depth.csv"))

  plot_depth_histogram(depth_master, depth_limit = NULL, output_png = file.path("output", "check_figures", "depth_histogram_full.png"), plot_title = "Depth distribution")
  plot_depth_histogram(depth_master, depth_limit = 100, output_png = file.path("output", "check_figures", "depth_histogram_trim100.png"), plot_title = "Depth distribution (<= 100 m)")
  plot_depth_histogram(depth_master, depth_limit = 60, output_png = file.path("output", "check_figures", "depth_histogram_trim60.png"), plot_title = "Depth distribution (<= 60 m)")
  plot_depth_by_year_boxplot(depth_master, depth_limit = NULL, output_png = file.path("output", "check_figures", "depth_by_year_boxplot_full.png"), plot_title = "Depth by year")
  plot_depth_by_year_boxplot(depth_master, depth_limit = 100, output_png = file.path("output", "check_figures", "depth_by_year_boxplot_trim100.png"), plot_title = "Depth by year (<= 100 m)")
  plot_depth_missing_by_year_area(depth_master)
  plot_depth_cpue_scatter(plot_tbl, output_png = file.path("output", "check_figures", "diagnostic_depth_cpue_scatter_full.png"), plot_title = "Depth and CPUE by size class", smooth_method = "lm", depth_limit = NULL)
  plot_depth_shallow_vs_deep_distribution(depth_master)

  cpue_extreme_tbl <- cpue_main_cleaned |>
    dplyr::group_by(.data$size_id, .data$size_label) |>
    dplyr::mutate(
      threshold_99 = stats::quantile(.data$cpue, 0.99, na.rm = TRUE),
      threshold_995 = stats::quantile(.data$cpue, 0.995, na.rm = TRUE),
      threshold_999 = stats::quantile(.data$cpue, 0.999, na.rm = TRUE)
    ) |>
    dplyr::ungroup() |>
    dplyr::filter(.data$cpue >= .data$threshold_99 | .data$cpue >= .data$threshold_995 | .data$cpue >= .data$threshold_999) |>
    dplyr::transmute(
      row_id = .data$row_id,
      size_id = .data$size_id,
      size_label = as.character(.data$size_label),
      year = .data$year,
      date = .data$date,
      vessel = .data$vessel,
      area = .data$area,
      depth_shallow = .data$depth_shallow,
      depth_deep = .data$depth_deep,
      depth_mid = .data$depth_mid,
      effort_hours = .data$effort_hours,
      catch = .data$catch,
      cpue = .data$cpue,
      flag_top_1pct = .data$cpue >= .data$threshold_99,
      flag_top_0_5pct = .data$cpue >= .data$threshold_995,
      flag_top_0_1pct = .data$cpue >= .data$threshold_999
    )
  save_check_table(cpue_extreme_tbl, file.path("output", "check_tables", "extreme_cpue_by_size.csv"))

  short_effort_summary <- cpue_main_cleaned |>
    dplyr::group_by(.data$size_id, .data$size_label) |>
    dplyr::summarise(
      n_rows = dplyr::n(),
      effort_lt_0_5 = sum(.data$effort_hours < 0.5, na.rm = TRUE),
      effort_lt_1 = sum(.data$effort_hours < 1, na.rm = TRUE),
      .groups = "drop"
    )
  save_check_table(short_effort_summary, file.path("output", "check_tables", "short_effort_summary.csv"))

  trimmed_plot_data <- cpue_main_cleaned |>
    dplyr::filter(.data$year %in% 2020:2024, .data$effort_hours > 0, !is.na(.data$depth_mid), .data$depth_mid <= 100) |>
    dplyr::group_by(.data$size_id) |>
    dplyr::mutate(
      cpue_q99 = stats::quantile(.data$cpue, 0.99, na.rm = TRUE),
      cpue_plot_trim = pmin(.data$cpue, .data$cpue_q99),
      plot_filter_effort_ge1 = .data$effort_hours >= 1
    ) |>
    dplyr::ungroup()
  save_check_table(trimmed_plot_data, file.path("output", "check_tables", "depth_cpue_trimmed_plot_data.csv"))

  plot_depth_cpue_scatter_diagnostic(
    trimmed_plot_data |>
      dplyr::filter(.data$plot_filter_effort_ge1) |>
      dplyr::mutate(cpue = .data$cpue_plot_trim),
    output_png = file.path("output", "check_figures", "diagnostic_depth_cpue_scatter_trimmed.png"),
    plot_title = "Diagnostic depth and CPUE by size class (trimmed)",
    y_free = FALSE
  )

  plot_depth_cpue_scatter_diagnostic(
    trimmed_plot_data |>
      dplyr::filter(.data$plot_filter_effort_ge1) |>
      dplyr::mutate(cpue = .data$cpue_plot_trim),
    output_png = file.path("output", "check_figures", "depth_cpue_by_size_scatter_trimmed.png"),
    plot_title = "Depth and CPUE by size class (trimmed)",
    y_free = FALSE
  )

  plot_depth_cpue_scatter_diagnostic(
    trimmed_plot_data |>
      dplyr::filter(.data$plot_filter_effort_ge1) |>
      dplyr::mutate(cpue = .data$cpue_plot_trim),
    output_png = file.path("output", "check_figures", "depth_cpue_by_size_scatter_trimmed_free_y.png"),
    plot_title = "Depth and CPUE by size class (trimmed, free y)",
    y_free = TRUE
  )

  depth_bin_cpue_summary <- trimmed_plot_data |>
    dplyr::filter(.data$plot_filter_effort_ge1) |>
    dplyr::mutate(
      depth_bin_2m = floor(.data$depth_mid / 2) * 2,
      depth_bin_center = .data$depth_bin_2m + 1
    ) |>
    dplyr::group_by(.data$size_id, .data$depth_bin_2m, .data$depth_bin_center) |>
    dplyr::summarise(
      n = dplyr::n(),
      median_cpue = stats::median(.data$cpue_plot_trim, na.rm = TRUE),
      mean_cpue = mean(.data$cpue_plot_trim, na.rm = TRUE),
      trimmed_mean_cpue = mean(.data$cpue_plot_trim, na.rm = TRUE, trim = 0.1),
      q25 = as.numeric(stats::quantile(.data$cpue_plot_trim, 0.25, na.rm = TRUE)),
      q75 = as.numeric(stats::quantile(.data$cpue_plot_trim, 0.75, na.rm = TRUE)),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      median_cpue = dplyr::if_else(.data$n < 5, NA_real_, .data$median_cpue),
      mean_cpue = dplyr::if_else(.data$n < 5, NA_real_, .data$mean_cpue),
      trimmed_mean_cpue = dplyr::if_else(.data$n < 5, NA_real_, .data$trimmed_mean_cpue),
      q25 = dplyr::if_else(.data$n < 5, NA_real_, .data$q25),
      q75 = dplyr::if_else(.data$n < 5, NA_real_, .data$q75)
    )
  save_check_table(depth_bin_cpue_summary, file.path("output", "check_tables", "depth_bin_cpue_summary.csv"))

  if (max(depth_bin_cpue_summary$median_cpue, na.rm = TRUE) <= 1.5) {
    cat("report plot will prioritize median because y-range is visually stable.\n")
  } else {
    cat("report plot y-range is wide; median is prioritized for readability.\n")
  }

  plot_report_depth_cpue(
    depth_bin_cpue_summary,
    y_col = "median_cpue",
    output_png = file.path("output", "check_figures", "report_depth_cpue_binned_median.png"),
    plot_title = "Depth-binned median CPUE by size class",
    ribbon = TRUE
  )
  plot_report_depth_cpue(
    depth_bin_cpue_summary,
    y_col = "trimmed_mean_cpue",
    output_png = file.path("output", "check_figures", "report_depth_cpue_binned_trimmed_mean.png"),
    plot_title = "Depth-binned trimmed mean CPUE by size class",
    ribbon = FALSE
  )

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

  data_cleaning_candidates <- tibble::tibble(
    `cleaning candidate` = c("drop out-of-period years", "swap shallow/deep when inverted", "hold or set depth_mid > 100 to NA", "hold or set depth_range > 100 to NA", "exclude very short effort from display", "inspect extreme CPUE rows"),
    `target variable` = c("year/date", "depth_shallow/depth_deep", "depth_mid", "depth_range", "effort_hours", "cpue"),
    reason = c(
      "解析対象年は 2020:2024 に固定",
      "shallow > deep は列意味逆転または入力順の不安定さを示す",
      "depth 列の意味確認前にそのまま本番投入しないため",
      "range > 100 は入力規則の不安定さを示すため",
      "短時間 effort が CPUE を過大化し得るため",
      "高 CPUE 外れ値の原票確認が必要なため"
    ),
    `affected rows` = c(
      date_out_of_scope_n,
      sum(depth_master$flag_depth_shallow_gt_deep, na.rm = TRUE),
      sum(depth_master$flag_depth_mid_gt100, na.rm = TRUE),
      sum(depth_master$flag_depth_range_gt100, na.rm = TRUE),
      sum(cpue_main_cleaned$effort_hours < 1, na.rm = TRUE),
      nrow(cpue_extreme_tbl)
    ),
    `expected impact` = c(
      "解析期間を統一",
      "depth 順序の一貫性を改善",
      "depth 代表値の極端値を保留できる",
      "depth 幅の極端値を保留できる",
      "発表用図の見え方を安定化",
      "CPUE 外れ値の原因特定"
    ),
    priority = c("high", "high", "high", "high", "medium", "high")
  )
  save_check_table(data_cleaning_candidates, file.path("output", "check_tables", "data_cleaning_candidates.csv"))

  model_note_tbl <- tibble::tibble(
    candidate = c("depth_none", "depth_raw", "depth_swapped", "depth_trimmed", "depth_shallow fixed effect", "depth_mid fixed effect", "depth quadratic", "depth nonlinear", "count + offset(log effort)", "CPUE summary-only"),
    note = c(
      "depth を使わない基準モデル",
      "raw shallow/deep/mid/range をそのまま使う候補",
      "inversion 修正後の depth を使う候補",
      "swapped 後に >100 を保留した depth を使う候補",
      "shallow 側を fixed effect で投入",
      "mid を fixed effect で投入",
      "2次項で曲線を許す候補",
      "spline/GAM など非線形候補",
      "GLMM 本命候補",
      "発表用・要約用の比較候補"
    )
  )
  save_check_table(model_note_tbl, file.path("output", "check_tables", "model_structure_candidates.csv"))

  invisible(list(
    depth_summary = depth_summary,
    flag_count_tbl = flag_count_tbl,
    corr_tbl = corr_tbl,
    depth_rule_summary = depth_rule_summary
  ))
}

if (sys.nframe() == 0) {
  run_check_depth_cpue_patterns()
}
