# =========================================
# make_clean_data_and_figures.R
# チェック用出力を読み、cleaned data とチェック後図を保存する
# =========================================

source(file.path("R", "00_load_packages.R"))
source(file.path("R", "utils_cleaning.R"))

load_project_packages(
  required_pkgs = c("dplyr", "ggplot2", "readr", "scales", "stringr", "tidyr", "tibble"),
  optional_pkgs = c("sf")
)

ensure_project_dirs()

dir.create(file.path("output", "check_tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path("output", "check_figures"), showWarnings = FALSE, recursive = TRUE)

check_data_path <- file.path("output", "check_tables", "raw_data_check.csv")
cleaned_data_output_path <- file.path("data_processed", "akagai_cleaned_data.csv")
glmm_input_output_path <- file.path("data_processed", "akagai_glmm_input.csv")

save_check_csv <- function(data, path) {
  readr::write_csv(data, path, na = "")
  cat("saved:", path, "\n")
}

build_glmm_input_tbl <- function(raw_tbl) {
  area_start_src <- if ("area_start" %in% names(raw_tbl)) raw_tbl$area_start else raw_tbl$area
  area_end_src <- if ("area_end" %in% names(raw_tbl)) raw_tbl$area_end else raw_tbl$area
  speed_src <- if ("speed_kt" %in% names(raw_tbl)) raw_tbl$speed_kt else rep(NA_real_, nrow(raw_tbl))
  duration_hours_src <- if ("duration_hours" %in% names(raw_tbl)) raw_tbl$duration_hours else raw_tbl$effort_hours
  count_total_raw_src <- if ("count_total_raw" %in% names(raw_tbl)) raw_tbl$count_total_raw else raw_tbl$catch_total
  catch_ware_src <- if ("catch_ware" %in% names(raw_tbl)) raw_tbl$catch_ware else rep(0, nrow(raw_tbl))
  catch_sho_src <- if ("catch_sho" %in% names(raw_tbl)) raw_tbl$catch_sho else rep(0, nrow(raw_tbl))
  catch_shosho_src <- if ("catch_shosho" %in% names(raw_tbl)) raw_tbl$catch_shosho else rep(0, nrow(raw_tbl))

  glmm_input <- raw_tbl |>
    dplyr::mutate(
      date = as.Date(.data$ymd_reiwa),
      year = suppressWarnings(as.integer(.data$year)),
      month = suppressWarnings(as.integer(parse_numeric_trim(.data$month))),
      day = suppressWarnings(as.integer(parse_numeric_trim(.data$day))),
      month = dplyr::if_else(!is.na(.data$month) & .data$month %in% 1:12, .data$month, NA_integer_),
      day = dplyr::if_else(!is.na(.data$day) & .data$day %in% 1:31, .data$day, NA_integer_),
      day = dplyr::if_else(!is.na(.data$year) & !is.na(.data$month) & !is.na(.data$day) & is.na(.data$date), NA_integer_, .data$day),
      vessel = suppressWarnings(as.integer(parse_numeric_trim(.data$vessel))),
      vessel = dplyr::if_else(!is.na(.data$vessel) & .data$vessel %in% 1:5, .data$vessel, NA_integer_),
      area_start = parse_numeric_trim(.env$area_start_src),
      area_end = parse_numeric_trim(.env$area_end_src),
      area = dplyr::case_when(
        !is.na(.data$area_start) & as.integer(.data$area_start) %in% valid_area_codes ~ as.integer(.data$area_start),
        (is.na(.data$area_start) | !(as.integer(.data$area_start) %in% valid_area_codes)) &
          !is.na(.data$area_end) & as.integer(.data$area_end) %in% valid_area_codes ~ as.integer(.data$area_end),
        TRUE ~ NA_integer_
      ),
      count_total_raw = parse_numeric_trim(.env$count_total_raw_src),
      chu_raw = parse_numeric_trim(.data$catch_medium),
      dai_raw = parse_numeric_trim(.data$catch_dai),
      toku_raw = parse_numeric_trim(.data$catch_toku),
      tokudai_raw = parse_numeric_trim(.data$catch_tokudai),
      ware_raw = parse_numeric_trim(.env$catch_ware_src),
      sho_raw = parse_numeric_trim(.env$catch_sho_src),
      shosho_raw = parse_numeric_trim(.env$catch_shosho_src),
      flag_count_total_raw_rounded = !is.na(.data$count_total_raw) & abs(.data$count_total_raw - round(.data$count_total_raw)) > 1e-8,
      flag_chu_rounded = !is.na(.data$chu_raw) & abs(.data$chu_raw - round(.data$chu_raw)) > 1e-8,
      flag_dai_rounded = !is.na(.data$dai_raw) & abs(.data$dai_raw - round(.data$dai_raw)) > 1e-8,
      flag_toku_rounded = !is.na(.data$toku_raw) & abs(.data$toku_raw - round(.data$toku_raw)) > 1e-8,
      flag_tokudai_rounded = !is.na(.data$tokudai_raw) & abs(.data$tokudai_raw - round(.data$tokudai_raw)) > 1e-8,
      flag_ware_rounded = !is.na(.data$ware_raw) & abs(.data$ware_raw - round(.data$ware_raw)) > 1e-8,
      flag_sho_rounded = !is.na(.data$sho_raw) & abs(.data$sho_raw - round(.data$sho_raw)) > 1e-8,
      flag_shosho_rounded = !is.na(.data$shosho_raw) & abs(.data$shosho_raw - round(.data$shosho_raw)) > 1e-8,
      flag_count_total_raw_negative = !is.na(.data$count_total_raw) & .data$count_total_raw < 0,
      flag_chu_negative = !is.na(.data$chu_raw) & .data$chu_raw < 0,
      flag_dai_negative = !is.na(.data$dai_raw) & .data$dai_raw < 0,
      flag_toku_negative = !is.na(.data$toku_raw) & .data$toku_raw < 0,
      flag_tokudai_negative = !is.na(.data$tokudai_raw) & .data$tokudai_raw < 0,
      flag_ware_negative = !is.na(.data$ware_raw) & .data$ware_raw < 0,
      flag_sho_negative = !is.na(.data$sho_raw) & .data$sho_raw < 0,
      flag_shosho_negative = !is.na(.data$shosho_raw) & .data$shosho_raw < 0,
      count_total_raw = dplyr::if_else(.data$flag_count_total_raw_negative, NA_real_, round(.data$count_total_raw)),
      chu = dplyr::if_else(.data$flag_chu_negative, NA_real_, round(.data$chu_raw)),
      dai = dplyr::if_else(.data$flag_dai_negative, NA_real_, round(.data$dai_raw)),
      toku = dplyr::if_else(.data$flag_toku_negative, NA_real_, round(.data$toku_raw)),
      tokudai = dplyr::if_else(.data$flag_tokudai_negative, NA_real_, round(.data$tokudai_raw)),
      ware = dplyr::if_else(.data$flag_ware_negative, NA_real_, round(.data$ware_raw)),
      sho = dplyr::if_else(.data$flag_sho_negative, NA_real_, round(.data$sho_raw)),
      shosho = dplyr::if_else(.data$flag_shosho_negative, NA_real_, round(.data$shosho_raw)),
      count_total_recalc = dplyr::if_else(
        is.na(.data$chu) | is.na(.data$dai) | is.na(.data$toku) | is.na(.data$tokudai) | is.na(.data$ware) | is.na(.data$sho) | is.na(.data$shosho),
        NA_real_,
        .data$chu + .data$dai + .data$toku + .data$tokudai + .data$ware + .data$sho + .data$shosho
      ),
      flag_count_total_mismatch = xor(is.na(.data$count_total_raw), is.na(.data$count_total_recalc)) |
        (!is.na(.data$count_total_raw) & !is.na(.data$count_total_recalc) & .data$count_total_raw != .data$count_total_recalc),
      count_total = .data$count_total_recalc,
      depth_raw = suppressWarnings(as.numeric(.data$depth_use_raw_rule)),
      depth_min_raw = suppressWarnings(as.numeric(.data$depth_raw_1)),
      depth_max_raw = suppressWarnings(as.numeric(.data$depth_raw_2)),
      depth_flag_base = make_depth_use(.data$depth_raw_1, .data$depth_raw_2),
      depth_cleaning_raw = depth_flag_base,
      flag_depth_missing = is.na(.data$depth_raw_1) & is.na(.data$depth_raw_2),
      flag_depth_out_of_range = !flag_depth_missing & is.na(depth_flag_base),
      flag_depth_bad = flag_depth_missing | flag_depth_out_of_range,
      depth_glmm = dplyr::if_else(flag_depth_bad, NA_real_, as.numeric(depth_flag_base)),
      speed_kt_raw = parse_numeric_trim(.env$speed_src),
      flag_speed_replaced = is.na(.data$speed_kt_raw) | .data$speed_kt_raw <= 0.5 | .data$speed_kt_raw > 5,
      speed_kt = dplyr::if_else(.data$flag_speed_replaced, 3, .data$speed_kt_raw),
      duration_min_raw = suppressWarnings(as.numeric(.env$duration_hours_src)) * 60,
      flag_duration_replaced = is.na(.data$duration_min_raw) | .data$duration_min_raw <= 10 | .data$duration_min_raw > 90,
      duration_min_glmm = dplyr::if_else(.data$flag_duration_replaced, 60, .data$duration_min_raw),
      flag_duration_missing = is.na(.data$duration_min_raw),
      flag_duration_out_of_range = !is.na(.data$duration_min_raw) & (.data$duration_min_raw <= 10 | .data$duration_min_raw > 90),
      flag_duration_bad = .data$flag_duration_replaced,
      effort_raw = duration_min_raw,
      effort = duration_min_glmm,
      effort_glmm = .data$speed_kt * (.data$duration_min_glmm / 60),
      flag_area_missing = is.na(.data$area),
      flag_vessel_missing = is.na(.data$vessel),
      flag_month_missing = is.na(.data$month),
      flag_day_missing = is.na(.data$day),
      flag_non_integer_count = .data$flag_count_total_raw_rounded |
        .data$flag_chu_rounded |
        .data$flag_dai_rounded |
        .data$flag_toku_rounded |
        .data$flag_tokudai_rounded |
        .data$flag_ware_rounded |
        .data$flag_sho_rounded |
        .data$flag_shosho_rounded,
      flag_year_out_of_range = is.na(.data$year) | !(.data$year %in% 2020:2024),
      flag_effort_glmm_bad = is.na(.data$effort_glmm) | !is.finite(.data$effort_glmm) | .data$effort_glmm <= 0,
      flag_use_for_main_glmm = !flag_year_out_of_range &
        !flag_month_missing &
        !flag_vessel_missing &
        !flag_area_missing &
        !flag_effort_glmm_bad &
        !is.na(.data$count_total),
      flag_use_for_chu_glmm = .data$flag_use_for_main_glmm & !is.na(.data$chu),
      flag_use_for_dai_glmm = .data$flag_use_for_main_glmm & !is.na(.data$dai),
      flag_use_for_toku_glmm = .data$flag_use_for_main_glmm & !is.na(.data$toku),
      flag_use_for_tokudai_glmm = .data$flag_use_for_main_glmm & !is.na(.data$tokudai),
      catch_total = .data$count_total,
      catch_medium = .data$chu,
      catch_dai = .data$dai,
      catch_toku = .data$toku,
      catch_tokudai = .data$tokudai,
      catch_ware = .data$ware,
      catch_sho = .data$sho,
      catch_shosho = .data$shosho
    ) |>
    dplyr::select(
      "date", "year", "month", "day", "area", "vessel", "effort", "count_total",
      "tokudai", "toku", "dai", "chu", "sho", "shosho",
      "row_id", "depth_raw", "depth_min_raw", "depth_max_raw", "depth_flag_base", "depth_cleaning_raw", "depth_glmm",
      "duration_min_raw", "duration_min_glmm", "speed_kt_raw", "speed_kt", "effort_raw", "effort_glmm",
      "flag_depth_missing", "flag_depth_out_of_range", "flag_depth_bad",
      "flag_duration_missing", "flag_duration_out_of_range", "flag_duration_bad",
      "flag_speed_replaced", "flag_duration_replaced",
      "flag_area_missing", "flag_vessel_missing", "flag_month_missing", "flag_day_missing", "flag_non_integer_count", "flag_year_out_of_range", "flag_effort_glmm_bad", "flag_count_total_mismatch", "flag_use_for_main_glmm",
      "flag_use_for_chu_glmm", "flag_use_for_dai_glmm", "flag_use_for_toku_glmm", "flag_use_for_tokudai_glmm",
      "flag_count_total_raw_rounded", "flag_count_total_raw_negative",
      "flag_chu_rounded", "flag_chu_negative",
      "flag_dai_rounded", "flag_dai_negative",
      "flag_toku_rounded", "flag_toku_negative",
      "flag_tokudai_rounded", "flag_tokudai_negative",
      "flag_ware_rounded", "flag_ware_negative",
      "flag_sho_rounded", "flag_sho_negative",
      "flag_shosho_rounded", "flag_shosho_negative",
      "count_total_raw", "count_total_recalc", "catch_total", "catch_medium", "catch_dai", "catch_toku", "catch_tokudai", "catch_ware", "catch_sho", "catch_shosho",
      "ware"
    )

  glmm_input
}

size_levels <- c("chu", "dai", "toku", "tokudai")
size_colors <- c("chu" = "#4E79A7", "dai" = "#59A14F", "toku" = "#E15759", "tokudai" = "#9C755F")

size_long_from_cleaned <- function(cleaned_tbl) {
  cleaned_tbl |>
    dplyr::select("row_id", "year", "area", "effort_hours", "depth_use", "catch_medium", "catch_dai", "catch_toku", "catch_tokudai") |>
    tidyr::pivot_longer(
      cols = c("catch_medium", "catch_dai", "catch_toku", "catch_tokudai"),
      names_to = "size_var",
      values_to = "catch"
    ) |>
    dplyr::mutate(
      size_id = dplyr::recode(.data$size_var, catch_medium = "chu", catch_dai = "dai", catch_toku = "toku", catch_tokudai = "tokudai"),
      size_label = factor(.data$size_id, levels = size_levels),
      cpue = dplyr::if_else(.data$effort_hours > 0, .data$catch / .data$effort_hours, NA_real_)
    ) |>
    dplyr::select("row_id", "year", "area", "effort_hours", "depth_use", "size_id", "size_label", "catch", "cpue")
}

make_annual_total_table <- function(cleaned_tbl) {
  cleaned_tbl |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(
      total_count = sum(.data$catch_total, na.rm = TRUE),
      total_effort_hours = sum(.data$effort_hours, na.rm = TRUE),
      cpue_ratio = .data$total_count / .data$total_effort_hours,
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$year)
}

make_annual_by_size_table <- function(cleaned_long_tbl) {
  cleaned_long_tbl |>
    dplyr::group_by(.data$year, .data$size_label) |>
    dplyr::summarise(
      total_count = sum(.data$catch, na.rm = TRUE),
      total_effort_hours = sum(.data$effort_hours, na.rm = TRUE),
      cpue_ratio = .data$total_count / .data$total_effort_hours,
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$year, .data$size_label)
}

# 年別の全サイズ合計 CPUE を折れ線図で描き、PNGで保存する関数
plot_annual_cpue_total <- function(plot_tbl, output_png) {
  p <- ggplot2::ggplot(plot_tbl, ggplot2::aes(x = .data$year, y = .data$cpue_ratio)) +
    ggplot2::geom_line(linewidth = 1.2, color = "#2166AC") +
    ggplot2::geom_point(size = 2.8, color = "#2166AC") +
    ggplot2::scale_x_continuous(breaks = 2020:2024, limits = c(2020, 2024)) +
    ggplot2::labs(
      title = "Annual CPUE of total catch",
      x = "Year",
      y = "CPUE (count/hour)"
    ) +
    ggplot2::theme_bw()

  ggplot2::ggsave(output_png, p, width = 9, height = 5.4, dpi = 300)
  cat("saved:", output_png, "\n")
}

# 年別のサイズ別 CPUE を折れ線図で描き、PNGで保存する関数
plot_annual_cpue_by_size <- function(plot_tbl, output_png) {
  p <- ggplot2::ggplot(plot_tbl, ggplot2::aes(x = .data$year, y = .data$cpue_ratio, color = .data$size_label, group = .data$size_label)) +
    ggplot2::geom_line(linewidth = 1.1) +
    ggplot2::geom_point(size = 2.4) +
    ggplot2::scale_x_continuous(breaks = 2020:2024, limits = c(2020, 2024)) +
    ggplot2::scale_color_manual(values = size_colors, drop = FALSE) +
    ggplot2::labs(
      title = "Annual CPUE by size class",
      x = "Year",
      y = "CPUE (count/hour)",
      color = "Size"
    ) +
    ggplot2::theme_bw()

  ggplot2::ggsave(output_png, p, width = 9.5, height = 5.6, dpi = 300)
  cat("saved:", output_png, "\n")
}

# 深度とサイズ別CPUEの関係を散布図+平滑線で描き、PNGで保存する関数
plot_depth_cpue_pdf_style <- function(cleaned_long_tbl, output_png) {
  plot_tbl <- cleaned_long_tbl |>
    dplyr::filter(.data$effort_hours >= 1, !is.na(.data$depth_use), .data$depth_use >= 10, .data$depth_use <= depth_threshold_m, !is.na(.data$cpue)) |>
    dplyr::group_by(.data$size_label) |>
    dplyr::mutate(
      cpue_q99 = stats::quantile(.data$cpue, probs = 0.99, na.rm = TRUE, names = FALSE),
      cpue_plot = pmin(.data$cpue, .data$cpue_q99)
    ) |>
    dplyr::ungroup()

  p <- ggplot2::ggplot(plot_tbl, ggplot2::aes(x = .data$depth_use, y = .data$cpue_plot)) +
    ggplot2::geom_point(alpha = 0.22, size = 0.9, color = "#2C7FB8") +
    ggplot2::geom_smooth(se = FALSE, color = "#D95F0E", linewidth = 0.9) +
    ggplot2::facet_wrap(~size_label, scales = "free_y") +
    ggplot2::labs(
      title = "Depth and CPUE by size class",
      subtitle = "Display uses effort >= 1 hour and CPUE clipped at the 99th percentile",
      x = "Depth (m)",
      y = "CPUE (count/hour)"
    ) +
    ggplot2::theme_bw()

  ggplot2::ggsave(output_png, p, width = 11, height = 7.5, dpi = 300)
  cat("saved:", output_png, "\n")
}

#年ごとの指標を大きい順に順位づけ
compute_year_rank <- function(plot_tbl, value_col, target_year = 2023) {
  rank_tbl <- plot_tbl |>
    dplyr::filter(!is.na(.data[[value_col]])) |>
    dplyr::arrange(dplyr::desc(.data[[value_col]])) |>
    dplyr::mutate(rank = dplyr::row_number())

  rank_tbl$rank[rank_tbl$year == target_year][[1]] %||% NA_integer_
}

`%||%` <- function(x, y) {
  if (length(x) == 0 || is.null(x)) {
    return(y)
  }
  x
}

#各 area を構成する頂点IDの対応表
area_polygon_def <- list(
  "151" = c(1, 2, 5, 6),
  "152" = c(2, 3, 4, 5),
  "161" = c(7, 9, 10, 8),
  "162" = c(9, 11, 12, 10),
  "171" = c(17, 16, 15, 18),
  "172" = c(16, 13, 14, 15),
  "181" = c(19, 20, 23, 24),
  "182" = c(20, 21, 22, 23),
  "191" = c(25, 64, 27, 26),
  "192" = c(64, 29, 30, 31, 32, 27),
  "201" = c(26, 28, 44, 43, 42, 41, 40, 39),
  "202" = c(28, 32, 37, 38),
  "203" = c(32, 33, 34, 35, 36, 37),
  "212" = c(38, 37, 48, 47, 46, 45),
  "213" = c(37, 36, 62, 61),
  "214" = c(36, 57, 58, 59, 60, 62),
  "223" = c(61, 62, 63, 51, 50, 49),
  "224" = c(62, 60, 56, 55, 54, 53, 52, 63)
)

#polygon地図が使えないときの代替レイアウト
make_tile_layout <- function(area_levels) {
  area_levels <- unique(as.character(area_levels))
  area_levels <- area_levels[!is.na(area_levels) & area_levels != ""]
  n_col <- ceiling(sqrt(length(area_levels)))

  tibble::tibble(
    area = area_levels,
    x = ((seq_along(area_levels) - 1L) %% n_col) + 1L,
    y = -(((seq_along(area_levels) - 1L) %/% n_col) + 1L)
  )
}

#頂点IDの列から、実際の sf polygon を1個作る関数
make_polygon_from_ids <- function(id_vec, pts_df) {
  xy <- pts_df |>
    dplyr::filter(.data$point_id %in% id_vec) |>
    dplyr::slice(match(id_vec, .data$point_id)) |>
    dplyr::select("lon", "lat") |>
    as.matrix()

  if (!all(xy[1, ] == xy[nrow(xy), ])) {
    xy <- rbind(xy, xy[1, ])
  }

  sf::st_polygon(list(xy))
}

#全漁区の polygon をまとめて sf オブジェクトにする関数
build_polygon_geometry <- function() {
  if (!requireNamespace("sf", quietly = TRUE)) {
    return(NULL)
  }

  area_lonlat_path <- file.path("ActualData", "AreaLonLat.csv")
  if (!file.exists(area_lonlat_path)) {
    return(NULL)
  }

  pts_df <- readr::read_csv(area_lonlat_path, col_names = FALSE, show_col_types = FALSE) |>
    stats::setNames(c("lon", "lat")) |>
    dplyr::mutate(
      point_id = dplyr::row_number(),
      lon = as.numeric(.data$lon),
      lat = as.numeric(.data$lat)
    )

  poly_list <- lapply(names(area_polygon_def), function(area_i) {
    sf::st_sf(
      area = area_i,
      geometry = sf::st_sfc(make_polygon_from_ids(area_polygon_def[[area_i]], pts_df), crs = 4326)
    )
  })

  do.call(rbind, poly_list) |>
    sf::st_as_sf()
}

#ある1サイズについて、area別CPUE前年差の図を描く関数
plot_area_map_single <- function(plot_tbl, size_label, output_png) {
  map_tbl <- plot_tbl |>
    dplyr::filter(!is.na(.data$year), .data$year %in% 2021:2024) |>
    dplyr::mutate(ratio_percent = .data$ratio_change * 100)

  fill_upper <- suppressWarnings(stats::quantile(map_tbl$ratio_percent, probs = 0.95, na.rm = TRUE, names = FALSE))
  if (!is.finite(fill_upper) || fill_upper < 100) {
    fill_upper <- max(map_tbl$ratio_percent, na.rm = TRUE)
  }
  if (!is.finite(fill_upper) || fill_upper < 100) {
    fill_upper <- 100
  }
  fill_limits <- c(0, fill_upper)

  poly_sf <- build_polygon_geometry()

  if (!is.null(poly_sf)) {
    map_sf <- poly_sf |>
      dplyr::left_join(map_tbl, by = "area") |>
      dplyr::filter(!is.na(.data$year))

    p <- ggplot2::ggplot(map_sf) +
      ggplot2::geom_sf(ggplot2::aes(fill = scales::squish(.data$ratio_percent, fill_limits)), color = "grey55", linewidth = 0.25) +
      ggplot2::facet_wrap(~year) +
      ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 100, limits = fill_limits, oob = scales::squish, na.value = "grey90") +
      ggplot2::labs(
        title = paste("Area CPUE percent ratio:", size_label),
        subtitle = "Percent relative to the previous year",
        fill = "Percent"
      ) +
      ggplot2::theme_bw()
  } else {
    tile_tbl <- make_tile_layout(sort(unique(map_tbl$area))) |>
      dplyr::left_join(map_tbl, by = "area")

    p <- ggplot2::ggplot(tile_tbl, ggplot2::aes(x = .data$x, y = .data$y, fill = scales::squish(.data$ratio_percent, fill_limits))) +
      ggplot2::geom_tile(color = "grey55", linewidth = 0.35) +
      ggplot2::geom_text(ggplot2::aes(label = .data$area), size = 3) +
      ggplot2::facet_wrap(~year) +
      ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 100, limits = fill_limits, oob = scales::squish, na.value = "grey90") +
      ggplot2::labs(
        title = paste("Area CPUE percent ratio:", size_label),
        subtitle = "Percent relative to the previous year",
        x = NULL,
        y = NULL,
        fill = "Percent"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank())
  }

  ggplot2::ggsave(output_png, p, width = 12, height = 7, dpi = 300)
  cat("saved:", output_png, "\n")
}

raw_tbl <- readr::read_csv(check_data_path, show_col_types = FALSE)
glmm_input_tbl <- build_glmm_input_tbl(raw_tbl)

cat("vessel_na_n=", sum(is.na(glmm_input_tbl$vessel)), "\n", sep = "")
cat("area_na_n=", sum(is.na(glmm_input_tbl$area)), "\n", sep = "")
cat("speed_replaced_n=", sum(glmm_input_tbl$flag_speed_replaced, na.rm = TRUE), "\n", sep = "")
cat("duration_replaced_n=", sum(glmm_input_tbl$flag_duration_replaced, na.rm = TRUE), "\n", sep = "")
cat("month_na_n=", sum(is.na(glmm_input_tbl$month)), "\n", sep = "")
cat("day_na_n=", sum(is.na(glmm_input_tbl$day)), "\n", sep = "")
cat("count_total_rounded_n=", sum(glmm_input_tbl$flag_count_total_raw_rounded, na.rm = TRUE), "\n", sep = "")
cat("count_total_negative_to_na_n=", sum(glmm_input_tbl$flag_count_total_raw_negative, na.rm = TRUE), "\n", sep = "")
cat("chu_rounded_n=", sum(glmm_input_tbl$flag_chu_rounded, na.rm = TRUE), "\n", sep = "")
cat("chu_negative_to_na_n=", sum(glmm_input_tbl$flag_chu_negative, na.rm = TRUE), "\n", sep = "")
cat("dai_rounded_n=", sum(glmm_input_tbl$flag_dai_rounded, na.rm = TRUE), "\n", sep = "")
cat("dai_negative_to_na_n=", sum(glmm_input_tbl$flag_dai_negative, na.rm = TRUE), "\n", sep = "")
cat("toku_rounded_n=", sum(glmm_input_tbl$flag_toku_rounded, na.rm = TRUE), "\n", sep = "")
cat("toku_negative_to_na_n=", sum(glmm_input_tbl$flag_toku_negative, na.rm = TRUE), "\n", sep = "")
cat("tokudai_rounded_n=", sum(glmm_input_tbl$flag_tokudai_rounded, na.rm = TRUE), "\n", sep = "")
cat("tokudai_negative_to_na_n=", sum(glmm_input_tbl$flag_tokudai_negative, na.rm = TRUE), "\n", sep = "")
cat("ware_rounded_n=", sum(glmm_input_tbl$flag_ware_rounded, na.rm = TRUE), "\n", sep = "")
cat("ware_negative_to_na_n=", sum(glmm_input_tbl$flag_ware_negative, na.rm = TRUE), "\n", sep = "")
cat("sho_rounded_n=", sum(glmm_input_tbl$flag_sho_rounded, na.rm = TRUE), "\n", sep = "")
cat("sho_negative_to_na_n=", sum(glmm_input_tbl$flag_sho_negative, na.rm = TRUE), "\n", sep = "")
cat("shosho_rounded_n=", sum(glmm_input_tbl$flag_shosho_rounded, na.rm = TRUE), "\n", sep = "")
cat("shosho_negative_to_na_n=", sum(glmm_input_tbl$flag_shosho_negative, na.rm = TRUE), "\n", sep = "")
cat("count_total_mismatch_n=", sum(glmm_input_tbl$flag_count_total_mismatch, na.rm = TRUE), "\n", sep = "")
cat("flag_use_for_main_glmm_true_n=", sum(glmm_input_tbl$flag_use_for_main_glmm, na.rm = TRUE), "\n", sep = "")

cleaned_tbl <- glmm_input_tbl |>
  dplyr::mutate(
    year = suppressWarnings(as.integer(.data$year)),
    area = as.character(.data$area),
    effort_hours = suppressWarnings(as.numeric(.data$duration_min_glmm)) / 60,
    depth_use = suppressWarnings(as.numeric(.data$depth_glmm)),
    catch_total = suppressWarnings(as.numeric(.data$count_total)),
    catch_medium = suppressWarnings(as.numeric(.data$chu)),
    catch_dai = suppressWarnings(as.numeric(.data$dai)),
    catch_toku = suppressWarnings(as.numeric(.data$toku)),
    catch_tokudai = suppressWarnings(as.numeric(.data$tokudai))
  ) |>
  dplyr::filter(
    .data$year %in% 2020:2024,
    !is.na(.data$effort_hours),
    .data$effort_hours > 0,
    !is.na(.data$catch_total),
    !is.na(.data$catch_medium),
    !is.na(.data$catch_dai),
    !is.na(.data$catch_toku),
    !is.na(.data$catch_tokudai)
  )

cleaned_summary <- tibble::tibble(
  metric = c(
    "cleaned_rows",
    "cleaned_rows_depth_use_non_missing",
    "cleaned_rows_depth_use_missing",
    "year_min",
    "year_max",
    "effort_hours_sum",
    "catch_total_sum"
  ),
  value = c(
    nrow(cleaned_tbl),
    sum(!is.na(cleaned_tbl$depth_use)),
    sum(is.na(cleaned_tbl$depth_use)),
    min(cleaned_tbl$year, na.rm = TRUE),
    max(cleaned_tbl$year, na.rm = TRUE),
    sum(cleaned_tbl$effort_hours, na.rm = TRUE),
    sum(cleaned_tbl$catch_total, na.rm = TRUE)
  )
)

cleaned_long_tbl <- size_long_from_cleaned(cleaned_tbl)

annual_total_tbl <- make_annual_total_table(cleaned_tbl)
annual_by_size_tbl <- make_annual_by_size_table(cleaned_long_tbl)

area_cpue_tbl <- cleaned_long_tbl |>
  dplyr::filter(.data$size_id %in% size_levels, !is.na(.data$area), .data$area != "") |>
  dplyr::group_by(.data$size_id, .data$year, .data$area) |>
  dplyr::summarise(
    total_count = sum(.data$catch, na.rm = TRUE),
    total_effort_hours = sum(.data$effort_hours, na.rm = TRUE),
    cpue_ratio = .data$total_count / .data$total_effort_hours,
    .groups = "drop"
  ) |>
  dplyr::arrange(.data$size_id, .data$area, .data$year) |>
  dplyr::group_by(.data$size_id, .data$area) |>
  dplyr::mutate(
    cpue_prev = dplyr::lag(.data$cpue_ratio),
    ratio_change = dplyr::if_else(is.na(.data$cpue_prev) | .data$cpue_prev == 0, NA_real_, .data$cpue_ratio / .data$cpue_prev),
    ratio_percent = .data$ratio_change * 100
  ) |>
  dplyr::ungroup()

area_cpue_chu_tbl <- area_cpue_tbl |>
  dplyr::filter(.data$size_id == "chu")
area_cpue_dai_tbl <- area_cpue_tbl |>
  dplyr::filter(.data$size_id == "dai")
area_cpue_toku_tbl <- area_cpue_tbl |>
  dplyr::filter(.data$size_id == "toku")
area_cpue_tokudai_tbl <- area_cpue_tbl |>
  dplyr::filter(.data$size_id == "tokudai")

depth_glmm_na_rows <- glmm_input_tbl |>
  dplyr::filter(.data$flag_depth_bad) |>
  dplyr::select(dplyr::any_of(c(
    "row_id", "date", "year", "month", "vessel", "area",
    "depth_raw", "depth_min_raw", "depth_max_raw", "depth_flag_base", "depth_glmm",
    "flag_depth_missing", "flag_depth_out_of_range", "count_total"
  )))

main_glmm_dropped_rows <- glmm_input_tbl |>
  dplyr::filter(!.data$flag_use_for_main_glmm) |>
  dplyr::select(dplyr::any_of(c(
    "row_id", "date", "year", "month", "vessel", "area", "count_total",
    "duration_min_raw", "duration_min_glmm", "effort_raw", "effort_glmm",
    "flag_year_out_of_range", "flag_month_missing", "flag_vessel_missing", "flag_area_missing", "flag_effort_glmm_bad", "flag_non_integer_count", "flag_count_total_mismatch"
  )))

glmm_input_summary <- tibble::tibble(
  metric = c(
    "rows_total",
    "rows_flag_use_for_main_glmm",
    "rows_depth_glmm_non_missing",
    "rows_depth_glmm_missing",
    "count_total_sum",
    "effort_glmm_sum"
  ),
  value = c(
    nrow(glmm_input_tbl),
    sum(glmm_input_tbl$flag_use_for_main_glmm, na.rm = TRUE),
    sum(!is.na(glmm_input_tbl$depth_glmm)),
    sum(is.na(glmm_input_tbl$depth_glmm)),
    sum(glmm_input_tbl$count_total, na.rm = TRUE),
    sum(glmm_input_tbl$effort_glmm, na.rm = TRUE)
  )
)

save_check_csv(cleaned_tbl, file.path("output", "check_tables", "cleaned_data.csv"))
save_check_csv(cleaned_summary, file.path("output", "check_tables", "cleaned_data_summary.csv"))
save_check_csv(annual_total_tbl, file.path("output", "check_tables", "year_count_effort_cpue_total.csv"))
save_check_csv(annual_by_size_tbl, file.path("output", "check_tables", "year_count_effort_cpue_by_size.csv"))
save_check_csv(area_cpue_chu_tbl, file.path("output", "check_tables", "area_cpue_chu_tbl.csv"))
save_check_csv(area_cpue_dai_tbl, file.path("output", "check_tables", "area_cpue_dai_tbl.csv"))
save_check_csv(area_cpue_toku_tbl, file.path("output", "check_tables", "area_cpue_toku_tbl.csv"))
save_check_csv(area_cpue_tokudai_tbl, file.path("output", "check_tables", "area_cpue_tokudai_tbl.csv"))
save_check_csv(glmm_input_summary, file.path("output", "check_tables", "glmm_input_summary.csv"))
save_check_csv(depth_glmm_na_rows, file.path("output", "check_tables", "check_depth_glmm_na_rows.csv"))
save_check_csv(main_glmm_dropped_rows, file.path("output", "check_tables", "check_main_glmm_dropped_rows.csv"))
save_check_csv(cleaned_tbl, cleaned_data_output_path)
save_check_csv(glmm_input_tbl, glmm_input_output_path)

plot_annual_cpue_total(annual_total_tbl, file.path("output", "check_figures", "annual_cpue_total.png"))
plot_annual_cpue_by_size(annual_by_size_tbl, file.path("output", "check_figures", "annual_cpue_by_size.png"))
plot_depth_cpue_pdf_style(cleaned_long_tbl, file.path("output", "check_figures", "depth_cpue_pdf_style.png"))
plot_area_map_single(area_cpue_chu_tbl, "chu", file.path("output", "check_figures", "area_cpue_map_chu.png"))
plot_area_map_single(area_cpue_dai_tbl, "dai", file.path("output", "check_figures", "area_cpue_map_dai.png"))
plot_area_map_single(area_cpue_toku_tbl, "toku", file.path("output", "check_figures", "area_cpue_map_toku.png"))
plot_area_map_single(area_cpue_tokudai_tbl, "tokudai", file.path("output", "check_figures", "area_cpue_map_tokudai.png"))

cat("cleaned rows =", nrow(cleaned_tbl), "\n")
cat("depth both > 150 n =", sum(!is.na(raw_tbl$depth_raw_1) & !is.na(raw_tbl$depth_raw_2) & raw_tbl$depth_raw_1 > depth_threshold_m & raw_tbl$depth_raw_2 > depth_threshold_m, na.rm = TRUE), "\n")
cat("depth one<=150 one>150 n =", sum(!is.na(raw_tbl$depth_raw_1) & !is.na(raw_tbl$depth_raw_2) & ((raw_tbl$depth_raw_1 <= depth_threshold_m & raw_tbl$depth_raw_2 > depth_threshold_m) | (raw_tbl$depth_raw_1 > depth_threshold_m & raw_tbl$depth_raw_2 <= depth_threshold_m)), na.rm = TRUE), "\n")
cat("cleaned rows with depth_use non-missing =", sum(!is.na(cleaned_tbl$depth_use)), "\n")
cat("annual CPUE summary =\n")
print(annual_total_tbl)
cat("area map size = chu | nrow =", nrow(area_cpue_chu_tbl), "\n")
cat("area map size = dai | nrow =", nrow(area_cpue_dai_tbl), "\n")
cat("area map size = toku | nrow =", nrow(area_cpue_toku_tbl), "\n")
cat("area map size = tokudai | nrow =", nrow(area_cpue_tokudai_tbl), "\n")
cat("glmm input rows =", nrow(glmm_input_tbl), "\n")
cat("flag_use_for_main_glmm rows =", sum(glmm_input_tbl$flag_use_for_main_glmm, na.rm = TRUE), "\n")
cat("depth_glmm missing rows =", sum(is.na(glmm_input_tbl$depth_glmm)), "\n")
cat("saved cleaned data =", cleaned_data_output_path, "\n")
cat("saved glmm input =", glmm_input_output_path, "\n")
identical_chu_dai <- identical(area_cpue_chu_tbl, area_cpue_dai_tbl)
cat("identical(area_cpue_chu_tbl, area_cpue_dai_tbl) =", identical_chu_dai, "\n")
if (identical_chu_dai) warning("area_cpue_chu_tbl and area_cpue_dai_tbl are identical")
identical_chu_toku <- identical(area_cpue_chu_tbl, area_cpue_toku_tbl)
cat("identical(area_cpue_chu_tbl, area_cpue_toku_tbl) =", identical_chu_toku, "\n")
if (identical_chu_toku) warning("area_cpue_chu_tbl and area_cpue_toku_tbl are identical")
identical_chu_tokudai <- identical(area_cpue_chu_tbl, area_cpue_tokudai_tbl)
cat("identical(area_cpue_chu_tbl, area_cpue_tokudai_tbl) =", identical_chu_tokudai, "\n")
if (identical_chu_tokudai) warning("area_cpue_chu_tbl and area_cpue_tokudai_tbl are identical")
identical_dai_toku <- identical(area_cpue_dai_tbl, area_cpue_toku_tbl)
cat("identical(area_cpue_dai_tbl, area_cpue_toku_tbl) =", identical_dai_toku, "\n")
if (identical_dai_toku) warning("area_cpue_dai_tbl and area_cpue_toku_tbl are identical")
identical_dai_tokudai <- identical(area_cpue_dai_tbl, area_cpue_tokudai_tbl)
cat("identical(area_cpue_dai_tbl, area_cpue_tokudai_tbl) =", identical_dai_tokudai, "\n")
if (identical_dai_tokudai) warning("area_cpue_dai_tbl and area_cpue_tokudai_tbl are identical")
identical_toku_tokudai <- identical(area_cpue_toku_tbl, area_cpue_tokudai_tbl)
cat("identical(area_cpue_toku_tbl, area_cpue_tokudai_tbl) =", identical_toku_tokudai, "\n")
if (identical_toku_tokudai) warning("area_cpue_toku_tbl and area_cpue_tokudai_tbl are identical")
cat("2023 rank in count =", compute_year_rank(annual_total_tbl, "total_count"), "\n")
cat("2023 rank in effort =", compute_year_rank(annual_total_tbl, "total_effort_hours"), "\n")
cat("2023 rank in CPUE =", compute_year_rank(annual_total_tbl, "cpue_ratio"), "\n")
