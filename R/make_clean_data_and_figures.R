# =========================================
# make_clean_data_and_figures.R
# チェック用出力を読み、cleaned data とチェック後図を保存する
# =========================================

source(file.path("R", "00_load_packages.R"))

load_project_packages(
  required_pkgs = c("dplyr", "ggplot2", "readr", "scales", "stringr", "tidyr", "tibble"),
  optional_pkgs = c("sf")
)

dir.create(file.path("output", "check_tables"), showWarnings = FALSE, recursive = TRUE)
dir.create(file.path("output", "check_figures"), showWarnings = FALSE, recursive = TRUE)

check_data_path <- file.path("output", "check_tables", "raw_data_check.csv")

save_check_csv <- function(data, path) {
  readr::write_csv(data, path, na = "")
  cat("saved:", path, "\n")
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

size_levels <- c("medium", "dai", "toku", "tokudai")
size_labels <- c("Medium", "Large", "Special", "Extra large")
size_colors <- c("Medium" = "#4E79A7", "Large" = "#59A14F", "Special" = "#E15759", "Extra large" = "#9C755F")

size_long_from_cleaned <- function(cleaned_tbl) {
  cleaned_tbl |>
    dplyr::select("row_id", "year", "area", "effort_hours", "depth_use", "catch_medium", "catch_dai", "catch_toku", "catch_tokudai") |>
    tidyr::pivot_longer(
      cols = c("catch_medium", "catch_dai", "catch_toku", "catch_tokudai"),
      names_to = "size_var",
      values_to = "catch"
    ) |>
    dplyr::mutate(
      size_id = dplyr::recode(.data$size_var, catch_medium = "medium", catch_dai = "dai", catch_toku = "toku", catch_tokudai = "tokudai"),
      size_label = factor(
        dplyr::recode(.data$size_id, medium = "Medium", dai = "Large", toku = "Special", tokudai = "Extra large"),
        levels = size_labels
      ),
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

plot_depth_cpue_pdf_style <- function(cleaned_long_tbl, output_png) {
  plot_tbl <- cleaned_long_tbl |>
    dplyr::filter(.data$effort_hours >= 1, !is.na(.data$depth_use), .data$depth_use >= 10, .data$depth_use <= 50, !is.na(.data$cpue)) |>
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

build_polygon_geometry <- function() {
  if (!requireNamespace("sf", quietly = TRUE)) {
    return(NULL)
  }

  area_lonlat_path <- file.path("PrepareDummyData", "DataFromFig", "AreaLonLat.csv")
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

plot_area_map_single <- function(plot_tbl, size_id, output_png) {
  size_label <- dplyr::recode(size_id, medium = "Medium", dai = "Large", toku = "Special", tokudai = "Extra large")
  map_tbl <- plot_tbl |>
    dplyr::filter(.data$size_id == size_id, .data$year %in% 2021:2024)

  fill_limit <- max(abs(map_tbl$diff_log), na.rm = TRUE)
  if (!is.finite(fill_limit) || fill_limit <= 0) {
    fill_limit <- 1
  }

  poly_sf <- build_polygon_geometry()

  if (!is.null(poly_sf)) {
    map_sf <- poly_sf |>
      dplyr::left_join(map_tbl, by = "area")

    p <- ggplot2::ggplot(map_sf) +
      ggplot2::geom_sf(ggplot2::aes(fill = scales::squish(.data$diff_log, c(-fill_limit, fill_limit))), color = "grey55", linewidth = 0.25) +
      ggplot2::facet_wrap(~year) +
      ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0, limits = c(-fill_limit, fill_limit), na.value = "grey90") +
      ggplot2::labs(
        title = paste("Area CPUE change:", size_label),
        subtitle = "Log change from the previous year",
        fill = "Log change"
      ) +
      ggplot2::theme_bw()
  } else {
    tile_tbl <- make_tile_layout(sort(unique(map_tbl$area))) |>
      dplyr::left_join(map_tbl, by = "area")

    p <- ggplot2::ggplot(tile_tbl, ggplot2::aes(x = .data$x, y = .data$y, fill = scales::squish(.data$diff_log, c(-fill_limit, fill_limit)))) +
      ggplot2::geom_tile(color = "grey55", linewidth = 0.35) +
      ggplot2::geom_text(ggplot2::aes(label = .data$area), size = 3) +
      ggplot2::facet_wrap(~year) +
      ggplot2::scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B", midpoint = 0, limits = c(-fill_limit, fill_limit), na.value = "grey90") +
      ggplot2::labs(
        title = paste("Area CPUE change:", size_label),
        subtitle = "Log change from the previous year",
        x = NULL,
        y = NULL,
        fill = "Log change"
      ) +
      ggplot2::theme_bw() +
      ggplot2::theme(axis.text = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank())
  }

  ggplot2::ggsave(output_png, p, width = 12, height = 7, dpi = 300)
  cat("saved:", output_png, "\n")
}

raw_tbl <- readr::read_csv(check_data_path, show_col_types = FALSE)

cleaned_tbl <- raw_tbl |>
  dplyr::mutate(
    year = suppressWarnings(as.integer(.data$year)),
    area = as.character(.data$area),
    effort_hours = suppressWarnings(as.numeric(.data$effort_hours)),
    depth_raw_1 = suppressWarnings(as.numeric(.data$depth_raw_1)),
    depth_raw_2 = suppressWarnings(as.numeric(.data$depth_raw_2)),
    catch_total = suppressWarnings(as.numeric(.data$catch_total)),
    catch_medium = suppressWarnings(as.numeric(.data$catch_medium)),
    catch_dai = suppressWarnings(as.numeric(.data$catch_dai)),
    catch_toku = suppressWarnings(as.numeric(.data$catch_toku)),
    catch_tokudai = suppressWarnings(as.numeric(.data$catch_tokudai)),
    depth_use = make_depth_use(.data$depth_raw_1, .data$depth_raw_2)
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
    diff_log = log1p(.data$cpue_ratio) - log1p(.data$cpue_prev)
  ) |>
  dplyr::ungroup()

save_check_csv(cleaned_tbl, file.path("output", "check_tables", "cleaned_data.csv"))
save_check_csv(cleaned_summary, file.path("output", "check_tables", "cleaned_data_summary.csv"))
save_check_csv(annual_total_tbl, file.path("output", "check_tables", "year_count_effort_cpue_total.csv"))
save_check_csv(annual_by_size_tbl, file.path("output", "check_tables", "year_count_effort_cpue_by_size.csv"))

plot_annual_cpue_total(annual_total_tbl, file.path("output", "check_figures", "annual_cpue_total.png"))
plot_annual_cpue_by_size(annual_by_size_tbl, file.path("output", "check_figures", "annual_cpue_by_size.png"))
plot_depth_cpue_pdf_style(cleaned_long_tbl, file.path("output", "check_figures", "depth_cpue_pdf_style.png"))
plot_area_map_single(area_cpue_tbl, "medium", file.path("output", "check_figures", "area_cpue_map_medium.png"))
plot_area_map_single(area_cpue_tbl, "dai", file.path("output", "check_figures", "area_cpue_map_dai.png"))
plot_area_map_single(area_cpue_tbl, "toku", file.path("output", "check_figures", "area_cpue_map_toku.png"))
plot_area_map_single(area_cpue_tbl, "tokudai", file.path("output", "check_figures", "area_cpue_map_tokudai.png"))

cat("cleaned rows =", nrow(cleaned_tbl), "\n")
cat("cleaned rows with depth_use non-missing =", sum(!is.na(cleaned_tbl$depth_use)), "\n")
cat("annual CPUE summary =\n")
print(annual_total_tbl)
cat("2023 rank in count =", compute_year_rank(annual_total_tbl, "total_count"), "\n")
cat("2023 rank in effort =", compute_year_rank(annual_total_tbl, "total_effort_hours"), "\n")
cat("2023 rank in CPUE =", compute_year_rank(annual_total_tbl, "cpue_ratio"), "\n")
