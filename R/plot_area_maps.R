# =========================================
# plot_area_maps.R
# 漁区ごとの差分マップを作成する
# =========================================

source(file.path("R", "make_overview_figure.R"))

make_tile_layout <- function(area_levels) {
  area_levels <- unique(as.character(area_levels))
  area_levels <- area_levels[!is.na(area_levels) & area_levels != ""]

  if (length(area_levels) == 0) {
    return(tibble::tibble(area = character(), x = numeric(), y = numeric(), plot_order = integer()))
  }

  n_col <- ceiling(sqrt(length(area_levels)))

  tibble::tibble(
    area = area_levels,
    plot_order = seq_along(area_levels),
    x = ((seq_along(area_levels) - 1L) %% n_col) + 1L,
    y = -(((seq_along(area_levels) - 1L) %/% n_col) + 1L)
  )
}

extract_object_expression <- function(script_path, object_name) {
  parsed_expr <- tryCatch(parse(script_path, encoding = "UTF-8"), error = function(e) NULL)

  if (is.null(parsed_expr)) {
    return(NULL)
  }

  for (expr_i in parsed_expr) {
    if (is.call(expr_i) &&
        identical(expr_i[[1]], as.name("<-")) &&
        identical(expr_i[[2]], as.name(object_name))) {
      return(expr_i[[3]])
    }
  }

  NULL
}

load_area_geometry_from_check_points <- function(check_points_path = get_akagai_default_paths()$check_points_path) {
  cat("check_points.R exists =", file.exists(check_points_path), "\n")

  if (!file.exists(check_points_path)) {
    cat("漁区ごとの代表点が未取得: check_points.R が見つからないため仮の並び順で tile plot にフォールバックします。\n")
    return(list(
      geometry_type = "tile",
      source_object = NA_character_,
      area_levels = character(),
      tile_layout = tibble::tibble(area = character(), x = numeric(), y = numeric(), plot_order = integer())
    ))
  }

  polygon_expr <- extract_object_expression(check_points_path, "polygon_def")
  polygon_def <- tryCatch(eval(polygon_expr, envir = new.env(parent = baseenv())), error = function(e) NULL)
  area_levels <- if (is.null(polygon_def)) character() else names(polygon_def)

  if (!is.null(polygon_def)) {
    cat("check_points polygon object = polygon_def\n")
  } else {
    cat("check_points.R から polygon_def を取得できませんでした。\n")
  }

  area_lonlat_path <- file.path("PrepareDummyData", "DataFromFig", "AreaLonLat.csv")
  build_script_path <- file.path("R", "build_area_geometry.R")

  if (!is.null(polygon_def) && file.exists(area_lonlat_path) && file.exists(build_script_path) && requireNamespace("sf", quietly = TRUE)) {
    source(build_script_path)

    poly_sf <- tryCatch(
      build_area_polygon_sf(area_lonlat_path = area_lonlat_path, polygon_def = polygon_def, crs = 4326),
      error = function(e) {
        cat("polygon load failed:", conditionMessage(e), "\n")
        NULL
      }
    )

    if (!is.null(poly_sf)) {
      cent_sf <- build_area_centroids(poly_sf)
      cent_df <- cent_sf |>
        sf::st_drop_geometry() |>
        dplyr::transmute(
          area = as.character(.data$area),
          lon = .data$lon,
          lat = .data$lat,
          plot_order = match(as.character(.data$area), names(polygon_def))
        ) |>
        dplyr::arrange(.data$plot_order)

      cat("geometry mode = polygon\n")

      return(list(
        geometry_type = "polygon",
        source_object = "polygon_def",
        area_levels = names(polygon_def),
        polygon_sf = poly_sf |>
          dplyr::mutate(
            area = as.character(.data$area),
            plot_order = match(as.character(.data$area), names(polygon_def))
          ),
        centroid_df = cent_df
      ))
    }
  }

  if (file.exists("area_centroids.csv")) {
    cent_df <- readr::read_csv("area_centroids.csv", show_col_types = FALSE) |>
      dplyr::transmute(
        area = as.character(.data$area_id),
        lon = .data$lon,
        lat = .data$lat,
        plot_order = dplyr::row_number()
      )

    cat("漁区ごとの代表点が未取得: polygon は使えないため centroid 由来の tile plot にフォールバックします。\n")

    return(list(
      geometry_type = "tile",
      source_object = "area_centroids.csv",
      area_levels = cent_df$area,
      tile_layout = cent_df |>
        dplyr::mutate(
          x = dplyr::dense_rank(.data$lon),
          y = dplyr::dense_rank(dplyr::desc(.data$lat))
        )
    ))
  }

  cat("漁区ごとの代表点が未取得: 仮の並び順で tile plot にフォールバックします。\n")

  list(
    geometry_type = "tile",
    source_object = if (!is.null(polygon_def)) "polygon_def" else NA_character_,
    area_levels = area_levels,
    tile_layout = make_tile_layout(area_levels)
  )
}

make_plot_data_area_diff <- function(clean_dat) {
  target_sizes <- c("tokudai", "toku", "dai", "medium")

  area_sum <- clean_dat |>
    dplyr::filter(!is.na(.data$year), !is.na(.data$area), .data$size %in% target_sizes) |>
    dplyr::group_by(.data$year, .data$area, .data$size) |>
    dplyr::summarise(value = sum(.data$count, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(.data$size, .data$area, .data$year) |>
    dplyr::group_by(.data$size, .data$area) |>
    dplyr::mutate(
      value_tminus1 = dplyr::lag(.data$value),
      diff_pct = 100 * (.data$value - .data$value_tminus1) / pmax(abs(.data$value_tminus1), 1e-6)
    ) |>
    dplyr::ungroup()

  cat("diff_pct summary =\n")
  print(summary(area_sum$diff_pct))

  area_sum
}

plot_area_map_diff <- function(plot_data, geometry_info, size_id, output_png) {
  plot_df <- plot_data |>
    dplyr::filter(.data$size == size_id)

  if (nrow(plot_df) == 0) {
    placeholder <- ggplot2::ggplot() +
      ggplot2::annotate("text", x = 1, y = 1, label = paste("No data for", size_id)) +
      ggplot2::xlim(0, 2) +
      ggplot2::ylim(0, 2) +
      ggplot2::labs(title = paste("Area-wise year-on-year change:", size_id)) +
      theme_akagai_report()

    ggplot2::ggsave(output_png, plot = placeholder, width = 12, height = 7, dpi = 300)
    cat("saved:", output_png, "\n")
    return(invisible(NULL))
  }

  size_label_en <- dplyr::case_when(
    size_id == "tokudai" ~ "Extra large",
    size_id == "toku" ~ "Special",
    size_id == "dai" ~ "Large",
    size_id == "medium" ~ "Medium",
    TRUE ~ size_id
  )

  if (identical(geometry_info$geometry_type, "polygon") && !is.null(geometry_info$polygon_sf)) {
    map_sf <- geometry_info$polygon_sf |>
      dplyr::left_join(plot_df, by = "area")

    p <- ggplot2::ggplot(map_sf) +
      ggplot2::geom_sf(ggplot2::aes(fill = .data$diff_pct), color = "grey55", linewidth = 0.25) +
      ggplot2::facet_wrap(~year) +
      ggplot2::scale_fill_gradient2(
        low = "#D73027",
        mid = "white",
        high = "#2166AC",
        midpoint = 0,
        labels = scales::label_number(accuracy = 1),
        na.value = "grey90"
      ) +
      ggplot2::labs(
        title = paste("Area-wise year-on-year change:", size_label_en),
        x = "Longitude",
        y = "Latitude",
        fill = "YoY change (%)"
      ) +
      theme_akagai_report()
  } else {
    tile_df <- geometry_info$tile_layout |>
      dplyr::left_join(plot_df, by = "area")

    p <- ggplot2::ggplot(tile_df, ggplot2::aes(x = .data$x, y = .data$y, fill = .data$diff_pct)) +
      ggplot2::geom_tile(color = "grey55", linewidth = 0.35) +
      ggplot2::geom_text(ggplot2::aes(label = .data$area), size = 3) +
      ggplot2::facet_wrap(~year) +
      ggplot2::scale_fill_gradient2(
        low = "#D73027",
        mid = "white",
        high = "#2166AC",
        midpoint = 0,
        labels = scales::label_number(accuracy = 1),
        na.value = "grey90"
      ) +
      ggplot2::labs(
        title = paste("Area-wise year-on-year change:", size_label_en),
        x = "Tile X",
        y = "Tile Y",
        fill = "YoY change (%)"
      ) +
      theme_akagai_report()
  }

  ggplot2::ggsave(
    filename = output_png,
    plot = p,
    width = 12,
    height = 7,
    dpi = 300
  )

  cat("saved:", output_png, "\n")
}

run_plot_area_maps <- function() {
  paths <- get_akagai_default_paths()
  excel_obj <- load_akagai_excel(paths$excel_path)
  clean_dat <- clean_akagai_data(excel_obj$data)
  geometry_info <- load_area_geometry_from_check_points(paths$check_points_path)
  area_diff_data <- make_plot_data_area_diff(clean_dat)

  size_output_map <- c(
    tokudai = file.path("output", "figures", "area_diff_map_tokudai.png"),
    toku = file.path("output", "figures", "area_diff_map_toku.png"),
    dai = file.path("output", "figures", "area_diff_map_dai.png"),
    medium = file.path("output", "figures", "area_diff_map_medium.png")
  )

  for (size_id in names(size_output_map)) {
    plot_area_map_diff(
      plot_data = area_diff_data,
      geometry_info = geometry_info,
      size_id = size_id,
      output_png = size_output_map[[size_id]]
    )
  }

  invisible(list(
    excel_sheet = excel_obj$sheet,
    geometry_type = geometry_info$geometry_type,
    geometry_source_object = geometry_info$source_object,
    column_map = attr(clean_dat, "column_map")
  ))
}

if (sys.nframe() == 0) {
  run_plot_area_maps()
}
