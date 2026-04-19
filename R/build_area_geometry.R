# =========================================
# build_area_geometry.R
# 実ポリゴン由来の area geometry を構築して、centroid / adjacency を再利用する
# =========================================

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

ensure_area_geometry_packages <- function() {
  required_pkgs <- c("dplyr", "ggplot2", "readr", "sf", "tibble")
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing_pkgs) > 0) {
    stop(
      "Missing required packages for area geometry: ",
      paste(missing_pkgs, collapse = ", ")
    )
  }
}

get_component_membership_from_matrix <- function(adj_matrix) {
  area_levels <- rownames(adj_matrix)

  if (length(area_levels) == 0) {
    return(tibble::tibble(area = character(), component_id = integer()))
  }

  if (requireNamespace("igraph", quietly = TRUE)) {
    graph_obj <- igraph::graph_from_adjacency_matrix(
      adj_matrix,
      mode = "undirected",
      diag = FALSE
    )
    comp_obj <- igraph::components(graph_obj)

    return(tibble::tibble(
      area = names(comp_obj$membership),
      component_id = as.integer(comp_obj$membership)
    ))
  }

  visited <- rep(FALSE, length(area_levels))
  component_id <- integer(length(area_levels))
  component_counter <- 0L

  for (i in seq_along(area_levels)) {
    if (visited[[i]]) {
      next
    }

    component_counter <- component_counter + 1L
    queue_idx <- i
    visited[[i]] <- TRUE
    component_id[[i]] <- component_counter

    while (length(queue_idx) > 0) {
      current_idx <- queue_idx[[1]]
      queue_idx <- queue_idx[-1]
      neighbor_idx <- which(adj_matrix[current_idx, ] != 0)

      for (j in neighbor_idx) {
        if (!visited[[j]]) {
          visited[[j]] <- TRUE
          component_id[[j]] <- component_counter
          queue_idx <- c(queue_idx, j)
        }
      }
    }
  }

  tibble::tibble(
    area = area_levels,
    component_id = component_id
  )
}

make_polygon_from_ids <- function(id_vec, pts_df) {
  ensure_area_geometry_packages()

  missing_ids <- setdiff(id_vec, pts_df$point_id)

  if (length(missing_ids) > 0) {
    stop(
      "Unknown point_id in polygon_def: ",
      paste(missing_ids, collapse = ", ")
    )
  }

  xy <- pts_df |>
    dplyr::filter(point_id %in% id_vec) |>
    dplyr::slice(match(id_vec, point_id)) |>
    dplyr::select(lon, lat) |>
    as.matrix()

  # polygon を閉じる
  if (!all(xy[1, ] == xy[nrow(xy), ])) {
    xy <- rbind(xy, xy[1, ])
  }

  sf::st_polygon(list(xy))
}

build_area_polygon_sf <- function(area_lonlat_path, polygon_def = area_polygon_def, crs = 4326) {
  ensure_area_geometry_packages()

  if (!file.exists(area_lonlat_path)) {
    stop("AreaLonLat.csv not found: ", area_lonlat_path)
  }

  pts_df <- readr::read_csv(
    area_lonlat_path,
    col_names = FALSE,
    show_col_types = FALSE
  ) |>
    stats::setNames(c("lon", "lat")) |>
    dplyr::mutate(
      point_id = dplyr::row_number(),
      lon = as.numeric(lon),
      lat = as.numeric(lat)
    )

  poly_list <- lapply(names(polygon_def), function(area_i) {
    sf::st_sf(
      area = area_i,
      geometry = sf::st_sfc(make_polygon_from_ids(polygon_def[[area_i]], pts_df), crs = crs)
    )
  })

  poly_sf <- do.call(rbind, poly_list)
  poly_sf <- sf::st_as_sf(poly_sf)
  pts_sf <- sf::st_as_sf(pts_df, coords = c("lon", "lat"), crs = crs)

  attr(poly_sf, "pts_df") <- pts_df
  attr(poly_sf, "pts_sf") <- pts_sf
  poly_sf
}

build_area_centroids <- function(poly_sf) {
  ensure_area_geometry_packages()

  cent_sf <- poly_sf |>
    sf::st_transform(32654) |>
    sf::st_centroid() |>
    sf::st_transform(4326)

  cent_coords <- sf::st_coordinates(cent_sf)

  cent_sf |>
    dplyr::mutate(
      lon = cent_coords[, 1],
      lat = cent_coords[, 2]
    )
}

adjacency_matrix_to_edges <- function(adj_matrix) {
  edge_index <- which(adj_matrix != 0, arr.ind = TRUE)

  if (nrow(edge_index) == 0) {
    return(tibble::tibble(
      area_from = character(),
      area_to = character()
    ))
  }

  tibble::tibble(
    area_from = rownames(adj_matrix)[edge_index[, "row"]],
    area_to = colnames(adj_matrix)[edge_index[, "col"]]
  ) |>
    dplyr::filter(area_from != area_to) |>
    dplyr::arrange(area_from, area_to)
}

build_adjacency_diagnostics <- function(adj_matrix) {
  if (!all(adj_matrix == t(adj_matrix))) {
    stop("adjacency_matrix must be symmetric.")
  }

  if (any(diag(adj_matrix) != 0)) {
    stop("adjacency_matrix diagonal must be 0.")
  }

  component_membership <- get_component_membership_from_matrix(adj_matrix)
  degree_tbl <- tibble::tibble(
    area = rownames(adj_matrix),
    number_of_neighbors = as.integer(rowSums(adj_matrix))
  ) |>
    dplyr::left_join(component_membership, by = "area")

  list(
    degree_tbl = degree_tbl,
    isolated_areas = degree_tbl |>
      dplyr::filter(number_of_neighbors == 0) |>
      dplyr::pull(area),
    n_components = dplyr::n_distinct(component_membership$component_id),
    component_membership = component_membership
  )
}

build_area_adjacency_from_sf <- function(poly_sf) {
  ensure_area_geometry_packages()

  touch_list <- sf::st_touches(poly_sf)
  area_levels <- as.character(poly_sf$area)
  adjacency_matrix <- matrix(
    0L,
    nrow = nrow(poly_sf),
    ncol = nrow(poly_sf),
    dimnames = list(area_levels, area_levels)
  )

  for (i in seq_along(touch_list)) {
    if (length(touch_list[[i]]) == 0) {
      next
    }

    adjacency_matrix[i, touch_list[[i]]] <- 1L
  }

  adjacency_matrix <- pmax(adjacency_matrix, t(adjacency_matrix))
  diag(adjacency_matrix) <- 0L

  adjacency_edges <- adjacency_matrix_to_edges(adjacency_matrix)
  diagnostics <- build_adjacency_diagnostics(adjacency_matrix)

  list(
    adjacency_matrix = adjacency_matrix,
    adjacency_edges = adjacency_edges,
    degree_tbl = diagnostics$degree_tbl,
    isolated_areas = diagnostics$isolated_areas,
    n_components = diagnostics$n_components,
    component_membership = diagnostics$component_membership
  )
}

read_adjacency_matrix_csv <- function(path) {
  ensure_area_geometry_packages()

  if (!file.exists(path)) {
    stop("Adjacency matrix file not found: ", path)
  }

  adj_df <- readr::read_csv(path, show_col_types = FALSE)

  if (!"area" %in% names(adj_df)) {
    stop("Adjacency matrix CSV must contain an 'area' column.")
  }

  area_levels <- as.character(adj_df$area)
  adj_matrix <- as.matrix(adj_df[, setdiff(names(adj_df), "area"), drop = FALSE])
  storage.mode(adj_matrix) <- "numeric"
  rownames(adj_matrix) <- area_levels
  colnames(adj_matrix) <- setdiff(names(adj_df), "area")
  adj_matrix
}

adjacency_edges_to_segments <- function(adjacency_edges, cent_df) {
  undirected_edges <- adjacency_edges |>
    dplyr::mutate(
      area_min = pmin(area_from, area_to),
      area_max = pmax(area_from, area_to)
    ) |>
    dplyr::distinct(area_min, area_max, .keep_all = TRUE) |>
    dplyr::select(area_from = area_min, area_to = area_max)

  undirected_edges |>
    dplyr::left_join(
      cent_df |>
        dplyr::select(area, lon_from = lon, lat_from = lat),
      by = c("area_from" = "area")
    ) |>
    dplyr::left_join(
      cent_df |>
        dplyr::select(area, lon_to = lon, lat_to = lat),
      by = c("area_to" = "area")
    )
}

plot_area_geometry_check <- function(poly_sf, cent_sf, pts_sf, out_path) {
  ensure_area_geometry_packages()

  plot_obj <- ggplot2::ggplot() +
    ggplot2::geom_sf(data = poly_sf, fill = NA, color = "black", linewidth = 0.5) +
    ggplot2::geom_sf(data = pts_sf, size = 1.2) +
    ggplot2::geom_sf_text(data = pts_sf, ggplot2::aes(label = point_id), nudge_y = 0.00035, size = 2.6) +
    ggplot2::geom_sf(data = cent_sf, color = "#C0392B", size = 2) +
    ggplot2::geom_sf_text(data = cent_sf, ggplot2::aes(label = area), nudge_y = 0.00045, size = 3.2) +
    ggplot2::labs(
      x = "Longitude",
      y = "Latitude",
      title = "Area polygons and centroids"
    ) +
    ggplot2::theme_bw()

  ggplot2::ggsave(
    filename = out_path,
    plot = plot_obj,
    width = 12,
    height = 8,
    dpi = 300
  )

  invisible(plot_obj)
}

write_area_geometry_outputs <- function(poly_sf, cent_df, adjacency_matrix, adjacency_edges, output_dir) {
  ensure_area_geometry_packages()

  tables_dir <- file.path(output_dir, "tables")
  figures_dir <- file.path(output_dir, "figures")

  dir.create(tables_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(figures_dir, showWarnings = FALSE, recursive = TRUE)

  centroids_path <- file.path(tables_dir, "area_centroids.csv")
  adjacency_matrix_path <- file.path(tables_dir, "area_adjacency_matrix.csv")
  adjacency_edges_path <- file.path(tables_dir, "area_adjacency_edges_symmetrized.csv")
  degree_table_path <- file.path(tables_dir, "area_degree_table.csv")

  diagnostics <- build_adjacency_diagnostics(adjacency_matrix)

  readr::write_csv(cent_df, centroids_path)
  readr::write_csv(tibble::as_tibble(adjacency_matrix, rownames = "area"), adjacency_matrix_path)
  readr::write_csv(adjacency_edges, adjacency_edges_path)
  readr::write_csv(diagnostics$degree_tbl, degree_table_path)

  list(
    centroids_path = centroids_path,
    adjacency_matrix_path = adjacency_matrix_path,
    adjacency_edges_path = adjacency_edges_path,
    degree_table_path = degree_table_path,
    figure_path = file.path(figures_dir, "area_centroids_check.png")
  )
}

build_and_write_area_geometry <- function(
  area_lonlat_path = file.path("PrepareDummyData", "DataFromFig", "AreaLonLat.csv"),
  polygon_def = area_polygon_def,
  output_dir = "output"
) {
  poly_sf <- build_area_polygon_sf(
    area_lonlat_path = area_lonlat_path,
    polygon_def = polygon_def,
    crs = 4326
  )

  pts_df <- attr(poly_sf, "pts_df")
  pts_sf <- attr(poly_sf, "pts_sf")
  cent_sf <- build_area_centroids(poly_sf)
  cent_df <- cent_sf |>
    sf::st_drop_geometry() |>
    dplyr::select(area, lon, lat)
  adjacency_obj <- build_area_adjacency_from_sf(poly_sf)
  output_paths <- write_area_geometry_outputs(
    poly_sf = poly_sf,
    cent_df = cent_df,
    adjacency_matrix = adjacency_obj$adjacency_matrix,
    adjacency_edges = adjacency_obj$adjacency_edges,
    output_dir = output_dir
  )

  plot_area_geometry_check(
    poly_sf = poly_sf,
    cent_sf = cent_sf,
    pts_sf = pts_sf,
    out_path = output_paths$figure_path
  )

  cat("\n=== area geometry QA ===\n")
  cat("n_points=", nrow(pts_df), "\n", sep = "")
  cat("n_polygons=", nrow(poly_sf), "\n", sep = "")
  cat("polygon_area_ids=", paste(poly_sf$area, collapse = ","), "\n", sep = "")
  cat("centroid_count=", nrow(cent_df), "\n", sep = "")
  cat("adjacency_matrix_dim=", paste(dim(adjacency_obj$adjacency_matrix), collapse = "x"), "\n", sep = "")
  cat(
    "adjacency_row_sums=",
    paste(
      paste0(adjacency_obj$degree_tbl$area, ":", adjacency_obj$degree_tbl$number_of_neighbors),
      collapse = ","
    ),
    "\n",
    sep = ""
  )
  cat(
    "isolated_areas=",
    if (length(adjacency_obj$isolated_areas) == 0) "none" else paste(adjacency_obj$isolated_areas, collapse = ","),
    "\n",
    sep = ""
  )
  cat("connected_components=", adjacency_obj$n_components, "\n", sep = "")

  list(
    poly_sf = poly_sf,
    pts_df = pts_df,
    pts_sf = pts_sf,
    cent_sf = cent_sf,
    cent_df = cent_df,
    adjacency_matrix = adjacency_obj$adjacency_matrix,
    adjacency_edges = adjacency_obj$adjacency_edges,
    degree_tbl = adjacency_obj$degree_tbl,
    isolated_areas = adjacency_obj$isolated_areas,
    n_components = adjacency_obj$n_components,
    output_paths = output_paths
  )
}

if (sys.nframe() == 0) {
  build_and_write_area_geometry()
}
