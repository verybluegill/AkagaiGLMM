# =========================================
# 06b_fit_glmm_total_spatial_area_icar_brms.R
# 未検証の雛形。実行前に brms の CAR term 書式を要確認
# =========================================

source(file.path("R", "00_load_packages.R"))
source(file.path("R", "build_area_geometry.R"))

optional_pkgs <- load_project_packages(
  required_pkgs = c("tidyverse", "Matrix")
)

ensure_project_dirs()

input_path <- file.path("data_processed", "akagai_glmm_input.csv")
adjacency_matrix_path <- file.path("output", "tables", "brms_area_adjacency_matrix.csv")
input_preview_path <- file.path("output", "tables", "brms_spatial_input_preview.csv")
formula_preview_path <- file.path("output", "tables", "brms_formula_preview.txt")
edge_preview_path <- file.path("output", "tables", "brms_area_adjacency_edges.csv")
precision_preview_path <- file.path("output", "tables", "brms_area_precision_matrix.csv")

run_brms <- FALSE

if (!file.exists(input_path)) {
  stop("Input file not found: ", input_path)
}

geometry_obj <- build_and_write_area_geometry(output_dir = "output")
area_universe <- names(area_polygon_def)
adjacency_matrix_full <- geometry_obj$adjacency_matrix
adjacency_edges_full <- adjacency_matrix_to_edges(adjacency_matrix_full)

glmm_input <- readr::read_csv(input_path, show_col_types = FALSE) |>
  mutate(
    year = factor(year),
    month = factor(month, levels = sort(unique(month))),
    area = as.character(area),
    vessel = as.character(vessel),
    effort_glmm = as.numeric(effort_glmm),
    count_total = as.numeric(count_total),
    depth_glmm = as.numeric(depth_glmm)
  )

if ("flag_use_for_main_glmm" %in% names(glmm_input)) {
  glmm_input <- glmm_input |>
    mutate(flag_use_for_main_glmm = as.logical(flag_use_for_main_glmm))
} else {
  glmm_input <- glmm_input |>
    mutate(
      flag_use_for_main_glmm = as.integer(as.character(year)) %in% 2020:2024 &
        !is.na(area) &
        area != "" &
        !is.na(vessel) &
        vessel != "" &
        !is.na(effort_glmm) &
        is.finite(effort_glmm) &
        effort_glmm > 0
    )
}

glmm_dat_brms <- glmm_input |>
  filter(
    flag_use_for_main_glmm,
    !is.na(count_total),
    is.finite(count_total),
    !is.na(depth_glmm),
    is.finite(depth_glmm),
    area %in% area_universe
  )

if (nrow(glmm_dat_brms) == 0) {
  stop("No rows available for the brms spatial preview subset.")
}

depth_glmm_mean <- mean(glmm_dat_brms$depth_glmm, na.rm = TRUE)
depth_glmm_sd <- stats::sd(glmm_dat_brms$depth_glmm, na.rm = TRUE)

if (!is.finite(depth_glmm_sd) || depth_glmm_sd <= 0) {
  stop("depth_glmm standard deviation must be finite and > 0.")
}

area_levels_model <- area_universe[area_universe %in% sort(unique(glmm_dat_brms$area))]
adjacency_matrix_model <- adjacency_matrix_full[area_levels_model, area_levels_model, drop = FALSE]
adjacency_edges_model <- adjacency_edges_full |>
  filter(area_from %in% area_levels_model, area_to %in% area_levels_model)
icar_precision_matrix <- diag(rowSums(adjacency_matrix_model)) - adjacency_matrix_model

glmm_dat_brms <- glmm_dat_brms |>
  mutate(
    depth_glmm_sc = (depth_glmm - depth_glmm_mean) / depth_glmm_sd,
    area = factor(area, levels = area_levels_model),
    vessel = factor(vessel),
    area_index = as.integer(area)
  )

brms_input_preview_tbl <- glmm_dat_brms |>
  select(year, month, area, area_index, vessel, count_total, effort_glmm, depth_glmm, depth_glmm_sc)

readr::write_csv(brms_input_preview_tbl, input_preview_path)
readr::write_csv(tibble::as_tibble(adjacency_matrix_model, rownames = "area"), adjacency_matrix_path)
readr::write_csv(adjacency_edges_model, edge_preview_path)
readr::write_csv(tibble::as_tibble(icar_precision_matrix, rownames = "area"), precision_preview_path)

formula_preview_lines <- c(
  "# 未検証の雛形。brms::car() / areal term の実書式は実行前に要確認",
  "depth0:",
  "bf(count_total ~ year + month + offset(log(effort_glmm)) + (1 | vessel) + car(W, gr = area), family = negbinomial())",
  "",
  "depth1:",
  "bf(count_total ~ year + month + depth_glmm_sc + offset(log(effort_glmm)) + (1 | vessel) + car(W, gr = area), family = negbinomial())",
  "",
  "depth2:",
  "bf(count_total ~ year + month + depth_glmm_sc + I(depth_glmm_sc^2) + offset(log(effort_glmm)) + (1 | vessel) + car(W, gr = area), family = negbinomial())"
)

writeLines(formula_preview_lines, formula_preview_path, useBytes = TRUE)

cat("\n=== brms spatial preview ===\n")
cat("n_rows=", nrow(glmm_dat_brms), "\n", sep = "")
cat("area_levels_model=", paste(area_levels_model, collapse = ","), "\n", sep = "")
cat("run_brms=", run_brms, "\n", sep = "")
cat("input_preview_path=", input_preview_path, "\n", sep = "")
cat("adjacency_matrix_path=", adjacency_matrix_path, "\n", sep = "")
cat("edge_preview_path=", edge_preview_path, "\n", sep = "")
cat("precision_preview_path=", precision_preview_path, "\n", sep = "")
cat("formula_preview_path=", formula_preview_path, "\n", sep = "")

if (isTRUE(run_brms)) {
  if (!requireNamespace("brms", quietly = TRUE)) {
    stop("brms is not installed. Set run_brms <- FALSE or install brms.")
  }

  stop("run_brms <- TRUE is not yet implemented because the CAR term syntax must be verified in this environment.")
}
