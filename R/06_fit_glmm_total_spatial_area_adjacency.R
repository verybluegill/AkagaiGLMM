# =========================================
# 06_fit_glmm_total_spatial_area_adjacency.R
# total catch GLMM に area adjacency ベースの空間効果を追加して、
# 非空間モデルと同じ subset 上で比較するための探索スクリプト
# =========================================

source(file.path("R", "00_load_packages.R"))

optional_pkgs <- load_project_packages(
  required_pkgs = c("tidyverse", "glmmTMB", "Matrix")
)

if (!requireNamespace("spaMM", quietly = TRUE)) {
  stop(
    "Missing required package: spaMM. ",
    "Please install spaMM manually, then rerun this script."
  )
}

library(spaMM)

ensure_project_dirs()

input_path <- file.path("data_processed", "akagai_glmm_input.csv")
model_comparison_path <- file.path("output", "tables", "model_comparison_total_spatial_area.csv")
adjacency_matrix_path <- file.path("output", "tables", "area_adjacency_matrix.csv")
adjacency_edges_path <- file.path("output", "tables", "area_adjacency_edges_symmetrized.csv")
best_spatial_area_effects_path <- file.path("output", "tables", "best_spatial_area_effects.csv")
best_spatial_area_effects_plot_path <- file.path("output", "tables", "best_spatial_area_effects_plot_table.csv")
best_nonspatial_area_residuals_path <- file.path("output", "tables", "best_nonspatial_area_residual_summary.csv")
best_adjacency_area_residuals_path <- file.path("output", "tables", "best_adjacency_area_residual_summary.csv")

if (!file.exists(input_path)) {
  stop("Input file not found: ", input_path)
}

required_cols <- c(
  "year", "month", "area", "vessel", "effort_glmm", "count_total",
  "depth_glmm"
)

area_universe <- c(
  "22", "50", "52", "62", "68", "72", "78", "79",
  "102", "112", "120", "121", "122", "123", "124", "131", "132", "134", "142",
  "150", "151", "152", "153", "156", "157", "160", "161", "162", "163", "164", "165",
  "170", "171", "172", "173", "180", "181", "182", "185",
  "190", "191", "192", "193", "194", "195", "198",
  "201", "202", "203", "204", "205", "208",
  "210", "211", "212", "213", "214", "216", "217", "218", "219", "222", "223", "234", "242", "252", "262",
  "300", "306", "312", "315", "317"
)

# 注意:
# この edge list は area code の並びに基づく暫定 adjacency で、
# あとで漁区境界の実情報に合わせて人手で修正する前提とする。
area_neighbors <- tribble(
  ~area_from, ~area_to,
  "22", "50",
  "50", "52",
  "52", "62",
  "62", "68",
  "68", "72",
  "72", "78",
  "78", "79",
  "79", "102",
  "102", "112",
  "112", "120",
  "120", "121",
  "121", "122",
  "122", "123",
  "123", "124",
  "124", "131",
  "131", "132",
  "132", "134",
  "134", "142",
  "142", "150",
  "150", "151",
  "151", "152",
  "152", "153",
  "153", "156",
  "156", "157",
  "157", "160",
  "160", "161",
  "161", "162",
  "162", "163",
  "163", "164",
  "164", "165",
  "165", "170",
  "170", "171",
  "171", "172",
  "172", "173",
  "173", "180",
  "180", "181",
  "181", "182",
  "182", "185",
  "185", "190",
  "190", "191",
  "191", "192",
  "192", "193",
  "193", "194",
  "194", "195",
  "195", "198",
  "198", "201",
  "201", "202",
  "202", "203",
  "203", "204",
  "204", "205",
  "205", "208",
  "208", "210",
  "210", "211",
  "211", "212",
  "212", "213",
  "213", "214",
  "214", "216",
  "216", "217",
  "217", "218",
  "218", "219",
  "219", "222",
  "222", "223",
  "223", "234",
  "234", "242",
  "242", "252",
  "252", "262",
  "262", "300",
  "300", "306",
  "306", "312",
  "312", "315",
  "315", "317"
)

is_missing_character <- function(x) {
  is.na(x) | x == "" | x == "NA"
}

get_component_membership <- function(adj_matrix) {
  n_nodes <- nrow(adj_matrix)
  node_names <- rownames(adj_matrix)

  if (n_nodes == 0) {
    return(tibble(area = character(), component_id = integer()))
  }

  visited <- rep(FALSE, n_nodes)
  component_id <- integer(n_nodes)
  component_counter <- 0L

  for (i in seq_len(n_nodes)) {
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

  tibble(
    area = node_names,
    component_id = component_id
  )
}

build_adjacency_objects <- function(area_universe, area_neighbors) {
  if (anyDuplicated(area_universe) > 0) {
    stop("area_universe contains duplicated labels.")
  }

  if (any(area_neighbors$area_from == area_neighbors$area_to)) {
    stop("area_neighbors must not contain self-loops.")
  }

  edge_labels <- sort(unique(c(area_neighbors$area_from, area_neighbors$area_to)))
  unknown_edge_labels <- setdiff(edge_labels, area_universe)

  if (length(unknown_edge_labels) > 0) {
    cat("Unknown area labels in area_neighbors:\n")
    print(unknown_edge_labels)
    stop("area_neighbors contains labels outside area_universe.")
  }

  area_neighbors_sym <- bind_rows(
    area_neighbors,
    tibble(
      area_from = area_neighbors$area_to,
      area_to = area_neighbors$area_from
    )
  ) |>
    distinct(area_from, area_to) |>
    arrange(area_from, area_to)

  adjacency_matrix <- matrix(
    0,
    nrow = length(area_universe),
    ncol = length(area_universe),
    dimnames = list(area_universe, area_universe)
  )

  for (i in seq_len(nrow(area_neighbors_sym))) {
    adjacency_matrix[area_neighbors_sym$area_from[[i]], area_neighbors_sym$area_to[[i]]] <- 1
  }

  if (!all(adjacency_matrix == t(adjacency_matrix))) {
    stop("adjacency_matrix is not symmetric after symmetrization.")
  }

  if (any(diag(adjacency_matrix) != 0)) {
    stop("adjacency_matrix diagonal must be 0.")
  }

  component_membership <- get_component_membership(adjacency_matrix)
  degree_tbl <- tibble(
    area = area_universe,
    number_of_neighbors = rowSums(adjacency_matrix)
  ) |>
    left_join(component_membership, by = "area")

  list(
    area_neighbors_sym = area_neighbors_sym,
    adjacency_matrix = adjacency_matrix,
    degree_tbl = degree_tbl,
    n_components = dplyr::n_distinct(component_membership$component_id),
    isolated_areas = degree_tbl |>
      filter(number_of_neighbors == 0) |>
      pull(area),
    n_undirected_edges = sum(adjacency_matrix[upper.tri(adjacency_matrix)])
  )
}

safe_fit_glmmTMB <- function(model_name, formula_obj, data_obj) {
  warning_messages <- character(0)

  fit_obj <- tryCatch(
    withCallingHandlers(
      glmmTMB(
        formula = formula_obj,
        family = nbinom2(link = "log"),
        data = data_obj
      ),
      warning = function(w) {
        warning_messages <<- c(warning_messages, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      NULL
    }
  )

  list(
    model_name = model_name,
    package_name = "glmmTMB",
    fit = fit_obj,
    warnings = unique(warning_messages)
  )
}

safe_fit_spaMM <- function(model_name, formula_obj, data_obj, adj_matrix_obj) {
  warning_messages <- character(0)

  fit_obj <- tryCatch(
    withCallingHandlers(
      fitme(
        formula = formula_obj,
        data = data_obj,
        family = negbin(),
        adjMatrix = adj_matrix_obj,
        method = "ML"
      ),
      warning = function(w) {
        warning_messages <<- c(warning_messages, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      NULL
    }
  )

  list(
    model_name = model_name,
    package_name = "spaMM",
    fit = fit_obj,
    warnings = unique(warning_messages)
  )
}

extract_glmmTMB_converged <- function(fit_obj) {
  fit_convergence_code <- tryCatch(fit_obj$fit$convergence, error = function(e) NA_integer_)
  fit_pdHess <- tryCatch(isTRUE(fit_obj$sdr$pdHess), error = function(e) NA)

  if (is.na(fit_convergence_code) || is.na(fit_pdHess)) {
    return(NA)
  }

  isTRUE(fit_convergence_code == 0 && fit_pdHess)
}

extract_spaMM_converged <- function(fit_obj) {
  conv_obj <- tryCatch(convergence(fit_obj), error = function(e) NULL)

  if (is.null(conv_obj)) {
    return(NA)
  }

  if (is.logical(conv_obj) && length(conv_obj) == 1) {
    return(conv_obj)
  }

  if (is.list(conv_obj)) {
    if ("OK" %in% names(conv_obj) && is.logical(conv_obj$OK) && length(conv_obj$OK) == 1) {
      return(conv_obj$OK)
    }

    if ("code" %in% names(conv_obj) && length(conv_obj$code) == 1 && is.numeric(conv_obj$code)) {
      return(isTRUE(conv_obj$code == 0))
    }
  }

  NA
}

collapse_notes <- function(notes_vec) {
  notes_vec <- unique(notes_vec[!is.na(notes_vec) & nzchar(notes_vec)])

  if (length(notes_vec) == 0) {
    return(NA_character_)
  }

  paste(notes_vec, collapse = " | ")
}

build_model_comparison_row <- function(spec_i, fit_result_i) {
  fit_ok <- !is.null(fit_result_i$fit)

  base_notes <- character(0)

  if (length(fit_result_i$warnings) > 0) {
    base_notes <- c(base_notes, paste0("warnings: ", paste(unique(fit_result_i$warnings), collapse = "; ")))
  }

  if (!fit_ok) {
    return(tibble(
      model_name = spec_i$model_name,
      package_name = spec_i$package_name,
      area_structure = spec_i$area_structure,
      depth_degree = spec_i$depth_degree,
      fit_ok = FALSE,
      AIC = NA_real_,
      BIC = NA_real_,
      logLik = NA_real_,
      nobs = NA_integer_,
      converged = NA,
      notes = collapse_notes(c(base_notes, "fit failed"))
    ))
  }

  fit_obj <- fit_result_i$fit

  converged_value <- if (identical(spec_i$package_name, "glmmTMB")) {
    extract_glmmTMB_converged(fit_obj)
  } else {
    extract_spaMM_converged(fit_obj)
  }

  package_note <- if (identical(spec_i$package_name, "spaMM")) {
    "AIC/BIC/logLik are package-specific extractors from spaMM"
  } else {
    NA_character_
  }

  tibble(
    model_name = spec_i$model_name,
    package_name = spec_i$package_name,
    area_structure = spec_i$area_structure,
    depth_degree = spec_i$depth_degree,
    fit_ok = TRUE,
    AIC = tryCatch(as.numeric(AIC(fit_obj)[1]), error = function(e) NA_real_),
    BIC = tryCatch(as.numeric(BIC(fit_obj)[1]), error = function(e) NA_real_),
    logLik = tryCatch(as.numeric(logLik(fit_obj)), error = function(e) NA_real_),
    nobs = tryCatch(as.integer(stats::nobs(fit_obj)), error = function(e) NA_integer_),
    converged = converged_value,
    notes = collapse_notes(c(base_notes, package_note))
  )
}

extract_spatial_area_effects <- function(fit_obj, area_levels_model) {
  ranef_obj <- tryCatch(ranef(fit_obj), error = function(e) NULL)

  if (is.null(ranef_obj)) {
    return(NULL)
  }

  area_ranef <- NULL

  if (is.list(ranef_obj) && "area" %in% names(ranef_obj)) {
    area_ranef <- ranef_obj[["area"]]
  } else if (is.list(ranef_obj) && "adjacency(1 | area)" %in% names(ranef_obj)) {
    area_ranef <- ranef_obj[["adjacency(1 | area)"]]
  } else if (is.list(ranef_obj) && "adjacency(1|area)" %in% names(ranef_obj)) {
    area_ranef <- ranef_obj[["adjacency(1|area)"]]
  }

  if (is.null(area_ranef)) {
    return(NULL)
  }

  if (is.vector(area_ranef) && !is.null(names(area_ranef))) {
    return(tibble(
      area = names(area_ranef),
      effect_estimate = as.numeric(area_ranef)
    ) |>
      filter(area %in% area_levels_model))
  }

  if (is.matrix(area_ranef) || is.data.frame(area_ranef)) {
    area_ranef_df <- as.data.frame(area_ranef)
    area_ranef_df$area <- rownames(area_ranef_df)

    effect_col <- names(area_ranef_df)[names(area_ranef_df) != "area"][[1]]

    return(area_ranef_df |>
      transmute(
        area = as.character(area),
        effect_estimate = as.numeric(.data[[effect_col]])
      ) |>
      filter(area %in% area_levels_model))
  }

  NULL
}

extract_area_residual_summary <- function(model_name, fit_obj, data_obj) {
  residual_type <- "pearson"
  residual_vec <- tryCatch(
    residuals(fit_obj, type = residual_type),
    error = function(e) NULL
  )

  if (is.null(residual_vec)) {
    residual_type <- "response"
    residual_vec <- tryCatch(
      residuals(fit_obj, type = residual_type),
      error = function(e) NULL
    )
  }

  if (is.null(residual_vec)) {
    return(tibble(
      model_name = model_name,
      area = character(),
      mean_residual = numeric(),
      median_residual = numeric(),
      n_observations = integer(),
      residual_type = character()
    ))
  }

  data_obj |>
    mutate(.residual = as.numeric(residual_vec)) |>
    group_by(area) |>
    summarise(
      mean_residual = mean(.residual, na.rm = TRUE),
      median_residual = stats::median(.residual, na.rm = TRUE),
      n_observations = n(),
      .groups = "drop"
    ) |>
    mutate(
      model_name = model_name,
      residual_type = residual_type
    ) |>
    select(model_name, area, mean_residual, median_residual, n_observations, residual_type)
}

glmm_input <- readr::read_csv(input_path, show_col_types = FALSE)

missing_cols <- setdiff(required_cols, names(glmm_input))

if (length(missing_cols) > 0) {
  cat("Missing required columns:\n")
  print(missing_cols)
  stop("Required columns are missing.")
}

glmm_input <- glmm_input |>
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
        !is_missing_character(area) &
        !is_missing_character(vessel) &
        !is.na(effort_glmm) &
        is.finite(effort_glmm) &
        effort_glmm > 0
    )
}

glmm_dat_spatial <- glmm_input |>
  filter(
    flag_use_for_main_glmm,
    !is_missing_character(area),
    !is_missing_character(vessel),
    !is.na(effort_glmm),
    is.finite(effort_glmm),
    effort_glmm > 0,
    !is.na(count_total),
    is.finite(count_total)
  )

glmm_dat_spatial_depth <- glmm_dat_spatial |>
  filter(
    !is.na(depth_glmm),
    is.finite(depth_glmm)
  )

cat("\n=== spatial data summary ===\n")
cat("n_raw_input=", nrow(glmm_input), "\n", sep = "")
cat("n_main_subset=", nrow(glmm_dat_spatial), "\n", sep = "")
cat("n_depth_subset=", nrow(glmm_dat_spatial_depth), "\n", sep = "")

if (nrow(glmm_dat_spatial) == 0) {
  stop("No rows available for glmm_dat_spatial.")
}

if (nrow(glmm_dat_spatial_depth) == 0) {
  stop("No rows available for glmm_dat_spatial_depth.")
}

depth_glmm_sd <- stats::sd(glmm_dat_spatial_depth$depth_glmm, na.rm = TRUE)
depth_glmm_mean <- mean(glmm_dat_spatial_depth$depth_glmm, na.rm = TRUE)

if (!is.finite(depth_glmm_sd) || depth_glmm_sd <= 0) {
  stop("depth_glmm standard deviation must be finite and > 0.")
}

glmm_dat_spatial_depth <- glmm_dat_spatial_depth |>
  mutate(
    depth_glmm_sc = (depth_glmm - depth_glmm_mean) / depth_glmm_sd
  )

adjacency_objects <- build_adjacency_objects(
  area_universe = area_universe,
  area_neighbors = area_neighbors
)

write_csv(as_tibble(adjacency_objects$adjacency_matrix, rownames = "area"), adjacency_matrix_path)
write_csv(adjacency_objects$area_neighbors_sym, adjacency_edges_path)

main_area_observed <- sort(unique(glmm_dat_spatial$area))
unknown_main_areas <- setdiff(main_area_observed, area_universe)

if (length(unknown_main_areas) > 0) {
  cat("Unknown main-subset areas not covered by area_universe:\n")
  print(unknown_main_areas)
  stop("main subset contains area labels outside area_universe.")
}

main_areas_without_neighbors <- adjacency_objects$degree_tbl |>
  filter(area %in% main_area_observed, number_of_neighbors == 0) |>
  pull(area)

cat("\n=== adjacency QA ===\n")
cat("n_area_universe=", length(area_universe), "\n", sep = "")
cat("n_undirected_edges=", adjacency_objects$n_undirected_edges, "\n", sep = "")
cat("n_connected_components=", adjacency_objects$n_components, "\n", sep = "")
cat("isolated_areas=", paste(adjacency_objects$isolated_areas, collapse = ","), "\n", sep = "")

if (length(main_areas_without_neighbors) > 0) {
  cat("Areas present in data but without neighbor definition:\n")
  print(main_areas_without_neighbors)
  warning("Some areas present in data have no adjacency neighbors.")
}

if (length(adjacency_objects$isolated_areas) > 0) {
  warning("Isolated areas exist in adjacency_matrix.")
}

if (adjacency_objects$n_components >= 2) {
  warning("adjacency_matrix has 2 or more connected components.")
}

area_levels_model <- area_universe[area_universe %in% sort(unique(glmm_dat_spatial_depth$area))]
adjacency_matrix_model <- adjacency_objects$adjacency_matrix[area_levels_model, area_levels_model, drop = FALSE]

glmm_dat_spatial_depth <- glmm_dat_spatial_depth |>
  mutate(
    area = factor(area, levels = area_levels_model),
    vessel = factor(vessel),
    year = factor(year),
    month = factor(month, levels = sort(unique(month)))
  )

cat("\n=== spatial model area alignment ===\n")
cat("data_area_levels=", paste(levels(glmm_dat_spatial_depth$area), collapse = ","), "\n", sep = "")
cat("adjacency_matrix_dim=", paste(dim(adjacency_matrix_model), collapse = "x"), "\n", sep = "")
cat(
  "all_levels_covered=",
  identical(levels(glmm_dat_spatial_depth$area), rownames(adjacency_matrix_model)) &&
    identical(rownames(adjacency_matrix_model), colnames(adjacency_matrix_model)),
  "\n",
  sep = ""
)

if (!identical(levels(glmm_dat_spatial_depth$area), rownames(adjacency_matrix_model))) {
  stop("area factor levels do not match adjacency_matrix rownames.")
}

if (!identical(rownames(adjacency_matrix_model), colnames(adjacency_matrix_model))) {
  stop("adjacency_matrix rownames and colnames do not match.")
}

model_specs <- list(
  list(
    model_name = "fit_nb_total_area_re_depth0",
    package_name = "glmmTMB",
    area_structure = "random_iid",
    depth_degree = 0,
    formula = count_total ~ year + month + offset(log(effort_glmm)) + (1 | area) + (1 | vessel)
  ),
  list(
    model_name = "fit_nb_total_area_re_depth1",
    package_name = "glmmTMB",
    area_structure = "random_iid",
    depth_degree = 1,
    formula = count_total ~ year + month + depth_glmm_sc + offset(log(effort_glmm)) + (1 | area) + (1 | vessel)
  ),
  list(
    model_name = "fit_nb_total_area_re_depth2",
    package_name = "glmmTMB",
    area_structure = "random_iid",
    depth_degree = 2,
    formula = count_total ~ year + month + depth_glmm_sc + I(depth_glmm_sc^2) + offset(log(effort_glmm)) + (1 | area) + (1 | vessel)
  ),
  list(
    model_name = "fit_nb_total_area_fe_depth0",
    package_name = "glmmTMB",
    area_structure = "fixed",
    depth_degree = 0,
    formula = count_total ~ year + month + area + offset(log(effort_glmm)) + (1 | vessel)
  ),
  list(
    model_name = "fit_nb_total_area_fe_depth1",
    package_name = "glmmTMB",
    area_structure = "fixed",
    depth_degree = 1,
    formula = count_total ~ year + month + area + depth_glmm_sc + offset(log(effort_glmm)) + (1 | vessel)
  ),
  list(
    model_name = "fit_nb_total_area_fe_depth2",
    package_name = "glmmTMB",
    area_structure = "fixed",
    depth_degree = 2,
    formula = count_total ~ year + month + area + depth_glmm_sc + I(depth_glmm_sc^2) + offset(log(effort_glmm)) + (1 | vessel)
  ),
  list(
    model_name = "fit_nb_total_area_adj_depth0",
    package_name = "spaMM",
    area_structure = "adjacency",
    depth_degree = 0,
    formula = count_total ~ year + month + offset(log(effort_glmm)) + adjacency(1 | area) + (1 | vessel)
  ),
  list(
    model_name = "fit_nb_total_area_adj_depth1",
    package_name = "spaMM",
    area_structure = "adjacency",
    depth_degree = 1,
    formula = count_total ~ year + month + depth_glmm_sc + offset(log(effort_glmm)) + adjacency(1 | area) + (1 | vessel)
  ),
  list(
    model_name = "fit_nb_total_area_adj_depth2",
    package_name = "spaMM",
    area_structure = "adjacency",
    depth_degree = 2,
    formula = count_total ~ year + month + depth_glmm_sc + I(depth_glmm_sc^2) + offset(log(effort_glmm)) + adjacency(1 | area) + (1 | vessel)
  )
)

model_fit_results <- vector("list", length(model_specs))
names(model_fit_results) <- vapply(model_specs, function(x) x$model_name, character(1))

for (i in seq_along(model_specs)) {
  spec_i <- model_specs[[i]]

  cat("\n=== fitting model ===\n")
  cat("model_name=", spec_i$model_name, "\n", sep = "")

  if (identical(spec_i$package_name, "glmmTMB")) {
    model_fit_results[[spec_i$model_name]] <- safe_fit_glmmTMB(
      model_name = spec_i$model_name,
      formula_obj = spec_i$formula,
      data_obj = glmm_dat_spatial_depth
    )
  } else {
    model_fit_results[[spec_i$model_name]] <- safe_fit_spaMM(
      model_name = spec_i$model_name,
      formula_obj = spec_i$formula,
      data_obj = glmm_dat_spatial_depth,
      adj_matrix_obj = adjacency_matrix_model
    )
  }
}

for (spec_i in model_specs) {
  assign(spec_i$model_name, model_fit_results[[spec_i$model_name]]$fit, envir = .GlobalEnv)
}

model_comparison_tbl <- map_dfr(
  model_specs,
  function(spec_i) {
    build_model_comparison_row(
      spec_i = spec_i,
      fit_result_i = model_fit_results[[spec_i$model_name]]
    )
  }
) |>
  mutate(
    AIC_sort = if_else(is.na(AIC), Inf, AIC)
  ) |>
  arrange(desc(fit_ok), AIC_sort, model_name) |>
  select(
    model_name, package_name, area_structure, depth_degree,
    fit_ok, AIC, BIC, logLik, nobs, converged, notes
  )

write_csv(model_comparison_tbl, model_comparison_path)

cat("\n=== model comparison ===\n")
print(model_comparison_tbl, n = nrow(model_comparison_tbl))

best_nonspatial_row <- model_comparison_tbl |>
  filter(fit_ok, area_structure %in% c("fixed", "random_iid")) |>
  slice_head(n = 1)

best_adjacency_row <- model_comparison_tbl |>
  filter(fit_ok, area_structure == "adjacency") |>
  slice_head(n = 1)

empty_spatial_effects_tbl <- tibble(
  area = character(),
  effect_estimate = numeric()
)

empty_spatial_effects_plot_tbl <- tibble(
  area = character(),
  effect_estimate = numeric(),
  number_of_neighbors = integer(),
  observed_mean_count = numeric(),
  n_rows = integer()
)

if (nrow(best_adjacency_row) == 1) {
  best_adjacency_model_name <- best_adjacency_row$model_name[[1]]
  best_adjacency_fit <- model_fit_results[[best_adjacency_model_name]]$fit

  best_spatial_area_effects_tbl <- extract_spatial_area_effects(
    fit_obj = best_adjacency_fit,
    area_levels_model = area_levels_model
  )

  if (is.null(best_spatial_area_effects_tbl)) {
    best_spatial_area_effects_tbl <- empty_spatial_effects_tbl
  }

  best_spatial_area_effects_plot_tbl <- glmm_dat_spatial_depth |>
    group_by(area) |>
    summarise(
      observed_mean_count = mean(count_total, na.rm = TRUE),
      n_rows = n(),
      .groups = "drop"
    ) |>
    left_join(
      adjacency_objects$degree_tbl |>
        select(area, number_of_neighbors),
      by = "area"
    ) |>
    left_join(best_spatial_area_effects_tbl, by = "area") |>
    select(area, effect_estimate, number_of_neighbors, observed_mean_count, n_rows)
} else {
  best_spatial_area_effects_tbl <- empty_spatial_effects_tbl
  best_spatial_area_effects_plot_tbl <- empty_spatial_effects_plot_tbl
}

write_csv(best_spatial_area_effects_tbl, best_spatial_area_effects_path)
write_csv(best_spatial_area_effects_plot_tbl, best_spatial_area_effects_plot_path)

if (nrow(best_nonspatial_row) == 1) {
  best_nonspatial_model_name <- best_nonspatial_row$model_name[[1]]
  best_nonspatial_fit <- model_fit_results[[best_nonspatial_model_name]]$fit
  best_nonspatial_area_residual_tbl <- extract_area_residual_summary(
    model_name = best_nonspatial_model_name,
    fit_obj = best_nonspatial_fit,
    data_obj = glmm_dat_spatial_depth
  )
} else {
  best_nonspatial_area_residual_tbl <- tibble(
    model_name = character(),
    area = character(),
    mean_residual = numeric(),
    median_residual = numeric(),
    n_observations = integer(),
    residual_type = character()
  )
}

if (nrow(best_adjacency_row) == 1) {
  best_adjacency_model_name <- best_adjacency_row$model_name[[1]]
  best_adjacency_fit <- model_fit_results[[best_adjacency_model_name]]$fit
  best_adjacency_area_residual_tbl <- extract_area_residual_summary(
    model_name = best_adjacency_model_name,
    fit_obj = best_adjacency_fit,
    data_obj = glmm_dat_spatial_depth
  )
} else {
  best_adjacency_area_residual_tbl <- tibble(
    model_name = character(),
    area = character(),
    mean_residual = numeric(),
    median_residual = numeric(),
    n_observations = integer(),
    residual_type = character()
  )
}

write_csv(best_nonspatial_area_residual_tbl, best_nonspatial_area_residuals_path)
write_csv(best_adjacency_area_residual_tbl, best_adjacency_area_residuals_path)

failed_models <- model_comparison_tbl |>
  filter(!fit_ok) |>
  pull(model_name)

cat("\n=== summary ===\n")
cat("best_nonspatial_model=", if (nrow(best_nonspatial_row) == 1) best_nonspatial_row$model_name[[1]] else NA_character_, "\n", sep = "")
cat("best_adjacency_model=", if (nrow(best_adjacency_row) == 1) best_adjacency_row$model_name[[1]] else NA_character_, "\n", sep = "")
cat("failed_models=", paste(failed_models, collapse = ","), "\n", sep = "")
cat("main_areas_without_neighbors=", paste(main_areas_without_neighbors, collapse = ","), "\n", sep = "")
cat("isolated_areas=", paste(adjacency_objects$isolated_areas, collapse = ","), "\n", sep = "")
cat("model_comparison_path=", model_comparison_path, "\n", sep = "")
cat("adjacency_matrix_path=", adjacency_matrix_path, "\n", sep = "")
cat("adjacency_edges_path=", adjacency_edges_path, "\n", sep = "")
cat("best_spatial_area_effects_path=", best_spatial_area_effects_path, "\n", sep = "")
cat("best_spatial_area_effects_plot_path=", best_spatial_area_effects_plot_path, "\n", sep = "")
cat("best_nonspatial_area_residuals_path=", best_nonspatial_area_residuals_path, "\n", sep = "")
cat("best_adjacency_area_residuals_path=", best_adjacency_area_residuals_path, "\n", sep = "")
