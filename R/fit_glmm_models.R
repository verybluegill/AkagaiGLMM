# =========================================
# fit_glmm_models.R
# 本流 Step 3
# 入力: data_processed/akagai_glmm_input.csv
# 出力: total GLMM、depth 比較、size 別 GLMM のモデル・表・図
# =========================================

source(file.path("R", "00_load_packages.R"))

optional_pkgs <- load_project_packages(
  required_pkgs = c("dplyr", "ggplot2", "glmmTMB", "purrr", "readr", "tibble", "tidyr"),
  optional_pkgs = c("emmeans", "DHARMa")
)

ensure_project_dirs()

input_path <- file.path("data_processed", "akagai_glmm_input.csv")

fit_total_path <- file.path("output", "models", "fit_nb_total.rds")
emm_year_total_path <- file.path("output", "tables", "emm_year_total.csv")
index_total_fig_path <- file.path("output", "figures", "index_total_by_year.png")
row_summary_total_path <- file.path("output", "tables", "check_total_glmm_row_summary_by_year.csv")

fit_total_depth_path <- file.path("output", "models", "fit_nb_total_with_depth.rds")
emm_year_total_depth_path <- file.path("output", "tables", "emm_year_total_with_depth.csv")
index_total_depth_fig_path <- file.path("output", "figures", "index_total_with_depth_by_year.png")
row_summary_depth_path <- file.path("output", "tables", "check_depth_glmm_row_summary_by_year.csv")
model_comparison_total_path <- file.path("output", "tables", "model_comparison_total_area_depth.csv")
depth_effect_total_path <- file.path("output", "tables", "depth_effect_total_with_depth.csv")
depth_effect_total_fig_path <- file.path("output", "figures", "depth_effect_total_with_depth.png")
emm_year_depth0_path <- file.path("output", "tables", "emm_year_total_depth0_same_subset.csv")
emm_year_depth1_path <- file.path("output", "tables", "emm_year_total_depth1_same_subset.csv")
index_compare_fig_path <- file.path("output", "figures", "index_total_depth0_vs_depth1_same_subset.png")

variance_summary_path <- file.path("output", "tables", "glmm_by_size_variance_summary.csv")
run_status_path <- file.path("output", "tables", "glmm_by_size_run_status.csv")
row_summary_by_size_path <- file.path("output", "tables", "check_by_size_glmm_row_summary_by_year.csv")

get_year_index_col <- function(tbl) {
  matched <- intersect(c("response", "rate", "prob", "emmean"), names(tbl))

  if (length(matched) == 0) {
    stop("Failed to identify the year index column.")
  }

  matched[[1]]
}

`%||%` <- function(x, y) {
  if (length(x) == 0 || is.null(x)) {
    return(y)
  }

  x
}

get_observed_levels <- function(x) {
  sort(unique(as.character(x[!is.na(x)])))
}

pick_first_or_na <- function(x) {
  if (length(x) == 0) {
    return(NA_real_)
  }

  x[[1]]
}

compute_year_index_table <- function(model_obj, data_obj, optional_pkgs, depth_glmm_sc_value = NULL) {
  if (isTRUE(optional_pkgs[["emmeans"]])) {
    at_list <- list(effort_glmm = 1)

    if (!is.null(depth_glmm_sc_value)) {
      at_list$depth_glmm_sc <- depth_glmm_sc_value
    }

    return(as.data.frame(emmeans::emmeans(
      model_obj,
      specs = ~ year,
      at = at_list,
      type = "response"
    )))
  }

  year_levels_used <- get_observed_levels(data_obj$year)
  month_levels_used <- get_observed_levels(data_obj$month)
  area_levels_used <- get_observed_levels(data_obj$area)
  vessel_levels_used <- get_observed_levels(data_obj$vessel)

  pred_grid <- tidyr::expand_grid(
    year = factor(year_levels_used, levels = year_levels_used),
    month = factor(month_levels_used, levels = month_levels_used),
    area = factor(area_levels_used[[1]], levels = area_levels_used),
    vessel = factor(vessel_levels_used[[1]], levels = vessel_levels_used),
    effort_glmm = 1
  )

  if (!is.null(depth_glmm_sc_value)) {
    pred_grid$depth_glmm_sc <- depth_glmm_sc_value
  }

  pred_grid |>
    dplyr::mutate(predicted = predict(model_obj, newdata = pred_grid, type = "response", re.form = NA)) |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(response = mean(.data$predicted), .groups = "drop")
}

safe_fit_glmmTMB <- function(model_name, formula_obj, data_obj) {
  warning_messages <- character(0)

  fit_obj <- tryCatch(
    withCallingHandlers(
      glmmTMB::glmmTMB(
        formula = formula_obj,
        family = glmmTMB::nbinom2(),
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
    fit = fit_obj,
    warning_message = paste(unique(warning_messages), collapse = " | ")
  )
}

extract_model_metrics <- function(model_name, area_type, depth_degree, fit_obj, warning_message = "") {
  if (is.null(fit_obj)) {
    return(tibble::tibble(
      model_name = model_name,
      area_type = area_type,
      depth_degree = depth_degree,
      AIC = NA_real_,
      BIC = NA_real_,
      logLik = NA_real_,
      nobs = NA_integer_,
      converged = NA,
      pdHess = NA,
      warning_message = warning_message
    ))
  }

  tibble::tibble(
    model_name = model_name,
    area_type = area_type,
    depth_degree = depth_degree,
    AIC = tryCatch(as.numeric(AIC(fit_obj)[1]), error = function(e) NA_real_),
    BIC = tryCatch(as.numeric(BIC(fit_obj)[1]), error = function(e) NA_real_),
    logLik = tryCatch(as.numeric(logLik(fit_obj)), error = function(e) NA_real_),
    nobs = tryCatch(as.integer(stats::nobs(fit_obj)), error = function(e) NA_integer_),
    converged = tryCatch(isTRUE(fit_obj$fit$convergence == 0), error = function(e) NA),
    pdHess = tryCatch(isTRUE(fit_obj$sdr$pdHess), error = function(e) NA),
    warning_message = warning_message
  )
}

extract_re_sd <- function(varcorr_cond, grp_name) {
  if (is.null(varcorr_cond[[grp_name]])) {
    return(c(variance = NA_real_, sd = NA_real_))
  }

  grp_mat <- varcorr_cond[[grp_name]]
  grp_sd <- attr(grp_mat, "stddev")

  c(
    variance = as.numeric(grp_mat[1, 1]),
    sd = as.numeric(grp_sd[[1]])
  )
}

plot_year_index <- function(tbl, output_path) {
  year_index_col <- get_year_index_col(tbl)

  p <- ggplot2::ggplot(tbl, ggplot2::aes(x = .data$year, y = .data[[year_index_col]], group = 1)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(
      x = "Year",
      y = "Index"
    )

  if (all(c("lower.CL", "upper.CL") %in% names(tbl))) {
    p <- p +
      ggplot2::geom_errorbar(ggplot2::aes(ymin = .data$lower.CL, ymax = .data$upper.CL), width = 0.15)
  }

  ggplot2::ggsave(
    filename = output_path,
    plot = p,
    width = 10,
    height = 6,
    dpi = 150
  )
}

if (!file.exists(input_path)) {
  stop("Input file not found: ", input_path)
}

glmm_input <- readr::read_csv(input_path, show_col_types = FALSE) |>
  dplyr::mutate(
    year = suppressWarnings(as.integer(.data$year)),
    month = suppressWarnings(as.integer(.data$month)),
    area = as.character(.data$area),
    vessel = as.character(.data$vessel),
    count_total = suppressWarnings(as.numeric(.data$count_total)),
    effort_glmm = suppressWarnings(as.numeric(.data$effort_glmm)),
    depth_glmm = suppressWarnings(as.numeric(.data$depth_glmm)),
    chu = suppressWarnings(as.numeric(.data$chu)),
    dai = suppressWarnings(as.numeric(.data$dai)),
    toku = suppressWarnings(as.numeric(.data$toku)),
    tokudai = suppressWarnings(as.numeric(.data$tokudai)),
    flag_use_for_main_glmm = as.logical(.data$flag_use_for_main_glmm)
  )

required_cols <- c("year", "month", "area", "vessel", "count_total", "effort_glmm", "depth_glmm", "chu", "dai", "toku", "tokudai", "flag_use_for_main_glmm")
missing_cols <- setdiff(required_cols, names(glmm_input))

if (length(missing_cols) > 0) {
  stop("Required columns are missing: ", paste(missing_cols, collapse = ", "))
}

if (any(is.na(glmm_input$count_total))) {
  stop("count_total contains NA.")
}

row_summary_by_year <- glmm_input |>
  dplyr::group_by(.data$year) |>
  dplyr::summarise(
    n_input = dplyr::n(),
    n_used = sum(.data$flag_use_for_main_glmm, na.rm = TRUE),
    n_dropped = dplyr::n() - sum(.data$flag_use_for_main_glmm, na.rm = TRUE),
    .groups = "drop"
  )

readr::write_csv(row_summary_by_year, row_summary_total_path)
readr::write_csv(row_summary_by_year, row_summary_by_size_path)

glmm_dat_main <- glmm_input |>
  dplyr::filter(.data$flag_use_for_main_glmm) |>
  dplyr::mutate(
    year = factor(.data$year),
    month = factor(.data$month, levels = sort(unique(.data$month))),
    area = factor(.data$area),
    vessel = factor(.data$vessel)
  )

if (nrow(glmm_dat_main) == 0) {
  stop("No rows available for GLMM after flag_use_for_main_glmm filtering.")
}

if (any(is.na(glmm_dat_main$effort_glmm)) || any(!is.finite(glmm_dat_main$effort_glmm)) || any(glmm_dat_main$effort_glmm <= 0)) {
  stop("effort_glmm must be finite and > 0 in the filtered data.")
}

fit_nb_total <- glmmTMB::glmmTMB(
  formula = count_total ~ year + month + offset(log(effort_glmm)) + (1 | area) + (1 | vessel),
  family = glmmTMB::nbinom2(),
  data = glmm_dat_main
)

saveRDS(fit_nb_total, fit_total_path)

if (isTRUE(optional_pkgs[["DHARMa"]])) {
  print(try(DHARMa::testDispersion(DHARMa::simulateResiduals(fit_nb_total, plot = FALSE)), silent = TRUE))
}

emm_year_total_tbl <- compute_year_index_table(
  model_obj = fit_nb_total,
  data_obj = glmm_dat_main,
  optional_pkgs = optional_pkgs
)

readr::write_csv(emm_year_total_tbl, emm_year_total_path)
plot_year_index(emm_year_total_tbl, index_total_fig_path)

glmm_dat_depth <- glmm_dat_main |>
  dplyr::filter(!is.na(.data$depth_glmm), is.finite(.data$depth_glmm))

if (nrow(glmm_dat_depth) == 0) {
  stop("No rows available for depth comparison.")
}

depth_glmm_mean <- mean(glmm_dat_depth$depth_glmm, na.rm = TRUE)
depth_glmm_sd <- stats::sd(glmm_dat_depth$depth_glmm, na.rm = TRUE)

if (!is.finite(depth_glmm_sd) || depth_glmm_sd <= 0) {
  stop("depth_glmm standard deviation must be finite and > 0.")
}

glmm_dat_depth <- glmm_dat_depth |>
  dplyr::mutate(depth_glmm_sc = (.data$depth_glmm - depth_glmm_mean) / depth_glmm_sd)

row_summary_depth <- glmm_input |>
  dplyr::group_by(.data$year) |>
  dplyr::summarise(
    n_input = dplyr::n(),
    n_used = sum(.data$flag_use_for_main_glmm & !is.na(.data$depth_glmm), na.rm = TRUE),
    n_dropped = dplyr::n() - sum(.data$flag_use_for_main_glmm & !is.na(.data$depth_glmm), na.rm = TRUE),
    .groups = "drop"
  )

readr::write_csv(row_summary_depth, row_summary_depth_path)

model_specs <- list(
  list(model_name = "fit_nb_total_area_re_depth0", area_type = "random", depth_degree = 0L, formula = count_total ~ year + month + offset(log(effort_glmm)) + (1 | area) + (1 | vessel)),
  list(model_name = "fit_nb_total_area_re_depth1", area_type = "random", depth_degree = 1L, formula = count_total ~ year + month + depth_glmm_sc + offset(log(effort_glmm)) + (1 | area) + (1 | vessel)),
  list(model_name = "fit_nb_total_area_re_depth2", area_type = "random", depth_degree = 2L, formula = count_total ~ year + month + depth_glmm_sc + I(depth_glmm_sc^2) + offset(log(effort_glmm)) + (1 | area) + (1 | vessel)),
  list(model_name = "fit_nb_total_area_fe_depth0", area_type = "fixed", depth_degree = 0L, formula = count_total ~ year + month + area + offset(log(effort_glmm)) + (1 | vessel)),
  list(model_name = "fit_nb_total_area_fe_depth1", area_type = "fixed", depth_degree = 1L, formula = count_total ~ year + month + area + depth_glmm_sc + offset(log(effort_glmm)) + (1 | vessel)),
  list(model_name = "fit_nb_total_area_fe_depth2", area_type = "fixed", depth_degree = 2L, formula = count_total ~ year + month + area + depth_glmm_sc + I(depth_glmm_sc^2) + offset(log(effort_glmm)) + (1 | vessel))
)

model_fit_results <- stats::setNames(vector("list", length(model_specs)), vapply(model_specs, function(x) x$model_name, character(1)))

for (spec_i in model_specs) {
  model_fit_results[[spec_i$model_name]] <- safe_fit_glmmTMB(
    model_name = spec_i$model_name,
    formula_obj = spec_i$formula,
    data_obj = glmm_dat_depth
  )
}

model_comparison_tbl <- purrr::map_dfr(
  model_specs,
  function(spec_i) {
    fit_out <- model_fit_results[[spec_i$model_name]]
    extract_model_metrics(
      model_name = spec_i$model_name,
      area_type = spec_i$area_type,
      depth_degree = spec_i$depth_degree,
      fit_obj = fit_out$fit,
      warning_message = fit_out$warning_message
    )
  }
) |>
  dplyr::arrange(.data$AIC, .data$BIC, .data$model_name)

readr::write_csv(model_comparison_tbl, model_comparison_total_path)

successful_total_tbl <- model_comparison_tbl |>
  dplyr::filter(!is.na(.data$AIC))

if (nrow(successful_total_tbl) == 0) {
  stop("All total depth comparison models failed.")
}

best_total_model_name <- successful_total_tbl$model_name[[1]]
best_total_depth_degree <- successful_total_tbl$depth_degree[[1]]
best_total_model <- model_fit_results[[best_total_model_name]]$fit

saveRDS(best_total_model, fit_total_depth_path)

depth_effect_total_tbl <- as.data.frame(summary(best_total_model)$coefficients$cond) |>
  tibble::rownames_to_column(var = "term") |>
  dplyr::transmute(
    term = .data$term,
    estimate = .data$Estimate,
    std_error = .data$`Std. Error`,
    z_value = .data$`z value`,
    p_value = .data$`Pr(>|z|)`,
    depth_glmm_mean = depth_glmm_mean,
    depth_glmm_sd = depth_glmm_sd,
    best_model_name = best_total_model_name
  ) |>
  dplyr::filter(.data$term %in% c("depth_glmm_sc", "I(depth_glmm_sc^2)"))

readr::write_csv(depth_effect_total_tbl, depth_effect_total_path)

if (isTRUE(optional_pkgs[["DHARMa"]])) {
  print(try(DHARMa::testDispersion(DHARMa::simulateResiduals(best_total_model, plot = FALSE)), silent = TRUE))
}

depth_glmm_ref <- stats::median(glmm_dat_depth$depth_glmm, na.rm = TRUE)
depth_glmm_sc_ref <- (depth_glmm_ref - depth_glmm_mean) / depth_glmm_sd

fit_nb_total_depth0 <- model_fit_results[["fit_nb_total_area_re_depth0"]]$fit
fit_nb_total_depth1 <- model_fit_results[["fit_nb_total_area_re_depth1"]]$fit

emm_year_depth0_tbl <- compute_year_index_table(
  model_obj = fit_nb_total_depth0,
  data_obj = glmm_dat_depth,
  optional_pkgs = optional_pkgs
)

emm_year_depth1_tbl <- compute_year_index_table(
  model_obj = fit_nb_total_depth1,
  data_obj = glmm_dat_depth,
  optional_pkgs = optional_pkgs,
  depth_glmm_sc_value = depth_glmm_sc_ref
)

emm_year_total_with_depth_tbl <- compute_year_index_table(
  model_obj = best_total_model,
  data_obj = glmm_dat_depth,
  optional_pkgs = optional_pkgs,
  depth_glmm_sc_value = if (best_total_depth_degree > 0) depth_glmm_sc_ref else NULL
)

readr::write_csv(emm_year_depth0_tbl, emm_year_depth0_path)
readr::write_csv(emm_year_depth1_tbl, emm_year_depth1_path)
readr::write_csv(emm_year_total_with_depth_tbl, emm_year_total_depth_path)

plot_year_index(emm_year_total_with_depth_tbl, index_total_depth_fig_path)

year_index_col_depth0 <- get_year_index_col(emm_year_depth0_tbl)
year_index_col_depth1 <- get_year_index_col(emm_year_depth1_tbl)

year_index_compare_tbl <- dplyr::bind_rows(
  emm_year_depth0_tbl |>
    dplyr::transmute(year = .data$year, response = .data[[year_index_col_depth0]], model = "Depth0"),
  emm_year_depth1_tbl |>
    dplyr::transmute(year = .data$year, response = .data[[year_index_col_depth1]], model = "Depth1")
)

ggplot2::ggsave(
  filename = index_compare_fig_path,
  plot = ggplot2::ggplot(year_index_compare_tbl, ggplot2::aes(x = .data$year, y = .data$response, color = .data$model, group = .data$model)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(
      x = "Year",
      y = "Index",
      color = "Model"
    ),
  width = 10,
  height = 6,
  dpi = 150
)

if (best_total_depth_degree > 0) {
  depth_plot_grid <- tidyr::expand_grid(
    year = factor(levels(glmm_dat_depth$year)[[1]], levels = levels(glmm_dat_depth$year)),
    month = factor(levels(glmm_dat_depth$month), levels = levels(glmm_dat_depth$month)),
    depth_glmm = seq(min(glmm_dat_depth$depth_glmm, na.rm = TRUE), max(glmm_dat_depth$depth_glmm, na.rm = TRUE), length.out = 100)
  ) |>
    dplyr::mutate(
      depth_glmm_sc = (.data$depth_glmm - depth_glmm_mean) / depth_glmm_sd,
      effort_glmm = 1,
      area = factor(levels(glmm_dat_depth$area)[[1]], levels = levels(glmm_dat_depth$area)),
      vessel = factor(levels(glmm_dat_depth$vessel)[[1]], levels = levels(glmm_dat_depth$vessel))
    )

  depth_plot_grid$predicted_count <- predict(
    best_total_model,
    newdata = depth_plot_grid,
    type = "response",
    re.form = NA
  )

  depth_plot_grid <- depth_plot_grid |>
    dplyr::group_by(.data$depth_glmm) |>
    dplyr::summarise(predicted_count = mean(.data$predicted_count), .groups = "drop")

  ggplot2::ggsave(
    filename = depth_effect_total_fig_path,
    plot = ggplot2::ggplot(depth_plot_grid, ggplot2::aes(x = .data$depth_glmm, y = .data$predicted_count)) +
      ggplot2::geom_line() +
      ggplot2::labs(
        x = "Depth",
        y = "Predicted count"
      ),
    width = 10,
    height = 6,
    dpi = 150
  )
}

size_vars <- c("chu", "dai", "toku", "tokudai")

variance_summary_tbl <- tibble::tibble(
  size = character(),
  n_obs = integer(),
  n_zero = integer(),
  zero_ratio = numeric(),
  aic = numeric(),
  logLik = numeric(),
  disp_parameter = numeric(),
  area_variance = numeric(),
  area_sd = numeric(),
  vessel_variance = numeric(),
  vessel_sd = numeric(),
  best_model_name = character(),
  depth_degree = integer(),
  depth_linear_estimate = numeric(),
  depth_quadratic_estimate = numeric(),
  depth_glmm_mean = numeric(),
  depth_glmm_sd = numeric(),
  converged = logical(),
  fit_convergence_code = integer(),
  pdHess = logical(),
  warning_message = character()
)

run_status_tbl <- tibble::tibble(
  size = size_vars,
  status = "not_run",
  error_message = NA_character_,
  warning_message = NA_character_,
  model_path = NA_character_,
  emm_year_path = NA_character_,
  index_fig_path = NA_character_,
  best_model_name = NA_character_,
  best_depth_degree = NA_integer_,
  comparison_path = NA_character_,
  depth_effect_path = NA_character_,
  depth_effect_fig_path = NA_character_
)

for (sv in size_vars) {
  model_path_i <- file.path("output", "models", paste0("fit_nb_", sv, ".rds"))
  emm_year_path_i <- file.path("output", "tables", paste0("emm_year_", sv, ".csv"))
  index_fig_path_i <- file.path("output", "figures", paste0("index_", sv, "_by_year.png"))
  comparison_path_i <- file.path("output", "tables", paste0("model_comparison_by_size_", sv, ".csv"))
  depth_effect_path_i <- file.path("output", "tables", paste0("depth_effect_", sv, ".csv"))
  depth_effect_fig_path_i <- file.path("output", "figures", paste0("depth_effect_", sv, "_best_model.png"))

  fit_depth0 <- safe_fit_glmmTMB(
    model_name = paste0("fit_", sv, "_depth0"),
    formula_obj = stats::as.formula(paste0(sv, " ~ year + month + offset(log(effort_glmm)) + (1 | area) + (1 | vessel)")),
    data_obj = glmm_dat_depth
  )
  fit_depth1 <- safe_fit_glmmTMB(
    model_name = paste0("fit_", sv, "_depth1"),
    formula_obj = stats::as.formula(paste0(sv, " ~ year + month + depth_glmm_sc + offset(log(effort_glmm)) + (1 | area) + (1 | vessel)")),
    data_obj = glmm_dat_depth
  )
  fit_depth2 <- safe_fit_glmmTMB(
    model_name = paste0("fit_", sv, "_depth2"),
    formula_obj = stats::as.formula(paste0(sv, " ~ year + month + depth_glmm_sc + I(depth_glmm_sc^2) + offset(log(effort_glmm)) + (1 | area) + (1 | vessel)")),
    data_obj = glmm_dat_depth
  )

  comparison_tbl <- dplyr::bind_rows(
    extract_model_metrics(fit_depth0$model_name, "random", 0L, fit_depth0$fit, fit_depth0$warning_message),
    extract_model_metrics(fit_depth1$model_name, "random", 1L, fit_depth1$fit, fit_depth1$warning_message),
    extract_model_metrics(fit_depth2$model_name, "random", 2L, fit_depth2$fit, fit_depth2$warning_message)
  ) |>
    dplyr::arrange(.data$AIC, .data$BIC, .data$model_name)

  readr::write_csv(comparison_tbl, comparison_path_i)

  successful_size_tbl <- comparison_tbl |>
    dplyr::filter(!is.na(.data$AIC))

  if (nrow(successful_size_tbl) == 0) {
    run_status_tbl <- run_status_tbl |>
      dplyr::mutate(
        status = dplyr::if_else(.data$size == sv, "fit_failed", .data$status),
        comparison_path = dplyr::if_else(.data$size == sv, comparison_path_i, .data$comparison_path)
      )
    next
  }

  best_model_name_i <- successful_size_tbl$model_name[[1]]
  best_depth_degree_i <- successful_size_tbl$depth_degree[[1]]
  best_model_obj <- if (identical(best_model_name_i, fit_depth0$model_name)) {
    fit_depth0$fit
  } else if (identical(best_model_name_i, fit_depth1$model_name)) {
    fit_depth1$fit
  } else {
    fit_depth2$fit
  }

  saveRDS(best_model_obj, model_path_i)

  if (isTRUE(optional_pkgs[["DHARMa"]])) {
    print(try(DHARMa::testDispersion(DHARMa::simulateResiduals(best_model_obj, plot = FALSE)), silent = TRUE))
  }

  emm_year_tbl <- compute_year_index_table(
    model_obj = best_model_obj,
    data_obj = glmm_dat_depth,
    optional_pkgs = optional_pkgs,
    depth_glmm_sc_value = if (best_depth_degree_i > 0) 0 else NULL
  )

  readr::write_csv(emm_year_tbl, emm_year_path_i)
  plot_year_index(emm_year_tbl, index_fig_path_i)

  depth_effect_tbl <- as.data.frame(summary(best_model_obj)$coefficients$cond) |>
    tibble::rownames_to_column(var = "term") |>
    dplyr::transmute(
      size = sv,
      term = .data$term,
      estimate = .data$Estimate,
      std_error = .data$`Std. Error`,
      z_value = .data$`z value`,
      p_value = .data$`Pr(>|z|)`,
      depth_glmm_mean = depth_glmm_mean,
      depth_glmm_sd = depth_glmm_sd,
      best_model_name = best_model_name_i
    ) |>
    dplyr::filter(.data$term %in% c("depth_glmm_sc", "I(depth_glmm_sc^2)"))

  if (nrow(depth_effect_tbl) == 0) {
    depth_effect_tbl <- tibble::tibble(
      size = sv,
      term = NA_character_,
      estimate = NA_real_,
      std_error = NA_real_,
      z_value = NA_real_,
      p_value = NA_real_,
      depth_glmm_mean = depth_glmm_mean,
      depth_glmm_sd = depth_glmm_sd,
      best_model_name = best_model_name_i
    )
  }

  readr::write_csv(depth_effect_tbl, depth_effect_path_i)

  depth_effect_fig_path_saved <- NA_character_

  if (best_depth_degree_i == 2L) {
    depth_plot_dat <- tibble::tibble(
      year = factor(levels(glmm_dat_depth$year)[[1]], levels = levels(glmm_dat_depth$year)),
      month = factor(levels(glmm_dat_depth$month)[[1]], levels = levels(glmm_dat_depth$month)),
      area = factor(levels(glmm_dat_depth$area)[[1]], levels = levels(glmm_dat_depth$area)),
      vessel = factor(levels(glmm_dat_depth$vessel)[[1]], levels = levels(glmm_dat_depth$vessel)),
      effort_glmm = 1,
      depth_glmm_sc = seq(min(glmm_dat_depth$depth_glmm_sc, na.rm = TRUE), max(glmm_dat_depth$depth_glmm_sc, na.rm = TRUE), length.out = 200)
    )

    depth_plot_dat$predicted <- predict(
      best_model_obj,
      newdata = depth_plot_dat,
      type = "response",
      re.form = NA
    )

    ref_pred <- predict(
      best_model_obj,
      newdata = tibble::tibble(
        year = factor(levels(glmm_dat_depth$year)[[1]], levels = levels(glmm_dat_depth$year)),
        month = factor(levels(glmm_dat_depth$month)[[1]], levels = levels(glmm_dat_depth$month)),
        area = factor(levels(glmm_dat_depth$area)[[1]], levels = levels(glmm_dat_depth$area)),
        vessel = factor(levels(glmm_dat_depth$vessel)[[1]], levels = levels(glmm_dat_depth$vessel)),
        effort_glmm = 1,
        depth_glmm_sc = 0
      ),
      type = "response",
      re.form = NA
    )

    depth_plot_dat <- depth_plot_dat |>
      dplyr::mutate(relative_index = .data$predicted / as.numeric(ref_pred[[1]]))

    ggplot2::ggsave(
      filename = depth_effect_fig_path_i,
      plot = ggplot2::ggplot(depth_plot_dat, ggplot2::aes(x = .data$depth_glmm_sc, y = .data$relative_index)) +
        ggplot2::geom_line() +
        ggplot2::labs(
          x = "Standardized depth",
          y = "Relative index"
        ),
      width = 10,
      height = 6,
      dpi = 150
    )

    depth_effect_fig_path_saved <- depth_effect_fig_path_i
  }

  vc_cond <- VarCorr(best_model_obj)$cond
  area_stats <- extract_re_sd(vc_cond, "area")
  vessel_stats <- extract_re_sd(vc_cond, "vessel")

  variance_summary_tbl <- dplyr::bind_rows(
    variance_summary_tbl,
    tibble::tibble(
      size = sv,
      n_obs = nrow(glmm_dat_depth),
      n_zero = sum(glmm_dat_depth[[sv]] == 0, na.rm = TRUE),
      zero_ratio = mean(glmm_dat_depth[[sv]] == 0, na.rm = TRUE),
      aic = tryCatch(as.numeric(AIC(best_model_obj)[1]), error = function(e) NA_real_),
      logLik = tryCatch(as.numeric(logLik(best_model_obj)), error = function(e) NA_real_),
      disp_parameter = tryCatch(as.numeric(sigma(best_model_obj)), error = function(e) NA_real_),
      area_variance = area_stats[["variance"]],
      area_sd = area_stats[["sd"]],
      vessel_variance = vessel_stats[["variance"]],
      vessel_sd = vessel_stats[["sd"]],
      best_model_name = best_model_name_i,
      depth_degree = best_depth_degree_i,
      depth_linear_estimate = pick_first_or_na(depth_effect_tbl$estimate[depth_effect_tbl$term == "depth_glmm_sc"]),
      depth_quadratic_estimate = pick_first_or_na(depth_effect_tbl$estimate[depth_effect_tbl$term == "I(depth_glmm_sc^2)"]),
      depth_glmm_mean = depth_glmm_mean,
      depth_glmm_sd = depth_glmm_sd,
      converged = tryCatch(isTRUE(best_model_obj$fit$convergence == 0), error = function(e) NA),
      fit_convergence_code = tryCatch(as.integer(best_model_obj$fit$convergence), error = function(e) NA_integer_),
      pdHess = tryCatch(isTRUE(best_model_obj$sdr$pdHess), error = function(e) NA),
      warning_message = successful_size_tbl$warning_message[[1]]
    )
  )

  run_status_tbl <- run_status_tbl |>
    dplyr::mutate(
      status = dplyr::if_else(.data$size == sv, "completed", .data$status),
      warning_message = dplyr::if_else(.data$size == sv, successful_size_tbl$warning_message[[1]], .data$warning_message),
      model_path = dplyr::if_else(.data$size == sv, model_path_i, .data$model_path),
      emm_year_path = dplyr::if_else(.data$size == sv, emm_year_path_i, .data$emm_year_path),
      index_fig_path = dplyr::if_else(.data$size == sv, index_fig_path_i, .data$index_fig_path),
      best_model_name = dplyr::if_else(.data$size == sv, best_model_name_i, .data$best_model_name),
      best_depth_degree = dplyr::if_else(.data$size == sv, best_depth_degree_i, .data$best_depth_degree),
      comparison_path = dplyr::if_else(.data$size == sv, comparison_path_i, .data$comparison_path),
      depth_effect_path = dplyr::if_else(.data$size == sv, depth_effect_path_i, .data$depth_effect_path),
      depth_effect_fig_path = dplyr::if_else(.data$size == sv, depth_effect_fig_path_saved, .data$depth_effect_fig_path)
    )
}

readr::write_csv(variance_summary_tbl, variance_summary_path)
readr::write_csv(run_status_tbl, run_status_path)
