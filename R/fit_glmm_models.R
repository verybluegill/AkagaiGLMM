# =========================================
# fit_glmm_models.R
# 本流 Step 3
# 入力: data_processed/akagai_glmm_input.csv
# 出力: 主解析 size 別 GLMM、sub total/depth、感度分析のモデル・表・図
# =========================================

source(file.path("R", "00_load_packages.R"))

optional_pkgs <- load_project_packages(
  required_pkgs = c("dplyr", "ggplot2", "glmmTMB", "purrr", "readr", "tibble", "tidyr"),
  optional_pkgs = c("emmeans", "DHARMa")
)

ensure_project_dirs()

input_path <- file.path("data_processed", "akagai_glmm_input.csv")
model_summary_path <- file.path("output", "tables", "glmm_model_summary.csv")
model_manifest_path <- file.path("output", "tables", "glmm_model_manifest.csv")
raw_cpue_manifest_path <- file.path("output", "tables", "raw_cpue_manifest.csv")
variance_summary_path <- file.path("output", "tables", "glmm_by_size_variance_summary.csv")
run_status_path <- file.path("output", "tables", "glmm_by_size_run_status.csv")

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

to_year_numeric <- function(x) {
  suppressWarnings(as.integer(as.character(x)))
}

compute_year_index_table <- function(model_obj, data_obj, optional_pkgs, depth_glmm_sc_value = NULL) {
  model_frame_names <- names(stats::model.frame(model_obj))

  if (isTRUE(optional_pkgs[["emmeans"]])) {
    at_list <- list(effort_glmm = 1)

    if ("depth_glmm_sc" %in% model_frame_names) {
      at_list$depth_glmm_sc <- depth_glmm_sc_value %||% 0
    }

    if ("area" %in% model_frame_names) {
      at_list$area <- get_observed_levels(data_obj$area)[[1]]
    }

    if ("vessel" %in% model_frame_names) {
      at_list$vessel <- get_observed_levels(data_obj$vessel)[[1]]
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

  pred_grid <- tidyr::expand_grid(
    year = factor(year_levels_used, levels = levels(data_obj$year)),
    month = factor(month_levels_used, levels = levels(data_obj$month)),
    effort_glmm = 1
  )

  if ("depth_glmm_sc" %in% model_frame_names) {
    pred_grid$depth_glmm_sc <- depth_glmm_sc_value %||% 0
  }

  if ("area" %in% model_frame_names) {
    area_levels_used <- get_observed_levels(data_obj$area)
    pred_grid$area <- factor(area_levels_used[[1]], levels = levels(data_obj$area))
  }

  if ("vessel" %in% model_frame_names) {
    vessel_levels_used <- get_observed_levels(data_obj$vessel)
    pred_grid$vessel <- factor(vessel_levels_used[[1]], levels = levels(data_obj$vessel))
  }

  pred_out <- predict(model_obj, newdata = pred_grid, type = "response", re.form = NA, se.fit = TRUE)
  fit_vals <- pred_out$fit %||% pred_out
  se_vals <- pred_out$se.fit %||% rep(NA_real_, length(fit_vals))

  pred_grid |>
    dplyr::mutate(
      predicted = fit_vals,
      lower.CL = predicted - 1.96 * se_vals,
      upper.CL = predicted + 1.96 * se_vals
    ) |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(
      response = mean(.data$predicted),
      lower.CL = mean(.data$lower.CL, na.rm = TRUE),
      upper.CL = mean(.data$upper.CL, na.rm = TRUE),
      .groups = "drop"
    )
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

plot_year_index <- function(tbl, output_path, title_text, show_ci = FALSE) {
  year_index_col <- get_year_index_col(tbl)
  plot_tbl <- tbl |>
    dplyr::mutate(year = to_year_numeric(.data$year))

  p <- ggplot2::ggplot(plot_tbl, ggplot2::aes(x = .data$year, y = .data[[year_index_col]], group = 1)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(
      title = title_text,
      x = "Year",
      y = "Index"
    ) +
    ggplot2::scale_x_continuous(breaks = sort(unique(plot_tbl$year))) +
    ggplot2::theme_bw()

  if (isTRUE(show_ci) && all(c("lower.CL", "upper.CL") %in% names(plot_tbl))) {
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

  cat("saved:", output_path, "\n")
}

make_raw_cpue_table <- function(data_obj, response_col) {
  data_obj |>
    dplyr::filter(!is.na(.data$year), !is.na(.data$effort_glmm), is.finite(.data$effort_glmm), .data$effort_glmm > 0, !is.na(.data[[response_col]])) |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(
      total_count = sum(.data[[response_col]], na.rm = TRUE),
      total_effort = sum(.data$effort_glmm, na.rm = TRUE),
      cpue = .data$total_count / .data$total_effort,
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$year)
}

plot_raw_cpue <- function(tbl, output_path, title_text) {
  plot_tbl <- tbl |>
    dplyr::mutate(year = to_year_numeric(.data$year))

  p <- ggplot2::ggplot(plot_tbl, ggplot2::aes(x = .data$year, y = .data$cpue, group = 1)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(
      title = title_text,
      x = "Year",
      y = "CPUE"
    ) +
    ggplot2::scale_x_continuous(breaks = sort(unique(plot_tbl$year))) +
    ggplot2::theme_bw()

  ggplot2::ggsave(
    filename = output_path,
    plot = p,
    width = 10,
    height = 6,
    dpi = 150
  )

  cat("saved:", output_path, "\n")
}

build_standardized_index_table <- function(tbl, response_name) {
  year_index_col <- get_year_index_col(tbl)
  estimate_vals <- suppressWarnings(as.numeric(tbl[[year_index_col]]))
  lower_vals <- if ("lower.CL" %in% names(tbl)) suppressWarnings(as.numeric(tbl[["lower.CL"]])) else rep(NA_real_, nrow(tbl))
  upper_vals <- if ("upper.CL" %in% names(tbl)) suppressWarnings(as.numeric(tbl[["upper.CL"]])) else rep(NA_real_, nrow(tbl))
  estimate_mean <- mean(estimate_vals, na.rm = TRUE)

  if (!is.finite(estimate_mean) || estimate_mean <= 0) {
    stop("Failed to standardize annual index because the reference mean is not positive.")
  }

  tibble::tibble(
    response = response_name,
    year = to_year_numeric(tbl$year),
    estimate = estimate_vals / estimate_mean,
    lower.CL = lower_vals / estimate_mean,
    upper.CL = upper_vals / estimate_mean
  )
}

build_raw_vs_index_compare_tbl <- function(raw_tbl, index_tbl) {
  raw_plot_tbl <- raw_tbl |>
    dplyr::mutate(
      panel = "Raw annual CPUE",
      value = .data$cpue,
      lower.CL = NA_real_,
      upper.CL = NA_real_
    ) |>
    dplyr::select("response", "year", "panel", "value", "lower.CL", "upper.CL")

  index_plot_tbl <- index_tbl |>
    dplyr::mutate(
      panel = "Standardized annual index",
      value = .data$estimate
    ) |>
    dplyr::select("response", "year", "panel", "value", "lower.CL", "upper.CL")

  dplyr::bind_rows(raw_plot_tbl, index_plot_tbl) |>
    dplyr::mutate(
      response = factor(.data$response, levels = c("chu", "dai", "toku", "tokudai")),
      panel = factor(.data$panel, levels = c("Raw annual CPUE", "Standardized annual index"))
    ) |>
    dplyr::arrange(.data$panel, .data$response, .data$year)
}

build_raw_relative_cpue_table <- function(raw_tbl) {
  raw_tbl |>
    dplyr::group_by(.data$response) |>
    dplyr::mutate(cpue_mean = mean(.data$cpue, na.rm = TRUE)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      raw_relative = dplyr::if_else(is.finite(.data$cpue_mean) & .data$cpue_mean > 0, .data$cpue / .data$cpue_mean, NA_real_)
    ) |>
    dplyr::select("response", "year", "raw_relative", "cpue_mean")
}

build_relative_overlay_tbl <- function(raw_relative_tbl, index_tbl) {
  if (any(!is.finite(raw_relative_tbl$cpue_mean)) || any(raw_relative_tbl$cpue_mean <= 0)) {
    stop("Failed to scale raw annual CPUE because the response-specific mean is not positive.")
  }

  raw_plot_tbl <- raw_relative_tbl |>
    dplyr::transmute(
      response = .data$response,
      year = .data$year,
      series = "Raw relative CPUE",
      value = .data$raw_relative,
      lower.CL = NA_real_,
      upper.CL = NA_real_
    )

  index_plot_tbl <- index_tbl |>
    dplyr::transmute(
      response = .data$response,
      year = .data$year,
      series = "Standardized index",
      value = .data$estimate,
      lower.CL = .data$lower.CL,
      upper.CL = .data$upper.CL
    )

  dplyr::bind_rows(raw_plot_tbl, index_plot_tbl) |>
    dplyr::mutate(
      response = factor(.data$response, levels = c("chu", "dai", "toku", "tokudai")),
      series = factor(.data$series, levels = c("Raw relative CPUE", "Standardized index"))
    ) |>
    dplyr::arrange(.data$response, .data$series, .data$year)
}

plot_raw_vs_index_compare <- function(compare_tbl, output_path, title_text) {
  p <- ggplot2::ggplot(compare_tbl, ggplot2::aes(x = .data$year, y = .data$value, group = 1)) +
    ggplot2::geom_hline(
      data = dplyr::filter(compare_tbl, .data$panel == "Standardized annual index"),
      ggplot2::aes(yintercept = 1),
      linetype = "dashed",
      color = "grey40"
    ) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 1.8) +
    ggplot2::geom_errorbar(
      data = dplyr::filter(compare_tbl, .data$panel == "Standardized annual index" & is.finite(.data$lower.CL) & is.finite(.data$upper.CL)),
      ggplot2::aes(ymin = .data$lower.CL, ymax = .data$upper.CL),
      width = 0.15
    ) +
    ggplot2::facet_grid(
      ggplot2::vars(panel),
      ggplot2::vars(response),
      scales = "free_y",
      labeller = ggplot2::labeller(
        panel = c("Raw annual CPUE" = "Raw annual CPUE", "Standardized annual index" = "Standardized index")
      )
    ) +
    ggplot2::labs(
      title = title_text,
      x = "Year",
      y = NULL
    ) +
    ggplot2::scale_x_continuous(breaks = sort(unique(compare_tbl$year))) +
    ggplot2::scale_y_continuous(name = NULL) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "white"),
      panel.grid.minor = ggplot2::element_blank()
    )

  ggplot2::ggsave(
    filename = output_path,
    plot = p,
    width = 16,
    height = 8,
    dpi = 150
  )

  cat("saved:", output_path, "\n")
}

plot_relative_overlay_compare <- function(compare_tbl, output_path, title_text) {
  p <- ggplot2::ggplot(compare_tbl, ggplot2::aes(x = .data$year, y = .data$value, color = .data$series, linetype = .data$series, shape = .data$series, group = .data$series)) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_errorbar(
      data = dplyr::filter(compare_tbl, .data$series == "Standardized index" & is.finite(.data$lower.CL) & is.finite(.data$upper.CL)),
      ggplot2::aes(ymin = .data$lower.CL, ymax = .data$upper.CL),
      width = 0.15
    ) +
    ggplot2::facet_grid(. ~ response, scales = "fixed") +
    ggplot2::labs(
      title = title_text,
      x = "Year",
      y = "Relative value",
      color = NULL,
      linetype = NULL,
      shape = NULL
    ) +
    ggplot2::scale_x_continuous(breaks = sort(unique(compare_tbl$year))) +
    ggplot2::theme_bw() +
    ggplot2::theme(
      strip.background = ggplot2::element_rect(fill = "white"),
      panel.grid.minor = ggplot2::element_blank()
    )

  ggplot2::ggsave(
    filename = output_path,
    plot = p,
    width = 16,
    height = 4.5,
    dpi = 150
  )

  cat("saved:", output_path, "\n")
}

summarise_rows_by_year <- function(glmm_input, response_col, depth_required = FALSE) {
  glmm_input |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(
      n_input = dplyr::n(),
      n_used = sum(
        .data$flag_use_for_main_glmm &
          !is.na(.data[[response_col]]) &
          (!depth_required | !is.na(.data$depth_glmm)),
        na.rm = TRUE
      ),
      n_dropped = .data$n_input - .data$n_used,
      .groups = "drop"
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

cat("count_total_na_n=", sum(is.na(glmm_input$count_total)), "\n", sep = "")

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

glmm_dat_depth <- glmm_dat_main |>
  dplyr::filter(!is.na(.data$depth_glmm), is.finite(.data$depth_glmm))

depth_glmm_mean <- if (nrow(glmm_dat_depth) > 0) mean(glmm_dat_depth$depth_glmm, na.rm = TRUE) else NA_real_
depth_glmm_sd <- if (nrow(glmm_dat_depth) > 1) stats::sd(glmm_dat_depth$depth_glmm, na.rm = TRUE) else NA_real_

if (nrow(glmm_dat_depth) > 0 && is.finite(depth_glmm_sd) && depth_glmm_sd > 0) {
  glmm_dat_depth <- glmm_dat_depth |>
    dplyr::mutate(depth_glmm_sc = (.data$depth_glmm - depth_glmm_mean) / depth_glmm_sd)
}

analysis_specs <- list(
  list(response = "chu", result_group = "main", depth_flag = "depth0", area_vessel_flag = "av0", dataset = "main", model_formula = chu ~ year + month + offset(log(effort_glmm)), include_raw_cpue = TRUE, title = "chu main analysis depth0 no Area/Vessel"),
  list(response = "dai", result_group = "main", depth_flag = "depth0", area_vessel_flag = "av0", dataset = "main", model_formula = dai ~ year + month + offset(log(effort_glmm)), include_raw_cpue = TRUE, title = "dai main analysis depth0 no Area/Vessel"),
  list(response = "toku", result_group = "main", depth_flag = "depth0", area_vessel_flag = "av0", dataset = "main", model_formula = toku ~ year + month + offset(log(effort_glmm)), include_raw_cpue = TRUE, title = "toku main analysis depth0 no Area/Vessel"),
  list(response = "tokudai", result_group = "main", depth_flag = "depth0", area_vessel_flag = "av0", dataset = "main", model_formula = tokudai ~ year + month + offset(log(effort_glmm)), include_raw_cpue = TRUE, title = "tokudai main analysis depth0 no Area/Vessel"),
  list(response = "count_total", result_group = "sub", depth_flag = "depth0", area_vessel_flag = "av0", dataset = "main", model_formula = count_total ~ year + month + offset(log(effort_glmm)), include_raw_cpue = TRUE, title = "total sub analysis depth0 no Area/Vessel"),
  list(response = "chu", result_group = "sub", depth_flag = "depth1", area_vessel_flag = "av0", dataset = "depth", model_formula = chu ~ year + month + depth_glmm_sc + offset(log(effort_glmm)), include_raw_cpue = FALSE, title = "chu sub analysis depth1 no Area/Vessel"),
  list(response = "dai", result_group = "sub", depth_flag = "depth1", area_vessel_flag = "av0", dataset = "depth", model_formula = dai ~ year + month + depth_glmm_sc + offset(log(effort_glmm)), include_raw_cpue = FALSE, title = "dai sub analysis depth1 no Area/Vessel"),
  list(response = "toku", result_group = "sub", depth_flag = "depth1", area_vessel_flag = "av0", dataset = "depth", model_formula = toku ~ year + month + depth_glmm_sc + offset(log(effort_glmm)), include_raw_cpue = FALSE, title = "toku sub analysis depth1 no Area/Vessel"),
  list(response = "tokudai", result_group = "sub", depth_flag = "depth1", area_vessel_flag = "av0", dataset = "depth", model_formula = tokudai ~ year + month + depth_glmm_sc + offset(log(effort_glmm)), include_raw_cpue = FALSE, title = "tokudai sub analysis depth1 no Area/Vessel"),
  list(response = "count_total", result_group = "sub", depth_flag = "depth1", area_vessel_flag = "av0", dataset = "depth", model_formula = count_total ~ year + month + depth_glmm_sc + offset(log(effort_glmm)), include_raw_cpue = FALSE, title = "total sub analysis depth1 no Area/Vessel"),
  list(response = "chu", result_group = "sensitivity", depth_flag = "depth0", area_vessel_flag = "av1", dataset = "main", model_formula = chu ~ year + month + offset(log(effort_glmm)) + (1 | area) + (1 | vessel), include_raw_cpue = FALSE, title = "chu sensitivity depth0 with Area/Vessel"),
  list(response = "dai", result_group = "sensitivity", depth_flag = "depth0", area_vessel_flag = "av1", dataset = "main", model_formula = dai ~ year + month + offset(log(effort_glmm)) + (1 | area) + (1 | vessel), include_raw_cpue = FALSE, title = "dai sensitivity depth0 with Area/Vessel"),
  list(response = "toku", result_group = "sensitivity", depth_flag = "depth0", area_vessel_flag = "av1", dataset = "main", model_formula = toku ~ year + month + offset(log(effort_glmm)) + (1 | area) + (1 | vessel), include_raw_cpue = FALSE, title = "toku sensitivity depth0 with Area/Vessel"),
  list(response = "tokudai", result_group = "sensitivity", depth_flag = "depth0", area_vessel_flag = "av1", dataset = "main", model_formula = tokudai ~ year + month + offset(log(effort_glmm)) + (1 | area) + (1 | vessel), include_raw_cpue = FALSE, title = "tokudai sensitivity depth0 with Area/Vessel"),
  list(response = "count_total", result_group = "sensitivity", depth_flag = "depth0", area_vessel_flag = "av1", dataset = "main", model_formula = count_total ~ year + month + offset(log(effort_glmm)) + (1 | area) + (1 | vessel), include_raw_cpue = FALSE, title = "total sensitivity depth0 with Area/Vessel"),
  list(response = "chu", result_group = "sensitivity", depth_flag = "depth1", area_vessel_flag = "av1", dataset = "depth", model_formula = chu ~ year + month + depth_glmm_sc + offset(log(effort_glmm)) + (1 | area) + (1 | vessel), include_raw_cpue = FALSE, title = "chu sensitivity depth1 with Area/Vessel"),
  list(response = "dai", result_group = "sensitivity", depth_flag = "depth1", area_vessel_flag = "av1", dataset = "depth", model_formula = dai ~ year + month + depth_glmm_sc + offset(log(effort_glmm)) + (1 | area) + (1 | vessel), include_raw_cpue = FALSE, title = "dai sensitivity depth1 with Area/Vessel"),
  list(response = "toku", result_group = "sensitivity", depth_flag = "depth1", area_vessel_flag = "av1", dataset = "depth", model_formula = toku ~ year + month + depth_glmm_sc + offset(log(effort_glmm)) + (1 | area) + (1 | vessel), include_raw_cpue = FALSE, title = "toku sensitivity depth1 with Area/Vessel"),
  list(response = "tokudai", result_group = "sensitivity", depth_flag = "depth1", area_vessel_flag = "av1", dataset = "depth", model_formula = tokudai ~ year + month + depth_glmm_sc + offset(log(effort_glmm)) + (1 | area) + (1 | vessel), include_raw_cpue = FALSE, title = "tokudai sensitivity depth1 with Area/Vessel"),
  list(response = "count_total", result_group = "sensitivity", depth_flag = "depth1", area_vessel_flag = "av1", dataset = "depth", model_formula = count_total ~ year + month + depth_glmm_sc + offset(log(effort_glmm)) + (1 | area) + (1 | vessel), include_raw_cpue = FALSE, title = "total sensitivity depth1 with Area/Vessel")
)

model_summary_tbl <- tibble::tibble()
model_manifest_tbl <- tibble::tibble()
raw_cpue_manifest_tbl <- tibble::tibble()
main_raw_cpue_compare_tbl <- tibble::tibble()
main_index_compare_tbl <- tibble::tibble()

for (spec_i in analysis_specs) {
  dataset_i <- if (identical(spec_i$dataset, "depth")) glmm_dat_depth else glmm_dat_main

  if (identical(spec_i$dataset, "depth") && (!is.finite(depth_glmm_sd) || depth_glmm_sd <= 0 || nrow(glmm_dat_depth) == 0)) {
    next
  }

  data_i <- dataset_i |>
    dplyr::filter(!is.na(.data[[spec_i$response]]))

  if (nrow(data_i) == 0) {
    next
  }

  response_file_tag_i <- if (identical(spec_i$response, "count_total")) "total" else spec_i$response
  prefix_i <- paste(response_file_tag_i, spec_i$result_group, spec_i$depth_flag, spec_i$area_vessel_flag, sep = "_")
  model_path_i <- file.path("output", "models", paste0("fit_nb_", prefix_i, ".rds"))
  emm_path_i <- file.path("output", "tables", paste0("emm_year_", prefix_i, ".csv"))
  row_summary_path_i <- file.path("output", "tables", paste0("check_", prefix_i, "_row_summary_by_year.csv"))
  index_fig_path_i <- file.path("output", "figures", paste0("index_", prefix_i, ".png"))
  index_ci_fig_path_i <- file.path("output", "figures", paste0("index_", prefix_i, "_ci.png"))
  raw_cpue_table_path_i <- file.path("output", "tables", paste0("raw_annual_cpue_", prefix_i, ".csv"))
  raw_cpue_fig_path_i <- file.path("output", "figures", paste0("index_", prefix_i, "_raw_cpue.png"))

  fit_out_i <- safe_fit_glmmTMB(
    model_name = paste0("fit_nb_", prefix_i),
    formula_obj = spec_i$model_formula,
    data_obj = data_i
  )

  if (is.null(fit_out_i$fit)) {
    model_manifest_tbl <- dplyr::bind_rows(
      model_manifest_tbl,
      tibble::tibble(
        response = spec_i$response,
        result_group = spec_i$result_group,
        depth_flag = spec_i$depth_flag,
        area_vessel_flag = spec_i$area_vessel_flag,
        status = "fit_failed",
        model_path = model_path_i,
        emm_year_path = emm_path_i,
        row_summary_path = row_summary_path_i,
        index_fig_path = index_fig_path_i,
        index_ci_fig_path = index_ci_fig_path_i,
        raw_cpue_table_path = dplyr::if_else(spec_i$include_raw_cpue, raw_cpue_table_path_i, NA_character_),
        raw_cpue_fig_path = dplyr::if_else(spec_i$include_raw_cpue, raw_cpue_fig_path_i, NA_character_),
        warning_message = fit_out_i$warning_message
      )
    )
    next
  }

  saveRDS(fit_out_i$fit, model_path_i)

  if (isTRUE(optional_pkgs[["DHARMa"]])) {
    print(try(DHARMa::testDispersion(DHARMa::simulateResiduals(fit_out_i$fit, plot = FALSE)), silent = TRUE))
  }

  emm_year_tbl_i <- compute_year_index_table(
    model_obj = fit_out_i$fit,
    data_obj = data_i,
    optional_pkgs = optional_pkgs,
    depth_glmm_sc_value = if (identical(spec_i$depth_flag, "depth1")) 0 else NULL
  )

  row_summary_tbl_i <- summarise_rows_by_year(
    glmm_input = glmm_input,
    response_col = spec_i$response,
    depth_required = identical(spec_i$depth_flag, "depth1")
  )

  readr::write_csv(emm_year_tbl_i, emm_path_i)
  readr::write_csv(row_summary_tbl_i, row_summary_path_i)
  plot_year_index(emm_year_tbl_i, index_fig_path_i, spec_i$title, show_ci = FALSE)
  plot_year_index(emm_year_tbl_i, index_ci_fig_path_i, spec_i$title, show_ci = TRUE)

  raw_cpue_table_i <- NULL

  if (isTRUE(spec_i$include_raw_cpue)) {
    raw_cpue_table_i <- make_raw_cpue_table(glmm_dat_main, spec_i$response)
    readr::write_csv(raw_cpue_table_i, raw_cpue_table_path_i)
    plot_raw_cpue(raw_cpue_table_i, raw_cpue_fig_path_i, paste(response_file_tag_i, "raw annual CPUE reference"))

    raw_cpue_manifest_tbl <- dplyr::bind_rows(
      raw_cpue_manifest_tbl,
      tibble::tibble(
        response = spec_i$response,
        result_group = spec_i$result_group,
        depth_flag = spec_i$depth_flag,
        raw_cpue_table_path = raw_cpue_table_path_i,
        raw_cpue_fig_path = raw_cpue_fig_path_i
      )
    )

    if (identical(spec_i$result_group, "main") && identical(spec_i$depth_flag, "depth0") && identical(spec_i$area_vessel_flag, "av0") && spec_i$response %in% c("chu", "dai", "toku", "tokudai")) {
      main_raw_cpue_compare_tbl <- dplyr::bind_rows(
        main_raw_cpue_compare_tbl,
        raw_cpue_table_i |>
          dplyr::mutate(
            response = spec_i$response,
            year = to_year_numeric(.data$year)
          ) |>
          dplyr::select("response", "year", "cpue")
      )

      main_index_compare_tbl <- dplyr::bind_rows(
        main_index_compare_tbl,
        build_standardized_index_table(emm_year_tbl_i, spec_i$response)
      )
    }
  }

  vc_cond_i <- VarCorr(fit_out_i$fit)$cond
  area_stats_i <- extract_re_sd(vc_cond_i, "area")
  vessel_stats_i <- extract_re_sd(vc_cond_i, "vessel")

  model_summary_tbl <- dplyr::bind_rows(
    model_summary_tbl,
    tibble::tibble(
      response = spec_i$response,
      result_group = spec_i$result_group,
      depth_flag = spec_i$depth_flag,
      area_vessel_flag = spec_i$area_vessel_flag,
      n_obs = nrow(data_i),
      n_zero = sum(data_i[[spec_i$response]] == 0, na.rm = TRUE),
      zero_ratio = mean(data_i[[spec_i$response]] == 0, na.rm = TRUE),
      aic = tryCatch(as.numeric(AIC(fit_out_i$fit)[1]), error = function(e) NA_real_),
      logLik = tryCatch(as.numeric(logLik(fit_out_i$fit)), error = function(e) NA_real_),
      disp_parameter = tryCatch(as.numeric(sigma(fit_out_i$fit)), error = function(e) NA_real_),
      area_variance = area_stats_i[["variance"]],
      area_sd = area_stats_i[["sd"]],
      vessel_variance = vessel_stats_i[["variance"]],
      vessel_sd = vessel_stats_i[["sd"]],
      converged = tryCatch(isTRUE(fit_out_i$fit$fit$convergence == 0), error = function(e) NA),
      fit_convergence_code = tryCatch(as.integer(fit_out_i$fit$fit$convergence), error = function(e) NA_integer_),
      pdHess = tryCatch(isTRUE(fit_out_i$fit$sdr$pdHess), error = function(e) NA),
      warning_message = fit_out_i$warning_message,
      model_path = model_path_i,
      emm_year_path = emm_path_i,
      row_summary_path = row_summary_path_i,
      index_fig_path = index_fig_path_i,
      index_ci_fig_path = index_ci_fig_path_i
    )
  )

  model_manifest_tbl <- dplyr::bind_rows(
    model_manifest_tbl,
    tibble::tibble(
      response = spec_i$response,
      result_group = spec_i$result_group,
      depth_flag = spec_i$depth_flag,
      area_vessel_flag = spec_i$area_vessel_flag,
      status = "completed",
      model_path = model_path_i,
      emm_year_path = emm_path_i,
      row_summary_path = row_summary_path_i,
      index_fig_path = index_fig_path_i,
      index_ci_fig_path = index_ci_fig_path_i,
      raw_cpue_table_path = if (isTRUE(spec_i$include_raw_cpue)) raw_cpue_table_path_i else NA_character_,
      raw_cpue_fig_path = if (isTRUE(spec_i$include_raw_cpue)) raw_cpue_fig_path_i else NA_character_,
      warning_message = fit_out_i$warning_message
    )
  )

  cat("analysis =", spec_i$result_group, "| response =", spec_i$response, "|", spec_i$depth_flag, "|", spec_i$area_vessel_flag, "\n")
  cat("saved model =", model_path_i, "\n")
  cat("saved year index =", index_fig_path_i, "\n")
  cat("saved year index ci =", index_ci_fig_path_i, "\n")
  if (isTRUE(spec_i$include_raw_cpue)) {
    cat("saved raw annual CPUE =", raw_cpue_fig_path_i, "\n")
  }
}

readr::write_csv(model_summary_tbl, model_summary_path)
readr::write_csv(model_manifest_tbl, model_manifest_path)
readr::write_csv(raw_cpue_manifest_tbl, raw_cpue_manifest_path)
readr::write_csv(model_summary_tbl, variance_summary_path)
readr::write_csv(model_manifest_tbl, run_status_path)

if (nrow(main_raw_cpue_compare_tbl) > 0 && nrow(main_index_compare_tbl) > 0) {
  compare_fig_tbl <- build_raw_vs_index_compare_tbl(main_raw_cpue_compare_tbl, main_index_compare_tbl)
  compare_fig_path <- file.path("output", "figures", "raw_vs_index_main_depth0_av0_panel.png")
  plot_raw_vs_index_compare(compare_fig_tbl, compare_fig_path, "Raw annual CPUE vs standardized annual index")
  cat("comparison figure = raw annual CPUE vs standardized annual index (main/depth0/av0)\n")

  relative_overlay_tbl <- build_relative_overlay_tbl(
    build_raw_relative_cpue_table(main_raw_cpue_compare_tbl),
    main_index_compare_tbl
  )
  relative_overlay_fig_path <- file.path("output", "figures", "raw_relative_vs_index_main_depth0_av0_overlay.png")
  plot_relative_overlay_compare(relative_overlay_tbl, relative_overlay_fig_path, "Relative raw CPUE vs standardized annual index")
  cat("comparison figure = relative raw CPUE overlaid with standardized annual index (main/depth0/av0)\n")
}

cat("main results = size-specific depth0 av0 year indices: chu, dai, toku, tokudai\n")
cat("sub results = total depth0/depth1 and size-specific depth1 year indices\n")
cat("sensitivity results = Area/Vessel models saved separately with av1 in file names\n")
