# =========================================
# compare_glmm_aic.R
# same-subset 上で size 別 GLMM の AIC 比較と
# Raw relative CPUE vs standardized annual index の比較図を出力
# =========================================

source(file.path("R", "00_load_packages.R"))

optional_pkgs <- load_project_packages(
  required_pkgs = c("dplyr", "ggplot2", "glmmTMB", "readr", "tibble", "tidyr"),
  optional_pkgs = c("emmeans", "DHARMa")
)

ensure_project_dirs()

input_path <- file.path("data_processed", "akagai_glmm_input.csv")

`%||%` <- function(x, y) {
  if (length(x) == 0 || is.null(x)) {
    return(y)
  }

  x
}

to_year_numeric <- function(x) {
  x_chr <- as.character(x)
  out <- suppressWarnings(as.integer(x_chr))
  out
}

get_observed_levels <- function(x) {
  sort(unique(as.character(x[!is.na(x)])))
}

get_year_index_col <- function(tbl) {
  matched <- intersect(c("response", "rate", "prob", "emmean"), names(tbl))

  if (length(matched) == 0) {
    stop("Failed to identify the year index column.")
  }

  matched[[1]]
}

combine_messages <- function(...) {
  vals <- unlist(list(...), use.names = FALSE)
  vals <- vals[!is.na(vals) & nzchar(vals)]

  if (length(vals) == 0) {
    return("")
  }

  paste(vals, collapse = " | ")
}

safe_fit_glmmTMB <- function(model_name, formula_obj, data_obj) {
  warning_messages <- character(0)
  error_message <- NA_character_

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
      error_message <<- conditionMessage(e)
      NULL
    }
  )

  list(
    model_name = model_name,
    fit = fit_obj,
    warning_message = paste(unique(warning_messages), collapse = " | "),
    error_message = error_message
  )
}

compute_year_index_table <- function(model_obj, data_obj, optional_pkgs, depth_glmm_sc_value = NULL, depth_cat_value = NULL) {
  model_frame_names <- names(stats::model.frame(model_obj))
  use_emmeans <- isTRUE(optional_pkgs[["emmeans"]]) &&
    !any(c("area", "vessel") %in% model_frame_names)

  if (use_emmeans) {
    at_list <- list(effort_glmm = 1)

    if ("depth_glmm_sc" %in% model_frame_names) {
      at_list$depth_glmm_sc <- depth_glmm_sc_value %||% 0
    }

    if ("depth_cat" %in% model_frame_names) {
      at_list$depth_cat <- depth_cat_value
    }

    emm_tbl <- as.data.frame(emmeans::emmeans(
      model_obj,
      specs = ~ year,
      at = at_list,
      type = "response"
    ))

    emm_tbl$year <- to_year_numeric(emm_tbl$year)
    return(emm_tbl)
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

  if ("depth_cat" %in% model_frame_names) {
    pred_grid$depth_cat <- factor(depth_cat_value, levels = levels(data_obj$depth_cat))
  }

  if ("area" %in% model_frame_names) {
    area_levels_used <- get_observed_levels(data_obj$area)
    pred_grid$area <- factor(area_levels_used[[1]], levels = levels(data_obj$area))
  }

  if ("vessel" %in% model_frame_names) {
    vessel_levels_used <- get_observed_levels(data_obj$vessel)
    pred_grid$vessel <- factor(vessel_levels_used[[1]], levels = levels(data_obj$vessel))
  }

  pred_out <- predict(
    model_obj,
    newdata = pred_grid,
    type = "response",
    re.form = NA,
    se.fit = TRUE
  )

  fit_vals <- pred_out$fit %||% pred_out
  se_vals <- pred_out$se.fit %||% rep(NA_real_, length(fit_vals))

  pred_grid |>
    dplyr::mutate(
      year = to_year_numeric(.data$year),
      predicted = as.numeric(fit_vals),
      lower.CL = .data$predicted - 1.96 * as.numeric(se_vals),
      upper.CL = .data$predicted + 1.96 * as.numeric(se_vals)
    ) |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(
      response = mean(.data$predicted),
      lower.CL = mean(.data$lower.CL, na.rm = TRUE),
      upper.CL = mean(.data$upper.CL, na.rm = TRUE),
      .groups = "drop"
    )
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

make_raw_cpue_table <- function(data_obj, response_col) {
  data_obj |>
    dplyr::filter(
      !is.na(.data$year),
      !is.na(.data$effort_glmm),
      is.finite(.data$effort_glmm),
      .data$effort_glmm > 0,
      !is.na(.data[[response_col]])
    ) |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(
      total_count = sum(.data[[response_col]], na.rm = TRUE),
      total_effort = sum(.data$effort_glmm, na.rm = TRUE),
      cpue = .data$total_count / .data$total_effort,
      .groups = "drop"
    ) |>
    dplyr::mutate(year = to_year_numeric(.data$year)) |>
    dplyr::arrange(.data$year)
}

build_raw_relative_cpue_table <- function(raw_tbl, response_name) {
  cpue_mean <- mean(raw_tbl$cpue, na.rm = TRUE)

  raw_tbl |>
    dplyr::mutate(
      response = response_name,
      cpue_mean = cpue_mean,
      raw_relative = dplyr::if_else(
        is.finite(.data$cpue_mean) & .data$cpue_mean > 0,
        .data$cpue / .data$cpue_mean,
        NA_real_
      )
    ) |>
    dplyr::select("response", "year", "total_count", "total_effort", "cpue", "cpue_mean", "raw_relative")
}

build_relative_overlay_tbl <- function(raw_relative_tbl, index_tbl, model_id, model_label, subset_type) {
  raw_plot_tbl <- raw_relative_tbl |>
    dplyr::transmute(
      response = .data$response,
      subset_type = subset_type,
      year = .data$year,
      model_id = model_id,
      model_label = model_label,
      series = "Raw relative CPUE",
      value = .data$raw_relative,
      lower.CL = NA_real_,
      upper.CL = NA_real_
    )

  index_plot_tbl <- index_tbl |>
    dplyr::transmute(
      response = .data$response,
      subset_type = subset_type,
      year = .data$year,
      model_id = model_id,
      model_label = model_label,
      series = "Standardized index",
      value = .data$estimate,
      lower.CL = .data$lower.CL,
      upper.CL = .data$upper.CL
    )

  dplyr::bind_rows(raw_plot_tbl, index_plot_tbl)
}

plot_relative_overlay_compare <- function(compare_tbl, output_path, title_text, model_levels) {
  compare_tbl$model_id <- factor(compare_tbl$model_id, levels = model_levels)

  p <- ggplot2::ggplot(
    compare_tbl,
    ggplot2::aes(
      x = .data$year,
      y = .data$value,
      color = .data$series,
      linetype = .data$series,
      shape = .data$series,
      group = .data$series
    )
  ) +
    ggplot2::geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 2) +
    ggplot2::geom_errorbar(
      data = dplyr::filter(compare_tbl, .data$series == "Standardized index" & is.finite(.data$lower.CL) & is.finite(.data$upper.CL)),
      ggplot2::aes(ymin = .data$lower.CL, ymax = .data$upper.CL),
      width = 0.15
    ) +
    ggplot2::facet_grid(. ~ model_id, scales = "fixed", drop = FALSE) +
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
    width = max(16, length(model_levels) * 2.8),
    height = 4.5,
    dpi = 150
  )
}

extract_model_metrics <- function(response_name, subset_type, model_id, model_label, fit_obj, warning_message, error_message, dispersion_out) {
  if (is.null(fit_obj)) {
    return(tibble::tibble(
      response = response_name,
      subset_type = subset_type,
      model_id = model_id,
      model_label = model_label,
      status = "fit_failed",
      nobs = NA_integer_,
      AIC = NA_real_,
      BIC = NA_real_,
      logLik = NA_real_,
      converged = NA,
      pdHess = NA,
      dispersion = dispersion_out$dispersion,
      dispersion_p = dispersion_out$dispersion_p,
      warning_message = combine_messages(warning_message, error_message)
    ))
  }

  tibble::tibble(
    response = response_name,
    subset_type = subset_type,
    model_id = model_id,
    model_label = model_label,
    status = "completed",
    nobs = tryCatch(as.integer(stats::nobs(fit_obj)), error = function(e) NA_integer_),
    AIC = tryCatch(as.numeric(AIC(fit_obj)[1]), error = function(e) NA_real_),
    BIC = tryCatch(as.numeric(BIC(fit_obj)[1]), error = function(e) NA_real_),
    logLik = tryCatch(as.numeric(logLik(fit_obj)), error = function(e) NA_real_),
    converged = tryCatch(isTRUE(fit_obj$fit$convergence == 0), error = function(e) NA),
    pdHess = tryCatch(isTRUE(fit_obj$sdr$pdHess), error = function(e) NA),
    dispersion = dispersion_out$dispersion,
    dispersion_p = dispersion_out$dispersion_p,
    warning_message = combine_messages(warning_message, error_message)
  )
}

run_dharma_dispersion <- function(fit_obj, optional_pkgs) {
  if (!isTRUE(optional_pkgs[["DHARMa"]]) || is.null(fit_obj)) {
    return(list(dispersion = NA_real_, dispersion_p = NA_real_))
  }

  out <- tryCatch(
    DHARMa::testDispersion(DHARMa::simulateResiduals(fit_obj, plot = FALSE)),
    error = function(e) NULL
  )

  if (is.null(out)) {
    return(list(dispersion = NA_real_, dispersion_p = NA_real_))
  }

  list(
    dispersion = suppressWarnings(as.numeric(out$statistic[[1]] %||% NA_real_)),
    dispersion_p = suppressWarnings(as.numeric(out$p.value %||% NA_real_))
  )
}

model_specs <- list(
  list(
    model_id = "M_year_only",
    model_label = "year + offset",
    formula_text = "response ~ year + offset(log(effort_glmm))",
    subset_type = "base_same_subset",
    uses_depth = FALSE,
    uses_depth_cat = FALSE,
    uses_area = FALSE,
    uses_vessel = FALSE
  ),
  list(
    model_id = "M0",
    model_label = "year + month + offset",
    formula_text = "response ~ year + month + offset(log(effort_glmm))",
    subset_type = "base_same_subset",
    uses_depth = FALSE,
    uses_depth_cat = FALSE,
    uses_area = FALSE,
    uses_vessel = FALSE
  ),
  list(
    model_id = "M_area",
    model_label = "year + month + offset + (1|area)",
    formula_text = "response ~ year + month + offset(log(effort_glmm)) + (1 | area)",
    subset_type = "base_same_subset",
    uses_depth = FALSE,
    uses_depth_cat = FALSE,
    uses_area = TRUE,
    uses_vessel = FALSE
  ),
  list(
    model_id = "M_vessel",
    model_label = "year + month + offset + (1|vessel)",
    formula_text = "response ~ year + month + offset(log(effort_glmm)) + (1 | vessel)",
    subset_type = "base_same_subset",
    uses_depth = FALSE,
    uses_depth_cat = FALSE,
    uses_area = FALSE,
    uses_vessel = TRUE
  ),
  list(
    model_id = "M_av",
    model_label = "year + month + offset + (1|area) + (1|vessel)",
    formula_text = "response ~ year + month + offset(log(effort_glmm)) + (1 | area) + (1 | vessel)",
    subset_type = "base_same_subset",
    uses_depth = FALSE,
    uses_depth_cat = FALSE,
    uses_area = TRUE,
    uses_vessel = TRUE
  ),
  list(
    model_id = "M_depth",
    model_label = "year + month + depth + offset",
    formula_text = "response ~ year + month + depth_glmm_sc + offset(log(effort_glmm))",
    subset_type = "depth_same_subset",
    uses_depth = TRUE,
    uses_depth_cat = FALSE,
    uses_area = FALSE,
    uses_vessel = FALSE
  ),
  list(
    model_id = "M_depth_av",
    model_label = "year + month + depth + offset + (1|area) + (1|vessel)",
    formula_text = "response ~ year + month + depth_glmm_sc + offset(log(effort_glmm)) + (1 | area) + (1 | vessel)",
    subset_type = "depth_same_subset",
    uses_depth = TRUE,
    uses_depth_cat = FALSE,
    uses_area = TRUE,
    uses_vessel = TRUE
  ),
  list(
    model_id = "M_depth_cat",
    model_label = "year + month + depth_cat + offset",
    formula_text = "response ~ year + month + depth_cat + offset(log(effort_glmm))",
    subset_type = "depth_same_subset",
    uses_depth = FALSE,
    uses_depth_cat = TRUE,
    uses_area = FALSE,
    uses_vessel = FALSE
  ),
  list(
    model_id = "M_depth_cat_area",
    model_label = "year + month + depth_cat + offset + (1|area)",
    formula_text = "response ~ year + month + depth_cat + offset(log(effort_glmm)) + (1 | area)",
    subset_type = "depth_same_subset",
    uses_depth = FALSE,
    uses_depth_cat = TRUE,
    uses_area = TRUE,
    uses_vessel = FALSE
  ),
  list(
    model_id = "M_depth_cat_vessel",
    model_label = "year + month + depth_cat + offset + (1|vessel)",
    formula_text = "response ~ year + month + depth_cat + offset(log(effort_glmm)) + (1 | vessel)",
    subset_type = "depth_same_subset",
    uses_depth = FALSE,
    uses_depth_cat = TRUE,
    uses_area = FALSE,
    uses_vessel = TRUE
  ),
  list(
    model_id = "M_depth_cat_av",
    model_label = "year + month + depth_cat + offset + (1|area) + (1|vessel)",
    formula_text = "response ~ year + month + depth_cat + offset(log(effort_glmm)) + (1 | area) + (1 | vessel)",
    subset_type = "depth_same_subset",
    uses_depth = FALSE,
    uses_depth_cat = TRUE,
    uses_area = TRUE,
    uses_vessel = TRUE
  )
)

response_levels <- c("chu", "dai", "toku", "tokudai")
base_model_specs <- Filter(function(x) identical(x$subset_type, "base_same_subset"), model_specs)
depth_model_specs <- Filter(function(x) identical(x$subset_type, "depth_same_subset"), model_specs)
base_model_levels <- vapply(base_model_specs, `[[`, character(1), "model_id")
depth_model_levels <- vapply(depth_model_specs, `[[`, character(1), "model_id")
all_base_results_tbl <- tibble::tibble()
all_depth_results_tbl <- tibble::tibble()

if (!file.exists(input_path)) {
  stop("Input file not found: ", input_path)
}

glmm_input <- readr::read_csv(input_path, show_col_types = FALSE) |>
  dplyr::mutate(
    year = to_year_numeric(.data$year),
    month = suppressWarnings(as.integer(as.character(.data$month))),
    area = as.character(.data$area),
    vessel = as.character(.data$vessel),
    effort_glmm = suppressWarnings(as.numeric(.data$effort_glmm)),
    depth_glmm = suppressWarnings(as.numeric(.data$depth_glmm)),
    chu = suppressWarnings(as.numeric(.data$chu)),
    dai = suppressWarnings(as.numeric(.data$dai)),
    toku = suppressWarnings(as.numeric(.data$toku)),
    tokudai = suppressWarnings(as.numeric(.data$tokudai)),
    flag_use_for_main_glmm = as.logical(.data$flag_use_for_main_glmm)
  )

required_cols <- c("year", "month", "area", "vessel", "effort_glmm", "depth_glmm", "chu", "dai", "toku", "tokudai", "flag_use_for_main_glmm")
missing_cols <- setdiff(required_cols, names(glmm_input))

if (length(missing_cols) > 0) {
  stop("Required columns are missing: ", paste(missing_cols, collapse = ", "))
}

for (response_name in response_levels) {
  base_compare_dat <- glmm_input |>
    dplyr::filter(.data$flag_use_for_main_glmm) |>
    dplyr::transmute(
      year = .data$year,
      month = .data$month,
      area = .data$area,
      vessel = .data$vessel,
      effort_glmm = .data$effort_glmm,
      depth_glmm = .data$depth_glmm,
      response = .data[[response_name]]
    ) |>
    dplyr::filter(
      !is.na(.data$response),
      !is.na(.data$year),
      !is.na(.data$month),
      !is.na(.data$area),
      !is.na(.data$vessel),
      !is.na(.data$effort_glmm),
      is.finite(.data$response),
      is.finite(.data$effort_glmm),
      .data$effort_glmm > 0
    ) |>
    dplyr::mutate(
      year = factor(.data$year),
      month = factor(.data$month, levels = sort(unique(.data$month))),
      area = factor(.data$area),
      vessel = factor(.data$vessel)
    )

  depth_compare_dat <- glmm_input |>
    dplyr::filter(.data$flag_use_for_main_glmm) |>
    dplyr::transmute(
      year = .data$year,
      month = .data$month,
      area = .data$area,
      vessel = .data$vessel,
      effort_glmm = .data$effort_glmm,
      depth_glmm = .data$depth_glmm,
      response = .data[[response_name]]
    ) |>
    dplyr::filter(
      !is.na(.data$response),
      !is.na(.data$year),
      !is.na(.data$month),
      !is.na(.data$area),
      !is.na(.data$vessel),
      !is.na(.data$effort_glmm),
      !is.na(.data$depth_glmm),
      is.finite(.data$response),
      is.finite(.data$effort_glmm),
      is.finite(.data$depth_glmm),
      .data$effort_glmm > 0
    )

  cat("response =", response_name, "| base compare dataset rows =", nrow(base_compare_dat), "\n")
  cat("response =", response_name, "| depth compare dataset rows =", nrow(depth_compare_dat), "\n")

  base_response_csv_path <- file.path("output", "tables", paste0("aic_compare_", response_name, "_base_same_subset.csv"))
  depth_response_csv_path <- file.path("output", "tables", paste0("aic_compare_", response_name, "_depth_same_subset.csv"))
  base_raw_relative_path <- file.path("output", "tables", paste0("raw_relative_cpue_", response_name, "_base_same_subset.csv"))
  depth_raw_relative_path <- file.path("output", "tables", paste0("raw_relative_cpue_", response_name, "_depth_same_subset.csv"))
  base_overlay_fig_path <- file.path("output", "figures", paste0("overlay_compare_", response_name, "_base_same_subset.png"))
  depth_overlay_fig_path <- file.path("output", "figures", paste0("overlay_compare_", response_name, "_depth_same_subset.png"))

  if (nrow(base_compare_dat) == 0) {
    base_results_tbl <- tibble::tibble(
      response = response_name,
      subset_type = "base_same_subset",
      model_id = vapply(base_model_specs, `[[`, character(1), "model_id"),
      model_label = vapply(base_model_specs, `[[`, character(1), "model_label"),
      status = "skipped_no_rows",
      nobs = 0L,
      AIC = NA_real_,
      BIC = NA_real_,
      logLik = NA_real_,
      converged = NA,
      pdHess = NA,
      dispersion = NA_real_,
      dispersion_p = NA_real_,
      warning_message = "No rows available after same-subset filtering.",
      delta_AIC = NA_real_
    )

    readr::write_csv(base_results_tbl, base_response_csv_path)
    all_base_results_tbl <- dplyr::bind_rows(all_base_results_tbl, base_results_tbl)
    cat("fitted base model ids = none\n")
    cat("best base AIC model = skipped\n")
    cat("saved csv path =", base_response_csv_path, "\n")
    cat("saved overlay figure path = skipped\n")
  } else {
    base_raw_cpue_tbl <- make_raw_cpue_table(base_compare_dat, "response")
    base_raw_relative_tbl <- build_raw_relative_cpue_table(base_raw_cpue_tbl, response_name)
    readr::write_csv(base_raw_relative_tbl, base_raw_relative_path)

    base_results_tbl <- tibble::tibble()
    base_overlay_tbl <- tibble::tibble()
    fitted_base_model_ids <- character(0)

    for (spec_i in base_model_specs) {
      formula_i <- stats::as.formula(spec_i$formula_text)
      fit_out_i <- safe_fit_glmmTMB(
        model_name = paste(response_name, spec_i$model_id, sep = "_"),
        formula_obj = formula_i,
        data_obj = base_compare_dat
      )

      dispersion_out_i <- run_dharma_dispersion(fit_out_i$fit, optional_pkgs)

      base_results_tbl <- dplyr::bind_rows(
        base_results_tbl,
        extract_model_metrics(
          response_name = response_name,
          subset_type = spec_i$subset_type,
          model_id = spec_i$model_id,
          model_label = spec_i$model_label,
          fit_obj = fit_out_i$fit,
          warning_message = fit_out_i$warning_message,
          error_message = fit_out_i$error_message,
          dispersion_out = dispersion_out_i
        )
      )

      if (is.null(fit_out_i$fit)) {
        next
      }

      fitted_base_model_ids <- c(fitted_base_model_ids, spec_i$model_id)

      model_rds_path <- file.path("output", "models", paste0("fit_compare_", response_name, "_", spec_i$model_id, "_", spec_i$subset_type, ".rds"))
      saveRDS(fit_out_i$fit, model_rds_path)

      year_index_tbl_i <- compute_year_index_table(
        model_obj = fit_out_i$fit,
        data_obj = base_compare_dat,
        optional_pkgs = optional_pkgs
      )

      standardized_index_tbl_i <- build_standardized_index_table(year_index_tbl_i, response_name) |>
        dplyr::mutate(
          model_id = spec_i$model_id,
          model_label = spec_i$model_label,
          subset_type = spec_i$subset_type
        ) |>
        dplyr::select("response", "subset_type", "model_id", "model_label", "year", "estimate", "lower.CL", "upper.CL")

      year_index_path_i <- file.path("output", "tables", paste0("year_index_", response_name, "_", spec_i$model_id, "_", spec_i$subset_type, ".csv"))
      readr::write_csv(standardized_index_tbl_i, year_index_path_i)

      base_overlay_tbl <- dplyr::bind_rows(
        base_overlay_tbl,
        build_relative_overlay_tbl(
          raw_relative_tbl = base_raw_relative_tbl,
          index_tbl = standardized_index_tbl_i,
          model_id = spec_i$model_id,
          model_label = spec_i$model_label,
          subset_type = spec_i$subset_type
        )
      )
    }

    if (any(is.finite(base_results_tbl$AIC))) {
      base_results_tbl <- base_results_tbl |>
        dplyr::mutate(delta_AIC = .data$AIC - min(.data$AIC, na.rm = TRUE))
    } else {
      base_results_tbl <- base_results_tbl |>
        dplyr::mutate(delta_AIC = NA_real_)
    }

    readr::write_csv(base_results_tbl, base_response_csv_path)
    all_base_results_tbl <- dplyr::bind_rows(all_base_results_tbl, base_results_tbl)

    if (nrow(base_overlay_tbl) > 0) {
      plot_relative_overlay_compare(
        compare_tbl = base_overlay_tbl,
        output_path = base_overlay_fig_path,
        title_text = paste0(response_name, ": relative raw CPUE vs standardized index (base same-subset)"),
        model_levels = base_model_levels
      )
    }

    best_base_model_candidates <- base_results_tbl |>
      dplyr::filter(is.finite(.data$AIC)) |>
      dplyr::arrange(.data$AIC, .data$model_id) |>
      dplyr::pull("model_id")

    best_base_model_id <- if (length(best_base_model_candidates) == 0) {
      NA_character_
    } else {
      best_base_model_candidates[[1]]
    }

    cat("fitted base model ids =", if (length(fitted_base_model_ids) == 0) "none" else paste(fitted_base_model_ids, collapse = ", "), "\n")
    cat("best base AIC model =", best_base_model_id %||% "none", "\n")
    cat("saved csv path =", base_response_csv_path, "\n")
    cat("saved overlay figure path =", if (nrow(base_overlay_tbl) > 0) base_overlay_fig_path else "skipped", "\n")
  }

  if (nrow(depth_compare_dat) == 0) {
    depth_results_tbl <- tibble::tibble(
      response = response_name,
      subset_type = "depth_same_subset",
      model_id = vapply(depth_model_specs, `[[`, character(1), "model_id"),
      model_label = vapply(depth_model_specs, `[[`, character(1), "model_label"),
      status = "skipped_no_rows",
      nobs = 0L,
      AIC = NA_real_,
      BIC = NA_real_,
      logLik = NA_real_,
      converged = NA,
      pdHess = NA,
      dispersion = NA_real_,
      dispersion_p = NA_real_,
      warning_message = "No rows available after same-subset filtering.",
      delta_AIC = NA_real_
    )

    readr::write_csv(depth_results_tbl, depth_response_csv_path)
    all_depth_results_tbl <- dplyr::bind_rows(all_depth_results_tbl, depth_results_tbl)
    cat("fitted depth model ids = none\n")
    cat("best depth AIC model = skipped\n")
    cat("saved csv path =", depth_response_csv_path, "\n")
    cat("saved overlay figure path = skipped\n")
    cat("base and depth AIC are compared within their own same-subset only.\n")
    next
  }

  depth_glmm_mean <- mean(depth_compare_dat$depth_glmm, na.rm = TRUE)
  depth_glmm_sd <- stats::sd(depth_compare_dat$depth_glmm, na.rm = TRUE)

  if (!is.finite(depth_glmm_sd) || depth_glmm_sd <= 0) {
    depth_results_tbl <- tibble::tibble(
      response = response_name,
      subset_type = "depth_same_subset",
      model_id = vapply(depth_model_specs, `[[`, character(1), "model_id"),
      model_label = vapply(depth_model_specs, `[[`, character(1), "model_label"),
      status = "skipped_depth_sd_invalid",
      nobs = nrow(depth_compare_dat),
      AIC = NA_real_,
      BIC = NA_real_,
      logLik = NA_real_,
      converged = NA,
      pdHess = NA,
      dispersion = NA_real_,
      dispersion_p = NA_real_,
      warning_message = "depth_glmm standard deviation is not finite or <= 0 on depth same-subset data.",
      delta_AIC = NA_real_
    )

    readr::write_csv(depth_results_tbl, depth_response_csv_path)
    all_depth_results_tbl <- dplyr::bind_rows(all_depth_results_tbl, depth_results_tbl)
    cat("fitted depth model ids = none\n")
    cat("best depth AIC model = skipped\n")
    cat("saved csv path =", depth_response_csv_path, "\n")
    cat("saved overlay figure path = skipped\n")
    cat("base and depth AIC are compared within their own same-subset only.\n")
    next
  }

  depth_compare_dat <- depth_compare_dat |>
    dplyr::mutate(
      year = factor(.data$year),
      month = factor(.data$month, levels = sort(unique(.data$month))),
      area = factor(.data$area),
      vessel = factor(.data$vessel),
      depth_glmm_sc = (.data$depth_glmm - depth_glmm_mean) / depth_glmm_sd,
      depth_cat = dplyr::case_when(
        .data$depth_glmm < 30 ~ "shallow",
        .data$depth_glmm < 50 ~ "mid",
        .data$depth_glmm >= 50 ~ "deep",
        TRUE ~ NA_character_
      ),
      depth_cat = factor(.data$depth_cat, levels = c("shallow", "mid", "deep"))
    )

  depth_cat_counts_tbl <- depth_compare_dat |>
    dplyr::count(.data$depth_cat, .drop = FALSE) |>
    dplyr::filter(!is.na(.data$depth_cat))

  depth_cat_counts_text <- paste(
    paste0(as.character(depth_cat_counts_tbl$depth_cat), ": ", depth_cat_counts_tbl$n),
    collapse = ", "
  )

  cat("response =", response_name, "| depth_cat counts =", depth_cat_counts_text, "\n")

  depth_cat_reference <- if ("mid" %in% get_observed_levels(depth_compare_dat$depth_cat)) {
    "mid"
  } else {
    get_observed_levels(depth_compare_dat$depth_cat)[[1]]
  }

  depth_raw_cpue_tbl <- make_raw_cpue_table(depth_compare_dat, "response")
  depth_raw_relative_tbl <- build_raw_relative_cpue_table(depth_raw_cpue_tbl, response_name)
  readr::write_csv(depth_raw_relative_tbl, depth_raw_relative_path)

  depth_results_tbl <- tibble::tibble()
  depth_overlay_tbl <- tibble::tibble()
  fitted_depth_model_ids <- character(0)

  for (spec_i in depth_model_specs) {
    formula_i <- stats::as.formula(spec_i$formula_text)
    fit_out_i <- safe_fit_glmmTMB(
      model_name = paste(response_name, spec_i$model_id, sep = "_"),
      formula_obj = formula_i,
      data_obj = depth_compare_dat
    )

    dispersion_out_i <- run_dharma_dispersion(fit_out_i$fit, optional_pkgs)

    depth_results_tbl <- dplyr::bind_rows(
      depth_results_tbl,
      extract_model_metrics(
        response_name = response_name,
        subset_type = spec_i$subset_type,
        model_id = spec_i$model_id,
        model_label = spec_i$model_label,
        fit_obj = fit_out_i$fit,
        warning_message = fit_out_i$warning_message,
        error_message = fit_out_i$error_message,
        dispersion_out = dispersion_out_i
      )
    )

    if (is.null(fit_out_i$fit)) {
      next
    }

    fitted_depth_model_ids <- c(fitted_depth_model_ids, spec_i$model_id)

    model_rds_path <- file.path("output", "models", paste0("fit_compare_", response_name, "_", spec_i$model_id, "_", spec_i$subset_type, ".rds"))
    saveRDS(fit_out_i$fit, model_rds_path)

    year_index_tbl_i <- compute_year_index_table(
      model_obj = fit_out_i$fit,
      data_obj = depth_compare_dat,
      optional_pkgs = optional_pkgs,
      depth_glmm_sc_value = if (isTRUE(spec_i$uses_depth)) 0 else NULL,
      depth_cat_value = if (isTRUE(spec_i$uses_depth_cat)) depth_cat_reference else NULL
    )

    standardized_index_tbl_i <- build_standardized_index_table(year_index_tbl_i, response_name) |>
      dplyr::mutate(
        model_id = spec_i$model_id,
        model_label = spec_i$model_label,
        subset_type = spec_i$subset_type
      ) |>
      dplyr::select("response", "subset_type", "model_id", "model_label", "year", "estimate", "lower.CL", "upper.CL")

    year_index_path_i <- file.path("output", "tables", paste0("year_index_", response_name, "_", spec_i$model_id, "_", spec_i$subset_type, ".csv"))
    readr::write_csv(standardized_index_tbl_i, year_index_path_i)

    depth_overlay_tbl <- dplyr::bind_rows(
      depth_overlay_tbl,
      build_relative_overlay_tbl(
        raw_relative_tbl = depth_raw_relative_tbl,
        index_tbl = standardized_index_tbl_i,
        model_id = spec_i$model_id,
        model_label = spec_i$model_label,
        subset_type = spec_i$subset_type
      )
    )
  }

  if (any(is.finite(depth_results_tbl$AIC))) {
    depth_results_tbl <- depth_results_tbl |>
      dplyr::mutate(delta_AIC = .data$AIC - min(.data$AIC, na.rm = TRUE))
  } else {
    depth_results_tbl <- depth_results_tbl |>
      dplyr::mutate(delta_AIC = NA_real_)
  }

  readr::write_csv(depth_results_tbl, depth_response_csv_path)
  all_depth_results_tbl <- dplyr::bind_rows(all_depth_results_tbl, depth_results_tbl)

  if (nrow(depth_overlay_tbl) > 0) {
    plot_relative_overlay_compare(
      compare_tbl = depth_overlay_tbl,
      output_path = depth_overlay_fig_path,
      title_text = paste0(response_name, ": relative raw CPUE vs standardized index (depth same-subset)"),
      model_levels = depth_model_levels
    )
  }

  best_depth_model_candidates <- depth_results_tbl |>
    dplyr::filter(is.finite(.data$AIC)) |>
    dplyr::arrange(.data$AIC, .data$model_id) |>
    dplyr::pull("model_id")

  best_depth_model_id <- if (length(best_depth_model_candidates) == 0) {
    NA_character_
  } else {
    best_depth_model_candidates[[1]]
  }

  cat("fitted depth model ids =", if (length(fitted_depth_model_ids) == 0) "none" else paste(fitted_depth_model_ids, collapse = ", "), "\n")
  cat("best depth AIC model =", best_depth_model_id %||% "none", "\n")
  cat("saved csv path =", depth_response_csv_path, "\n")
  cat("saved overlay figure path =", if (nrow(depth_overlay_tbl) > 0) depth_overlay_fig_path else "skipped", "\n")
  cat("base and depth AIC are compared within their own same-subset only.\n")
}

all_base_csv_path <- file.path("output", "tables", "aic_compare_all_base_same_subset.csv")
all_depth_csv_path <- file.path("output", "tables", "aic_compare_all_depth_same_subset.csv")
readr::write_csv(all_base_results_tbl, all_base_csv_path)
readr::write_csv(all_depth_results_tbl, all_depth_csv_path)

cat("saved csv path =", all_base_csv_path, "\n")
cat("saved csv path =", all_depth_csv_path, "\n")
cat("AIC comparison completed on same-subset data.\n")
