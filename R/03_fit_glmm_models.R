# =========================================
# fit_glmm_models.R
# Step 3
# 入力: data_processed/akagai_glmm_input.csv
# 出力: AIC比較、best model、年指数、図、診断
# =========================================

source(file.path("R", "00_load_packages.R"))

optional_pkgs <- load_project_packages(
  required_pkgs = c("dplyr", "ggplot2", "glmmTMB", "readr", "tibble", "tidyr", "purrr"),
  optional_pkgs = c("DHARMa")
)

ensure_project_dirs()

input_path <- file.path("data_processed", "akagai_glmm_input.csv")
response_levels <- c("chu", "dai", "toku", "tokudai", "count_total")

path_depth_definition <- file.path("output", "tables", "depth_category_definition.csv")
path_depth_summary <- file.path("output", "tables", "depth_category_summary.csv")
path_aic_compare_all <- file.path("output", "tables", "aic_compare_all.csv")
path_best_model_table <- file.path("output", "tables", "best_model_table.csv")
path_best_model_summary <- file.path("output", "tables", "best_model_summary.csv")
path_results_rds <- file.path("output", "models", "glmm_workflow_results.rds")

# 左辺が NULL または長さ 0 のとき右辺を返す。
`%||%` <- function(x, y) {
  if (length(x) == 0 || is.null(x)) {
    return(y)
  }

  x
}

# 重複を除いたメッセージを 1 つの文字列にまとめる。
combine_messages <- function(...) {
  vals <- unlist(list(...), use.names = FALSE)
  vals <- vals[!is.na(vals) & nzchar(vals)]

  if (length(vals) == 0) {
    return("")
  }

  paste(unique(vals), collapse = " | ")
}

# year 値を整数に変換する。
to_year_numeric <- function(x) {
  suppressWarnings(as.integer(as.character(x)))
}

# 欠損を除いた観測済みの水準を文字列で返す。
get_observed_levels <- function(x) {
  sort(unique(as.character(x[!is.na(x)])))
}

# 年指数テーブルから推定値の列名を特定する。
get_year_index_col <- function(tbl) {
  matched <- intersect(c("response", "rate", "prob", "emmean"), names(tbl))

  if (length(matched) == 0) {
    stop("Failed to identify the year index column.")
  }

  matched[[1]]
}

# 指定したランダム効果の分散と標準偏差を取り出す。
extract_re_sd <- function(varcorr_cond, grp_name) {
  if (is.null(varcorr_cond[[grp_name]])) {
    return(c(variance = NA_real_, sd = NA_real_))
  }

  grp_mat <- varcorr_cond[[grp_name]]
  grp_sd <- attr(grp_mat, "stddev")

  c(
    variance = suppressWarnings(as.numeric(grp_mat[1, 1])),
    sd = suppressWarnings(as.numeric(grp_sd[[1]]))
  )
}

# glmmTMB の当てはめ結果と警告・エラーをまとめて返す。
safe_fit_glmmTMB <- function(model_id, formula_obj, data_obj) {
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
    model_id = model_id,
    fit = fit_obj,
    warning_message = paste(unique(warning_messages), collapse = " | "),
    error_message = error_message
  )
}

# DHARMa を使って分散診断を実行し、必要なら診断図も保存する。
run_dharma_dispersion <- function(fit_obj, optional_pkgs, output_plot_path = NULL) {
  if (!isTRUE(optional_pkgs[["DHARMa"]]) || is.null(fit_obj)) {
    return(list(
      status = "not_run",
      dispersion = NA_real_,
      dispersion_p = NA_real_,
      warning_message = "DHARMa is not available."
    ))
  }

  simulated_obj <- tryCatch(
    DHARMa::simulateResiduals(fit_obj, plot = FALSE),
    error = function(e) e
  )

  if (inherits(simulated_obj, "error")) {
    return(list(
      status = "failed",
      dispersion = NA_real_,
      dispersion_p = NA_real_,
      warning_message = conditionMessage(simulated_obj)
    ))
  }

  if (!is.null(output_plot_path)) {
    tryCatch(
      {
        grDevices::png(filename = output_plot_path, width = 1400, height = 1000, res = 150)
        on.exit(grDevices::dev.off(), add = TRUE)
        plot(simulated_obj)
      },
      error = function(e) NULL
    )
  }

  dispersion_out <- tryCatch(
    DHARMa::testDispersion(simulated_obj),
    error = function(e) e
  )

  if (inherits(dispersion_out, "error")) {
    return(list(
      status = "failed",
      dispersion = NA_real_,
      dispersion_p = NA_real_,
      warning_message = conditionMessage(dispersion_out)
    ))
  }

  list(
    status = "completed",
    dispersion = suppressWarnings(as.numeric(dispersion_out$statistic[[1]] %||% NA_real_)),
    dispersion_p = suppressWarnings(as.numeric(dispersion_out$p.value %||% NA_real_)),
    warning_message = ""
  )
}

# 3 区分用の depth 境界候補の組み合わせを作る。
build_depth_boundary_candidates <- function(depth_values) {
  unique_depth <- sort(unique(depth_values))

  if (length(unique_depth) < 3) {
    stop("depth_glmm must have at least 3 unique values to create 3 categories.")
  }

  lower_candidates <- unique_depth[seq_len(length(unique_depth) - 2)]
  upper_candidates <- unique_depth[seq(2, length(unique_depth) - 1)]

  tidyr::expand_grid(c1 = lower_candidates, c2 = upper_candidates) |>
    dplyr::filter(.data$c1 < .data$c2)
}

# 指定した 2 つの境界で depth 区分を作り、各区分の件数を集計する。
summarise_depth_split <- function(depth_values, c1, c2) {
  depth_cat_chr <- dplyr::case_when(
    depth_values <= c1 ~ "shallow",
    depth_values <= c2 ~ "mid",
    depth_values > c2 ~ "deep",
    TRUE ~ NA_character_
  )

  count_tbl <- tibble::tibble(depth_cat = depth_cat_chr, depth_glmm = depth_values) |>
    dplyr::group_by(.data$depth_cat) |>
    dplyr::summarise(
      n = dplyr::n(),
      min_depth = min(.data$depth_glmm, na.rm = TRUE),
      max_depth = max(.data$depth_glmm, na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::right_join(
      tibble::tibble(depth_cat = c("shallow", "mid", "deep")),
      by = "depth_cat"
    ) |>
    dplyr::mutate(
      n = dplyr::coalesce(.data$n, 0L),
      min_depth = .data$min_depth,
      max_depth = .data$max_depth
    )

  n_total <- length(depth_values)
  target_n <- n_total / 3

  count_tbl |>
    dplyr::mutate(
      c1 = c1,
      c2 = c2,
      score = sum((.data$n - target_n) ^ 2),
      min_prop = min(.data$n / n_total)
    )
}

# 主解析に使うデータから共通の depth 区分境界を決める。
determine_depth_boundaries <- function(glmm_input) {
  depth_source_tbl <- glmm_input |>
    dplyr::filter(.data$flag_use_for_main_glmm) |>
    dplyr::filter(
      !is.na(.data$year),
      !is.na(.data$month),
      !is.na(.data$area),
      !is.na(.data$vessel),
      !is.na(.data$effort_glmm),
      !is.na(.data$depth_glmm),
      is.finite(.data$effort_glmm),
      is.finite(.data$depth_glmm),
      .data$effort_glmm > 0
    ) |>
    dplyr::transmute(depth_glmm = .data$depth_glmm)

  if (nrow(depth_source_tbl) == 0) {
    stop("No rows available for common depth boundary determination.")
  }

  depth_values <- depth_source_tbl$depth_glmm
  quantile_vals <- stats::quantile(depth_values, probs = c(1 / 3, 2 / 3), na.rm = TRUE, names = FALSE, type = 7)
  quantile_candidate <- summarise_depth_split(depth_values, quantile_vals[[1]], quantile_vals[[2]])

  use_search <- !isTRUE(quantile_vals[[1]] < quantile_vals[[2]]) || any(quantile_candidate$n == 0) || isTRUE(min(quantile_candidate$n) < 0.10 * length(depth_values))

  if (isTRUE(use_search)) {
    boundary_tbl <- build_depth_boundary_candidates(depth_values)

    candidate_tbl <- purrr::pmap_dfr(
      list(boundary_tbl$c1, boundary_tbl$c2),
      function(c1, c2) {
        summarise_depth_split(depth_values, c1, c2)
      }
    ) |>
      dplyr::distinct(.data$depth_cat, .data$n, .data$min_depth, .data$max_depth, .data$c1, .data$c2, .data$score, .data$min_prop)

    best_boundary_tbl <- candidate_tbl |>
      dplyr::group_by(.data$c1, .data$c2, .data$score, .data$min_prop) |>
      dplyr::summarise(.groups = "drop") |>
      dplyr::arrange(.data$score, dplyr::desc(.data$min_prop), .data$c1, .data$c2)

    selected_c1 <- best_boundary_tbl$c1[[1]]
    selected_c2 <- best_boundary_tbl$c2[[1]]
    selected_summary_tbl <- candidate_tbl |>
      dplyr::filter(.data$c1 == selected_c1, .data$c2 == selected_c2) |>
      dplyr::select("depth_cat", "n", "min_depth", "max_depth")
  } else {
    selected_c1 <- quantile_vals[[1]]
    selected_c2 <- quantile_vals[[2]]
    selected_summary_tbl <- quantile_candidate |>
      dplyr::select("depth_cat", "n", "min_depth", "max_depth")
  }

  reference_level <- if ("mid" %in% selected_summary_tbl$depth_cat[selected_summary_tbl$n > 0]) {
    "mid"
  } else {
    selected_summary_tbl$depth_cat[selected_summary_tbl$n > 0][[1]]
  }

  list(
    c1 = selected_c1,
    c2 = selected_c2,
    reference_level = reference_level,
    definition_tbl = tibble::tibble(
      depth_cat = c("shallow", "mid", "deep"),
      lower_bound = c(-Inf, selected_c1, selected_c2),
      upper_bound = c(selected_c1, selected_c2, Inf),
      lower_bound_closed = c(TRUE, FALSE, FALSE),
      upper_bound_closed = c(TRUE, TRUE, FALSE),
      reference_level = c(reference_level, reference_level, reference_level)
    ),
    summary_tbl = selected_summary_tbl
  )
}

# depth 境界情報に基づいて depth カテゴリ列を付与する。
attach_depth_category <- function(data_obj, depth_boundary_info) {
  data_obj |>
    dplyr::mutate(
      depth_cat = dplyr::case_when(
        .data$depth_glmm <= depth_boundary_info$c1 ~ "shallow",
        .data$depth_glmm <= depth_boundary_info$c2 ~ "mid",
        .data$depth_glmm > depth_boundary_info$c2 ~ "deep",
        TRUE ~ NA_character_
      ),
      depth_cat = factor(.data$depth_cat, levels = c("shallow", "mid", "deep"))
    ) |>
    dplyr::mutate(
      depth_cat = stats::relevel(.data$depth_cat, ref = depth_boundary_info$reference_level)
    )
}

# モデル用に year・month・area・vessel を factor 化する。
prepare_model_factors <- function(data_obj) {
  data_obj |>
    dplyr::mutate(
      year = factor(.data$year, levels = sort(unique(.data$year))),
      month = factor(.data$month, levels = sort(unique(.data$month))),
      area = factor(.data$area, levels = sort(unique(.data$area))),
      vessel = factor(.data$vessel, levels = sort(unique(.data$vessel)))
    )
}

# モデル比較用の解析データを作成する。
make_compare_dataset <- function(glmm_input, response_name, depth_boundary_info) {
  compare_tbl <- glmm_input |>
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

  if (nrow(compare_tbl) == 0) {
    stop("No rows available for compare dataset.")
  }

  depth_mean <- mean(compare_tbl$depth_glmm, na.rm = TRUE)
  depth_sd <- stats::sd(compare_tbl$depth_glmm, na.rm = TRUE)

  if (!is.finite(depth_sd) || depth_sd <= 0) {
    stop("depth_glmm standard deviation must be finite and > 0 on compare dataset.")
  }

  compare_tbl |>
    dplyr::mutate(
      depth_glmm_sc = (.data$depth_glmm - depth_mean) / depth_sd
    ) |>
    attach_depth_category(depth_boundary_info = depth_boundary_info) |>
    prepare_model_factors()
}

# 選択モデルの再当てはめに使う最終データを作成する。
make_final_dataset <- function(glmm_input, response_name, uses_depth, depth_boundary_info, compare_tbl = NULL) {
  if (isTRUE(uses_depth)) {
    return(compare_tbl)
  }

  final_tbl <- glmm_input |>
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
    attach_depth_category(depth_boundary_info = depth_boundary_info) |>
    prepare_model_factors()

  if (nrow(final_tbl) == 0) {
    stop("No rows available for final dataset.")
  }

  final_tbl
}

# 比較対象とする候補モデルの仕様一覧を作成する。
build_candidate_model_specs <- function() {
  fixed_specs <- list(
    list(
      fixed_mode = "year_only",
      fixed_tag = "Y",
      fixed_terms = "year",
      label_terms = "year"
    ),
    list(
      fixed_mode = "year_month",
      fixed_tag = "YM",
      fixed_terms = "year + month",
      label_terms = "year + month"
    )
  )

  depth_specs <- list(
    list(
      depth_mode = "none",
      depth_tag = "D0",
      uses_depth = FALSE,
      depth_terms = NULL,
      label_terms = NULL
    ),
    list(
      depth_mode = "quad",
      depth_tag = "D2",
      uses_depth = TRUE,
      depth_terms = "depth_glmm_sc + I(depth_glmm_sc^2)",
      label_terms = "depth_quad"
    ),
    list(
      depth_mode = "cat",
      depth_tag = "Dc",
      uses_depth = TRUE,
      depth_terms = "depth_cat",
      label_terms = "depth_cat"
    )
  )

  random_specs <- list(
    list(
      random_mode = "none",
      random_tag = "R0",
      random_terms = NULL,
      label_terms = NULL
    ),
    list(
      random_mode = "area",
      random_tag = "Ra",
      random_terms = "(1 | area)",
      label_terms = "(1|area)"
    ),
    list(
      random_mode = "vessel",
      random_tag = "Rv",
      random_terms = "(1 | vessel)",
      label_terms = "(1|vessel)"
    ),
    list(
      random_mode = "area_vessel",
      random_tag = "Rav",
      random_terms = "(1 | area) + (1 | vessel)",
      label_terms = "(1|area) + (1|vessel)"
    )
  )

  spec_grid <- expand.grid(
    fixed_idx = seq_along(fixed_specs),
    depth_idx = seq_along(depth_specs),
    random_idx = seq_along(random_specs),
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  purrr::pmap(
    list(spec_grid$fixed_idx, spec_grid$depth_idx, spec_grid$random_idx),
    function(fixed_idx, depth_idx, random_idx) {
      fixed_spec <- fixed_specs[[fixed_idx]]
      depth_spec <- depth_specs[[depth_idx]]
      random_spec <- random_specs[[random_idx]]
      rhs_terms <- c(fixed_spec$fixed_terms, depth_spec$depth_terms, "offset(log(effort_glmm))", random_spec$random_terms)
      label_terms <- c(fixed_spec$label_terms, depth_spec$label_terms, "offset", random_spec$label_terms)

      list(
        model_id = paste(fixed_spec$fixed_tag, fixed_spec$fixed_mode, depth_spec$depth_tag, random_spec$random_tag, sep = "_"),
        model_label = paste(label_terms[!is.na(label_terms) & nzchar(label_terms)], collapse = " + "),
        formula_text = paste("response ~", paste(rhs_terms[!is.na(rhs_terms) & nzchar(rhs_terms)], collapse = " + ")),
        uses_depth = depth_spec$uses_depth,
        depth_mode = depth_spec$depth_mode,
        random_mode = random_spec$random_mode,
        fixed_mode = fixed_spec$fixed_mode
      )
    }
  )
}

# 当てはめ結果からモデル比較用の指標を取り出す。
extract_model_metrics <- function(response_name, spec_i, fit_out_i) {
  fit_obj <- fit_out_i$fit
  warning_message <- combine_messages(fit_out_i$warning_message, fit_out_i$error_message)

  if (is.null(fit_obj)) {
    return(tibble::tibble(
      response = response_name,
      model_id = spec_i$model_id,
      model_label = spec_i$model_label,
      status = "fit_failed",
      nobs = NA_integer_,
      AIC = NA_real_,
      BIC = NA_real_,
      logLik = NA_real_,
      converged = NA,
      pdHess = NA,
      warning_message = warning_message,
      uses_depth = spec_i$uses_depth,
      depth_mode = spec_i$depth_mode,
      random_mode = spec_i$random_mode,
      fixed_mode = spec_i$fixed_mode
    ))
  }

  tibble::tibble(
    response = response_name,
    model_id = spec_i$model_id,
    model_label = spec_i$model_label,
    status = "completed",
    nobs = tryCatch(as.integer(stats::nobs(fit_obj)), error = function(e) NA_integer_),
    AIC = tryCatch(as.numeric(AIC(fit_obj)[1]), error = function(e) NA_real_),
    BIC = tryCatch(as.numeric(BIC(fit_obj)[1]), error = function(e) NA_real_),
    logLik = tryCatch(as.numeric(logLik(fit_obj)), error = function(e) NA_real_),
    converged = tryCatch(isTRUE(fit_obj$fit$convergence == 0), error = function(e) NA),
    pdHess = tryCatch(isTRUE(fit_obj$sdr$pdHess), error = function(e) NA),
    warning_message = warning_message,
    uses_depth = spec_i$uses_depth,
    depth_mode = spec_i$depth_mode,
    random_mode = spec_i$random_mode,
    fixed_mode = spec_i$fixed_mode
  )
}

# 収束条件を満たす候補の中から AIC 最小のモデルを選ぶ。
select_best_model <- function(compare_results_tbl) {
  valid_tbl <- compare_results_tbl |>
    dplyr::filter(
      .data$status == "completed",
      .data$converged == TRUE,
      .data$pdHess == TRUE,
      is.finite(.data$AIC)
    ) |>
    dplyr::arrange(.data$AIC, .data$model_id)

  if (nrow(valid_tbl) == 0) {
    return(NULL)
  }

  valid_tbl[1, , drop = FALSE]
}

# モデル予測から年別の標準化前指数を計算する。
compute_year_index_table <- function(model_obj, data_obj, depth_mode, depth_reference_level = NULL) {
  model_frame_names <- names(stats::model.frame(model_obj))
  year_levels_used <- get_observed_levels(data_obj$year)
  pred_grid <- tidyr::expand_grid(
    year = factor(year_levels_used, levels = levels(data_obj$year))
  )

  if ("month" %in% model_frame_names) {
    month_levels_used <- get_observed_levels(data_obj$month)
    pred_grid <- tidyr::expand_grid(
      year = factor(year_levels_used, levels = levels(data_obj$year)),
      month = factor(month_levels_used, levels = levels(data_obj$month))
    )
  }

  pred_grid$effort_glmm <- 1

  if ("depth_glmm_sc" %in% model_frame_names && identical(depth_mode, "quad")) {
    pred_grid$depth_glmm_sc <- 0
  }

  if ("depth_cat" %in% model_frame_names && identical(depth_mode, "cat")) {
    pred_grid$depth_cat <- factor(depth_reference_level, levels = levels(data_obj$depth_cat))
  }

  if ("area" %in% model_frame_names) {
    pred_grid$area <- factor(get_observed_levels(data_obj$area)[[1]], levels = levels(data_obj$area))
  }

  if ("vessel" %in% model_frame_names) {
    pred_grid$vessel <- factor(get_observed_levels(data_obj$vessel)[[1]], levels = levels(data_obj$vessel))
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
      lower.CL = pmax(.data$predicted - 1.96 * as.numeric(se_vals), 0),
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

# 年指数を平均 1 基準に標準化したテーブルへ変換する。
build_standardized_index_table <- function(tbl, response_name, best_model_id) {
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
    best_model_id = best_model_id,
    year = to_year_numeric(tbl$year),
    estimate = estimate_vals / estimate_mean,
    lower.CL = lower_vals / estimate_mean,
    upper.CL = upper_vals / estimate_mean
  )
}

# 年別の raw CPUE を集計する。
make_raw_cpue_table <- function(data_obj, response_name) {
  data_obj |>
    dplyr::filter(
      !is.na(.data$year),
      !is.na(.data$effort_glmm),
      !is.na(.data$response),
      is.finite(.data$effort_glmm),
      is.finite(.data$response),
      .data$effort_glmm > 0
    ) |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(
      total_count = sum(.data$response, na.rm = TRUE),
      total_effort = sum(.data$effort_glmm, na.rm = TRUE),
      cpue = .data$total_count / .data$total_effort,
      .groups = "drop"
    ) |>
    dplyr::mutate(
      response = response_name,
      year = to_year_numeric(.data$year)
    ) |>
    dplyr::arrange(.data$year)
}

# 標準化指数と raw CPUE を重ね描き用の形式に整える。
build_overlay_table <- function(index_tbl, raw_cpue_tbl, response_name, best_model_id) {
  cpue_mean <- mean(raw_cpue_tbl$cpue, na.rm = TRUE)

  raw_tbl <- raw_cpue_tbl |>
    dplyr::transmute(
      response = response_name,
      best_model_id = best_model_id,
      year = .data$year,
      series = "Raw relative CPUE",
      value = if (is.finite(cpue_mean) && cpue_mean > 0) .data$cpue / cpue_mean else NA_real_,
      lower.CL = NA_real_,
      upper.CL = NA_real_
    )

  index_plot_tbl <- index_tbl |>
    dplyr::transmute(
      response = response_name,
      best_model_id = best_model_id,
      year = .data$year,
      series = "Standardized index",
      value = .data$estimate,
      lower.CL = .data$lower.CL,
      upper.CL = .data$upper.CL
    )

  dplyr::bind_rows(raw_tbl, index_plot_tbl)
}

# 年別の標準化指数を図として保存する。
plot_year_index <- function(index_tbl, output_path, title_text, show_ci = FALSE) {
  p <- ggplot2::ggplot(index_tbl, ggplot2::aes(x = .data$year, y = .data$estimate, group = 1)) +
    ggplot2::geom_line() +
    ggplot2::geom_point(size = 2) +
    ggplot2::labs(
      title = title_text,
      x = "Year",
      y = "Standardized index"
    ) +
    ggplot2::scale_x_continuous(breaks = sort(unique(index_tbl$year))) +
    ggplot2::theme_bw()

  if (isTRUE(show_ci)) {
    p <- p +
      ggplot2::geom_errorbar(
        ggplot2::aes(ymin = .data$lower.CL, ymax = .data$upper.CL),
        width = 0.15
      )
  }

  ggplot2::ggsave(
    filename = output_path,
    plot = p,
    width = 10,
    height = 6,
    dpi = 150
  )
}

# 標準化指数と raw CPUE の比較図を保存する。
plot_overlay <- function(overlay_tbl, output_path, title_text) {
  p <- ggplot2::ggplot(
    overlay_tbl,
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
      data = dplyr::filter(overlay_tbl, .data$series == "Standardized index"),
      ggplot2::aes(ymin = .data$lower.CL, ymax = .data$upper.CL),
      width = 0.15
    ) +
    ggplot2::labs(
      title = title_text,
      x = "Year",
      y = "Relative value",
      color = NULL,
      linetype = NULL,
      shape = NULL
    ) +
    ggplot2::scale_x_continuous(breaks = sort(unique(overlay_tbl$year))) +
    ggplot2::theme_bw()

  ggplot2::ggsave(
    filename = output_path,
    plot = p,
    width = 10,
    height = 6,
    dpi = 150
  )
}

# 最良モデルの summary 出力をテキスト保存する。
write_best_model_detail <- function(fit_obj, output_path) {
  detail_lines <- capture.output(summary(fit_obj))
  writeLines(detail_lines, con = output_path, useBytes = TRUE)
}

# 最良モデルの要約指標を 1 行のテーブルにまとめる。
build_best_model_summary_row <- function(response_name, best_model_row, final_fit_out, final_dataset_rows, compare_dataset_rows) {
  fit_obj <- final_fit_out$fit
  vc_cond <- tryCatch(VarCorr(fit_obj)$cond, error = function(e) NULL)
  area_stats <- extract_re_sd(vc_cond, "area")
  vessel_stats <- extract_re_sd(vc_cond, "vessel")

  tibble::tibble(
    response = response_name,
    best_model_id = best_model_row$model_id,
    best_model_label = best_model_row$model_label,
    formula = paste(base::deparse(stats::formula(fit_obj)), collapse = " "),
    compare_dataset_rows = compare_dataset_rows,
    final_dataset_rows = final_dataset_rows,
    n_zero = sum(model.frame(fit_obj)[["response"]] == 0, na.rm = TRUE),
    zero_ratio = mean(model.frame(fit_obj)[["response"]] == 0, na.rm = TRUE),
    AIC = tryCatch(as.numeric(AIC(fit_obj)[1]), error = function(e) NA_real_),
    BIC = tryCatch(as.numeric(BIC(fit_obj)[1]), error = function(e) NA_real_),
    logLik = tryCatch(as.numeric(logLik(fit_obj)), error = function(e) NA_real_),
    dispersion_parameter = tryCatch(as.numeric(sigma(fit_obj)), error = function(e) NA_real_),
    convergence_code = tryCatch(as.integer(fit_obj$fit$convergence), error = function(e) NA_integer_),
    converged = tryCatch(isTRUE(fit_obj$fit$convergence == 0), error = function(e) NA),
    pdHess = tryCatch(isTRUE(fit_obj$sdr$pdHess), error = function(e) NA),
    area_variance = area_stats[["variance"]],
    area_sd = area_stats[["sd"]],
    vessel_variance = vessel_stats[["variance"]],
    vessel_sd = vessel_stats[["sd"]],
    warning_message = combine_messages(final_fit_out$warning_message, final_fit_out$error_message)
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
    effort_glmm = suppressWarnings(as.numeric(.data$effort_glmm)),
    depth_glmm = suppressWarnings(as.numeric(.data$depth_glmm)),
    chu = suppressWarnings(as.numeric(.data$chu)),
    dai = suppressWarnings(as.numeric(.data$dai)),
    toku = suppressWarnings(as.numeric(.data$toku)),
    tokudai = suppressWarnings(as.numeric(.data$tokudai)),
    count_total = suppressWarnings(as.numeric(.data$count_total)),
    flag_use_for_main_glmm = as.logical(.data$flag_use_for_main_glmm)
  )

required_cols <- c(
  "year", "month", "area", "vessel", "effort_glmm", "depth_glmm",
  "chu", "dai", "toku", "tokudai", "count_total", "flag_use_for_main_glmm"
)
missing_cols <- setdiff(required_cols, names(glmm_input))

if (length(missing_cols) > 0) {
  stop("Required columns are missing: ", paste(missing_cols, collapse = ", "))
}

depth_boundary_info <- determine_depth_boundaries(glmm_input)

readr::write_csv(depth_boundary_info$definition_tbl, path_depth_definition)
readr::write_csv(depth_boundary_info$summary_tbl, path_depth_summary)

cat("depth category boundary =", depth_boundary_info$c1, ",", depth_boundary_info$c2, "\n")
cat(
  "depth category counts =",
  paste0(depth_boundary_info$summary_tbl$depth_cat, ": ", depth_boundary_info$summary_tbl$n, collapse = ", "),
  "\n"
)

candidate_model_specs <- build_candidate_model_specs()
all_compare_results_tbl <- tibble::tibble()
best_model_table_tbl <- tibble::tibble()
best_model_summary_tbl <- tibble::tibble()
workflow_results <- list()

for (response_name in response_levels) {
  response_result <- tryCatch(
    {
      compare_tbl <- make_compare_dataset(glmm_input, response_name, depth_boundary_info)
      compare_dataset_rows <- nrow(compare_tbl)
      compare_results_tbl <- tibble::tibble()
      compare_fit_list <- list()

      cat("response =", response_name, "| compare dataset rows =", compare_dataset_rows, "\n")
      cat("response =", response_name, "| depth category boundary =", depth_boundary_info$c1, ",", depth_boundary_info$c2, "\n")

      for (spec_i in candidate_model_specs) {
        fit_out_i <- safe_fit_glmmTMB(
          model_id = spec_i$model_id,
          formula_obj = stats::as.formula(spec_i$formula_text),
          data_obj = compare_tbl
        )

        compare_fit_list[[spec_i$model_id]] <- fit_out_i
        compare_results_tbl <- dplyr::bind_rows(
          compare_results_tbl,
          extract_model_metrics(response_name, spec_i, fit_out_i)
        )
      }

      min_aic <- if (any(is.finite(compare_results_tbl$AIC))) {
        min(compare_results_tbl$AIC, na.rm = TRUE)
      } else {
        NA_real_
      }

      compare_results_tbl <- compare_results_tbl |>
        dplyr::mutate(
          delta_AIC = if (is.finite(min_aic)) .data$AIC - min_aic else NA_real_
        ) |>
        dplyr::select(
          "response", "model_id", "model_label", "status", "nobs", "AIC", "BIC", "logLik",
          "converged", "pdHess", "warning_message", "delta_AIC",
          "uses_depth", "depth_mode", "random_mode", "fixed_mode"
        ) |>
        dplyr::arrange(.data$delta_AIC, .data$model_id)

      compare_csv_path <- file.path("output", "tables", paste0("aic_compare_", response_name, ".csv"))
      readr::write_csv(compare_results_tbl, compare_csv_path)

      cat(
        "response =", response_name, "| fitted candidate model ids =",
        paste(compare_results_tbl$model_id, collapse = ", "),
        "\n"
      )

      best_model_row <- select_best_model(compare_results_tbl)

      if (is.null(best_model_row)) {
        best_model_table_row <- tibble::tibble(
          response = response_name,
          best_model_id = NA_character_,
          best_model_label = NA_character_,
          AIC = NA_real_,
          delta_AIC = NA_real_,
          nobs_compare = NA_integer_,
          uses_depth = NA,
          depth_mode = NA_character_,
          random_mode = NA_character_,
          fixed_mode = NA_character_,
          compare_dataset_rows = compare_dataset_rows,
          final_dataset_rows = NA_integer_
        )

        best_model_table_tbl <- dplyr::bind_rows(best_model_table_tbl, best_model_table_row)
        all_compare_results_tbl <- dplyr::bind_rows(all_compare_results_tbl, compare_results_tbl)

        cat("response =", response_name, "| best model id = none\n")
        cat("response =", response_name, "| best model uses depth = none\n")
        cat("response =", response_name, "| final dataset rows = skipped\n")
        cat("response =", response_name, "| saved file paths =", compare_csv_path, "\n")

        return(list(
          response = response_name,
          compare_tbl = compare_tbl,
          compare_results_tbl = compare_results_tbl,
          best_model_table_row = best_model_table_row,
          best_model_summary_row = NULL
        ))
      }

      best_spec <- candidate_model_specs[[match(best_model_row$model_id, vapply(candidate_model_specs, `[[`, character(1), "model_id"))]]
      final_tbl <- make_final_dataset(
        glmm_input = glmm_input,
        response_name = response_name,
        uses_depth = best_spec$uses_depth,
        depth_boundary_info = depth_boundary_info,
        compare_tbl = compare_tbl
      )
      final_dataset_rows <- nrow(final_tbl)

      final_fit_out <- safe_fit_glmmTMB(
        model_id = paste0(best_model_row$model_id, "_final"),
        formula_obj = stats::as.formula(best_spec$formula_text),
        data_obj = final_tbl
      )

      if (is.null(final_fit_out$fit)) {
        stop("Final refit failed: ", combine_messages(final_fit_out$warning_message, final_fit_out$error_message))
      }

      model_rds_path <- file.path("output", "models", paste0("fit_best_", response_name, ".rds"))
      year_index_csv_path <- file.path("output", "tables", paste0("year_index_best_", response_name, ".csv"))
      raw_cpue_csv_path <- file.path("output", "tables", paste0("raw_cpue_best_", response_name, ".csv"))
      detail_txt_path <- file.path("output", "tables", paste0("best_model_detail_", response_name, ".txt"))
      dharma_csv_path <- file.path("output", "tables", paste0("dharma_best_", response_name, ".csv"))
      index_fig_path <- file.path("output", "figures", paste0("index_best_", response_name, ".png"))
      index_ci_fig_path <- file.path("output", "figures", paste0("index_best_", response_name, "_ci.png"))
      overlay_fig_path <- file.path("output", "figures", paste0("overlay_best_", response_name, ".png"))
      dharma_fig_path <- file.path("output", "figures", paste0("diagnostic_best_", response_name, ".png"))

      year_index_tbl <- compute_year_index_table(
        model_obj = final_fit_out$fit,
        data_obj = final_tbl,
        depth_mode = best_spec$depth_mode,
        depth_reference_level = depth_boundary_info$reference_level
      )
      standardized_index_tbl <- build_standardized_index_table(
        tbl = year_index_tbl,
        response_name = response_name,
        best_model_id = best_model_row$model_id
      )
      raw_cpue_tbl <- make_raw_cpue_table(final_tbl, response_name)
      overlay_tbl <- build_overlay_table(
        index_tbl = standardized_index_tbl,
        raw_cpue_tbl = raw_cpue_tbl,
        response_name = response_name,
        best_model_id = best_model_row$model_id
      )
      dharma_out <- run_dharma_dispersion(
        fit_obj = final_fit_out$fit,
        optional_pkgs = optional_pkgs,
        output_plot_path = dharma_fig_path
      )

      saveRDS(final_fit_out$fit, model_rds_path)
      readr::write_csv(standardized_index_tbl, year_index_csv_path)
      readr::write_csv(raw_cpue_tbl, raw_cpue_csv_path)
      write_best_model_detail(final_fit_out$fit, detail_txt_path)
      readr::write_csv(
        tibble::tibble(
          response = response_name,
          best_model_id = best_model_row$model_id,
          status = dharma_out$status,
          dispersion = dharma_out$dispersion,
          dispersion_p = dharma_out$dispersion_p,
          warning_message = dharma_out$warning_message
        ),
        dharma_csv_path
      )
      plot_year_index(standardized_index_tbl, index_fig_path, paste(response_name, "best model index"), show_ci = FALSE)
      plot_year_index(standardized_index_tbl, index_ci_fig_path, paste(response_name, "best model index with CI"), show_ci = TRUE)
      plot_overlay(overlay_tbl, overlay_fig_path, paste(response_name, "raw relative CPUE vs standardized index"))

      best_model_table_row <- tibble::tibble(
        response = response_name,
        best_model_id = best_model_row$model_id,
        best_model_label = best_model_row$model_label,
        AIC = best_model_row$AIC,
        delta_AIC = best_model_row$delta_AIC,
        nobs_compare = best_model_row$nobs,
        uses_depth = best_model_row$uses_depth,
        depth_mode = best_model_row$depth_mode,
        random_mode = best_model_row$random_mode,
        fixed_mode = best_model_row$fixed_mode,
        compare_dataset_rows = compare_dataset_rows,
        final_dataset_rows = final_dataset_rows
      )

      best_model_summary_row <- build_best_model_summary_row(
        response_name = response_name,
        best_model_row = best_model_row,
        final_fit_out = final_fit_out,
        final_dataset_rows = final_dataset_rows,
        compare_dataset_rows = compare_dataset_rows
      )

      best_model_table_tbl <- dplyr::bind_rows(best_model_table_tbl, best_model_table_row)
      best_model_summary_tbl <- dplyr::bind_rows(best_model_summary_tbl, best_model_summary_row)
      all_compare_results_tbl <- dplyr::bind_rows(all_compare_results_tbl, compare_results_tbl)

      cat("response =", response_name, "| best model id =", best_model_row$model_id, "\n")
      cat("response =", response_name, "| best model uses depth =", best_model_row$uses_depth, "\n")
      cat("response =", response_name, "| final dataset rows =", final_dataset_rows, "\n")
      cat(
        "response =", response_name, "| saved file paths =",
        paste(c(compare_csv_path, model_rds_path, year_index_csv_path, index_fig_path, index_ci_fig_path, overlay_fig_path), collapse = ", "),
        "\n"
      )

      list(
        response = response_name,
        compare_tbl = compare_tbl,
        final_tbl = final_tbl,
        compare_results_tbl = compare_results_tbl,
        best_model_table_row = best_model_table_row,
        best_model_summary_row = best_model_summary_row
      )
    },
    error = function(e) {
      warning_message <- conditionMessage(e)
      failed_compare_tbl <- tibble::tibble(
        response = response_name,
        model_id = NA_character_,
        model_label = NA_character_,
        status = "response_failed",
        nobs = NA_integer_,
        AIC = NA_real_,
        BIC = NA_real_,
        logLik = NA_real_,
        converged = NA,
        pdHess = NA,
        warning_message = warning_message,
        delta_AIC = NA_real_,
        uses_depth = NA,
        depth_mode = NA_character_,
        random_mode = NA_character_,
        fixed_mode = NA_character_
      )

      failed_best_tbl <- tibble::tibble(
        response = response_name,
        best_model_id = NA_character_,
        best_model_label = NA_character_,
        AIC = NA_real_,
        delta_AIC = NA_real_,
        nobs_compare = NA_integer_,
        uses_depth = NA,
        depth_mode = NA_character_,
        random_mode = NA_character_,
        fixed_mode = NA_character_,
        compare_dataset_rows = NA_integer_,
        final_dataset_rows = NA_integer_
      )

      compare_csv_path <- file.path("output", "tables", paste0("aic_compare_", response_name, ".csv"))
      readr::write_csv(failed_compare_tbl, compare_csv_path)

      all_compare_results_tbl <<- dplyr::bind_rows(all_compare_results_tbl, failed_compare_tbl)
      best_model_table_tbl <<- dplyr::bind_rows(best_model_table_tbl, failed_best_tbl)

      cat("response =", response_name, "| failed =", warning_message, "\n")
      cat("response =", response_name, "| saved file paths =", compare_csv_path, "\n")

      list(
        response = response_name,
        compare_tbl = NULL,
        final_tbl = NULL,
        compare_results_tbl = failed_compare_tbl,
        best_model_table_row = failed_best_tbl,
        best_model_summary_row = NULL
      )
    }
  )

  workflow_results[[response_name]] <- response_result
}

readr::write_csv(all_compare_results_tbl, path_aic_compare_all)
readr::write_csv(best_model_table_tbl, path_best_model_table)
readr::write_csv(best_model_summary_tbl, path_best_model_summary)
saveRDS(
  list(
    depth_boundary_info = depth_boundary_info,
    all_compare_results = all_compare_results_tbl,
    best_model_table = best_model_table_tbl,
    best_model_summary = best_model_summary_tbl,
    per_response = workflow_results
  ),
  path_results_rds
)

cat("saved csv path =", path_depth_definition, "\n")
cat("saved csv path =", path_depth_summary, "\n")
cat("saved csv path =", path_aic_compare_all, "\n")
cat("saved csv path =", path_best_model_table, "\n")
cat("saved csv path =", path_best_model_summary, "\n")
cat("saved rds path =", path_results_rds, "\n")
cat("GLMM workflow completed.\n")
