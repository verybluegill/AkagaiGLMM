# =========================================
# 04_fit_glmm_by_size.R
# GLMM-ready CSV を読み込み、主要4サイズの size-specific response に対して
# NB-GLMM を当て、年効果表・年指数図・モデルRDS・分散要約を保存する
# =========================================

source(file.path("R", "00_load_packages.R"))

optional_pkgs <- load_project_packages(
  required_pkgs = c("tidyverse", "glmmTMB"),
  optional_pkgs = c("emmeans", "DHARMa")
)

ensure_project_dirs()

input_path <- file.path("data_processed", "akagai_glmm_input.csv")
variance_summary_path <- file.path("output", "tables", "glmm_by_size_variance_summary.csv")
run_status_path <- file.path("output", "tables", "glmm_by_size_run_status.csv")
row_summary_by_year_path <- file.path("output", "tables", "check_by_size_glmm_row_summary_by_year.csv")

if (!file.exists(input_path)) {
  stop("Input file not found: ", input_path)
}

glmm_input <- readr::read_csv(input_path, show_col_types = FALSE)

required_cols <- c("year", "month", "area", "vessel", "effort_glmm", "depth_glmm", "chu", "dai", "toku", "tokudai")
missing_cols <- setdiff(required_cols, names(glmm_input))

if (length(missing_cols) > 0) {
  cat("Missing required columns:\n")
  print(missing_cols)
  stop("Required columns are missing.")
}

glmm_input <- glmm_input |>
  mutate(
    effort_glmm = as.numeric(effort_glmm),
    depth_glmm = as.numeric(depth_glmm)
  )

if ("flag_use_for_main_glmm" %in% names(glmm_input)) {
  glmm_input <- glmm_input |>
    mutate(flag_use_for_main_glmm = as.logical(flag_use_for_main_glmm))
} else {
  glmm_input <- glmm_input |>
    mutate(
      flag_use_for_main_glmm = year %in% 2020:2024 &
        !is.na(area) &
        area != "" &
        !is.na(effort_glmm) &
        is.finite(effort_glmm) &
        effort_glmm > 0
    )
}

n_input <- nrow(glmm_input)
glmm_dat <- glmm_input |>
  filter(flag_use_for_main_glmm) |>
  mutate(
    year = factor(year),
    month = factor(month, levels = sort(unique(month))),
    area = factor(area),
    vessel = factor(vessel),
    effort_glmm = as.numeric(effort_glmm)
  )

glmm_dat_depth <- glmm_dat |>
  filter(!is.na(depth_glmm), is.finite(depth_glmm))

n_used_main <- nrow(glmm_dat)
n_dropped <- n_input - n_used_main
dropped_prop <- if (n_input == 0) NA_real_ else n_dropped / n_input
n_used_depth <- nrow(glmm_dat_depth)
n_dropped_for_depth <- n_used_main - n_used_depth
dropped_prop_for_depth <- if (n_used_main == 0) NA_real_ else n_dropped_for_depth / n_used_main

depth_glmm_mean <- mean(glmm_dat_depth$depth_glmm, na.rm = TRUE)
depth_glmm_sd <- sd(glmm_dat_depth$depth_glmm, na.rm = TRUE)

if (!is.finite(depth_glmm_sd) || depth_glmm_sd <= 0) {
  stop("depth_glmm_sd must be finite and > 0 on glmm_dat_depth.")
}

glmm_dat_depth <- glmm_dat_depth |>
  mutate(depth_glmm_sc = (depth_glmm - depth_glmm_mean) / depth_glmm_sd)

row_summary_by_year <- glmm_input |>
  group_by(year) |>
  summarise(
    n_input = n(),
    n_used = sum(flag_use_for_main_glmm),
    n_dropped = n_input - n_used,
    .groups = "drop"
  )

write_csv(row_summary_by_year, row_summary_by_year_path)

cat("\n=== GLMM row summary ===\n")
cat("n_input=", n_input, "\n", sep = "")
cat("n_used_main=", n_used_main, "\n", sep = "")
cat("n_used_depth=", n_used_depth, "\n", sep = "")
cat("n_dropped=", n_dropped, "\n", sep = "")
cat("dropped_prop=", dropped_prop, "\n", sep = "")
cat("n_dropped_for_depth=", n_dropped_for_depth, "\n", sep = "")
cat("dropped_prop_for_depth=", dropped_prop_for_depth, "\n", sep = "")
cat("depth_glmm_mean=", depth_glmm_mean, "\n", sep = "")
cat("depth_glmm_sd=", depth_glmm_sd, "\n", sep = "")
print(row_summary_by_year, n = nrow(row_summary_by_year))

depth_row_summary <- tibble(
  n_input = n_input,
  n_used_main = n_used_main,
  n_used_depth = n_used_depth,
  n_dropped_for_depth = n_dropped_for_depth,
  dropped_prop_for_depth = dropped_prop_for_depth
)

cat("\n=== depth row summary ===\n")
print(depth_row_summary)

if (n_used_main == 0) {
  stop("No rows available for the by-size GLMM after filtering.")
}

if (n_used_depth == 0) {
  stop("No rows available for the by-size GLMM depth comparison.")
}

if (any(is.na(glmm_dat$effort_glmm)) || any(!is.finite(glmm_dat$effort_glmm)) || any(glmm_dat$effort_glmm <= 0)) {
  stop("effort_glmm must be finite and > 0 in the filtered data.")
}

size_vars <- c("chu", "dai", "toku", "tokudai")

fit_results <- vector("list", length(size_vars))
names(fit_results) <- size_vars

variance_summary_tbl <- tibble(
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

run_status_tbl <- tibble(
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

build_year_index_fallback <- function(fit_obj, data_obj) {
  pred_grid <- expand_grid(
    year = factor(levels(data_obj$year), levels = levels(data_obj$year)),
    month = factor(levels(data_obj$month), levels = levels(data_obj$month)),
    area = factor(levels(data_obj$area)[1], levels = levels(data_obj$area)),
    vessel = factor(levels(data_obj$vessel)[1], levels = levels(data_obj$vessel)),
    effort_glmm = 1
  )

  if ("depth_glmm_sc" %in% names(data_obj)) {
    pred_grid <- pred_grid |>
      mutate(depth_glmm_sc = data_obj$depth_glmm_sc[[1]])
  }

  pred_grid |>
    mutate(predicted = predict(fit_obj, newdata = pred_grid, type = "response", re.form = NA)) |>
    group_by(year) |>
    summarise(response = mean(predicted), .groups = "drop")
}

safe_fit_glmmTMB <- function(formula, data, model_name) {
  warning_messages <- character()

  fit_obj <- tryCatch(
    withCallingHandlers(
      glmmTMB(
        formula = formula,
        family = nbinom2(),
        data = data
      ),
      warning = function(w) {
        warning_messages <<- c(warning_messages, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      structure(
        list(
          message = conditionMessage(e)
        ),
        class = "fit_error"
      )
    }
  )

  if (inherits(fit_obj, "fit_error")) {
    return(list(
      model_name = model_name,
      fit_ok = FALSE,
      fit = NULL,
      warning_message = paste(unique(c(warning_messages, fit_obj$message)), collapse = " | ")
    ))
  }

  list(
    model_name = model_name,
    fit_ok = TRUE,
    fit = fit_obj,
    warning_message = paste(unique(warning_messages), collapse = " | ")
  )
}

extract_fit_metrics <- function(fit_out, depth_degree, size_name) {
  fit_obj <- fit_out$fit

  if (!isTRUE(fit_out$fit_ok) || is.null(fit_obj)) {
    return(tibble(
      size = size_name,
      model_name = fit_out$model_name,
      depth_degree = depth_degree,
      AIC = NA_real_,
      BIC = NA_real_,
      logLik = NA_real_,
      nobs = NA_real_,
      converged = NA,
      pdHess = NA,
      fit_ok = FALSE,
      warning_message = fit_out$warning_message
    ))
  }

  tibble(
    size = size_name,
    model_name = fit_out$model_name,
    depth_degree = depth_degree,
    AIC = tryCatch(AIC(fit_obj), error = function(e) NA_real_),
    BIC = tryCatch(BIC(fit_obj), error = function(e) NA_real_),
    logLik = tryCatch(as.numeric(logLik(fit_obj)), error = function(e) NA_real_),
    nobs = tryCatch(as.numeric(stats::nobs(fit_obj)), error = function(e) NA_real_),
    converged = tryCatch(fit_obj$fit$convergence == 0, error = function(e) NA),
    pdHess = tryCatch(isTRUE(fit_obj$sdr$pdHess), error = function(e) NA),
    fit_ok = TRUE,
    warning_message = fit_out$warning_message
  )
}

for (sv in size_vars) {
  cat("\n====================\n")
  cat("size = ", sv, "\n", sep = "")

  model_path_i <- file.path("output", "models", paste0("fit_nb_", sv, ".rds"))
  emm_year_path_i <- file.path("output", "tables", paste0("emm_year_", sv, ".csv"))
  index_fig_path_i <- file.path("output", "figures", paste0("index_", sv, "_by_year.png"))

  comparison_path_i <- file.path("output", "tables", paste0("model_comparison_by_size_", sv, ".csv"))
  depth_effect_path_i <- file.path("output", "tables", paste0("depth_effect_", sv, ".csv"))
  depth_effect_fig_path_i <- file.path("output", "figures", paste0("depth_effect_", sv, "_best_model.png"))

  fit_out <- tryCatch(
    {
      response_vec <- glmm_dat_depth[[sv]]

      if (any(is.na(response_vec))) {
        stop(sv, " contains NA.")
      }

      response_vec <- as.numeric(response_vec)

      if (any(is.na(response_vec)) || any(!is.finite(response_vec))) {
        stop(sv, " must be finite numeric.")
      }

      glmm_dat_i <- glmm_dat_depth
      glmm_dat_i[[sv]] <- response_vec

        # size-specific response に対して total と同じ線形予測子を使う
        fit_formula <- as.formula(
          paste0(sv, " ~ year + month + offset(log(effort_glmm)) + (1 | area) + (1 | vessel)")
        )

      fit_formula_depth0 <- as.formula(
        paste0(sv, " ~ year + month + offset(log(effort_glmm)) + (1 | area) + (1 | vessel)")
      )
      fit_formula_depth1 <- as.formula(
        paste0(sv, " ~ year + month + depth_glmm_sc + offset(log(effort_glmm)) + (1 | area) + (1 | vessel)")
      )
      fit_formula_depth2 <- as.formula(
        paste0(sv, " ~ year + month + depth_glmm_sc + I(depth_glmm_sc^2) + offset(log(effort_glmm)) + (1 | area) + (1 | vessel)")
      )

      fit_depth0 <- safe_fit_glmmTMB(fit_formula_depth0, glmm_dat_i, paste0("fit_", sv, "_depth0"))
      fit_depth1 <- safe_fit_glmmTMB(fit_formula_depth1, glmm_dat_i, paste0("fit_", sv, "_depth1"))
      fit_depth2 <- safe_fit_glmmTMB(fit_formula_depth2, glmm_dat_i, paste0("fit_", sv, "_depth2"))

      comparison_tbl <- bind_rows(
        extract_fit_metrics(fit_depth0, 0L, sv),
        extract_fit_metrics(fit_depth1, 1L, sv),
        extract_fit_metrics(fit_depth2, 2L, sv)
      ) |>
        arrange(AIC)

      write_csv(comparison_tbl, comparison_path_i)

      cat("\n=== model comparison ===\n")
      print(comparison_tbl, n = nrow(comparison_tbl))

      nobs_unique <- comparison_tbl |>
        filter(fit_ok, !is.na(nobs)) |>
        pull(nobs) |>
        unique()

      cat("\n=== nobs check for AIC comparison ===\n")
      cat("nobs_unique=", paste(nobs_unique, collapse = ", "), "\n", sep = "")

      if (length(nobs_unique) > 1) {
        warning("nobs_unique has multiple values in AIC comparison.")
      }

      successful_models <- comparison_tbl |>
        filter(fit_ok, !is.na(AIC))

      if (nrow(successful_models) == 0) {
        stop("All candidate models failed: ", sv)
      }

      best_model_name_i <- successful_models$model_name[[1]]
      best_depth_degree_i <- successful_models$depth_degree[[1]]
      best_model_obj <- if (identical(best_model_name_i, fit_depth0$model_name)) {
        fit_depth0$fit
      } else if (identical(best_model_name_i, fit_depth1$model_name)) {
        fit_depth1$fit
      } else {
        fit_depth2$fit
      }
      best_warning_message_i <- successful_models$warning_message[[1]]

      cat("best_model_name=", best_model_name_i, "\n", sep = "")
      cat("fit_", sv, "_depth0_pdHess=", comparison_tbl$pdHess[comparison_tbl$model_name == paste0("fit_", sv, "_depth0")], "\n", sep = "")
      cat("fit_", sv, "_depth1_pdHess=", comparison_tbl$pdHess[comparison_tbl$model_name == paste0("fit_", sv, "_depth1")], "\n", sep = "")
      cat("fit_", sv, "_depth2_pdHess=", comparison_tbl$pdHess[comparison_tbl$model_name == paste0("fit_", sv, "_depth2")], "\n", sep = "")
      cat("fit_", sv, "_depth0_convergence=", comparison_tbl$converged[comparison_tbl$model_name == paste0("fit_", sv, "_depth0")], "\n", sep = "")
      cat("fit_", sv, "_depth1_convergence=", comparison_tbl$converged[comparison_tbl$model_name == paste0("fit_", sv, "_depth1")], "\n", sep = "")
      cat("fit_", sv, "_depth2_convergence=", comparison_tbl$converged[comparison_tbl$model_name == paste0("fit_", sv, "_depth2")], "\n", sep = "")

      saveRDS(best_model_obj, model_path_i)

      cat("\n=== model summary ===\n")
      best_summary <- summary(best_model_obj)
      print(best_summary)

      if (isTRUE(optional_pkgs[["DHARMa"]])) {
        cat("\n=== DHARMa residual diagnostics ===\n")
        dharma_out <- try(
          DHARMa::testDispersion(DHARMa::simulateResiduals(best_model_obj, plot = FALSE)),
          silent = TRUE
        )
        print(dharma_out)
      }

      emm_at <- list(effort_glmm = 1)
      if (best_depth_degree_i > 0) {
        emm_at$depth_glmm_sc <- 0
      }

      if (isTRUE(optional_pkgs[["emmeans"]])) {
        emm_year_i <- emmeans::emmeans(best_model_obj, specs = ~ year, at = emm_at, type = "response")
        emm_year_tbl <- as.data.frame(emm_year_i)
      } else {
        fallback_dat <- glmm_dat_i
        if (best_depth_degree_i > 0) {
          fallback_dat <- fallback_dat |>
            mutate(depth_glmm_sc = 0)
        }
        emm_year_tbl <- build_year_index_fallback(best_model_obj, fallback_dat)
      }

      cat("\n=== year effect table ===\n")
      print(emm_year_tbl)

      write_csv(emm_year_tbl, emm_year_path_i)

      year_index_col <- intersect(c("response", "rate", "prob", "emmean"), names(emm_year_tbl))

      if (length(year_index_col) == 0) {
        stop("Failed to identify the year effect column for plotting: ", sv)
      }

      year_index_col <- year_index_col[[1]]

      index_by_year <- ggplot(emm_year_tbl, aes(x = year, y = .data[[year_index_col]], group = 1)) +
        geom_line() +
        geom_point(size = 2) +
        labs(
          x = "Year",
          y = "Index"
        )

      if (all(c("lower.CL", "upper.CL") %in% names(emm_year_tbl))) {
        index_by_year <- index_by_year +
          geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15)
      }

      ggsave(
        filename = index_fig_path_i,
        plot = index_by_year,
        width = 10,
        height = 6,
        dpi = 150
      )

      coef_cond <- as.data.frame(best_summary$coefficients$cond)
      coef_cond$term <- rownames(coef_cond)
      rownames(coef_cond) <- NULL

      depth_effect_tbl <- coef_cond |>
        filter(term %in% c("depth_glmm_sc", "I(depth_glmm_sc^2)")) |>
        transmute(
          size = sv,
          term = term,
          estimate = Estimate,
          std_error = `Std. Error`,
          z_value = `z value`,
          p_value = `Pr(>|z|)`,
          depth_glmm_mean = depth_glmm_mean,
          depth_glmm_sd = depth_glmm_sd,
          best_model_name = best_model_name_i
        )

      if (nrow(depth_effect_tbl) == 0) {
        depth_effect_tbl <- tibble(
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

      write_csv(depth_effect_tbl, depth_effect_path_i)

      depth_effect_fig_path_saved <- NA_character_

      if (best_depth_degree_i == 2L) {
        depth_plot_dat <- tibble(
          year = factor(levels(glmm_dat_i$year)[1], levels = levels(glmm_dat_i$year)),
          month = factor(levels(glmm_dat_i$month)[1], levels = levels(glmm_dat_i$month)),
          area = factor(levels(glmm_dat_i$area)[1], levels = levels(glmm_dat_i$area)),
          vessel = factor(levels(glmm_dat_i$vessel)[1], levels = levels(glmm_dat_i$vessel)),
          effort_glmm = 1,
          depth_glmm_sc = seq(
            min(glmm_dat_i$depth_glmm_sc, na.rm = TRUE),
            max(glmm_dat_i$depth_glmm_sc, na.rm = TRUE),
            length.out = 200
          )
        )

        depth_plot_dat <- depth_plot_dat |>
          mutate(predicted = predict(best_model_obj, newdata = depth_plot_dat, type = "response", re.form = NA))

        ref_pred <- predict(
          best_model_obj,
          newdata = tibble(
            year = factor(levels(glmm_dat_i$year)[1], levels = levels(glmm_dat_i$year)),
            month = factor(levels(glmm_dat_i$month)[1], levels = levels(glmm_dat_i$month)),
            area = factor(levels(glmm_dat_i$area)[1], levels = levels(glmm_dat_i$area)),
            vessel = factor(levels(glmm_dat_i$vessel)[1], levels = levels(glmm_dat_i$vessel)),
            effort_glmm = 1,
            depth_glmm_sc = 0
          ),
          type = "response",
          re.form = NA
        )

        depth_plot_dat <- depth_plot_dat |>
          mutate(relative_index = predicted / as.numeric(ref_pred[[1]]))

        depth_effect_fig <- ggplot(depth_plot_dat, aes(x = depth_glmm_sc, y = relative_index)) +
          geom_line() +
          labs(
            x = "Standardized depth",
            y = "Relative index"
          )

        ggsave(
          filename = depth_effect_fig_path_i,
          plot = depth_effect_fig,
          width = 10,
          height = 6,
          dpi = 150
        )

        depth_effect_fig_path_saved <- depth_effect_fig_path_i
      }

      vc_cond <- VarCorr(best_model_obj)$cond
      area_stats <- extract_re_sd(vc_cond, "area")
      vessel_stats <- extract_re_sd(vc_cond, "vessel")
      disp_parameter <- tryCatch(as.numeric(sigma(best_model_obj)), error = function(e) NA_real_)
      fit_convergence_code <- tryCatch(best_model_obj$fit$convergence, error = function(e) NA_integer_)
      fit_pdHess <- tryCatch(isTRUE(best_model_obj$sdr$pdHess), error = function(e) NA)
      converged_flag <- tryCatch(best_model_obj$fit$convergence == 0, error = function(e) NA)

      if (isTRUE(fit_convergence_code != 0) || !isTRUE(fit_pdHess)) {
        warning(paste0("Convergence issue may exist in ", best_model_name_i))
      }

      depth_linear_estimate_i <- depth_effect_tbl |>
        filter(term == "depth_glmm_sc") |>
        pull(estimate)

      depth_quadratic_estimate_i <- depth_effect_tbl |>
        filter(term == "I(depth_glmm_sc^2)") |>
        pull(estimate)

      variance_summary_tbl <<- bind_rows(
        variance_summary_tbl,
        tibble(
          size = sv,
          n_obs = nrow(glmm_dat_i),
          n_zero = sum(glmm_dat_i[[sv]] == 0),
          zero_ratio = mean(glmm_dat_i[[sv]] == 0),
          aic = tryCatch(AIC(best_model_obj), error = function(e) NA_real_),
          logLik = tryCatch(as.numeric(logLik(best_model_obj)), error = function(e) NA_real_),
          disp_parameter = disp_parameter,
          area_variance = unname(area_stats[["variance"]]),
          area_sd = unname(area_stats[["sd"]]),
          vessel_variance = unname(vessel_stats[["variance"]]),
          vessel_sd = unname(vessel_stats[["sd"]]),
          best_model_name = best_model_name_i,
          depth_degree = best_depth_degree_i,
          depth_linear_estimate = if (length(depth_linear_estimate_i) == 0) NA_real_ else depth_linear_estimate_i[[1]],
          depth_quadratic_estimate = if (length(depth_quadratic_estimate_i) == 0) NA_real_ else depth_quadratic_estimate_i[[1]],
          depth_glmm_mean = depth_glmm_mean,
          depth_glmm_sd = depth_glmm_sd,
          converged = converged_flag,
          fit_convergence_code = fit_convergence_code,
          pdHess = fit_pdHess,
          warning_message = best_warning_message_i
        )
      )

      run_status_tbl <<- run_status_tbl |>
        mutate(
          status = if_else(size == sv, "success", status),
          error_message = if_else(size == sv, NA_character_, error_message),
          warning_message = if_else(size == sv, best_warning_message_i, warning_message),
          model_path = if_else(size == sv, model_path_i, model_path),
          emm_year_path = if_else(size == sv, emm_year_path_i, emm_year_path),
          index_fig_path = if_else(size == sv, index_fig_path_i, index_fig_path),
          best_model_name = if_else(size == sv, best_model_name_i, best_model_name),
          best_depth_degree = if_else(size == sv, best_depth_degree_i, best_depth_degree),
          comparison_path = if_else(size == sv, comparison_path_i, comparison_path),
          depth_effect_path = if_else(size == sv, depth_effect_path_i, depth_effect_path),
          depth_effect_fig_path = if_else(size == sv, depth_effect_fig_path_saved, depth_effect_fig_path)
        )

      cat("\n=== saved files ===\n")
      cat(model_path_i, "\n", sep = "")
      cat(emm_year_path_i, "\n", sep = "")
      cat(index_fig_path_i, "\n", sep = "")
      cat(comparison_path_i, "\n", sep = "")
      cat(depth_effect_path_i, "\n", sep = "")
      if (!is.na(depth_effect_fig_path_saved)) {
        cat(depth_effect_fig_path_saved, "\n", sep = "")
      }

      list(
        fit = best_model_obj,
        emm_year_tbl = emm_year_tbl,
        model_path = model_path_i,
        emm_year_path = emm_year_path_i,
        index_fig_path = index_fig_path_i,
        comparison_tbl = comparison_tbl,
        comparison_path = comparison_path_i,
        depth_effect_path = depth_effect_path_i,
        depth_effect_fig_path = depth_effect_fig_path_saved,
        best_model_name = best_model_name_i,
        best_depth_degree = best_depth_degree_i
      )
    },
    error = function(e) {
      run_status_tbl <<- run_status_tbl |>
        mutate(
          status = if_else(size == sv, "failed", status),
          error_message = if_else(size == sv, conditionMessage(e), error_message),
          warning_message = if_else(size == sv, NA_character_, warning_message),
          model_path = if_else(size == sv, model_path_i, model_path),
          emm_year_path = if_else(size == sv, emm_year_path_i, emm_year_path),
          index_fig_path = if_else(size == sv, index_fig_path_i, index_fig_path),
          comparison_path = if_else(size == sv, comparison_path_i, comparison_path),
          depth_effect_path = if_else(size == sv, depth_effect_path_i, depth_effect_path),
          depth_effect_fig_path = if_else(size == sv, depth_effect_fig_path_i, depth_effect_fig_path)
        )

      cat("\n=== error ===\n")
      cat(conditionMessage(e), "\n", sep = "")
      NULL
    }
  )

  fit_results[[sv]] <- fit_out
}

write_csv(variance_summary_tbl, variance_summary_path)
write_csv(run_status_tbl, run_status_path)

success_sizes <- run_status_tbl |>
  filter(status == "success") |>
  pull(size)

failed_sizes <- run_status_tbl |>
  filter(status == "failed") |>
  pull(size)

cat("\n====================\n")
cat("successful sizes:\n")
print(success_sizes)

cat("\nfailed sizes:\n")
print(failed_sizes)

cat("\n=== variance summary path ===\n")
cat(variance_summary_path, "\n", sep = "")

cat("\n=== run status path ===\n")
cat(run_status_path, "\n", sep = "")

cat("\n=== row summary path ===\n")
cat(row_summary_by_year_path, "\n", sep = "")
