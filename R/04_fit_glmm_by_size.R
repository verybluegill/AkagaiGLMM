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

required_cols <- c("year", "month", "area", "vessel", "effort_glmm", "chu", "dai", "toku", "tokudai")
missing_cols <- setdiff(required_cols, names(glmm_input))

if (length(missing_cols) > 0) {
  cat("Missing required columns:\n")
  print(missing_cols)
  stop("Required columns are missing.")
}

glmm_input <- glmm_input |>
  mutate(
    effort_glmm = as.numeric(effort_glmm)
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

n_used <- nrow(glmm_dat)
n_dropped <- n_input - n_used
dropped_prop <- if (n_input == 0) NA_real_ else n_dropped / n_input

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
cat("n_used=", n_used, "\n", sep = "")
cat("n_dropped=", n_dropped, "\n", sep = "")
cat("dropped_prop=", dropped_prop, "\n", sep = "")
print(row_summary_by_year, n = nrow(row_summary_by_year))

if (n_used == 0) {
  stop("No rows available for the by-size GLMM after filtering.")
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
  converged = logical(),
  fit_convergence_code = integer(),
  pdHess = logical(),
  warning_message = character()
)

run_status_tbl <- tibble(
  size = size_vars,
  status = "not_run",
  error_message = NA_character_,
  model_path = NA_character_,
  emm_year_path = NA_character_,
  index_fig_path = NA_character_
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

  pred_grid |>
    mutate(predicted = predict(fit_obj, newdata = pred_grid, type = "response", re.form = NA)) |>
    group_by(year) |>
    summarise(response = mean(predicted), .groups = "drop")
}

for (sv in size_vars) {
  cat("\n====================\n")
  cat("size = ", sv, "\n", sep = "")

  model_path_i <- file.path("output", "models", paste0("fit_nb_", sv, ".rds"))
  emm_year_path_i <- file.path("output", "tables", paste0("emm_year_", sv, ".csv"))
  index_fig_path_i <- file.path("output", "figures", paste0("index_", sv, "_by_year.png"))

  warning_messages <- character()

  fit_out <- tryCatch(
    withCallingHandlers(
      {
        response_vec <- glmm_dat[[sv]]

        if (any(is.na(response_vec))) {
          stop(sv, " contains NA.")
        }

        response_vec <- as.numeric(response_vec)

        if (any(is.na(response_vec)) || any(!is.finite(response_vec))) {
          stop(sv, " must be finite numeric.")
        }

        glmm_dat_i <- glmm_dat
        glmm_dat_i[[sv]] <- response_vec

        # size-specific response に対して total と同じ線形予測子を使う
        fit_formula <- as.formula(
          paste0(sv, " ~ year + month + offset(log(effort_glmm)) + (1 | area) + (1 | vessel)")
        )

        fit_i <- glmmTMB(
          formula = fit_formula,
          family = nbinom2(),
          data = glmm_dat_i
        )

        saveRDS(fit_i, model_path_i)

        cat("\n=== model summary ===\n")
        print(summary(fit_i))

        if (isTRUE(optional_pkgs[["DHARMa"]])) {
          cat("\n=== DHARMa residual diagnostics ===\n")
          dharma_out <- try(
            DHARMa::testDispersion(DHARMa::simulateResiduals(fit_i, plot = FALSE)),
            silent = TRUE
          )
          print(dharma_out)
        }

        if (isTRUE(optional_pkgs[["emmeans"]])) {
          emm_year_i <- emmeans::emmeans(fit_i, specs = ~ year, at = list(effort_glmm = 1), type = "response")
          emm_year_tbl <- as.data.frame(emm_year_i)
        } else {
          emm_year_tbl <- build_year_index_fallback(fit_i, glmm_dat_i)
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

        vc_cond <- VarCorr(fit_i)$cond
        area_stats <- extract_re_sd(vc_cond, "area")
        vessel_stats <- extract_re_sd(vc_cond, "vessel")
        disp_parameter <- tryCatch(as.numeric(sigma(fit_i)), error = function(e) NA_real_)
        converged_flag <- isTRUE(fit_i$sdr$pdHess)
        fit_convergence_code <- fit_i$fit$convergence
        fit_pdHess <- isTRUE(fit_i$sdr$pdHess)

        cat("\n=== convergence check ===\n")
        cat("fit_convergence_code=", fit_convergence_code, "\n", sep = "")
        cat("pdHess=", fit_pdHess, "\n", sep = "")

        if (fit_convergence_code != 0 || !fit_pdHess) {
          warning(paste0("Convergence issue may exist in fit_", sv))
        }

        variance_summary_tbl <<- bind_rows(
          variance_summary_tbl,
          tibble(
            size = sv,
            n_obs = nrow(glmm_dat_i),
            n_zero = sum(glmm_dat_i[[sv]] == 0),
            zero_ratio = mean(glmm_dat_i[[sv]] == 0),
            aic = AIC(fit_i),
            logLik = as.numeric(logLik(fit_i)),
            disp_parameter = disp_parameter,
            area_variance = unname(area_stats[["variance"]]),
            area_sd = unname(area_stats[["sd"]]),
            vessel_variance = unname(vessel_stats[["variance"]]),
            vessel_sd = unname(vessel_stats[["sd"]]),
            converged = converged_flag,
            fit_convergence_code = fit_convergence_code,
            pdHess = fit_pdHess,
            warning_message = paste(unique(warning_messages), collapse = " | ")
          )
        )

        run_status_tbl <<- run_status_tbl |>
          mutate(
            status = if_else(size == sv, "success", status),
            error_message = if_else(size == sv, NA_character_, error_message),
            model_path = if_else(size == sv, model_path_i, model_path),
            emm_year_path = if_else(size == sv, emm_year_path_i, emm_year_path),
            index_fig_path = if_else(size == sv, index_fig_path_i, index_fig_path)
          )

        cat("\n=== saved files ===\n")
        cat(model_path_i, "\n", sep = "")
        cat(emm_year_path_i, "\n", sep = "")
        cat(index_fig_path_i, "\n", sep = "")

        list(
          fit = fit_i,
          emm_year_tbl = emm_year_tbl,
          model_path = model_path_i,
          emm_year_path = emm_year_path_i,
          index_fig_path = index_fig_path_i
        )
      },
      warning = function(w) {
        warning_messages <<- c(warning_messages, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      run_status_tbl <<- run_status_tbl |>
        mutate(
          status = if_else(size == sv, "failed", status),
          error_message = if_else(size == sv, conditionMessage(e), error_message),
          model_path = if_else(size == sv, model_path_i, model_path),
          emm_year_path = if_else(size == sv, emm_year_path_i, emm_year_path),
          index_fig_path = if_else(size == sv, index_fig_path_i, index_fig_path)
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
