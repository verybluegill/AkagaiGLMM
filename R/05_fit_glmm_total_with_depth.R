# =========================================
# 05_fit_glmm_total_with_depth.R
# depth_glmm と effort_glmm を使う試験版 total GLMM
# 02_make_glmm_input.R で作成した cleaned 列を使い、depth の入り方を exploratory に確認する
# =========================================

source(file.path("R", "00_load_packages.R"))
source(file.path("R", "build_area_geometry.R"))

optional_pkgs <- load_project_packages(
  required_pkgs = c("tidyverse", "glmmTMB"),
  optional_pkgs = c("emmeans", "DHARMa", "mgcv")
)

ensure_project_dirs()

get_observed_factor_levels <- function(x) {
  sort(unique(as.character(x[!is.na(x)])))
}

compute_year_index_table <- function(model_obj, data_obj, optional_pkgs, depth_glmm_sc_value = NULL) {
  if (isTRUE(optional_pkgs[["emmeans"]])) {
    at_list <- list(effort_glmm = 1)

    if (!is.null(depth_glmm_sc_value)) {
      at_list$depth_glmm_sc <- depth_glmm_sc_value
    }

    out <- emmeans::emmeans(
      model_obj,
      specs = ~ year,
      at = at_list,
      type = "response"
    )

    return(as.data.frame(out))
  }

  year_levels_used <- get_observed_factor_levels(data_obj$year)
  month_levels_used <- get_observed_factor_levels(data_obj$month)
  area_levels_used <- get_observed_factor_levels(data_obj$area)
  vessel_levels_used <- get_observed_factor_levels(data_obj$vessel)

  # depth subset で実際に使われた level を使わないと predict() が落ちることがある
  pred_grid <- expand_grid(
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
    mutate(predicted = predict(model_obj, newdata = pred_grid, type = "response", re.form = NA)) |>
    group_by(year) |>
    summarise(response = mean(predicted), .groups = "drop")
}

collapse_notes <- function(notes_vec) {
  notes_vec <- unique(notes_vec[!is.na(notes_vec) & nzchar(notes_vec)])

  if (length(notes_vec) == 0) {
    return(NA_character_)
  }

  paste(notes_vec, collapse = " | ")
}

safe_fit_gam <- function(model_name, formula_obj, data_obj, mgcv_available) {
  # centroid 付き subset に対して spatial smooth GAM を安全に当てる
  warning_messages <- character(0)
  base_notes <- character(0)

  if (!isTRUE(mgcv_available)) {
    return(list(
      fit = NULL,
      warnings = character(0),
      notes = "package missing"
    ))
  }

  fit_obj <- tryCatch(
    withCallingHandlers(
      mgcv::gam(
        formula = formula_obj,
        family = mgcv::nb(link = "log"),
        method = "REML",
        data = data_obj
      ),
      warning = function(w) {
        warning_messages <<- c(warning_messages, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    ),
    error = function(e) {
      cat("Warning: model failed:", model_name, "\n", sep = "")
      cat("  error: ", conditionMessage(e), "\n", sep = "")
      base_notes <<- c(base_notes, paste0("error: ", conditionMessage(e)))
      NULL
    }
  )

  if (length(warning_messages) > 0) {
    cat("Warning: warnings were issued for ", model_name, "\n", sep = "")
    print(unique(warning_messages))
  }

  list(
    fit = fit_obj,
    warnings = unique(warning_messages),
    notes = collapse_notes(base_notes)
  )
}

build_spatial_comparison_row <- function(model_name, package_name, area_structure, depth_degree, subset_label, fit_obj, fit_ok, warnings = character(0), notes = character(0)) {
  combined_notes <- notes

  if (length(warnings) > 0) {
    combined_notes <- c(combined_notes, paste0("warnings: ", paste(unique(warnings), collapse = "; ")))
  }

  if (!fit_ok) {
    return(tibble(
      model_name = model_name,
      package_name = package_name,
      area_structure = area_structure,
      depth_degree = depth_degree,
      subset_label = subset_label,
      fit_ok = FALSE,
      AIC = NA_real_,
      BIC = NA_real_,
      logLik = NA_real_,
      nobs = NA_integer_,
      converged = NA,
      notes = collapse_notes(combined_notes)
    ))
  }

  converged_value <- if (identical(package_name, "mgcv")) {
    tryCatch(isTRUE(fit_obj$converged), error = function(e) TRUE)
  } else {
    tryCatch(isTRUE(fit_obj$fit$convergence == 0) && isTRUE(fit_obj$sdr$pdHess), error = function(e) NA)
  }

  tibble(
    model_name = model_name,
    package_name = package_name,
    area_structure = area_structure,
    depth_degree = depth_degree,
    subset_label = subset_label,
    fit_ok = TRUE,
    AIC = tryCatch(as.numeric(AIC(fit_obj)[1]), error = function(e) NA_real_),
    BIC = tryCatch(as.numeric(BIC(fit_obj)[1]), error = function(e) NA_real_),
    logLik = tryCatch(as.numeric(logLik(fit_obj)), error = function(e) NA_real_),
    nobs = tryCatch(as.integer(stats::nobs(fit_obj)), error = function(e) NA_integer_),
    converged = converged_value,
    notes = collapse_notes(combined_notes)
  )
}

compute_year_index_table_gam <- function(model_obj, data_obj, depth_glmm_sc_value = NULL) {
  year_levels_used <- get_observed_factor_levels(data_obj$year)
  month_levels_used <- get_observed_factor_levels(data_obj$month)
  vessel_levels_used <- get_observed_factor_levels(data_obj$vessel)
  centroid_tbl <- data_obj |>
    distinct(area, lon, lat) |>
    arrange(area)

  pred_grid <- expand_grid(
    year = factor(year_levels_used, levels = year_levels_used),
    month = factor(month_levels_used, levels = month_levels_used),
    centroid_tbl
  ) |>
    mutate(
      vessel = factor(vessel_levels_used[[1]], levels = levels(data_obj$vessel)),
      effort_glmm = 1
    )

  if (!is.null(depth_glmm_sc_value)) {
    pred_grid$depth_glmm_sc <- depth_glmm_sc_value
  }

  pred_grid |>
    mutate(
      predicted = as.numeric(predict(
        model_obj,
        newdata = pred_grid,
        type = "response",
        exclude = "s(vessel)"
      ))
    ) |>
    group_by(year) |>
    summarise(response = mean(predicted), .groups = "drop")
}

input_path <- file.path("data_processed", "akagai_glmm_input.csv")
fit_path <- file.path("output", "models", "fit_nb_total_with_depth.rds")
emm_year_path <- file.path("output", "tables", "emm_year_total_with_depth.csv")
index_fig_path <- file.path("output", "figures", "index_total_with_depth_by_year.png")
depth_effect_path <- file.path("output", "tables", "depth_effect_total_with_depth.csv")
depth_fig_path <- file.path("output", "figures", "depth_effect_total_with_depth.png")
depth_fig_average_year_path <- file.path("output", "figures", "depth_effect_total_average_over_year_month.png")
row_summary_by_year_path <- file.path("output", "tables", "check_depth_glmm_row_summary_by_year.csv")
model_comparison_path <- file.path("output", "tables", "model_comparison_total_area_depth.csv")
depth_raw_quantiles_path <- file.path("output", "tables", "check_depth_raw_quantiles.csv")
depth_raw_hist_path <- file.path("output", "figures", "check_hist_depth_raw.png")
depth_missing_by_year_path <- file.path("output", "tables", "check_depth_missing_by_year.csv")
depth_missing_by_month_path <- file.path("output", "tables", "check_depth_missing_by_month.csv")
depth_missing_by_area_path <- file.path("output", "tables", "check_depth_missing_by_area.csv")
depth_missing_by_vessel_path <- file.path("output", "tables", "check_depth_missing_by_vessel.csv")
depth_missing_logistic_path <- file.path("output", "tables", "depth_missing_logistic_coefficients.csv")
depth_missing_year_fig_path <- file.path("output", "figures", "depth_missing_by_year.png")
depth_missing_area_fig_path <- file.path("output", "figures", "depth_missing_by_area.png")
emm_year_depth0_path <- file.path("output", "tables", "emm_year_total_depth0_same_subset.csv")
emm_year_depth1_path <- file.path("output", "tables", "emm_year_total_depth1_same_subset.csv")
index_compare_fig_path <- file.path("output", "figures", "index_total_depth0_vs_depth1_same_subset.png")
depth_fill_area_year_summary_path <- file.path("output", "tables", "check_depth_fill_area_year_summary.csv")
depth_effect_best_fig_path <- file.path("output", "figures", "depth_effect_best_total_model.png")
area_centroids_path <- file.path("output", "tables", "area_centroids.csv")
spatial_model_comparison_path <- file.path("output", "tables", "model_comparison_total_area_depth_spatial.csv")
area_centroid_coverage_path <- file.path("output", "tables", "area_centroid_coverage_in_depth_subset.csv")
spatial_smooth_effect_best_path <- file.path("output", "figures", "spatial_smooth_effect_total_depth_best.png")
spatial_smooth_prediction_grid_path <- file.path("output", "tables", "spatial_smooth_prediction_grid_best.csv")
emm_year_spatial_best_path <- file.path("output", "tables", "emm_year_total_with_depth_spatial_best.csv")

if (!file.exists(input_path)) {
  stop("Input file not found: ", input_path)
}

glmm_input <- readr::read_csv(input_path, show_col_types = FALSE)

required_cols <- c("year", "month", "area", "vessel", "count_total", "depth_raw", "depth_glmm", "effort_glmm")
missing_cols <- setdiff(required_cols, names(glmm_input))

if (length(missing_cols) > 0) {
  cat("Missing required columns:\n")
  print(missing_cols)
  stop("Required columns are missing.")
}

glmm_dat <- glmm_input |>
  mutate(
    year = factor(year),
    month = factor(month, levels = sort(unique(month))),
    area = factor(area),
    vessel = factor(vessel),
    count_total = as.numeric(count_total),
    depth_raw = as.numeric(depth_raw),
    depth_glmm = as.numeric(depth_glmm),
    effort_glmm = as.numeric(effort_glmm),
    depth_missing = is.na(depth_glmm),
    depth_missing_int = as.integer(depth_missing),
    log_effort_glmm = if_else(!is.na(effort_glmm) & is.finite(effort_glmm) & effort_glmm > 0, log(effort_glmm), NA_real_)
  )

if (any(is.na(glmm_dat$count_total))) {
  stop("count_total contains NA.")
}

depth_raw_quantile_probs <- c(0, 0.01, 0.05, 0.25, 0.5, 0.75, 0.95, 0.99, 1)
depth_raw_quantiles <- tibble(
  quantile = depth_raw_quantile_probs,
  depth_raw = as.numeric(stats::quantile(glmm_dat$depth_raw, probs = depth_raw_quantile_probs, na.rm = TRUE, names = FALSE))
)

write_csv(depth_raw_quantiles, depth_raw_quantiles_path)

depth_raw_hist <- ggplot(glmm_dat, aes(x = depth_raw)) +
  geom_histogram(bins = 30, color = "white") +
  labs(
    x = "Depth raw",
    y = "Frequency"
  )

ggsave(
  filename = depth_raw_hist_path,
  plot = depth_raw_hist,
  width = 10,
  height = 6,
  dpi = 150
)

cat("\n=== depth_raw quantiles ===\n")
print(depth_raw_quantiles)

depth_missing_by_year <- glmm_dat |>
  mutate(year_chr = as.character(year)) |>
  group_by(year = year_chr) |>
  summarise(
    n_total = n(),
    n_missing = sum(depth_missing),
    n_nonmissing = n_total - n_missing,
    prop_missing = n_missing / n_total,
    .groups = "drop"
  )

depth_missing_by_month <- glmm_dat |>
  mutate(month_chr = as.character(month)) |>
  group_by(month = month_chr) |>
  summarise(
    n_total = n(),
    n_missing = sum(depth_missing),
    n_nonmissing = n_total - n_missing,
    prop_missing = n_missing / n_total,
    .groups = "drop"
  )

depth_missing_by_area <- glmm_dat |>
  mutate(area_chr = as.character(area)) |>
  group_by(area = area_chr) |>
  summarise(
    n_total = n(),
    n_missing = sum(depth_missing),
    n_nonmissing = n_total - n_missing,
    prop_missing = n_missing / n_total,
    .groups = "drop"
  ) |>
  arrange(desc(prop_missing), desc(n_total))

depth_missing_by_vessel <- glmm_dat |>
  mutate(vessel_chr = as.character(vessel)) |>
  group_by(vessel = vessel_chr) |>
  summarise(
    n_total = n(),
    n_missing = sum(depth_missing),
    n_nonmissing = n_total - n_missing,
    prop_missing = n_missing / n_total,
    .groups = "drop"
  ) |>
  arrange(desc(prop_missing), desc(n_total))

write_csv(depth_missing_by_year, depth_missing_by_year_path)
write_csv(depth_missing_by_month, depth_missing_by_month_path)
write_csv(depth_missing_by_area, depth_missing_by_area_path)
write_csv(depth_missing_by_vessel, depth_missing_by_vessel_path)

depth_missing_by_year_fig <- ggplot(depth_missing_by_year, aes(x = year, y = prop_missing)) +
  geom_col() +
  labs(
    x = "Year",
    y = "Proportion missing"
  )

ggsave(
  filename = depth_missing_year_fig_path,
  plot = depth_missing_by_year_fig,
  width = 10,
  height = 6,
  dpi = 150
)

depth_missing_by_area_fig <- ggplot(depth_missing_by_area, aes(x = reorder(area, prop_missing), y = prop_missing)) +
  geom_col() +
  coord_flip() +
  labs(
    x = "Area",
    y = "Proportion missing"
  )

ggsave(
  filename = depth_missing_area_fig_path,
  plot = depth_missing_by_area_fig,
  width = 10,
  height = 8,
  dpi = 150
)

fit_depth_missing_glm <- glm(
  formula = depth_missing_int ~ year + month + area + vessel + log_effort_glmm,
  family = binomial(),
  data = glmm_dat
)

cat("\n=== depth missing logistic summary ===\n")
print(summary(fit_depth_missing_glm))

depth_missing_glm_tbl <- as.data.frame(summary(fit_depth_missing_glm)$coefficients) |>
  tibble::rownames_to_column(var = "term") |>
  transmute(
    term = term,
    estimate = Estimate,
    std_error = `Std. Error`,
    z_value = `z value`,
    p_value = `Pr(>|z|)`
  )

write_csv(depth_missing_glm_tbl, depth_missing_logistic_path)

depth_fill_area_year_lookup <- glmm_dat |>
  group_by(area, year) |>
  summarise(
    depth_glmm_area_year_median = if (all(is.na(depth_glmm))) NA_real_ else stats::median(depth_glmm, na.rm = TRUE),
    .groups = "drop"
  )

glmm_dat_fill_check <- glmm_dat |>
  left_join(depth_fill_area_year_lookup, by = c("area", "year")) |>
  mutate(
    depth_glmm_fill_area_year = if_else(is.na(depth_glmm), depth_glmm_area_year_median, depth_glmm),
    flag_depth_fillable_area_year = is.na(depth_glmm) & !is.na(depth_glmm_area_year_median)
  )

depth_fill_area_year_summary <- tibble(
  n_total = nrow(glmm_dat_fill_check),
  n_missing_original = sum(is.na(glmm_dat_fill_check$depth_glmm)),
  n_fillable_by_area_year = sum(glmm_dat_fill_check$flag_depth_fillable_area_year),
  n_still_missing_after_fill = sum(is.na(glmm_dat_fill_check$depth_glmm_fill_area_year)),
  prop_fillable_among_missing = if (sum(is.na(glmm_dat_fill_check$depth_glmm)) == 0) NA_real_ else sum(glmm_dat_fill_check$flag_depth_fillable_area_year) / sum(is.na(glmm_dat_fill_check$depth_glmm)),
  n_area_year_cells = nrow(depth_fill_area_year_lookup),
  n_area_year_cells_all_missing = sum(is.na(depth_fill_area_year_lookup$depth_glmm_area_year_median))
)

write_csv(depth_fill_area_year_summary, depth_fill_area_year_summary_path)

n_input_total <- nrow(glmm_dat)
n_depth_glmm_na <- sum(is.na(glmm_dat$depth_glmm) | !is.finite(glmm_dat$depth_glmm))
n_effort_glmm_na_or_bad <- sum(is.na(glmm_dat$effort_glmm) | !is.finite(glmm_dat$effort_glmm) | glmm_dat$effort_glmm <= 0)
n_area_missing <- sum(is.na(glmm_input$area) | glmm_input$area == "")

glmm_dat <- glmm_dat |>
  mutate(
    flag_use_for_main_glmm = if ("flag_use_for_main_glmm" %in% names(glmm_input)) as.logical(glmm_input$flag_use_for_main_glmm) else as.integer(as.character(year)) %in% 2020:2024 &
      !is.na(area) &
      as.character(area) != "" &
      !is.na(effort_glmm) &
      is.finite(effort_glmm) &
      effort_glmm > 0,
    flag_use_for_depth_glmm = !is.na(depth_glmm) &
      is.finite(depth_glmm) &
      flag_use_for_main_glmm
  )

glmm_dat_depth <- glmm_dat |>
  filter(flag_use_for_depth_glmm)

n_used_for_depth_glmm <- nrow(glmm_dat_depth)
n_dropped_for_depth_glmm <- n_input_total - n_used_for_depth_glmm
dropped_prop_for_depth_glmm <- n_dropped_for_depth_glmm / n_input_total

row_summary_by_year <- glmm_dat |>
  mutate(year_chr = as.character(year)) |>
  group_by(year = year_chr) |>
  summarise(
    n_input = n(),
    n_used = sum(flag_use_for_depth_glmm),
    n_dropped = n_input - n_used,
    .groups = "drop"
  )

write_csv(row_summary_by_year, row_summary_by_year_path)

cat("\n=== depth GLMM row summary ===\n")
cat("n_input_total=", n_input_total, "\n", sep = "")
cat("n_used_for_depth_glmm=", n_used_for_depth_glmm, "\n", sep = "")
cat("n_dropped_for_depth_glmm=", n_dropped_for_depth_glmm, "\n", sep = "")
cat("n_depth_glmm_na=", n_depth_glmm_na, "\n", sep = "")
cat("n_effort_glmm_na_or_bad=", n_effort_glmm_na_or_bad, "\n", sep = "")
cat("n_area_missing=", n_area_missing, "\n", sep = "")
cat("dropped_prop_for_depth_glmm=", dropped_prop_for_depth_glmm, "\n", sep = "")

if (dropped_prop_for_depth_glmm > 0.3) {
  cat("Warning: dropped proportion exceeds 0.3, so missingness bias should be checked carefully.\n")
}

if (dropped_prop_for_depth_glmm > 0.4) {
  cat("Warning: dropped proportion exceeds 0.4, so the depth model should be treated as a sensitivity analysis.\n")
}

if (n_used_for_depth_glmm == 0) {
  stop("No rows available for the depth GLMM after filtering.")
}

if (any(is.na(glmm_dat_depth$effort_glmm)) || any(!is.finite(glmm_dat_depth$effort_glmm)) || any(glmm_dat_depth$effort_glmm <= 0)) {
  stop("effort_glmm must be finite and > 0 in the filtered data.")
}

if (length(unique(glmm_dat_depth$depth_glmm)) <= 1) {
  stop("depth_glmm has only one unique value after filtering.")
}

depth_glmm_mean <- mean(glmm_dat_depth$depth_glmm, na.rm = TRUE)
depth_glmm_sd <- stats::sd(glmm_dat_depth$depth_glmm, na.rm = TRUE)

if (!is.finite(depth_glmm_sd) || depth_glmm_sd <= 0) {
  stop("depth_glmm standard deviation must be finite and > 0.")
}

glmm_dat_depth <- glmm_dat_depth |>
  mutate(
    depth_glmm_sc = (depth_glmm - depth_glmm_mean) / depth_glmm_sd
  )

cat("\n=== depth standardization ===\n")
cat("depth_glmm_mean=", depth_glmm_mean, "\n", sep = "")
cat("depth_glmm_sd=", depth_glmm_sd, "\n", sep = "")

if (!file.exists(area_centroids_path)) {
  build_and_write_area_geometry(output_dir = "output")
}

area_centroids_tbl <- readr::read_csv(area_centroids_path, show_col_types = FALSE) |>
  mutate(
    area = as.character(area),
    lon = as.numeric(lon),
    lat = as.numeric(lat)
  )

glmm_dat_depth_with_centroid <- glmm_dat_depth |>
  mutate(area_chr = as.character(area)) |>
  left_join(area_centroids_tbl, by = c("area_chr" = "area")) |>
  mutate(
    area = factor(area_chr, levels = levels(glmm_dat_depth$area))
  ) |>
  select(-area_chr)

area_centroid_coverage_tbl <- glmm_dat_depth_with_centroid |>
  mutate(area_chr = as.character(area)) |>
  group_by(area = area_chr) |>
  summarise(
    n_rows = n(),
    has_centroid = any(!is.na(lon) & !is.na(lat)),
    mean_count_total = mean(count_total, na.rm = TRUE),
    .groups = "drop"
  ) |>
  arrange(area)

write_csv(area_centroid_coverage_tbl, area_centroid_coverage_path)

areas_without_centroid <- glmm_dat_depth_with_centroid |>
  filter(is.na(lon) | is.na(lat)) |>
  distinct(area) |>
  pull(area) |>
  as.character() |>
  sort()

n_spatial_available <- sum(!is.na(glmm_dat_depth_with_centroid$lon) & !is.na(glmm_dat_depth_with_centroid$lat))
n_spatial_dropped <- nrow(glmm_dat_depth_with_centroid) - n_spatial_available
prop_spatial_available <- if (nrow(glmm_dat_depth_with_centroid) == 0) NA_real_ else n_spatial_available / nrow(glmm_dat_depth_with_centroid)

glmm_dat_depth_spatial <- glmm_dat_depth_with_centroid |>
  filter(!is.na(lon), !is.na(lat))

cat("\n=== centroid coverage in depth subset ===\n")
cat("n_spatial_available=", n_spatial_available, "\n", sep = "")
cat("n_spatial_dropped=", n_spatial_dropped, "\n", sep = "")
cat("prop_spatial_available=", prop_spatial_available, "\n", sep = "")
cat("areas_without_centroid=", if (length(areas_without_centroid) == 0) "none" else paste(areas_without_centroid, collapse = ","), "\n", sep = "")

if (nrow(glmm_dat_depth_spatial) == 0) {
  stop("Centroid join produced no spatially available rows in the depth subset.")
}

safe_fit_glmmTMB <- function(model_name, formula_obj, data_obj) {
  # 同じ depth subset を使って、area の random / fixed と depth 0 / 1 / 2 を比較する
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
      cat("Warning: model failed:", model_name, "\n", sep = "")
      cat("  error: ", conditionMessage(e), "\n", sep = "")
      NULL
    }
  )

  if (length(warning_messages) > 0) {
    cat("Warning: warnings were issued for ", model_name, "\n", sep = "")
    print(unique(warning_messages))
  }

  list(
    fit = fit_obj,
    warnings = unique(warning_messages)
  )
}

extract_fit_metrics <- function(model_name, area_type, depth_degree, fit_obj) {
  convergence_value <- tryCatch(fit_obj$fit$convergence, error = function(e) NA_integer_)
  sdr_available <- tryCatch(!is.null(fit_obj$sdr), error = function(e) FALSE)
  pdHess_value <- tryCatch(if (sdr_available) fit_obj$sdr$pdHess else NA, error = function(e) NA)
  converged_value <- isTRUE(convergence_value == 0)

  tibble(
    model_name = model_name,
    area_type = area_type,
    depth_degree = depth_degree,
    AIC = tryCatch(AIC(fit_obj), error = function(e) NA_real_),
    BIC = tryCatch(BIC(fit_obj), error = function(e) NA_real_),
    logLik = tryCatch(as.numeric(logLik(fit_obj)), error = function(e) NA_real_),
    nobs = tryCatch(stats::nobs(fit_obj), error = function(e) NA_integer_),
    converged = converged_value,
    pdHess = tryCatch(if (is.null(pdHess_value)) NA else isTRUE(pdHess_value), error = function(e) NA)
  )
}

model_specs <- list(
  list(model_name = "fit_nb_total_area_re_depth0", area_type = "random", depth_degree = 0, formula = count_total ~ year + month + offset(log(effort_glmm)) + (1 | area) + (1 | vessel)),
  list(model_name = "fit_nb_total_area_re_depth1", area_type = "random", depth_degree = 1, formula = count_total ~ year + month + depth_glmm_sc + offset(log(effort_glmm)) + (1 | area) + (1 | vessel)),
  list(model_name = "fit_nb_total_area_re_depth2", area_type = "random", depth_degree = 2, formula = count_total ~ year + month + depth_glmm_sc + I(depth_glmm_sc^2) + offset(log(effort_glmm)) + (1 | area) + (1 | vessel)),
  list(model_name = "fit_nb_total_area_fe_depth0", area_type = "fixed", depth_degree = 0, formula = count_total ~ year + month + area + offset(log(effort_glmm)) + (1 | vessel)),
  list(model_name = "fit_nb_total_area_fe_depth1", area_type = "fixed", depth_degree = 1, formula = count_total ~ year + month + area + depth_glmm_sc + offset(log(effort_glmm)) + (1 | vessel)),
  list(model_name = "fit_nb_total_area_fe_depth2", area_type = "fixed", depth_degree = 2, formula = count_total ~ year + month + area + depth_glmm_sc + I(depth_glmm_sc^2) + offset(log(effort_glmm)) + (1 | vessel))
)

model_fit_results <- vector("list", length(model_specs))
names(model_fit_results) <- vapply(model_specs, function(x) x$model_name, character(1))

for (i in seq_along(model_specs)) {
  spec_i <- model_specs[[i]]
  model_fit_results[[i]] <- safe_fit_glmmTMB(
    model_name = spec_i$model_name,
    formula_obj = spec_i$formula,
    data_obj = glmm_dat_depth
  )
}

fit_nb_total_area_re_depth0 <- model_fit_results[["fit_nb_total_area_re_depth0"]]$fit
fit_nb_total_area_re_depth1 <- model_fit_results[["fit_nb_total_area_re_depth1"]]$fit
fit_nb_total_area_re_depth2 <- model_fit_results[["fit_nb_total_area_re_depth2"]]$fit
fit_nb_total_area_fe_depth0 <- model_fit_results[["fit_nb_total_area_fe_depth0"]]$fit
fit_nb_total_area_fe_depth1 <- model_fit_results[["fit_nb_total_area_fe_depth1"]]$fit
fit_nb_total_area_fe_depth2 <- model_fit_results[["fit_nb_total_area_fe_depth2"]]$fit

fit_nb_total_depth0 <- fit_nb_total_area_re_depth0
fit_nb_total_depth1 <- fit_nb_total_area_re_depth1

successful_model_specs <- keep(model_specs, ~ !is.null(model_fit_results[[.x$model_name]]$fit))

if (length(successful_model_specs) == 0) {
  stop("All candidate models failed on glmm_dat_depth.")
}

model_comparison_tbl <- map_dfr(
  successful_model_specs,
  function(spec_i) {
    extract_fit_metrics(
      model_name = spec_i$model_name,
      area_type = spec_i$area_type,
      depth_degree = spec_i$depth_degree,
      fit_obj = model_fit_results[[spec_i$model_name]]$fit
    )
  }
) |>
  arrange(AIC, BIC, model_name)

write_csv(model_comparison_tbl, model_comparison_path)

cat("\n=== model comparison ===\n")
print(model_comparison_tbl)

nobs_unique <- sort(unique(model_comparison_tbl$nobs))
cat("\n=== nobs check for AIC comparison ===\n")
cat("nobs_unique=", paste(nobs_unique, collapse = ","), "\n", sep = "")

if (length(nobs_unique) == 1) {
  cat("All compared models use the same depth-available subset.\n")
} else {
  cat("Warning: nobs are not identical across compared models.\n")
}

best_model_name <- model_comparison_tbl$model_name[[1]]
best_model_obj <- model_fit_results[[best_model_name]]$fit
best_model_area_type <- model_comparison_tbl$area_type[[1]]
best_model_depth_degree <- model_comparison_tbl$depth_degree[[1]]

cat("\n=== best model ===\n")
cat("best_model_name=", best_model_name, "\n", sep = "")

saveRDS(best_model_obj, fit_path)

cat("\n=== model summary ===\n")
fit_summary <- summary(best_model_obj)
print(fit_summary)

cat("\n=== convergence check ===\n")
for (spec_i in successful_model_specs) {
  fit_obj_i <- model_fit_results[[spec_i$model_name]]$fit
  pdHess_i <- tryCatch(if (!is.null(fit_obj_i$sdr)) fit_obj_i$sdr$pdHess else NA, error = function(e) NA)
  convergence_i <- tryCatch(fit_obj_i$fit$convergence, error = function(e) NA_integer_)
  cat(spec_i$model_name, "_pdHess=", if (is.null(pdHess_i)) NA else isTRUE(pdHess_i), "\n", sep = "")
  cat(spec_i$model_name, "_convergence=", convergence_i, "\n", sep = "")
  if (!isTRUE(pdHess_i) || !isTRUE(convergence_i == 0)) {
    cat("Warning: convergence issue may exist in ", spec_i$model_name, "\n", sep = "")
  }
}

depth_effect_tbl <- as.data.frame(fit_summary$coefficients$cond) |>
  tibble::rownames_to_column(var = "term") |>
  transmute(
    term = term,
    estimate = Estimate,
    std_error = `Std. Error`,
    z_value = `z value`,
    p_value = `Pr(>|z|)`,
    depth_glmm_mean = depth_glmm_mean,
    depth_glmm_sd = depth_glmm_sd,
    best_model_name = best_model_name
  ) |>
  filter(term %in% c("depth_glmm_sc", "I(depth_glmm_sc^2)"))

write_csv(depth_effect_tbl, depth_effect_path)

cat("\n=== depth coefficient ===\n")
print(depth_effect_tbl)

if (isTRUE(optional_pkgs[["DHARMa"]])) {
  cat("\n=== DHARMa residual diagnostics ===\n")
  print(try(DHARMa::testDispersion(DHARMa::simulateResiduals(best_model_obj, plot = FALSE)), silent = TRUE))
}

depth_glmm_ref <- stats::median(glmm_dat_depth$depth_glmm, na.rm = TRUE)
depth_glmm_sc_ref <- (depth_glmm_ref - depth_glmm_mean) / depth_glmm_sd

emm_year_depth0_tbl <- if (!is.null(fit_nb_total_depth0)) {
  compute_year_index_table(
    model_obj = fit_nb_total_depth0,
    data_obj = glmm_dat_depth,
    optional_pkgs = optional_pkgs,
    depth_glmm_sc_value = NULL
  )
} else {
  tibble()
}

emm_year_depth1_tbl <- if (!is.null(fit_nb_total_depth1)) {
  compute_year_index_table(
    model_obj = fit_nb_total_depth1,
    data_obj = glmm_dat_depth,
    optional_pkgs = optional_pkgs,
    depth_glmm_sc_value = depth_glmm_sc_ref
  )
} else {
  tibble()
}

emm_year_total_tbl <- compute_year_index_table(
  model_obj = best_model_obj,
  data_obj = glmm_dat_depth,
  optional_pkgs = optional_pkgs,
  depth_glmm_sc_value = if (best_model_depth_degree > 0) depth_glmm_sc_ref else NULL
)

write_csv(emm_year_depth0_tbl, emm_year_depth0_path)
write_csv(emm_year_depth1_tbl, emm_year_depth1_path)

write_csv(emm_year_total_tbl, emm_year_path)

year_index_col <- intersect(c("response", "rate", "prob", "emmean"), names(emm_year_total_tbl))

if (length(year_index_col) == 0) {
  stop("Failed to identify the year effect column for plotting.")
}

year_index_col <- year_index_col[[1]]

index_total_with_depth_by_year <- ggplot(emm_year_total_tbl, aes(x = year, y = .data[[year_index_col]], group = 1)) +
  geom_line() +
  geom_point(size = 2) +
  labs(
    x = "Year",
    y = "Index"
  )

if (all(c("lower.CL", "upper.CL") %in% names(emm_year_total_tbl))) {
  index_total_with_depth_by_year <- index_total_with_depth_by_year +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15)
}

ggsave(
  filename = index_fig_path,
  plot = index_total_with_depth_by_year,
  width = 10,
  height = 6,
  dpi = 150
)

year_index_col_depth0 <- intersect(c("response", "rate", "prob", "emmean"), names(emm_year_depth0_tbl))
year_index_col_depth1 <- intersect(c("response", "rate", "prob", "emmean"), names(emm_year_depth1_tbl))

if (length(year_index_col_depth0) > 0 && length(year_index_col_depth1) > 0) {
  year_index_compare_tbl <- bind_rows(
    emm_year_depth0_tbl |>
      transmute(year = year, response = .data[[year_index_col_depth0[[1]]]], model = "Depth0"),
    emm_year_depth1_tbl |>
      transmute(year = year, response = .data[[year_index_col_depth1[[1]]]], model = "Depth1")
  )

  index_total_depth0_vs_depth1_same_subset <- ggplot(year_index_compare_tbl, aes(x = year, y = response, color = model, group = model)) +
    geom_line() +
    geom_point(size = 2) +
    labs(
      x = "Year",
      y = "Index",
      color = "Model"
    )

  ggsave(
    filename = index_compare_fig_path,
    plot = index_total_depth0_vs_depth1_same_subset,
    width = 10,
    height = 6,
    dpi = 150
  )
} else {
  cat("Warning: skipped depth0/depth1 index comparison plot because one of the models was unavailable.\n")
}

depth_seq <- seq(
  from = min(glmm_dat_depth$depth_glmm, na.rm = TRUE),
  to = max(glmm_dat_depth$depth_glmm, na.rm = TRUE),
  length.out = 100
)

year_levels_used <- get_observed_factor_levels(glmm_dat_depth$year)
month_levels_used <- get_observed_factor_levels(glmm_dat_depth$month)
area_levels_used <- get_observed_factor_levels(glmm_dat_depth$area)
vessel_levels_used <- get_observed_factor_levels(glmm_dat_depth$vessel)
year_ref <- year_levels_used[[1]]
depth_plot_grid <- expand_grid(
  year = factor(year_ref, levels = year_levels_used),
  month = factor(month_levels_used, levels = month_levels_used),
  depth_glmm = depth_seq
) |>
  mutate(
    depth_glmm_sc = (depth_glmm - depth_glmm_mean) / depth_glmm_sd,
    effort_glmm = 1,
    area = factor(area_levels_used[[1]], levels = area_levels_used),
    vessel = factor(vessel_levels_used[[1]], levels = vessel_levels_used)
  )

# depth 陷会ｽｹ隴ｫ諛ｷ蟲咏ｸｺ・ｯ陜難ｽｺ雋・・year 邵ｺ・ｨ reference month 郢ｧ雋槫ｴ玖楜螢ｹ・邵ｺ貅ｯ・｣諛ｷ蜍ｧ陜暦ｽｳ
# 陜暦ｽｺ陞ｳ螢ｹ・邵ｺ貊捺套闔会ｽｶ郢ｧ蜻域呵募ｾ娯・陷・ｽｺ陷牙ｸ呻ｼ邵ｺ・ｦ邵ｲ竏ｵ笆ｲ隲｢荵猟・ｧ邵ｺ謔溘・邵ｺ荵晢ｽ狗ｹｧ蛹ｻ竕ｧ邵ｺ・ｫ邵ｺ蜷ｶ・・
if (best_model_depth_degree > 0) {
  depth_plot_grid$predicted_count <- predict(
    best_model_obj,
    newdata = depth_plot_grid,
    type = "response",
    re.form = NA
  )

  depth_plot_dat <- depth_plot_grid |>
    group_by(depth_glmm) |>
    summarise(
      predicted_count = mean(predicted_count),
      .groups = "drop"
    )

  depth_effect_plot <- ggplot(depth_plot_dat, aes(x = depth_glmm, y = predicted_count)) +
    geom_line() +
    labs(
      x = "Depth",
      y = "Predicted count"
    )

  ggsave(
    filename = depth_fig_path,
    plot = depth_effect_plot,
    width = 10,
    height = 6,
    dpi = 150
  )

  depth_plot_grid_average_year <- expand_grid(
    year = factor(year_levels_used, levels = year_levels_used),
    month = factor(month_levels_used, levels = month_levels_used),
    depth_glmm = depth_seq
  ) |>
    mutate(
      depth_glmm_sc = (depth_glmm - depth_glmm_mean) / depth_glmm_sd,
      effort_glmm = 1,
      area = factor(area_levels_used[[1]], levels = area_levels_used),
      vessel = factor(vessel_levels_used[[1]], levels = vessel_levels_used)
    )

  depth_plot_grid_average_year$predicted_count <- predict(
    best_model_obj,
    newdata = depth_plot_grid_average_year,
    type = "response",
    re.form = NA
  )

  depth_plot_dat_average_year <- depth_plot_grid_average_year |>
    group_by(depth_glmm) |>
    summarise(
      predicted_count = mean(predicted_count),
      .groups = "drop"
    )

  depth_effect_plot_average_year <- ggplot(depth_plot_dat_average_year, aes(x = depth_glmm, y = predicted_count)) +
    geom_line() +
    labs(
      x = "Depth",
      y = "Predicted count"
    )

  ggsave(
    filename = depth_fig_average_year_path,
    plot = depth_effect_plot_average_year,
    width = 10,
    height = 6,
    dpi = 150
  )

  if (best_model_depth_degree == 2) {
    depth_seq_sc <- seq(
      from = min(glmm_dat_depth$depth_glmm_sc, na.rm = TRUE),
      to = max(glmm_dat_depth$depth_glmm_sc, na.rm = TRUE),
      length.out = 100
    )

    depth_effect_best_grid <- expand_grid(
      year = factor(year_ref, levels = year_levels_used),
      month = factor(month_levels_used, levels = month_levels_used),
      depth_glmm_sc = depth_seq_sc
    ) |>
      mutate(
        depth_glmm = depth_glmm_sc * depth_glmm_sd + depth_glmm_mean,
        effort_glmm = 1,
        area = factor(area_levels_used[[1]], levels = area_levels_used),
        vessel = factor(vessel_levels_used[[1]], levels = vessel_levels_used)
      )

    depth_effect_best_grid$predicted_relative_index <- predict(
      best_model_obj,
      newdata = depth_effect_best_grid,
      type = "response",
      re.form = NA
    )

    depth_effect_best_dat <- depth_effect_best_grid |>
      group_by(depth_glmm_sc) |>
      summarise(
        predicted_relative_index = mean(predicted_relative_index),
        .groups = "drop"
      ) |>
      mutate(
        predicted_relative_index = predicted_relative_index / predicted_relative_index[which.min(abs(depth_glmm_sc - depth_glmm_sc_ref))]
      )

    depth_effect_best_plot <- ggplot(depth_effect_best_dat, aes(x = depth_glmm_sc, y = predicted_relative_index)) +
      geom_line() +
      labs(
        x = "Standardized depth",
        y = "Relative index"
      )

    ggsave(
      filename = depth_effect_best_fig_path,
      plot = depth_effect_best_plot,
      width = 10,
      height = 6,
      dpi = 150
    )
  }
} else {
  cat("best model does not include depth, so depth effect plots were skipped.\n")
}

spatial_compare_note <- c(
  "same-subset comparison on rows with centroid",
  "AIC comparison across glmmTMB and mgcv is exploratory"
)

spatial_nonspatial_specs <- list(
  list(model_name = "fit_nb_total_area_re_depth0_spatial_subset", package_name = "glmmTMB", area_structure = "random_iid", depth_degree = 0, formula = count_total ~ year + month + offset(log(effort_glmm)) + (1 | area) + (1 | vessel)),
  list(model_name = "fit_nb_total_area_re_depth1_spatial_subset", package_name = "glmmTMB", area_structure = "random_iid", depth_degree = 1, formula = count_total ~ year + month + depth_glmm_sc + offset(log(effort_glmm)) + (1 | area) + (1 | vessel)),
  list(model_name = "fit_nb_total_area_re_depth2_spatial_subset", package_name = "glmmTMB", area_structure = "random_iid", depth_degree = 2, formula = count_total ~ year + month + depth_glmm_sc + I(depth_glmm_sc^2) + offset(log(effort_glmm)) + (1 | area) + (1 | vessel)),
  list(model_name = "fit_nb_total_area_fe_depth0_spatial_subset", package_name = "glmmTMB", area_structure = "fixed", depth_degree = 0, formula = count_total ~ year + month + area + offset(log(effort_glmm)) + (1 | vessel)),
  list(model_name = "fit_nb_total_area_fe_depth1_spatial_subset", package_name = "glmmTMB", area_structure = "fixed", depth_degree = 1, formula = count_total ~ year + month + area + depth_glmm_sc + offset(log(effort_glmm)) + (1 | vessel)),
  list(model_name = "fit_nb_total_area_fe_depth2_spatial_subset", package_name = "glmmTMB", area_structure = "fixed", depth_degree = 2, formula = count_total ~ year + month + area + depth_glmm_sc + I(depth_glmm_sc^2) + offset(log(effort_glmm)) + (1 | vessel))
)

spatial_gam_specs <- list(
  list(model_name = "fit_gam_total_spatial_depth0", package_name = "mgcv", area_structure = "centroid_smooth", depth_degree = 0, formula = count_total ~ year + month + s(vessel, bs = "re") + s(lon, lat, bs = "tp", k = 10) + offset(log(effort_glmm))),
  list(model_name = "fit_gam_total_spatial_depth1", package_name = "mgcv", area_structure = "centroid_smooth", depth_degree = 1, formula = count_total ~ year + month + depth_glmm_sc + s(vessel, bs = "re") + s(lon, lat, bs = "tp", k = 10) + offset(log(effort_glmm))),
  list(model_name = "fit_gam_total_spatial_depth2", package_name = "mgcv", area_structure = "centroid_smooth", depth_degree = 2, formula = count_total ~ year + month + depth_glmm_sc + I(depth_glmm_sc^2) + s(vessel, bs = "re") + s(lon, lat, bs = "tp", k = 10) + offset(log(effort_glmm)))
)

spatial_compare_specs <- c(spatial_nonspatial_specs, spatial_gam_specs)
spatial_compare_results <- vector("list", length(spatial_compare_specs))
names(spatial_compare_results) <- vapply(spatial_compare_specs, function(x) x$model_name, character(1))

for (i in seq_along(spatial_compare_specs)) {
  spec_i <- spatial_compare_specs[[i]]

  if (identical(spec_i$package_name, "glmmTMB")) {
    spatial_compare_results[[spec_i$model_name]] <- safe_fit_glmmTMB(
      model_name = spec_i$model_name,
      formula_obj = spec_i$formula,
      data_obj = glmm_dat_depth_spatial
    )
  } else {
    spatial_compare_results[[spec_i$model_name]] <- safe_fit_gam(
      model_name = spec_i$model_name,
      formula_obj = spec_i$formula,
      data_obj = glmm_dat_depth_spatial,
      mgcv_available = optional_pkgs[["mgcv"]]
    )
  }
}

spatial_model_comparison_tbl <- bind_rows(
  lapply(spatial_compare_specs, function(spec_i) {
    fit_result_i <- spatial_compare_results[[spec_i$model_name]]
    build_spatial_comparison_row(
      model_name = spec_i$model_name,
      package_name = spec_i$package_name,
      area_structure = spec_i$area_structure,
      depth_degree = spec_i$depth_degree,
      subset_label = "depth_common_with_centroid",
      fit_obj = fit_result_i$fit,
      fit_ok = !is.null(fit_result_i$fit),
      warnings = fit_result_i$warnings,
      notes = c(
        spatial_compare_note,
        fit_result_i$notes,
        paste0("n_spatial_available=", n_spatial_available),
        paste0("n_spatial_dropped=", n_spatial_dropped)
      )
    )
  })
) |>
  mutate(
    AIC_sort = if_else(is.na(AIC), Inf, AIC)
  ) |>
  arrange(desc(fit_ok), AIC_sort, model_name) |>
  select(
    model_name, package_name, area_structure, depth_degree, subset_label,
    fit_ok, AIC, BIC, logLik, nobs, converged, notes
  )

write_csv(spatial_model_comparison_tbl, spatial_model_comparison_path)

spatial_nobs_unique <- spatial_model_comparison_tbl |>
  filter(fit_ok) |>
  pull(nobs) |>
  unique() |>
  sort()

same_subset_spatial_compare <- length(spatial_nobs_unique) == 1 && identical(as.integer(spatial_nobs_unique[[1]]), as.integer(n_spatial_available))

best_nonspatial_same_subset_row <- spatial_model_comparison_tbl |>
  filter(fit_ok, area_structure %in% c("fixed", "random_iid")) |>
  slice_head(n = 1)

best_centroid_smooth_row <- spatial_model_comparison_tbl |>
  filter(fit_ok, area_structure == "centroid_smooth") |>
  slice_head(n = 1)

empty_spatial_year_tbl <- tibble(
  model_name = character(),
  year = character(),
  response = numeric(),
  depth_degree = integer()
)

empty_spatial_grid_tbl <- tibble(
  model_name = character(),
  lon = numeric(),
  lat = numeric(),
  predicted_count = numeric(),
  year_ref = character(),
  month_condition = character()
)

if (nrow(best_centroid_smooth_row) == 1) {
  best_centroid_smooth_model_name <- best_centroid_smooth_row$model_name[[1]]
  best_centroid_smooth_fit <- spatial_compare_results[[best_centroid_smooth_model_name]]$fit
  best_centroid_smooth_depth_degree <- best_centroid_smooth_row$depth_degree[[1]]

  emm_year_spatial_best_tbl <- compute_year_index_table_gam(
    model_obj = best_centroid_smooth_fit,
    data_obj = glmm_dat_depth_spatial,
    depth_glmm_sc_value = if (best_centroid_smooth_depth_degree > 0) depth_glmm_sc_ref else NULL
  ) |>
    mutate(
      model_name = best_centroid_smooth_model_name,
      depth_degree = best_centroid_smooth_depth_degree,
      .before = 1
    )

  spatial_year_levels_used <- get_observed_factor_levels(glmm_dat_depth_spatial$year)
  spatial_month_levels_used <- get_observed_factor_levels(glmm_dat_depth_spatial$month)
  spatial_vessel_levels_used <- get_observed_factor_levels(glmm_dat_depth_spatial$vessel)
  spatial_year_ref <- spatial_year_levels_used[[1]]

  spatial_prediction_grid <- expand_grid(
    lon = seq(min(glmm_dat_depth_spatial$lon), max(glmm_dat_depth_spatial$lon), length.out = 60),
    lat = seq(min(glmm_dat_depth_spatial$lat), max(glmm_dat_depth_spatial$lat), length.out = 60),
    month = factor(spatial_month_levels_used, levels = spatial_month_levels_used)
  ) |>
    mutate(
      year = factor(spatial_year_ref, levels = spatial_year_levels_used),
      vessel = factor(spatial_vessel_levels_used[[1]], levels = levels(glmm_dat_depth_spatial$vessel)),
      effort_glmm = 1
    )

  if (best_centroid_smooth_depth_degree > 0) {
    spatial_prediction_grid$depth_glmm_sc <- depth_glmm_sc_ref
  }

  spatial_prediction_grid$predicted_count <- as.numeric(predict(
    best_centroid_smooth_fit,
    newdata = spatial_prediction_grid,
    type = "response",
    exclude = "s(vessel)"
  ))

  spatial_smooth_prediction_grid_best_tbl <- spatial_prediction_grid |>
    group_by(lon, lat) |>
    summarise(
      predicted_count = mean(predicted_count),
      .groups = "drop"
    ) |>
    mutate(
      model_name = best_centroid_smooth_model_name,
      year_ref = spatial_year_ref,
      month_condition = "average_over_month_levels",
      .before = 1
    )

  centroids_used_tbl <- glmm_dat_depth_spatial |>
    distinct(area, lon, lat) |>
    arrange(area)

  spatial_smooth_effect_best_plot <- ggplot(spatial_smooth_prediction_grid_best_tbl, aes(x = lon, y = lat, fill = predicted_count)) +
    geom_raster() +
    geom_point(data = centroids_used_tbl, aes(x = lon, y = lat), inherit.aes = FALSE, color = "black", size = 1.6) +
    geom_text(data = centroids_used_tbl, aes(x = lon, y = lat, label = area), inherit.aes = FALSE, nudge_y = 0.00035, size = 3) +
    coord_equal() +
    labs(
      x = "Longitude",
      y = "Latitude",
      fill = "Predicted count",
      title = "Spatial smooth effect"
    ) +
    theme_bw()

  ggsave(
    filename = spatial_smooth_effect_best_path,
    plot = spatial_smooth_effect_best_plot,
    width = 10,
    height = 7,
    dpi = 150
  )
} else {
  best_centroid_smooth_model_name <- NA_character_
  best_centroid_smooth_depth_degree <- NA_integer_
  emm_year_spatial_best_tbl <- empty_spatial_year_tbl
  spatial_smooth_prediction_grid_best_tbl <- empty_spatial_grid_tbl

  ggsave(
    filename = spatial_smooth_effect_best_path,
    plot = ggplot() + theme_void() + labs(title = "No spatial smooth model was fitted"),
    width = 10,
    height = 7,
    dpi = 150
  )
}

write_csv(emm_year_spatial_best_tbl, emm_year_spatial_best_path)
write_csv(spatial_smooth_prediction_grid_best_tbl, spatial_smooth_prediction_grid_path)

cat("\n=== spatial same-subset comparison ===\n")
cat("best_nonspatial_model=", best_model_name, "\n", sep = "")
cat("best_centroid_smooth_model=", best_centroid_smooth_model_name, "\n", sep = "")
cat("best_nonspatial_nobs=", if (nrow(best_nonspatial_same_subset_row) == 1) best_nonspatial_same_subset_row$nobs[[1]] else NA_integer_, "\n", sep = "")
cat("best_centroid_smooth_nobs=", if (nrow(best_centroid_smooth_row) == 1) best_centroid_smooth_row$nobs[[1]] else NA_integer_, "\n", sep = "")
cat("same_subset_comparison_achieved=", same_subset_spatial_compare, "\n", sep = "")
cat("areas_in_geometry_subset=", paste(sort(unique(as.character(glmm_dat_depth_spatial$area))), collapse = ","), "\n", sep = "")

cat("\n=== saved files ===\n")
cat("input_path=", input_path, "\n", sep = "")
cat("fit_path=", fit_path, "\n", sep = "")
cat("model_comparison_path=", model_comparison_path, "\n", sep = "")
cat("spatial_model_comparison_path=", spatial_model_comparison_path, "\n", sep = "")
cat("area_centroids_path=", area_centroids_path, "\n", sep = "")
cat("area_centroid_coverage_path=", area_centroid_coverage_path, "\n", sep = "")
cat("row_summary_by_year_path=", row_summary_by_year_path, "\n", sep = "")
cat("depth_missing_by_year_path=", depth_missing_by_year_path, "\n", sep = "")
cat("depth_missing_by_month_path=", depth_missing_by_month_path, "\n", sep = "")
cat("depth_missing_by_area_path=", depth_missing_by_area_path, "\n", sep = "")
cat("depth_missing_by_vessel_path=", depth_missing_by_vessel_path, "\n", sep = "")
cat("depth_missing_logistic_path=", depth_missing_logistic_path, "\n", sep = "")
cat("depth_fill_area_year_summary_path=", depth_fill_area_year_summary_path, "\n", sep = "")
cat("emm_year_path=", emm_year_path, "\n", sep = "")
cat("emm_year_depth0_path=", emm_year_depth0_path, "\n", sep = "")
cat("emm_year_depth1_path=", emm_year_depth1_path, "\n", sep = "")
cat("index_fig_path=", index_fig_path, "\n", sep = "")
cat("index_compare_fig_path=", index_compare_fig_path, "\n", sep = "")
cat("depth_missing_year_fig_path=", depth_missing_year_fig_path, "\n", sep = "")
cat("depth_missing_area_fig_path=", depth_missing_area_fig_path, "\n", sep = "")
cat("depth_effect_path=", depth_effect_path, "\n", sep = "")
cat("depth_effect_best_fig_path=", depth_effect_best_fig_path, "\n", sep = "")
cat("depth_fig_path=", depth_fig_path, "\n", sep = "")
cat("spatial_smooth_effect_best_path=", spatial_smooth_effect_best_path, "\n", sep = "")
cat("spatial_smooth_prediction_grid_path=", spatial_smooth_prediction_grid_path, "\n", sep = "")
cat("emm_year_spatial_best_path=", emm_year_spatial_best_path, "\n", sep = "")
cat("depth_raw_quantiles_path=", depth_raw_quantiles_path, "\n", sep = "")
cat("depth_raw_hist_path=", depth_raw_hist_path, "\n", sep = "")
cat("depth_fig_average_year_path=", depth_fig_average_year_path, "\n", sep = "")
cat("n_used_for_depth_glmm=", n_used_for_depth_glmm, "\n", sep = "")
cat("n_dropped_for_depth_glmm=", n_dropped_for_depth_glmm, "\n", sep = "")
cat("dropped_prop_for_depth_glmm=", dropped_prop_for_depth_glmm, "\n", sep = "")
cat("depth_glmm_ref_for_year_index=", depth_glmm_ref, "\n", sep = "")
cat("effort_glmm_ref_for_year_index=1\n", sep = "")
cat("depth_plot_year_ref=", year_ref, "\n", sep = "")
cat("depth_plot_month_condition=average_over_month_levels\n", sep = "")
