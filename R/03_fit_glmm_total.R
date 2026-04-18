# =========================================
# 03_fit_glmm_total.R
# GLMM-ready CSV を読み、count_total に対して total 用の負の二項 GLMM を 1 本当てる
# =========================================

source(file.path("R", "00_load_packages.R"))

optional_pkgs <- load_project_packages(
  required_pkgs = c("tidyverse", "glmmTMB"),
  optional_pkgs = c("emmeans", "DHARMa")
)

ensure_project_dirs()

input_path <- file.path("data_processed", "akagai_glmm_input.csv")
fit_path <- file.path("output", "models", "fit_nb_total.rds")
emm_year_path <- file.path("output", "tables", "emm_year_total.csv")
index_fig_path <- file.path("output", "figures", "index_total_by_year.png")
row_summary_by_year_path <- file.path("output", "tables", "check_total_glmm_row_summary_by_year.csv")

if (!file.exists(input_path)) {
  stop("Input file not found: ", input_path)
}

glmm_input <- readr::read_csv(input_path, show_col_types = FALSE)

required_cols <- c("year", "month", "area", "vessel", "effort_glmm", "count_total")
missing_cols <- setdiff(required_cols, names(glmm_input))

if (length(missing_cols) > 0) {
  cat("Missing required columns:\n")
  print(missing_cols)
  stop("Required columns are missing.")
}

if (any(is.na(glmm_input$count_total))) {
  stop("count_total contains NA.")
}

glmm_input <- glmm_input |>
  mutate(
    effort_glmm = as.numeric(effort_glmm),
    count_total = as.numeric(count_total)
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
  stop("No rows available for the total GLMM after filtering.")
}

if (any(is.na(glmm_dat$effort_glmm)) || any(!is.finite(glmm_dat$effort_glmm)) || any(glmm_dat$effort_glmm <= 0)) {
  stop("effort_glmm must be finite and > 0 in the filtered data.")
}

# 固定効果は year + month、ランダム効果は area と vessel、effort_glmm は offset(log(effort_glmm)) で入れる
fit_formula <- count_total ~ year + month + offset(log(effort_glmm)) + (1 | area) + (1 | vessel)

fit_nb_total <- glmmTMB(
  formula = fit_formula,
  family = nbinom2(),
  data = glmm_dat
)

saveRDS(fit_nb_total, fit_path)

cat("\n=== model summary ===\n")
print(summary(fit_nb_total))

fit_convergence_code <- fit_nb_total$fit$convergence
fit_pdHess <- isTRUE(fit_nb_total$sdr$pdHess)

cat("\n=== convergence check ===\n")
cat("fit_convergence_code=", fit_convergence_code, "\n", sep = "")
cat("pdHess=", fit_pdHess, "\n", sep = "")

if (fit_convergence_code != 0 || !fit_pdHess) {
  warning("Convergence issue may exist in fit_nb_total")
}

if (isTRUE(optional_pkgs[["DHARMa"]])) {
  cat("\n=== DHARMa residual diagnostics ===\n")
  print(DHARMa::testDispersion(DHARMa::simulateResiduals(fit_nb_total, plot = FALSE)))
}

if (isTRUE(optional_pkgs[["emmeans"]])) {
  emm_year_total <- emmeans::emmeans(fit_nb_total, specs = ~ year, at = list(effort_glmm = 1), type = "response")
  emm_year_total_tbl <- as.data.frame(emm_year_total)
} else {
  pred_grid <- expand_grid(
    year = factor(levels(glmm_dat$year), levels = levels(glmm_dat$year)),
    month = factor(levels(glmm_dat$month), levels = levels(glmm_dat$month)),
    area = factor(levels(glmm_dat$area)[1], levels = levels(glmm_dat$area)),
    vessel = factor(levels(glmm_dat$vessel)[1], levels = levels(glmm_dat$vessel)),
    effort_glmm = 1
  )

  emm_year_total_tbl <- pred_grid |>
    mutate(predicted = predict(fit_nb_total, newdata = pred_grid, type = "response", re.form = NA)) |>
    group_by(year) |>
    summarise(response = mean(predicted), .groups = "drop")
}

write_csv(emm_year_total_tbl, emm_year_path)

year_index_col <- intersect(c("response", "rate", "prob", "emmean"), names(emm_year_total_tbl))

if (length(year_index_col) == 0) {
  stop("Failed to identify the year effect column for plotting.")
}

year_index_col <- year_index_col[[1]]

index_total_by_year <- ggplot(emm_year_total_tbl, aes(x = year, y = .data[[year_index_col]], group = 1)) +
  geom_line() +
  geom_point(size = 2) +
  labs(
    x = "Year",
    y = "Index"
  )

if (all(c("lower.CL", "upper.CL") %in% names(emm_year_total_tbl))) {
  index_total_by_year <- index_total_by_year +
    geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL), width = 0.15)
}

ggsave(
  filename = index_fig_path,
  plot = index_total_by_year,
  width = 10,
  height = 6,
  dpi = 150
)

cat("\n=== saved files ===\n")
cat(fit_path, "\n", sep = "")
cat(emm_year_path, "\n", sep = "")
cat(index_fig_path, "\n", sep = "")
cat(row_summary_by_year_path, "\n", sep = "")
