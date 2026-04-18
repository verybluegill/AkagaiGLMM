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

if (!file.exists(input_path)) {
  stop("Input file not found: ", input_path)
}

glmm_input <- readr::read_csv(input_path, show_col_types = FALSE)

required_cols <- c("year", "month", "area", "vessel", "effort", "count_total")
missing_cols <- setdiff(required_cols, names(glmm_input))

if (length(missing_cols) > 0) {
  cat("Missing required columns:\n")
  print(missing_cols)
  stop("Required columns are missing.")
}

if (any(is.na(glmm_input$count_total))) {
  stop("count_total contains NA.")
}

if (any(is.na(glmm_input$effort)) || any(!is.finite(glmm_input$effort)) || any(glmm_input$effort <= 0)) {
  stop("effort must be finite and > 0 before fitting the GLMM.")
}

glmm_dat <- glmm_input |>
  mutate(
    year = factor(year),
    month = factor(month, levels = sort(unique(month))),
    area = factor(area),
    vessel = factor(vessel),
    effort = as.numeric(effort),
    count_total = as.numeric(count_total)
  )

# 固定効果は year + month、ランダム効果は area と vessel、effort は offset(log(effort)) で入れる
fit_formula <- count_total ~ year + month + offset(log(effort)) + (1 | area) + (1 | vessel)

fit_nb_total <- glmmTMB(
  formula = fit_formula,
  family = nbinom2(),
  data = glmm_dat
)

saveRDS(fit_nb_total, fit_path)

cat("\n=== model summary ===\n")
print(summary(fit_nb_total))

if (isTRUE(optional_pkgs[["DHARMa"]])) {
  cat("\n=== DHARMa residual diagnostics ===\n")
  print(DHARMa::testDispersion(DHARMa::simulateResiduals(fit_nb_total, plot = FALSE)))
}

if (isTRUE(optional_pkgs[["emmeans"]])) {
  emm_year_total <- emmeans::emmeans(fit_nb_total, specs = ~ year, type = "response")
  emm_year_total_tbl <- as.data.frame(emm_year_total)
} else {
  pred_grid <- expand_grid(
    year = factor(levels(glmm_dat$year), levels = levels(glmm_dat$year)),
    month = factor(levels(glmm_dat$month), levels = levels(glmm_dat$month)),
    area = factor(levels(glmm_dat$area)[1], levels = levels(glmm_dat$area)),
    vessel = factor(levels(glmm_dat$vessel)[1], levels = levels(glmm_dat$vessel)),
    effort = 1
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
