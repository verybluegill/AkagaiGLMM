# =========================================
# plot_annual_trends.R
# 年別推移図を作成する
# =========================================

source(file.path("R", "make_overview_figure.R"))

size_id_to_label_en <- function(size_id) {
  dplyr::case_when(
    size_id == "medium" ~ "Medium",
    size_id == "dai" ~ "Large",
    size_id == "toku" ~ "Special",
    size_id == "tokudai" ~ "Extra large",
    size_id == "waregai" ~ "Broken shell",
    size_id == "sho" ~ "Small",
    size_id == "shosho" ~ "Very small",
    TRUE ~ as.character(size_id)
  )
}

make_plot_data_total <- function(clean_dat) {
  clean_dat |>
    dplyr::filter(!is.na(.data$year)) |>
    dplyr::group_by(.data$year) |>
    dplyr::summarise(total_count = sum(.data$count, na.rm = TRUE), .groups = "drop")
}

make_plot_data_by_size <- function(clean_dat) {
  size_order <- c("medium", "dai", "toku", "tokudai")

  clean_dat |>
    dplyr::filter(!is.na(.data$year), .data$size %in% size_order) |>
    dplyr::group_by(.data$year, .data$size) |>
    dplyr::summarise(total_count = sum(.data$count, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      size = factor(.data$size, levels = size_order),
      size_label = factor(size_id_to_label_en(as.character(.data$size)), levels = size_id_to_label_en(size_order))
    )
}

plot_annual_total <- function(plot_data, output_png = file.path("output", "figures", "annual_trend_total.png")) {
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$year, y = .data$total_count)) +
    ggplot2::geom_line(linewidth = 1.2, color = "#2166AC") +
    ggplot2::geom_point(size = 2.8, color = "#2166AC") +
    ggplot2::scale_x_continuous(breaks = sort(unique(plot_data$year))) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::labs(
      title = "Annual trend of total abundance",
      x = "Year",
      y = "Count"
    ) +
    theme_akagai_report()

  ggplot2::ggsave(
    filename = output_png,
    plot = p,
    width = 10,
    height = 5.5,
    dpi = 300
  )

  cat("saved:", output_png, "\n")
}

plot_annual_by_size <- function(plot_data, output_png = file.path("output", "figures", "annual_trend_by_size.png")) {
  p <- ggplot2::ggplot(plot_data, ggplot2::aes(x = .data$year, y = .data$total_count, color = .data$size_label, group = .data$size_label)) +
    ggplot2::geom_line(linewidth = 1.15) +
    ggplot2::geom_point(size = 2.5) +
    ggplot2::scale_x_continuous(breaks = sort(unique(plot_data$year))) +
    ggplot2::scale_y_continuous(labels = scales::comma) +
    ggplot2::scale_color_manual(
      values = c(
        "Medium" = "#4E79A7",
        "Large" = "#59A14F",
        "Special" = "#E15759",
        "Extra large" = "#9C755F"
      ),
      drop = FALSE
    ) +
    ggplot2::labs(
      title = "Annual trend by size class",
      x = "Year",
      y = "Count",
      color = "Size"
    ) +
    theme_akagai_report() +
    ggplot2::theme(legend.position = "right")

  ggplot2::ggsave(
    filename = output_png,
    plot = p,
    width = 10,
    height = 5.5,
    dpi = 300
  )

  cat("saved:", output_png, "\n")
}

run_plot_annual_trends <- function() {
  paths <- get_akagai_default_paths()
  excel_obj <- load_akagai_excel(paths$excel_path)
  clean_dat <- clean_akagai_data(excel_obj$data)
  total_data <- make_plot_data_total(clean_dat)
  by_size_data <- make_plot_data_by_size(clean_dat)

  plot_annual_total(total_data)
  plot_annual_by_size(by_size_data)

  invisible(list(
    excel_sheet = excel_obj$sheet,
    total_data = total_data,
    by_size_data = by_size_data,
    column_map = attr(clean_dat, "column_map")
  ))
}

if (sys.nframe() == 0) {
  run_plot_annual_trends()
}
