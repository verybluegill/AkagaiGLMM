# =========================================
# 00_load_packages.R
# 解析用スクリプトで共通に使うパッケージ読み込みと出力先作成
# =========================================

load_project_packages <- function(required_pkgs, optional_pkgs = character()) {
  missing_pkgs <- required_pkgs[!vapply(required_pkgs, requireNamespace, logical(1), quietly = TRUE)]

  if (length(missing_pkgs) > 0) {
    stop(
      "Missing required packages: ",
      paste(missing_pkgs, collapse = ", ")
    )
  }

  invisible(lapply(required_pkgs, library, character.only = TRUE))

  optional_available <- stats::setNames(
    vapply(optional_pkgs, requireNamespace, logical(1), quietly = TRUE),
    optional_pkgs
  )

  invisible(optional_available)
}

ensure_project_dirs <- function() {
  dir_candidates <- c(
    "data_processed",
    "output",
    file.path("output", "tables"),
    file.path("output", "figures"),
    file.path("output", "models")
  )

  invisible(lapply(dir_candidates, dir.create, showWarnings = FALSE, recursive = TRUE))
}
