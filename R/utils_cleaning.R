# =========================================
# utils_cleaning.R
# raw check と clean 作成で共通利用する補助関数・定数
# =========================================

# 数値列を文字列トリム後に numeric へ変換する
parse_numeric_trim <- function(x) {
  x_chr <- trimws(as.character(x))
  x_chr[x_chr %in% c("", "NA", "NaN")] <- NA_character_
  suppressWarnings(as.numeric(x_chr))
}

depth_threshold_m <- 150

# 深度2列から既存ルールどおりに利用深度を決める
make_depth_use <- function(depth_raw_1, depth_raw_2) {
  dplyr::case_when(
    is.na(depth_raw_1) & is.na(depth_raw_2) ~ NA_real_,
    !is.na(depth_raw_1) & is.na(depth_raw_2) ~ depth_raw_1,
    is.na(depth_raw_1) & !is.na(depth_raw_2) ~ depth_raw_2,
    !is.na(depth_raw_1) & !is.na(depth_raw_2) & depth_raw_1 <= depth_threshold_m & depth_raw_2 <= depth_threshold_m ~ (depth_raw_1 + depth_raw_2) / 2,
    !is.na(depth_raw_1) & !is.na(depth_raw_2) & ((depth_raw_1 <= depth_threshold_m & depth_raw_2 > depth_threshold_m) | (depth_raw_1 > depth_threshold_m & depth_raw_2 <= depth_threshold_m)) ~ pmin(depth_raw_1, depth_raw_2),
    !is.na(depth_raw_1) & !is.na(depth_raw_2) & depth_raw_1 > depth_threshold_m & depth_raw_2 > depth_threshold_m ~ NA_real_,
    TRUE ~ NA_real_
  )
}

# 利用可能な海域コードの一覧
valid_area_codes <- c(151L, 152L, 161L, 162L, 171L, 172L, 181L, 182L, 191L, 192L, 201L, 202L, 203L, 212L, 213L, 214L, 223L, 224L)
