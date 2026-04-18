# =========================================
# calc_area_centroids.R
# 赤点座標と対応表から各漁区の重心を計算する
# =========================================

library(readr)
library(dplyr)
library(purrr)
library(tibble)
library(sf)
library(ggplot2)

# -----------------------------------------
# 1. 点座標の読み込み
# -----------------------------------------
pts <- read_csv(
  "PrepareDummyData/DataFromFig/AreaLonLat.csv",
  col_names = FALSE,
  show_col_types = FALSE
) |>
  setNames(c("lon", "lat")) |>
  mutate(point_id = row_number())

cat("=== Point table summary ===\n")
cat("n_points =", nrow(pts), "\n")
cat("point_id range =", min(pts$point_id), "to", max(pts$point_id), "\n\n")

print(pts)

# -----------------------------------------
# 2. 漁区ごとの頂点対応表
# -----------------------------------------
polygon_def <- list(
  "151" = c(1, 2, 5, 6),
  "152" = c(2, 3, 4, 5),
  "161" = c(7, 9, 10, 8),
  "162" = c(9, 11, 12, 10),
  "171" = c(17, 16, 15, 18),
  "172" = c(16, 13, 14, 15),
  "181" = c(19, 20, 23, 24),
  "182" = c(20, 21, 22, 23),
  "191" = c(25, 64, 27, 26),
  "192" = c(64, 29, 30, 31, 32, 27),
  "201" = c(26, 28, 44, 43, 42, 41, 40, 39),
  "202" = c(28, 32, 37, 38),
  "203" = c(32, 33, 34, 35, 36, 37),
  "212" = c(38, 37, 48, 47, 46, 45),
  "213" = c(37, 36, 62, 61),
  "214" = c(36, 57, 58, 59, 60, 62),
  "223" = c(61, 62, 63, 51, 50, 49),
  "224" = c(62, 60, 56, 55, 54, 53, 52, 63)
)

# -----------------------------------------
# 3. polygon作成関数
# -----------------------------------------
make_polygon_from_ids <- function(id_vec, pts_df) {
  xy <- pts_df |>
    filter(point_id %in% id_vec) |>
    slice(match(id_vec, point_id)) |>
    select(lon, lat) |>
    as.matrix()
  
  # polygon を閉じる
  if (!all(xy[1, ] == xy[nrow(xy), ])) {
    xy <- rbind(xy, xy[1, ])
  }
  
  st_polygon(list(xy))
}

# -----------------------------------------
# 4. 点番号の妥当性チェック
# -----------------------------------------
all_ids_used <- sort(unique(unlist(polygon_def)))
missing_ids <- setdiff(all_ids_used, pts$point_id)

cat("=== Polygon ID check ===\n")
if (length(missing_ids) == 0) {
  cat("All point IDs used in polygon_def exist in AreaLonLat.csv\n\n")
} else {
  cat("Missing point IDs:", paste(missing_ids, collapse = ", "), "\n\n")
}

# -----------------------------------------
# 5. sf polygon 作成
# -----------------------------------------
poly_sf <- imap_dfr(
  polygon_def,
  ~ tibble(
    area_id = .y,
    geometry = st_sfc(make_polygon_from_ids(.x, pts), crs = 4326)
  )
) |>
  st_as_sf()

# -----------------------------------------
# 6. 重心計算
# -----------------------------------------
cent_sf <- st_centroid(poly_sf)

cent_df <- cent_sf |>
  mutate(
    lon = st_coordinates(geometry)[, 1],
    lat = st_coordinates(geometry)[, 2]
  ) |>
  st_drop_geometry() |>
  select(area_id, lon, lat) |>
  arrange(as.numeric(area_id))

cat("=== Area centroids ===\n")
print(cent_df)

# -----------------------------------------
# 7. CSV保存
# -----------------------------------------
write_csv(cent_df, "area_centroids.csv")
cat("\nSaved: area_centroids.csv\n")

# -----------------------------------------
# 8. 検証図
# -----------------------------------------
pts_sf <- st_as_sf(pts, coords = c("lon", "lat"), crs = 4326)

p_check <- ggplot() +
  geom_sf(data = poly_sf, fill = NA, color = "black", linewidth = 0.5) +
  geom_sf(data = pts_sf, size = 1.5) +
  geom_sf_text(data = pts_sf, aes(label = point_id), nudge_y = 0.0004, size = 2.8) +
  geom_sf(data = cent_sf, size = 2) +
  geom_sf_text(data = cent_sf, aes(label = area_id), nudge_y = 0.0005, size = 3.5) +
  labs(
    x = "Longitude",
    y = "Latitude",
    title = "Area polygons and centroids"
  ) +
  theme_bw()

print(p_check)

ggsave(
  filename = "area_centroids_check.png",
  plot = p_check,
  width = 12,
  height = 8,
  dpi = 300
)

cat("Saved: area_centroids_check.png\n")