test_that("raster_kgc works", {
  x = raster_kgc()
  expect_s3_class(x, "stars")
  expect_length(x, 1L)
  expect_identical(dim(x), c(x = 4320L, y = 2160L))
  expect_s3_class(x[[1L]], "factor")
  expect_length(levels(x[[1L]]), 32L)
})

test_that("raster_kgc works with mask", {
  geo = rnaturalearth::ne_countries(country = "Japan")
  x = raster_kgc(geo, keep_ocean = TRUE) |>
    expect_s3_class("stars") |>
    expect_length(1L)
  values = x[[1L]]
  expect_s3_class(values, "factor")
  expect_length(levels(values), 32L)
  n_na = sum(is.na(values)) |>
    expect_gt(0L)
  n_ocean = sum(values == "Ocean", na.rm = TRUE) |>
    expect_gt(0L)

  trimmed = raster_kgc(geo)[[1L]]
  expect_length(levels(trimmed), 32L)
  expect_identical(sum(trimmed == "Ocean", na.rm = TRUE), 0L)
  expect_identical(sum(is.na(trimmed)), n_na + n_ocean)

  expect_s3_class(scale_color_kgc(), c("ScaleDiscrete", "Scale", "ggproto"))
  expect_s3_class(scale_fill_kgc(), c("ScaleDiscrete", "Scale", "ggproto"))
  p = ggplot2::ggplot() +
    stars::geom_stars(data = x) +
    scale_fill_kgc() +
    scale_color_kgc()
  expect_s3_class(p, "gg")
})
