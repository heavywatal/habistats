test_that("quantify_raster works", {
  n = 8L
  m = seq_len(n) |> as.factor()
  dim(m) = c(x = n, y = 1L)
  ras = stars::st_as_stars(m)

  cnts = table_raster(ras) |>
    expect_type("list") |>
    expect_length(1L)
  cnts[[1L]] |>
    expect_type("integer") |>
    expect_length(n) |>
    expect_identical(rep.int(1L, n)) |>
    expect_named(NULL)
  table_raster(ras, use.names = TRUE)[[1L]] |>
    expect_identical(cnts[[1L]], ignore_attr = TRUE) |>
    expect_named(levels(m))

  d = quantify_raster(ras) |>
    expect_type("list") |>
    expect_length(length(ras))
  expect_s3_class(d[[1L]], "data.frame")
  expect_identical(nrow(d[[1L]]), 1L)
})

test_that("quantify_raster works for 3D", {
  .dim = c(x = 5L, y = 4L, band = 3L)
  n = prod(.dim)
  m = seq_len(n) |> as.factor()
  dim(m) = .dim
  ras = stars::st_as_stars(m)
  lst = quantify_raster(ras) |>
    expect_type("list") |>
    expect_length(1L)
  expect_s3_class(lst[[1L]], "data.frame")
  expect_identical(nrow(lst[[1L]]), .dim[3L], ignore_attr = TRUE)
})
