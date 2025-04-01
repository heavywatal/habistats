test_that("quantify_raster works", {
  n = 8L
  v = seq_len(n)
  m = matrix(v, nrow = n, ncol = 1L)
  dim(m) = c(x = n, y = 1L)
  ras = stars::st_as_stars(m)

  get_values_na_omit(ras) |>
    expect_type("integer") |>
    expect_length(n) |>
    expect_identical(seq_len(n))

  d = quantify_raster(ras) |>
    expect_s3_class("data.frame")
  expect_identical(nrow(d), 1L)
})
