test_that("index_brillouin works", {
  n = 8L
  v = seq_len(n)
  ras = raster::raster(vals = v, nrow = n, ncol = 1L)
  d = index_brillouin(ras) |>
    expect_type("double") |>
    expect_length(1) |>
    expect_gte(0)

  ras_mod = raster::raster(vals = v + 42L, nrow = n, ncol = 1L)
  expect_identical(index_brillouin(ras_mod), d)
})
