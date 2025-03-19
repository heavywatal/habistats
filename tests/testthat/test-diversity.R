test_that("index_brillouin works", {
  n = 8L
  v = seq_len(n)
  ras = terra::rast(vals = v, nrow = n, ncol = 1L)
  d = index_brillouin(ras) |>
    expect_type("double") |>
    expect_length(1) |>
    expect_gte(0)

  expect_identical(index_brillouin(ras + 42L), d)
})
