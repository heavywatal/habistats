test_that("lmap works", {
  x = list(a = 1.0, b = 2.0)
  n = length(x)
  lmap_lgl(x, is.list) |>
    expect_type("logical") |>
    expect_length(n) |>
    all() |>
    expect_true()
  lmap_int(x, length) |>
    expect_type("integer") |>
    expect_length(n) |>
    expect_identical(rep(1L, n) |> setNames(names(x)))
  lmap_dbl(x, unlist) |>
    expect_type("double") |>
    expect_length(n) |>
    expect_identical(unlist(x))
  lmap_chr(x, names) |>
    expect_type("character") |>
    expect_length(n) |>
    expect_identical(names(x) |> setNames(names(x)))
  lmap_vec(x, \(.) Sys.Date()) |>
    expect_s3_class("Date") |>
    expect_type("double") |>
    expect_length(n)
})

test_that("mclmap works", {
  x = list(a = 1.0, b = 2.0)
  n = length(x)
  mclmap_lgl(x, is.list) |>
    expect_type("logical") |>
    expect_length(n) |>
    all() |>
    expect_true()
  mclmap_int(x, length) |>
    expect_type("integer") |>
    expect_length(n) |>
    expect_identical(rep(1L, n) |> setNames(names(x)))
  mclmap_dbl(x, unlist) |>
    expect_type("double") |>
    expect_length(n) |>
    expect_identical(unlist(x))
  mclmap_chr(x, names) |>
    expect_type("character") |>
    expect_length(n) |>
    expect_identical(names(x) |> setNames(names(x)))
  mclmap_vec(x, \(.) Sys.Date()) |>
    expect_s3_class("Date") |>
    expect_type("double") |>
    expect_length(n)
})
