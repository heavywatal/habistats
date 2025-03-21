test_that("rowwise_map works", {
  n = nrow(mtcars)
  rowwise_map_lgl(mtcars, is.data.frame) |>
    expect_type("logical") |>
    expect_length(n) |>
    all() |>
    expect_true()
  rowwise_map_int(mtcars, nrow) |>
    expect_type("integer") |>
    expect_length(n) |>
    expect_identical(rep(1L, n))
  rowwise_map_dbl(mtcars, sum) |>
    expect_type("double") |>
    expect_length(n) |>
    expect_identical(rowSums(mtcars) |> as.vector())
  rowwise_map_chr(mtcars, toString) |>
    expect_type("character") |>
    expect_length(n) |>
    expect_identical(apply(mtcars, 1L, toString) |> as.vector())
  rowwise_map_vec(mtcars, \(.) Sys.Date()) |>
    expect_s3_class("Date") |>
    expect_type("double") |>
    expect_length(n)
})

test_that("rowwise_mcmap works", {
  n = nrow(mtcars)
  rowwise_mcmap_lgl(mtcars, is.data.frame) |>
    expect_type("logical") |>
    expect_length(n) |>
    all() |>
    expect_true()
  rowwise_mcmap_int(mtcars, nrow) |>
    expect_type("integer") |>
    expect_length(n) |>
    expect_identical(rep(1L, n))
  rowwise_mcmap_dbl(mtcars, sum) |>
    expect_type("double") |>
    expect_length(n) |>
    expect_identical(rowSums(mtcars) |> as.vector())
  rowwise_mcmap_chr(mtcars, toString) |>
    expect_type("character") |>
    expect_length(n) |>
    expect_identical(apply(mtcars, 1L, toString) |> as.vector())
  rowwise_mcmap_vec(mtcars, \(.) Sys.Date()) |>
    expect_s3_class("Date") |>
    expect_type("double") |>
    expect_length(n)
})
