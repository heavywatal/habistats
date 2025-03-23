test_that("raster_kgc works", {
  x = raster_kgc()
  expect_s3_class(x, "stars")
  expect_length(x, 1L)
  expect_identical(dim(x), c(x = 4320L, y = 2160L))
  expect_s3_class(x[[1L]], "factor")
  expect_length(levels(x[[1L]]), 32L)
})
