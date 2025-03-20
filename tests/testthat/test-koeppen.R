test_that("raster_kgc works", {
  x = raster_kgc()
  expect_s4_class(x, "SpatRaster")
  expect_identical(terra::nlyr(x), 1)
  expect_identical(terra::nrow(x), 2160)
  expect_identical(terra::ncol(x), 4320)
  expect_identical(terra::ncell(x), 2160  * 4320)
  expect_identical(as.vector(terra::minmax(x)), c(1, 32))
})
