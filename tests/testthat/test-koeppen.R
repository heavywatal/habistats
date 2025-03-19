test_that("raster_kmz works", {
  kmz = raster_kmz()
  expect_s4_class(kmz, "SpatRaster")
  expect_identical(terra::nlyr(kmz), 1)
  expect_identical(terra::nrow(kmz), 6480)
  expect_identical(terra::ncol(kmz), 12960)
  expect_identical(terra::ncell(kmz), 12960 * 6480)
  expect_identical(as.vector(terra::minmax(kmz)), c(1, 32))
})
