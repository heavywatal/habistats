test_that("raster_kmz works", {
  kmz = raster_kmz()
  expect_s4_class(kmz, "SpatRaster")
  expect_identical(terra::nlyr(kmz), 1)
  expect_identical(terra::nrow(kmz), 6480)
  expect_identical(terra::ncol(kmz), 12960)
  expect_identical(terra::ncell(kmz), 12960 * 6480)
  expect_identical(as.vector(terra::minmax(kmz)), c(1, 32))
})

test_that("raster_kgc works", {
  x = raster_kgc()
  expect_s4_class(x, "SpatRaster")
  expect_identical(terra::nlyr(x), 1)
  expect_identical(terra::nrow(x), 2160)
  expect_identical(terra::ncol(x), 4320)
  expect_identical(terra::ncell(x), 2160  * 4320)
  expect_identical(as.vector(terra::minmax(x)), c(1, 32))
})
