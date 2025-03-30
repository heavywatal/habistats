test_that("remove_minor_polygon works", {
  geo_jp = rnaturalearth::ne_countries(country = "Japan") |> sf::st_geometry()
  expect_length(geo_jp[[1]], 3L)
  expect_length(remove_minor_polygons(geo_jp, 1)[[1]], 3L)
  expect_length(remove_minor_polygons(geo_jp, 0.8)[[1]], 2L)
  expect_length(remove_minor_polygons(geo_jp, 1e-9)[[1]], 1L)
})
