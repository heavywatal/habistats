test_that("read_sf_union_geometry works", {
  geo_jp = rnaturalearth::ne_countries(country = "Japan") |> sf::st_geometry()
  multi_sf = sf::st_cast(geo_jp, "POLYGON") |> sf::st_sf()
  tmp_gpkg = tempfile(fileext = ".gpkg")
  sf::write_sf(multi_sf, tmp_gpkg)
  read_sf_union_geometry(tmp_gpkg) |>
    expect_s3_class("sfc") |>
    expect_length(1L)
})

test_that("remove_minor_polygon works", {
  geo_jp = rnaturalearth::ne_countries(country = "Japan") |> sf::st_geometry()
  expect_length(geo_jp[[1]], 3L)
  expect_length(remove_minor_polygons(geo_jp, 1)[[1]], 3L)
  expect_length(remove_minor_polygons(geo_jp, 0.8)[[1]], 2L)
  expect_length(remove_minor_polygons(geo_jp, 1e-9)[[1]], 1L)
})

test_that("remove_minor_polygon works", {
  geo_jp = rnaturalearth::ne_countries(country = "Japan") |> sf::st_geometry()
  msg = "although coordinates are longitude/latitude"
  expect_message(with_s2_false(sf::st_union(geo_jp)), msg)
})
