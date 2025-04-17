test_that("quantify_vector_kgc works", {
  geom = rnaturalearth::ne_countries(country = "Japan") |> sf::st_geometry()
  out = quantify_vector_kgc(geom) |>
    expect_s3_class("data.frame")

  quantify_vector_kgc(list(geom, geom)) |>
    expect_identical(rbind(out, out), ignore_attr = TRUE)
})

test_that("measure_vector_kgc works", {
  geom = rnaturalearth::ne_countries(country = "Japan") |> sf::st_geometry()
  dsn = tempfile(fileext = ".gpkg")
  sf::write_sf(geom, dsn)
  obj = measure_vector_kgc(dsn, overwrite = TRUE) |>
    expect_type("list")
  json = fs::path(fs::path_dir(dsn), "kgc.json")
  expect_true(fs::file_exists(json))
  expect_identical(measure_vector_kgc(dsn), obj)
})
