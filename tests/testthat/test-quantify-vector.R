test_that("quantify_vector_kgc works", {
  geom = rnaturalearth::ne_countries(country = "Japan") |> sf::st_geometry()
  quantify_vector_kgc(geom) |>
    expect_s3_class("data.frame")
})
