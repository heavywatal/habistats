test_that("quantify_vector_kgc works", {
  geom = rnaturalearth::ne_countries(country = "Japan") |> sf::st_geometry()
  out = quantify_vector_kgc(geom) |>
    expect_s3_class("data.frame")

  quantify_vector_kgc(list(geom, geom)) |>
    expect_identical(rbind(out, out), ignore_attr = TRUE)
})
