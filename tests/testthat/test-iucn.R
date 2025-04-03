test_that("iucn_source_path works", {
  temp = fs::path(tempdir())
  withr::with_options(
    list(habistats.iucn_source = temp),
    expect_identical(iucn_source_path(), temp)
  )
  extdata = fs::path(system.file("extdata", "iucn_source", package = "habistats"))
  withr::with_options(
    list(habistats.iucn_source = NULL),
    expect_identical(iucn_source_path(), extdata)
  )
})

test_that("iucn_spatial_features works", {
  withr::local_options(list(habistats.cache_dir = "~/.cache/habistats-example"))
  expect_message(
    {
      features = iucn_spatial_features(overwrite = TRUE)
    },
    "Wrote"
  )
  expect_s3_class(features, "data.frame")
  iucn_spatial_features(overwrite = FALSE) |>
    expect_identical(features)
})

test_that("iucn_spatial_species works", {
  withr::local_options(list(habistats.cache_dir = "~/.cache/habistats-example"))
  iucn_spatial_species() |>
    expect_s3_class("data.frame")
})

test_that("iucn_species_gpkg works", {
  withr::local_options(list(habistats.cache_dir = "~/.cache/habistats-example"))
  expect_message(
    {
      gpkgs = iucn_species_gpkg(overwrite = TRUE)
    },
    "Wrote"
  )
  expect_s3_class(gpkgs, "data.frame")
  expect_identical(iucn_species_gpkg(), gpkgs)

  evaluate_iucn_range(gpkgs$source) |>
    expect_s3_class("data.frame")
})
