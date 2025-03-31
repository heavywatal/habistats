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
