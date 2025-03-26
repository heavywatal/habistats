test_that("iucn_source_path works", {
  temp = fs::path(tempdir())
  withr::with_options(
    list(habistats.iucn_source = temp),
    expect_identical(iucn_source_path(), temp)
  )
  withr::with_options(
    list(habistats.iucn_source = NULL),
    expect_identical(iucn_source_path(), fs::path("~/db/iucnredlist.org"))
  )
})
