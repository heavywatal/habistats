test_that("sanitize_column works", {
  expect_identical(sanitize_column(" hello\n world\n"), "hello; world")
})

test_that("reduce works", {
  x = data.frame(a = 1:2, b = 3:4)
  expect_identical(reduce(x, file.path), c("1/3", "2/4"))
})

test_that("cache_dir works", {
  temp = fs::path(tempdir())
  withr::with_options(
    list(habistats.cache_dir = temp),
    expect_identical(cache_dir(), temp)
  )
  withr::with_options(
    list(habistats.cache_dir = NULL),
    expect_identical(cache_dir(), fs::path(tools::R_user_dir("habistats", "cache")))
  )
})

test_that("call_cache works", {
  if (exists("hello", cache_env())) {
    rm("hello", envir = cache_env())
  }
  expect_output(call_cache(print, "hello"), "hello") |>
    expect_identical("hello")
  expect_silent(call_cache(print, "hello")) |>
    expect_identical("hello")
  rm("hello", envir = cache_env())
})
