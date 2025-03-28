sanitize_column = function(x) {
  stringr::str_trim(x) |>
    stringr::str_replace_all("[\n\t]+", ";")
}

reduce = function(.x, .f) do.call(.f, .x)

cache_dir = function() {
  fs::path(getOption("habistats.cache_dir", tools::R_user_dir("habistats", "cache")))
}

call_cache = function(.f, .key, ...) {
  envir = cache_env()
  if (!exists(.key, envir)) {
    value = .f(.key, ...)
    assign(.key, value, envir)
  }
  get(.key, envir)
}

cache_env = function() {
  if (is.null(asNamespace("habistats")$.__DEVTOOLS__)) {
    .cache_env
  } else {
    .GlobalEnv
  }
}

.cache_env = new.env(parent = emptyenv())
