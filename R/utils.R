slice_max_cum = function(.data, order_by, at_least = 0.95) {
  .data = dplyr::arrange(.data, dplyr::desc({{ order_by }}))
  x = dplyr::pull(.data, {{ order_by }})
  revcum = rev(cumsum(rev(x)))
  .data[revcum / sum(x) > (1 - at_least), , drop = FALSE]
}

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
