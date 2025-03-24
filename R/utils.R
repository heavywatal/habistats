call_cache = function(.f, .key, ...) {
  envir = cache_env()
  if (!exists(.key, envir = envir)) {
    value = .f(.key, ...)
    assign(.key, value, envir = envir)
  }
  get(.key, envir = envir)
}

cache_env = function() {
  if (is.null(asNamespace("habistats")$.__DEVTOOLS__)) {
    .cache_env
  } else {
    .GlobalEnv
  }
}

.cache_env = new.env(parent = emptyenv())
