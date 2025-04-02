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

#' Utilities for caching
#'
#' Some expensive operations are cached to avoid recomputing them.
#' Intermediate results are stored in a cache directory.
#' Users can set it via `habistats.cache_dir` option.
#' The default is set with [tools::R_user_dir()].
#'
#' General recommendation is to set it to a persistent directory with easy access,
#' such as `~/.cache/habistats`, not a temporary directory like [tempdir()].
#' If you are working on a shared machine with other users,
#' setting the same cache directory will allow you to share the cache.
#'
#' @returns `cache_dir()` returns the path to the cache directory.
#' @rdname cache-dir
#' @export
#' @examples
#' old = options(habistats.cache_dir = "~/.cache/habistats-example")
#'
#' habistats::cache_dir()
#'
#' \dontrun{
#'   # Calculate the size
#'   fs::dir_info(habistats::cache_dir(), recurse = TRUE)$size |> sum()
#' }
#'
#' options(old) # reset for this example, not needed in real use
cache_dir = function() {
  fs::path(getOption("habistats.cache_dir", tools::R_user_dir("habistats", "cache")))
}

call_cache = function(.f, .x, ...) {
  f_quo = rlang::enquo(.f)
  f_name = rlang::quo_text(f_quo)
  .key = paste(f_name, .x, sep = "(")
  envir = cache_env()
  if (!exists(.key, envir)) {
    value = .f(.x, ...)
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
