read_sf_cache = function(dsn, ...) {
  call_cache(sf::read_sf, dsn, ...)
}

drop_geometry = function(x) {
  y = sf::st_set_geometry(x, NULL)
  dplyr::mutate(y, dplyr::across(dplyr::where(is.character), sanitize_column))
}

try_s2_area = function(x) {
  try_s2(x, sf::st_area)
}

try_s2_perimeter = function(x) {
  try_s2(x, sf::st_perimeter)
}

try_s2_union = function(x) {
  try_s2(x, sf::st_union)
}

try_s2_crop = function(x, mask, ...) {
  try_s2(x, sf::st_crop, mask, ...)
}

try_s2 = function(.x, .f, ...) {
  withr::local_options(list(sf_use_s2 = TRUE))
  f_quo = rlang::enquo(.f)
  tryCatch(
    .f(.x, ...),
    error = \(e) {
      f_name = rlang::quo_text(f_quo)
      tryCatch(
        {
          out = withr::with_options(list(sf_use_s2 = FALSE), .f(.x, ...))
          message(f_name, "() run with 'sf_use_s2 = FALSE': ", e$message)
          out
        },
        error = \(e) {
          warning(f_name, "() failed with 'sf_use_s2 = FALSE': ", e$message, call. = FALSE)
          NA
        }
      )
    }
  )
}

with_s2_false = function(...) {
  withr::with_options(list(sf_use_s2 = FALSE), ...)
}
