read_sf_cache = function(dsn, ...) {
  call_cache(sf::read_sf, dsn, ...)
}

read_sf_union_geometry = function(dsn) {
  cache_file = fs::path(fs::path_dir(dsn), "union.gpkg")
  if (fs::file_exists(cache_file)) {
    sf::read_sf(cache_file)
  } else {
    t_start = Sys.time()
    obj = sf::read_sf(dsn)
    geom = sf::st_geometry(obj)
    if (length(geom) > 1L) {
      geom = try_s2_union(geom)
    }
    t_end = Sys.time()
    if (t_end - t_start > 3.0 && startsWith(dsn, cache_dir())) {
      sf::write_sf(geom, cache_file)
    }
    geom
  }
}

drop_geometry = function(x) {
  y = sf::st_set_geometry(x, NULL)
  dplyr::mutate(y, dplyr::across(dplyr::where(is.character), sanitize_column))
}

remove_minor_polygons = function(x, p = 0.95) {
  len = length(x[[1L]])
  if (len > 1L) {
    polygons = sf::st_cast(x, "POLYGON")
    d = data.frame(
      idx = seq_len(len),
      area = as.numeric(sf::st_area(polygons))
    )
    d = slice_max_cum(d, "area", p)
    x = polygons[d$idx]
    if (length(x) > 1L) {
      x = sf::st_combine(x)
    }
  }
  x
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
