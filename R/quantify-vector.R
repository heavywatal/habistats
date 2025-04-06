#' Quantify spatial data in vector format
#'
#' `measure_vector_kgc()` counts the Köppen-Geiger climate classification
#' in the range cropped with [raster_kgc()], and calculates the basic spatial
#' features of a vector geometry such as perimeter and area.
#' `quantify_vector_kgc()`, in addition, summarizes the counts.
#'
#' @param geom An `sfc` or `sf` object. A path to sf-readable file is accepted.
#' @param overwrite A logical. If `TRUE`, the cache is not used and overwritten.
#' Relevant only when `geom` is a path to a file.
#' @returns `quantify_vector_kgc()` returns a data.frame.
#' @rdname quantify-vector
#' @export
#' @examples
#' geom = rnaturalearth::ne_countries(country = "Japan") |> sf::st_geometry()
#' measure_vector_kgc(geom)
#'
#' quantify_vector_kgc(geom)
quantify_vector_kgc = function(geom, overwrite = FALSE) {
  if (length(geom) > 1L) {
    rows = parallel::mclapply(geom, quantify_vector_kgc)
    out = purrr::list_rbind(rows)
    class(out) = c("tbl_df", "tbl", "data.frame")
    return(dplyr::mutate(out, name = names(geom), .before = 1L))
  }
  obj = measure_vector_kgc(geom, overwrite = overwrite)
  out = summarize_counts(obj$kgc[-32L])
  dplyr::mutate(
    out,
    perimeter = obj$perimeter,
    area = obj$area,
    .before = 1L
  )
}

#' @returns `measure_vector_kgc()` returns a list.
#' @rdname quantify-vector
#' @export
measure_vector_kgc = function(geom, overwrite = FALSE) {
  if (is.character(geom)) {
    measure_vector_kgc_cache(geom, overwrite = overwrite)
  } else {
    measure_vector_kgc_impl(geom)
  }
}

measure_vector_kgc_cache = function(dsn, overwrite = FALSE) {
  json = fs::path(fs::path_dir(dsn), "kgc.json")
  if (fs::file_exists(json) && !overwrite) {
    jsonlite::read_json(json, simplifyVector = TRUE)
  } else {
    geom = read_sf_union_geometry(dsn)
    obj = measure_vector_kgc_impl(geom)
    jsonlite::write_json(obj, json, auto_unbox = TRUE, digits = 12L)
    obj
  }
}

measure_vector_kgc_impl = function(geom) {
  kgc = raster_kgc(geom, keep_ocean = TRUE)
  counts = table_raster(kgc)
  list(
    kgc = counts[[1L]],
    perimeter = try_s2_perimeter(geom) |> as.numeric(),
    area = try_s2_area(geom) |> as.numeric()
  )
}
