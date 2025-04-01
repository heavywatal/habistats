#' Qantify spatial data in raster format
#'
#' `quantify_raster()` calculates various diversity indices from a raster layer.
#'
#' @param ras A `stars` object.
#' @returns `quantify_raster()` returns a data.frame.
#' @rdname quantify-raster
#' @export
quantify_raster = function(ras) {
  values = get_values_na_omit(ras)
  cnt = table_int(values)
  summarize_count(cnt)
}

#' @returns `count_values()` returns an integer vector.
#' @rdname quantify-raster
#' @export
get_values_na_omit = function(ras) {
  values = as.vector(ras[[1L]])
  as.vector(stats::na.omit(values))
}
