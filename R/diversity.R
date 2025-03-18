#' Diversity Indices
#'
#' @description
#' [`index_brillouin()`] calculates the Brillouin diversity index for a raster layer.
#' @param ras RasterLayer object.
#' @rdname diversity
#' @export
index_brillouin = function(ras) {
  cnt = count_values(ras)
  tabula::index_brillouin(cnt)
}

#' @description
#' [`count_values()`] counts the occurrence of values in a raster layer.
#' @rdname diversity
#' @export
count_values = function(ras) {
  values = get_values_na_omit(ras)
  tab = table(values, dnn = NULL)
  as.integer(tab)
}

get_values_na_omit = function(ras) {
  values = raster::values(ras)
  as.vector(stats::na.omit(values))
}
