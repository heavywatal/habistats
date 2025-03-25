#' Diversity Indices
#'
#' @description
#' [`summarize_raster()`] calculates various diversity indices from a raster layer.
#' @param ras A `stars` object.
#' @rdname diversity
#' @export
summarize_raster = function(ras) {
  cnt = count_values(ras)
  summarize_count(cnt)
}

summarize_count = function(cnt) {
  data.frame(
    pixels = sum(cnt),
    richness = length(cnt),
    idx_shannon = tabula::index_shannon(cnt),
    idx_simpson = tabula::index_simpson(cnt),
    idx_brillouin = tabula::index_brillouin(cnt)
  )
}

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
  table_int(values)
}

get_values_na_omit = function(ras) {
  values = as.vector(ras[[1L]])
  as.vector(stats::na.omit(values))
}

table_int = function(x) {
  tab = table(x, dnn = NULL)
  as.integer(tab)
}
