#' Qantify spatial data in raster format
#'
#' `quantify_raster()` calculates various diversity indices from raster layers.
#' `table_raster()` counts the occurrences.
#'
#' @param ras A `stars` object.
#' @returns `quantify_raster()` returns a data.frame.
#' @rdname quantify-raster
#' @export
#' @examples
#' ras = raster_kgc()
#' quantify_raster(ras)
#'
#' table_raster(ras, use.names = TRUE)
quantify_raster = function(ras) {
  cnt = table_raster(ras)
  rows = lapply(cnt, summarize_count)
  purrr::list_rbind(rows)
}

#' @param ... Additional arguments passed to [tabulate()] or [table()].
#' @param use.names A logical. If `TRUE`, the levels in the input are preserved
#' as names of the output.
#' @returns `table_raster()` returns a list of integer vectors.
#' @rdname quantify-raster
#' @export
table_raster = function(ras, ..., use.names = FALSE) {
  lapply(ras, table_int, ..., use.names = use.names)
}
