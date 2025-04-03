#' Quantify spatial data in raster format
#'
#' `quantify_raster()` calculates various diversity indices from raster layers.
#' `table_raster()` counts the occurrences.
#'
#' @param ras A `stars` object.
#' @param MARGIN A vector giving the subscripts that the function is applied over.
#' See [apply()]. Relevant only when `ras` has more than 2 dimensions.
#' @returns `quantify_raster()` returns a list of data.frames with the same length
#' as the input raster.
#' @rdname quantify-raster
#' @export
#' @examples
#' ras = raster_kgc()
#' quantify_raster(ras)
#'
#' table_raster(ras, use.names = TRUE)
quantify_raster = function(ras, MARGIN = 3L) {
  if (length(dim(ras)) > 2L) {
    st_apply_summarize_tabulate(ras, MARGIN = MARGIN)
  } else {
    lapply(ras, summarize_tabulate)
  }
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

st_apply_summarize_tabulate = function(ras, MARGIN = 3L) {
  lsts = stars::st_apply(ras, MARGIN, summarize_tabulate, simplify = FALSE, rename = FALSE)
  lapply(lsts, purrr::list_rbind)
}
