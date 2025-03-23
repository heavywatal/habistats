#' Köppen-Geiger Climate Classification
#'
#' @format A `stars` object with 2 dimensions and 1 attribute.
#' @source <https://koeppen-geiger.vu-wien.ac.at>
#'
#' @description
#' [`raster_kgc()`] returns the KGC raster masked by a given polygon.
#' @param mask sf or sfc object.
#' @rdname koeppen
#' @export
raster_kgc = function(mask = NULL) {
  if (is.null(mask)) {
    kg5m
  } else {
    suppressMessages(sf::st_crop(kg5m, mask))
  }
}

augment_raster = function(x, na.rm = FALSE) {
  res = as.data.frame(x)
  class(res) = c("tbl_df", "tbl", "data.frame")
  names(res)[3L] = "climate"
  res
}

.ignore_unused_imports = function() {
  stars::read_stars
}
