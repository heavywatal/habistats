#' Köppen-Geiger Climate Classification
#'
#' @seealso <https://koeppen-geiger.vu-wien.ac.at>
#'
#' @description
#' [`raster_kmz()`] returns the KGC raster masked by a given polygon.
#' @param mask sfg object.
#' @rdname koeppen
#' @export
raster_kmz = function(mask = NULL) {
  kmz = raster::raster(vals = kgc::kmz, nrow = 6480L, ncol = 12960L)
  if (!is.null(mask)) {
    mask = methods::as(mask, "Spatial")
    raster::mask(kmz, mask = mask)
  } else {
    kmz
  }
}

kmz_int = function() {
  as.integer(kgc::kmz)
}

kmz_fct = function() {
  factor(kgc::getZone(kgc::kmz), levels = kmz_zones())
}

kmz_zones = function() {
  kgc::getZone(seq_len(32L))
}
