#' Köppen-Geiger Climate Classification
#'
#' @format A `stars` object with 2 dimensions and 1 attribute.
#' @source <https://koeppen-geiger.vu-wien.ac.at>
#'
#' @examples
#' library(ggplot2)
#' geo_jp = rnaturalearth::ne_countries(country = "Japan")
#' kgc_jp = raster_kgc(geo_jp) |> print()
#' ggplot() +
#'   stars::geom_stars(data = kgc_jp) +
#'   scale_fill_kgc(na.value = "transparent") +
#'   coord_fixed() +
#'   theme_minimal()
#' @description
#' [`raster_kgc()`] returns the KGC raster masked by a given polygon.
#' @param mask sf or sfc object.
#' @rdname koeppen
#' @export
raster_kgc = function(mask = NULL) {
  if (is.null(mask)) {
    kg5m
  } else {
    requireNamespace("stars", quietly = TRUE)
    sf::st_crop(kg5m, mask)
  }
}

#' @param ... passed to [ggplot2::scale_fill_manual()].
#' @rdname koeppen
#' @export
scale_color_kgc = function(...) {
  ggplot2::scale_color_manual(
    name = "K\u00f6ppen-Geiger Climate",
    values = climate_colors,
    ...
  )
}

#' @rdname koeppen
#' @export
scale_fill_kgc = function(...) {
  ggplot2::scale_fill_manual(
    name = "K\u00f6ppen-Geiger Climate",
    values = climate_colors,
    ...
  )
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
