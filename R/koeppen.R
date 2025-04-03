#' Köppen-Geiger Climate Classification
#'
#' `raster_kgc()` gives easy access to the Köppen-Geiger climate classification
#' raster data. It can be cropped by a given polygon.
#' `scale_color_kgc()` and `scale_fill_kgc()` are ggplot2 color scales
#' to be used with the KGC raster.
#'
#' @format A `stars` object with 2 dimensions and 1 attribute.
#' @source <https://koeppen-geiger.vu-wien.ac.at>
#'
#' @param mask sf or sfc object for cropping. If `NULL` (default), the full
#' raster is returned.
#' @param keep_ocean A logical. If `FALSE` (default), "Ocean" is converted to
#' `NA` after cropping. Not relevant if `mask` is `NULL`.
#' @returns `raster_kgc()` returns a `stars` object.
#' @rdname koeppen
#' @export
#' @examples
#' library(ggplot2)
#' geo_jp = rnaturalearth::ne_countries(country = "Japan")
#' kgc_jp = habistats::raster_kgc(geo_jp, keep_ocean = TRUE) |> print()
#' ggplot(geo_jp) +
#'   stars::geom_stars(data = kgc_jp) +
#'   geom_sf(linewidth = 1, color = "#00000088", fill = NA) +
#'   scale_fill_kgc(na.value = "transparent") +
#'   theme_minimal()
raster_kgc = function(mask = NULL, keep_ocean = FALSE) {
  if (is.null(mask)) {
    kg5m
  } else {
    requireNamespace("stars", quietly = TRUE)
    out = try_s2_crop(kg5m, mask)
    if (!keep_ocean) {
      out = na_if_ocean(out)
    }
    out
  }
}

na_if_ocean = function(ras) {
  ras[[1L]][ras[[1L]] == "Ocean"] = NA
  ras
}

#' @param ... passed to [ggplot2::scale_fill_manual()].
#' @returns `scale_color_kgc()` and `scale_fill_kgc()` return a ggplot2 scale.
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
