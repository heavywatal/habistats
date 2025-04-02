#' Quantify spatial data in vector format
#'
#' `quantify_vector()` calculates the basic spatial features of a vector geometry
#' such as perimeter and area.
#' `quantify_vector_kgc()`, in addition, quantifies the Köppen-Geiger climate
#' classification in that range cropped with [raster_kgc()].
#'
#' @param geom An `sfc` or `sf` object. A path to sf-readable file is accepted.
#' @returns A data.frame.
#' @rdname quantify-vector
#' @export
#' @examples
#' geom = rnaturalearth::ne_countries(country = "Japan") |> sf::st_geometry()
#' quantify_vector_kgc(geom)
quantify_vector_kgc = function(geom) {
  if (length(geom) > 1L) {
    rows = parallel::mclapply(geom, quantify_vector_kgc)
    out = purrr::list_rbind(rows)
    class(out) = c("tbl_df", "tbl", "data.frame")
    return(dplyr::mutate(out, name = names(geom), .before = 1L))
  }
  if (is.character(geom)) {
    geom = read_sf_union_geometry(geom)
  }
  sum_vec = quantify_vector(geom)
  kgc = raster_kgc(geom)
  sum_ras = quantify_raster(kgc)
  dplyr::bind_cols(sum_vec, sum_ras)
}

#' @rdname quantify-vector
#' @export
quantify_vector = function(geom) {
  data.frame(
    perimeter = try_s2_perimeter(geom) |> as.numeric(),
    area = try_s2_area(geom) |> as.numeric()
  )
}
