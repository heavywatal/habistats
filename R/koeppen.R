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


climate_colors = c(
  Af = "#960000",
  Am = "#FF0000",
  As = "#FF6E6E",
  Aw = "#FFCCCC",
  BSh = "#CC8D14",
  BSk = "#CCAA54",
  BWh = "#FFCC00",
  BWk = "#FFFF64",
  Cfa = "#007800",
  Cfb = "#005000",
  Cfc = "#003200",
  Csa = "#96FF00",
  Csb = "#00D700",
  Csc = "#00AA00",
  Cwa = "#BEBE00",
  Cwb = "#8C8C00",
  Cwc = "#5A5A00",
  Dfa = "#550055",
  Dfb = "#820082",
  Dfc = "#C800C8",
  Dfd = "#FF6EFF",
  Dsa = "#646464",
  Dsb = "#8C8C8C",
  Dsc = "#BEBEBE",
  Dsd = "#E6E6E6",
  Dwa = "#6E28B4",
  Dwb = "#B464FA",
  Dwc = "#C89BFA",
  Dwd = "#C8C8FF",
  EF = "#6496FF",
  ET = "#64FFFF",
  Ocean = "#F5FFFF"
)

.ignore_unused_imports = function() {
  stars::read_stars
}
