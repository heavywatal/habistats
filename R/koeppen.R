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
  kmz = terra::rast(vals = kgc::kmz, nrow = 6480L, ncol = 12960L)
  if (!is.null(mask)) {
    if (!inherits(mask, "SpatVector")) {
      mask = terra::vect(mask)
      terra::crs(mask) = "OGC:CRS84"
    }
    kmz = terra::mask(kmz, mask = mask)
  }
  kmz
}

#' @rdname koeppen
#' @export
raster_kgc = function(mask = NULL) {
  kgc = unwrap_kgc()
  if (!is.null(mask)) {
    if (!inherits(mask, "SpatVector")) {
      mask = terra::vect(mask)
      terra::crs(mask) = "OGC:CRS84"
    }
    kgc = terra::mask(kgc, mask = mask)
  }
  kgc
}

unwrap_kgc = function() {
  name = "kgc_5m"
  if (!exists(name, envir = cache_env)) {
    x = terra::unwrap(.KG_1986_2010)
    terra::crs(x) = "OGC:CRS84"
    levels(x) = data.frame(
      id = seq_along(climate_colors),
      climate = names(climate_colors)
    )
    stopifnot(terra::is.factor(x))
    assign(name, x, envir = cache_env)
  }
  get(name, envir = cache_env)
}

augment_raster = function(x, na.rm = FALSE) {
  res = terra::as.data.frame(x, xy = TRUE, na.rm = na.rm)
  class(res) = c("tbl_df", "tbl", "data.frame")
  names(res)[3L] = "climate"
  res
}

cache_env = new.env()

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
