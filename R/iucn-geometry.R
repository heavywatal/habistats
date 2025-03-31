#' Use geometry column of IUCN Spatial Data
#'
#' The geometry column of IUCN Spatial Data is evaluated in several ways.
#' Currently, perimeter, area, and some other features are calculated.
#'
#' @inheritParams iucn_spatial_features
#' @param sci_name A character vector of species names.
#' @returns [`evaluate_iucn_range()`] returns a data.frame.
#' @rdname iucn-geometry
#' @export
#' @examples
#' \dontrun{
#' options(habistats.iucn_source = "path/to/your/data")
#' habistats::iucn_source_path()
#' }
#' old = options(habistats.cache_dir = "~/.cache/habistats-example")
#'
#' sn = c("Strato arctica", "Diva satanica")
#' habistats::evaluate_iucn_range(sn)
#'
#' options(old) # reset for this example, not needed in real use
evaluate_iucn_range = function(sci_name, iucn_source = NULL) {
  input = iucn_species_gpkg(iucn_source) |>
    dplyr::filter(.data$sci_name %in% !!sci_name)
  rows = parallel::mclapply(input$source, summarize_iucn_union_kgc)
  out = purrr::list_rbind(rows)
  dplyr::bind_cols(dplyr::select(input, !"source"), out)
}

summarize_iucn_union_kgc = function(dsn) {
  sf1 = summarize_iucn_union_cache(dsn)
  geom = attr(sf1, "sf_column")
  kgc = raster_kgc(sf1[[geom]])
  sum_ras = summarize_raster(kgc)
  sf1 |>
    dplyr::mutate(perimeter = try_s2_perimeter(!!as.name(geom)) |> as.numeric()) |>
    dplyr::mutate(area = try_s2_area(!!as.name(geom)) |> as.numeric()) |>
    sf::st_set_geometry(NULL) |>
    dplyr::bind_cols(sum_ras)
}

summarize_iucn_union_cache = function(dsn) {
  cache_file = fs::path(fs::path_dir(dsn), "union.gpkg")
  if (fs::file_exists(cache_file)) {
    sf::read_sf(cache_file)
  } else {
    t_start = Sys.time()
    obj = sf::read_sf(dsn)
    obj = summarize_iucn_union(obj)
    t_end = Sys.time()
    if (t_end - t_start > 3.0) {
      sf::write_sf(obj, cache_file)
    }
    obj
  }
}

summarize_iucn_union = function(x) {
  geom = attr(x, "sf_column")
  if (nrow(x) > 1L) {
    dplyr::summarize(
      x,
      SHAPE_Leng = sum(.data$SHAPE_Leng, na.rm = TRUE),
      SHAPE_Area = sum(.data$SHAPE_Area, na.rm = TRUE),
      !!geom := try_s2_union(x)
    )
  } else {
    dplyr::select(x, "SHAPE_Leng", "SHAPE_Area", !!geom)
  }
}
