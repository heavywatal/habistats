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
  rows = parallel::mclapply(input$source, quantify_vector_kgc)
  out = purrr::list_rbind(rows)
  dplyr::bind_cols(dplyr::select(input, !"source"), out)
}
