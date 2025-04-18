#' Species-level summary of IUCN Spatial Data
#'
#' @description
#' `iucn_species_gpkg()` splits IUCN shape files into separate gpkg files
#' for each species, and returns their paths. This allows for parallel processing
#' in downstream analysis, and avoids the need to read large shape files again.
#'
#' Use `iucn_spatial_species()` and [iucn_spatial_features()]
#' for quick skimming of the whole dataset, and determine which species to analyze.
#'
#' @inheritParams iucn_spatial_features
#' @returns `iucn_spatial_species()` returns a data.frame extracted from [iucn_spatial_features()].
#' @rdname iucn-species
#' @export
#' @examples
#' \dontrun{
#' options(habistats.iucn_source = "path/to/your/data")
#' habistats::iucn_source_path()
#' }
#' old = options(habistats.cache_dir = "~/.cache/habistats-example")
#'
#' habistats::cache_dir()
#'
#' habistats::iucn_spatial_species()
#'
#' habistats::iucn_species_gpkg()
#'
#' options(old) # reset for this example, not needed in real use
iucn_spatial_species = function(iucn_source = NULL) {
  features = iucn_spatial_features(iucn_source)
  out = distinct_iucn_species(
    features,
    .data$category,
    .data$marine,
    .data$terrestria,
    .data$freshwater
  )
  cnt = out |>
    dplyr::count(.data$sci_name) |>
    dplyr::filter(.data$n > 1L)
  if (nrow(cnt) > 0L) {
    toString(out$sci_name)
    warning("Multiple entries for ", toString(out$sci_name), call. = FALSE)
  }
  out
}

#' @returns `iucn_species_gpkg()` returns a data.frame with two columns,
#' `"sci_name"` and `"source"`, to be used for [quantify_vector_kgc()].
#' @rdname iucn-species
#' @export
iucn_species_gpkg = function(iucn_source = NULL, overwrite = FALSE) {
  if (is.null(iucn_source)) {
    iucn_source = iucn_source_path()
  }
  shapes = fs::dir_ls(iucn_source, recurse = TRUE, type = "file", glob = "*.shp")
  dfs = lapply(shapes, \(x) split_iucn_shp(x, overwrite = overwrite))
  purrr::list_rbind(dfs)
}

split_iucn_shp = function(dsn, overwrite = FALSE) {
  features = read_iucn_features(dsn)
  out = iucn_species_cache(features, "source.gpkg") |>
    dplyr::rename(source = "path")
  missing_gpkg = out |>
    dplyr::filter(!fs::file_exists(.data$source) | overwrite)
  if (nrow(missing_gpkg) > 0L) {
    iucn_sf = read_sf_cache(dsn)
    missing_dirs = fs::path_dir(missing_gpkg$source) |> unique()
    fs::dir_create(missing_dirs, recurse = TRUE)
    wrote = rowwise_mcmap_vec(missing_gpkg, \(row) {
      filtered = dplyr::filter(iucn_sf, .data$sci_name == row$sci_name)
      sf::write_sf(filtered, row$source)
      row$source
    })
    size = fs::file_size(wrote) |> sum()
    message("Wrote ", size, " in ", cache_dir() / "iucn/")
  }
  out
}

iucn_species_cache = function(features, ...) {
  path_components = distinct_iucn_species(features)
  relpaths = path_components |>
    reduce(file.path) |>
    stringr::str_replace(" +", "_")
  path_components |>
    dplyr::select("sci_name") |>
    dplyr::mutate(path = fs::path(cache_dir(), "iucn", relpaths, ...))
}

distinct_iucn_species = function(features, ...) {
  dplyr::distinct(
    features,
    .data$kingdom,
    .data$phylum,
    .data$class,
    .data$order_,
    .data$family,
    .data$genus,
    .data$sci_name,
    ...
  )
}
