#' Species-level summary of IUCN Spatial Data
#'
#' @description
#' [`iucn_species_gpkg()`] splits IUCN shape files into separate gpkg files
#' for each species, and returns their paths. This allows for parallel processing
#' in downstream analysis, and avoids the need to read large shape files again.
#'
#' Use [`iucn_spatial_features()`] for quick skimming of the whole dataset,
#' and determine which species to analyze.
#' @inheritParams iucn_spatial_features
#' @returns `iucn_spatial_species()` returns a data.frame extracted from [iucn_spatial_features()].
#' @rdname iucn-species
#' @export
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
#' `"sci_name"` and `"source"`, to be used for [evaluate_iucn_range()].
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
  path_components = distinct_iucn_species(features)
  relpaths = path_components |>
    reduce(file.path) |>
    stringr::str_replace(" +", "_")
  abspaths = fs::path(cache_dir(), "iucn", relpaths, "source.gpkg")
  gpkg_exists = fs::file_exists(abspaths) & !overwrite
  if (!all(gpkg_exists)) {
    missing_gpkg = path_components |>
      dplyr::select("sci_name") |>
      dplyr::mutate(gpkg = abspaths) |>
      dplyr::filter(!gpkg_exists)
    msg = paste(utils::capture.output(missing_gpkg), collapse = "\n")
    message("Writing ", msg)
    iucn_sf = read_sf_cache(dsn)
    wrote = rowwise_mcmap_chr(missing_gpkg, \(row) {
      filtered = dplyr::filter(iucn_sf, .data$sci_name == row$sci_name)
      fs::dir_create(fs::path_dir(row$gpkg), recurse = TRUE)
      sf::write_sf(filtered, row$gpkg)
      row$gpkg
    })
  }
  dplyr::mutate(path_components, .data$sci_name, source = abspaths, .keep = "used")
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
