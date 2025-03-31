#' Read and process IUCN Spatial Data
#'
#' @description
#' The IUCN Spatial Data is a large collection of shape files,
#' and this package provides efficient ways to read and process them.
#' It may take a while to run them for the first time,
#' but subsequent runs will be very quick because the results are cached.
#'
#' `iucn_spatial_features()` reads the IUCN shape files and drops the geometry column.
#' This is useful for quick skimming of the whole dataset.
#' Set `habistats.iucn_source` option to specify the path to the directory
#' containing IUCN shape files, and use `iucn_source_path()` to check it.
#'
#' @seealso <https://www.iucnredlist.org/resources/spatial-data-download>
#'
#' @param iucn_source Path to the directory containing IUCN shape files.
#' `iucn_source_path()` is used if `NULL` is given (default).
#' @param overwrite Whether to overwrite existing cache files.
#' @returns `iucn_spatial_features()` returns a data.frame with the feature columns.
#' @rdname iucn
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
#' habistats::iucn_spatial_features()
#'
#' options(old) # reset for this example, not needed in real use
iucn_spatial_features = function(iucn_source = NULL, overwrite = FALSE) {
  if (is.null(iucn_source)) {
    iucn_source = iucn_source_path()
  }
  call_cache(read_iucn_features_recursive, iucn_source, overwrite = overwrite)
}

#' @returns `iucn_source_path()` returns a path read from the option.
#' @rdname iucn
#' @export
iucn_source_path = function() {
  extdata = system.file("extdata", "iucn_source", package = "habistats")
  fs::path(getOption("habistats.iucn_source", extdata))
}

read_iucn_features_recursive = function(path, overwrite = FALSE) {
  shapes = fs::dir_ls(path, recurse = TRUE, type = "file", glob = "*.shp")
  dfs = parallel::mclapply(shapes, \(x) read_iucn_features(x, overwrite = overwrite))
  purrr::list_rbind(dfs)
}

read_iucn_features = function(dsn, overwrite = FALSE) {
  feature_file = features_path(dsn)
  if (!fs::file_exists(feature_file) || overwrite) {
    iucn_sf = read_sf_cache(dsn)
    out = drop_geometry(iucn_sf)
    fs::dir_create(fs::path_dir(feature_file), recurse = TRUE)
    readr::write_tsv(out, feature_file, na = "")
    message("Wrote ", feature_file)
  }
  read_tsv_iucn_features(feature_file)
}

features_path = function(dsn) {
  file_name = (fs::path_file(dsn) |> fs::path_ext_set("tsv.gz"))
  cache_dir() / "iucn" / "features" / file_name
}

read_tsv_iucn_features = function(file) {
  .cols = readr::cols(
    id_no = "i",
    presence = "i",
    origin = "i",
    seasonal = "i",
    yrcompiled = "i",
    subspecies = "c",
    subpop = "c",
    island = "c",
    tax_comm = "c",
    dist_comm = "c",
    generalisd = "i"
  )
  readr::read_tsv(file, col_types = .cols)
}
