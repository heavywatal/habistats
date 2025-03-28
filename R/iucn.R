#' Read and process IUCN Spatial Data
#'
#' @seealso <https://www.iucnredlist.org/resources/spatial-data-download>
#'
#' @description
#' The IUCN Spatial Data is a large collection of shape files, and these functions
#' provide an efficient way to read and process them.
#' It may take a while to run them for the first time,
#' but subsequent runs will be very quick because the results are cached.
#'
#' [`iucn_spatial_features()`] returns a data.frame without the geometry column.
#' This is useful for quick skimming of the whole dataset.
#' @param iucn_source Path to the directory containing IUCN shape files.
#' `getOption("habistats.iucn_source")` is used if `NULL` is given (default).
#' @rdname iucn
#' @export
iucn_spatial_features = function(iucn_source = NULL) {
  if (is.null(iucn_source)) {
    iucn_source = iucn_source_path()
  }
  call_cache(read_iucn_features_recursive, iucn_source)
}

#' @description
#' [`iucn_spatial_species()`] is a species-level summary of [`iucn_spatial_features()`].
#' @rdname iucn
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

#' @description
#' [`evaluate_iucn_range()`] evaluates the range of each species in the IUCN dataset.
#' @param sci_name A character vector of species names.
#' @rdname iucn
#' @export
evaluate_iucn_range = function(sci_name, iucn_source = NULL) {
  input = iucn_species_gpkg(iucn_source) |>
    dplyr::filter(.data$sci_name %in% !!sci_name)
  rows = parallel::mclapply(input$source, summarize_iucn_union_kgc)
  out = purrr::list_rbind(rows)
  dplyr::bind_cols(dplyr::select(input, !"source"), out)
}

#' @description
#' [`iucn_species_gpkg()`] splits IUCN shape files into separate gpkg files
#' for each species, and returns their paths. This allows for parallel processing
#' in downstream analysis, and avoids the need to read large shape files again.
#' @rdname iucn
#' @export
iucn_species_gpkg = function(iucn_source = NULL) {
  if (is.null(iucn_source)) {
    iucn_source = iucn_source_path()
  }
  shapes = fs::dir_ls(iucn_source, recurse = TRUE, type = "file", glob = "*.shp")
  dfs = lapply(shapes, split_iucn_shp)
  purrr::list_rbind(dfs)
}

iucn_source_path = function() {
  fs::path(getOption("habistats.iucn_source", "~/db/iucnredlist.org"))
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

split_iucn_shp = function(dsn) {
  features = read_iucn_features(dsn)
  path_components = distinct_iucn_species(features)
  relpaths = path_components |>
    reduce(file.path) |>
    stringr::str_replace(" +", "_")
  abspaths = fs::path(cache_dir(), "iucn", relpaths, "source.gpkg")
  gpkg_exists = fs::file_exists(abspaths)
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

read_iucn_features_recursive = function(path) {
  shapes = fs::dir_ls(path, recurse = TRUE, type = "file", glob = "*.shp")
  dfs = parallel::mclapply(shapes, read_iucn_features)
  purrr::list_rbind(dfs)
}

read_iucn_features = function(dsn, force = FALSE) {
  feature_file = features_path(dsn)
  if (!fs::file_exists(feature_file) || force) {
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
