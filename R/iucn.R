#' Read and process habitat data from IUCN
#'
#' @seealso <https://www.iucnredlist.org/resources/spatial-data-download>
#'
#' @description
#' [`ls_shapes()`] lists shape files in a directory.
#' @param directory Directory containing shape files.
#' @param recurse Logical, whether to search subdirectories.
#' @rdname iucn
#' @export
ls_shapes = function(directory, recurse = TRUE) {
  fs::dir_ls(directory, recurse = recurse, type = "file", glob = "*.shp")
}

#' @description
#' [`read_shapes()`] reads shape files and combines them into a single data frame.
#' Note that "geometry" column in `sf` data.frame cannot be removed and affects performance.
#' Use `as.data.frame()` for efficient data.frame operations.
#' @param files Vector of file paths to shape files.
#' @rdname iucn
#' @export
read_shapes = function(files) {
  dfs = parallel::mclapply(files, sf::read_sf)
  purrr::list_rbind(dfs, ptype = dfs[[1L]])
}
