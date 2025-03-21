#' Apply a function to each subset of a list.
#'
#' `lmap*()` are similar to `lapply()`, but they map over subset `.x[i]`
#' instead of element `.x[[i]]`.
#' It allows `.f` to access the attributes of the encapsulating list.
#' This is useful for operation on the `sfc` column in an `sf` data.frame.
#' [purrr::lmap()] does similar things, but it forces the output to be a list.
#' @seealso [rowwise_map()]
#' @param .x A list.
#' @param .f A function to apply to each subset of `.x`.
#' @param mode Output type.
#' @rdname lmap
#' @export
lmap_lst = function(.x, .f, mode = c("list", "integer", "double", "character", "logical")) {
  mode = match.arg(mode)
  n = length(.x)
  out = vector(mode, n)
  for (i in seq_len(n)) {
    out[[i]] = .f(.x[i])
  }
  names(out) = names(.x)
  out
}

#' @rdname lmap
#' @export
lmap_lgl = function(.x, .f) lmap_lst(.x, .f, "logical")

#' @rdname lmap
#' @export
lmap_int = function(.x, .f) lmap_lst(.x, .f, "integer")

#' @rdname lmap
#' @export
lmap_dbl = function(.x, .f) lmap_lst(.x, .f, "double")

#' @rdname lmap
#' @export
lmap_chr = function(.x, .f) lmap_lst(.x, .f, "character")

#' @rdname lmap
#' @export
lmap_vec = function(.x, .f) {
  out = lmap_lst(.x, .f)
  purrr::list_simplify(out)
}

#' @description
#' `mclmap*()` are parallelized versions of `lmap*()`.
#' @param ... Additional arguments passed to [parallel::mclapply()].
#' @rdname lmap
#' @export
mclmap_lst = function(.x, .f, ...) {
  lists = lmap_lst(.x, identity)
  parallel::mclapply(lists, .f, ...)
}

#' @rdname lmap
#' @export
mclmap_lgl = function(.x, .f, ...) {
  out = mclmap_lst(.x, .f, ...)
  purrr::list_simplify(out, ptype = logical(0L))
}

#' @rdname lmap
#' @export
mclmap_int = function(.x, .f, ...) {
  out = mclmap_lst(.x, .f, ...)
  purrr::list_simplify(out, ptype = integer(0L))
}

#' @rdname lmap
#' @export
mclmap_dbl = function(.x, .f, ...) {
  out = mclmap_lst(.x, .f, ...)
  purrr::list_simplify(out, ptype = double(0L))
}

#' @rdname lmap
#' @export
mclmap_chr = function(.x, .f, ...) {
  out = mclmap_lst(.x, .f)
  purrr::list_simplify(out, ptype = character(0L))
}

#' @rdname lmap
#' @export
mclmap_vec = function(.x, .f, ...) {
  out = mclmap_lst(.x, .f, ...)
  purrr::list_simplify(out)
}
