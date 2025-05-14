#' Rowwise operations
#'
#' `rowwise_*()` functions apply a function to each row of a data.frame.
#' The input to the function `.f` is a single-row subset with the same class.
#' This is useful for handling subclasses of data.frame, such as `sf`.
#' `base::apply(.x, 1L, .f)` cannot do this because it converts the input to a matrix.
#' @seealso [lmap_lst()]
#' @param .x A data frame-like object.
#' @param .f A function to apply to each row.
#' @param mode Output type.
#' @returns A vector of the same length as the number of rows in `.x`.
#' The suffix of the function name denotes the output type.
#' `*_vec()` applies [purrr::list_simplify()] before returning.
#' @examples
#' df = data.frame(a = 1:2, b = 3:4)
#' rowwise_map(df, identity)
#'
#' rowwise_map_lgl(df, is.data.frame)
#'
#' rowwise_map_int(df, ncol)
#'
#' rowwise_map_dbl(df, sum)
#'
#' rowwise_map_chr(df, toString)
#'
#' rowwise_map_vec(df, \(x) x$a + x$b * 1i)
#' @rdname rowwise
#' @export
rowwise_map = function(.x, .f, mode = c("list", "integer", "double", "character", "logical")) {
  mode = match.arg(mode)
  n = nrow(.x)
  out = vector(mode, n)
  for (i in seq_len(n)) {
    out[[i]] = .f(.x[i, ])
  }
  out
}

#' @rdname rowwise
#' @export
rowwise_map_lgl = function(.x, .f) rowwise_map(.x, .f, "logical")

#' @rdname rowwise
#' @export
rowwise_map_int = function(.x, .f) rowwise_map(.x, .f, "integer")

#' @rdname rowwise
#' @export
rowwise_map_dbl = function(.x, .f) rowwise_map(.x, .f, "double")

#' @rdname rowwise
#' @export
rowwise_map_chr = function(.x, .f) rowwise_map(.x, .f, "character")

#' @rdname rowwise
#' @export
rowwise_map_vec = function(.x, .f) {
  out = rowwise_map(.x, .f)
  purrr::list_simplify(out)
}

#' @description
#' `rowwise_mcmap*()` are parallelized versions of `rowwise_map*()`.
#' @param ... Additional arguments passed to [parallel::mclapply()].
#' @rdname rowwise
#' @export
rowwise_mcmap = function(.x, .f, ...) {
  rows = rowwise_map(.x, identity)
  parallel::mclapply(rows, .f, ...)
}

#' @rdname rowwise
#' @export
rowwise_mcmap_lgl = function(.x, .f, ...) {
  out = rowwise_mcmap(.x, .f, ...)
  purrr::list_simplify(out, ptype = logical(0L))
}

#' @rdname rowwise
#' @export
rowwise_mcmap_int = function(.x, .f, ...) {
  out = rowwise_mcmap(.x, .f, ...)
  purrr::list_simplify(out, ptype = integer(0L))
}

#' @rdname rowwise
#' @export
rowwise_mcmap_dbl = function(.x, .f, ...) {
  out = rowwise_mcmap(.x, .f, ...)
  purrr::list_simplify(out, ptype = double(0L))
}

#' @rdname rowwise
#' @export
rowwise_mcmap_chr = function(.x, .f, ...) {
  out = rowwise_mcmap(.x, .f, ...)
  purrr::list_simplify(out, ptype = character(0L))
}

#' @rdname rowwise
#' @export
rowwise_mcmap_vec = function(.x, .f, ...) {
  out = rowwise_mcmap(.x, .f, ...)
  purrr::list_simplify(out)
}
