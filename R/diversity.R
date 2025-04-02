index_brillouin = function(ras) {
  cnt = table_raster(ras)
  purrr::map_dbl(cnt, tabula::index_brillouin)
}

summarize_tabulate = function(x) {
  summarize_counts(table_int(x))
}

summarize_counts = function(cnt) {
  data.frame(
    abundance = sum(cnt),
    richness = length(cnt[cnt > 0L]),
    idx_shannon = tabula::index_shannon(cnt),
    idx_simpson = tabula::index_simpson(cnt),
    idx_brillouin = tabula::index_brillouin(cnt)
  )
}

table_int = function(x, ..., use.names = FALSE) {
  if (isFALSE(use.names) && (is.factor(x) || is.numeric(x))) {
    tabulate(x, ...)
  } else {
    c(table(x, ..., dnn = NULL))
  }
}
