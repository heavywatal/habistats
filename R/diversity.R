index_brillouin = function(cnt) {
  values = get_values_na_omit(ras)
  cnt = table_int(values)
  tabula::index_brillouin(cnt)
}

summarize_count = function(cnt) {
  data.frame(
    abundance = sum(cnt),
    richness = length(cnt[cnt > 0L]),
    idx_shannon = tabula::index_shannon(cnt),
    idx_simpson = tabula::index_simpson(cnt),
    idx_brillouin = tabula::index_brillouin(cnt)
  )
}

table_int = function(x) {
  tab = table(x, dnn = NULL)
  as.integer(tab)
}
