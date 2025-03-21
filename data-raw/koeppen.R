## code to prepare `koeppen` dataset goes here

cache_dir = tools::R_user_dir("habistats", "cache")
dir.create(cache_dir, showWarnings = FALSE, recursive = TRUE, mode = "0755")

url = "https://koeppen-geiger.vu-wien.ac.at/Rcode/Map_KG-Global.zip"
zip_file = file.path(cache_dir, basename(url))
extracted = base::sub("\\.[^.]+$", "", zip_file)
if (!file.exists(zip_file) && !dir.exists(extracted)) {
  download.file(url, destfile = zip_file, mode = "wb")
}
unzip(zip_file, exdir = cache_dir)

as_matrix_int = function(x) {
  d = dim(x)
  x = as.integer(x)
  dim(x) = d
  x
}

grd = file.path(extracted, "KG_1986-2010.grd")
kg5m = stars::read_stars(grd)
kg5m[[1]] = as_matrix_int(kg5m[[1]])

usethis::use_data(kg5m, internal = TRUE, overwrite = TRUE)
