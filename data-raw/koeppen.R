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

kgc_5m = terra::rast(file.path(extracted, "KG_1986-2010.grd"))
terra::values(kgc_5m) = terra::values(kgc_5m) |> as.integer()
stopifnot(terra::is.int(kgc_5m))

.KG_1986_2010 = terra::wrap(kgc_5m)

usethis::use_data(.KG_1986_2010, internal = TRUE, overwrite = TRUE)
