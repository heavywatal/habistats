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

r_lines = file.path(extracted, "Map_KG-Global.R") |>
  readr::read_lines(skip_empty_rows = TRUE) |>
  stringr::str_subset("c\\(") |>
  print()

climate_colors = r_lines |>
  stringr::str_subset("^climate\\.colors") |>
  stringr::str_extract("c\\(.+\\)") |>
  parse(text = _) |>
  eval() |>
  print()

climate_classes = r_lines |>
  stringr::str_subset("^rat\\$climate") |>
  stringr::str_extract("c\\(.+\\)") |>
  parse(text = _) |>
  eval() |>
  print()

names(climate_colors) = climate_classes

as_matrix_fct = function(x) {
  d = dim(x)
  x = as.integer(x)
  x = factor(x, levels = seq_along(climate_classes), labels = climate_classes)
  dim(x) = d
  x
}

grd = file.path(extracted, "KG_1986-2010.grd")
kg5m = stars::read_stars(grd)
kg5m[[1]] = as_matrix_fct(kg5m[[1]])
stopifnot(sf::st_crs(kg5m) == sf::st_crs("WGS84"))
stopifnot(sf::st_crs(kg5m) == sf::st_crs(4326L))
stopifnot(sf::st_crs(kg5m) != sf::st_crs(4612L))
sf::st_crs(kg5m) = sf::st_crs("WGS84")

usethis::use_data(kg5m, climate_colors, internal = TRUE, overwrite = TRUE)
