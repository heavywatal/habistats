## code to prepare `monsters` dataset goes here

make_legend = function(presence, origin, seasonal) {
  pre = c("Extant", "Probably Extant", "Possibly Extant", "Possibly Extinct", "Extinct", "Presence Uncertain")
  ori = c("", "Reintroduced", "Introduced", "Vagrant", "Origin Uncertain", "Assisted Colonisation")
  ori = paste0(ifelse(nzchar(ori), " & ", ""), ori)
  sea = c("resident", "breeding", "non-breeding", "passage", "seasonallity uncertain")
  sea = paste0(" (", sea, ")")
  paste0(pre[presence], ori[origin], ifelse(presence < 4L, sea[seasonal], ""))
}

ne_geometry = function(country, scale = 110) {
  z = lapply(country, \(x) {
    rnaturalearth::ne_countries(scale = scale, country = x) |>
      sf::st_geometry() |>
      habistats:::remove_minor_polygons()
  })
  do.call(c, z)
}

create_iucn_like = function(geom, id_no, sci_name, kingdom, phylum, class, order_, family, category = "LC") {
  x = tibble::tibble(
    id_no = id_no,
    sci_name = sci_name,
    presence = ifelse(category %in% c("EX", "EW"), 5L, 1L),
    origin = 1L,
    seasonal = 1L,
    compiler = "NWOBHM",
    yrcompiled = 2025L,
    citation = "habistats",
    subspecies = NA_character_,
    subpop = NA_character_,
    source = NA_character_,
    island = NA_character_,
    tax_comm = NA_character_,
    dist_comm = NA_character_,
    generalisd = 0L,
    legend = make_legend(presence, origin, seasonal),
    kingdom,
    phylum,
    class,
    order_,
    family,
    genus = stringr::str_extract(sci_name, "^[^ ]+"),
    category = category,
    marine = "false",
    terrestria = "true",
    freshwater = "false",
    SHAPE_Leng = purrr::map_dbl(geom, sf::st_perimeter),
    SHAPE_Area = purrr::map_dbl(geom, sf::st_area),
    geometry = geom
  )
  sf::st_sf(x)
}

fi = create_iucn_like(
  ne_geometry("Finland"),
  19990904L,
  "Strato arctica",
  kingdom = "ANIMALIA",
  phylum = "CHORDATA",
  class = "MAMMALIA",
  order_ = "CARNIVORA",
  family = "CANIDAE"
)

se = create_iucn_like(
  ne_geometry(c("Sweden", "Germany")),
  20010425L,
  "Diva satanica",
  kingdom = "ANIMALIA",
  phylum = "CHORDATA",
  class = "AVES",
  order_ = "PASSERIFORMES",
  family = "CORVIDAE"
)

de = create_iucn_like(
  ne_geometry("Germany"),
  19851118L,
  "Kai kiske",
  kingdom = "PLANTAE",
  phylum = "TRACHEOPHYTA",
  class = "MAGNOLIOPSIDA",
  order_ = "CUCURBITALES",
  family = "CUCURBITACEAE"
)

gb = create_iucn_like(
  ne_geometry("United Kingdom"),
  19710604L,
  "Elp tarkus",
  kingdom = "ANIMALIA",
  phylum = "CHORDATA",
  class = "MAMMALIA",
  order_ = "CINGULATA",
  family = "CHLAMYPHORIDAE",
  category = "EW"
)

monsters = dplyr::bind_rows(fi, se, de, gb) |> print()

outdir = fs::path("inst/extdata/iucn_source")
outfile = outdir / "monsters.shp"
fs::dir_create(outdir, recurse = TRUE)
sf::write_sf(monsters, outfile)
