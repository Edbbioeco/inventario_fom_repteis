# Pacotes ----

library(sf)

library(tidyverse)

library(readxl)

library(writexl)

# Dados ----

## Shapefile da grade ----

### Importando ----

grade <- sf::st_read("grade_fom.shp")

### Visualizando ----

grade

ggplot() +
  geom_sf(data = grade)

## Registros de ocorrência ----

### Importando ----

occ_specieslink <- readxl::read_xlsx("specieslink.xlsx")

### Visualizando ----

occ_specieslink

occ_specieslink |>  dplyr::glimpse()

# Recortar para a FOM ----

## Transformando em shapefile ----

specieslink_sf <- occ_specieslink |> 
  dplyr::filter(!longitude |> is.na() &
                  !latitude |> is.na() &
                  !scientificname |> is.na() &
                  !scientificname |> 
                  stringr::str_detect(" sp$| sp.$| sp,$| sp | aff| cf,") &
                  !scientificname |>
                  stringr::str_count(stringr::boundary("word")) == 1) |> 
  dplyr::mutate(latitude = latitude |> as.numeric(),
                scientificname = scientificname |> 
                  stringr::str_replace("^(\\S+\\s+\\S+)\\s+\\S+(.*)", 
                                       "\\1\\2")) |> 
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = grade |> sf::st_crs())

specieslink_sf

ggplot() +
  geom_sf(data = specieslink_sf)

## Intersectando para a FOM ----

specieslink_sf_fom <- specieslink_sf |> 
  sf::st_intersection(grade |> 
                        dplyr::summarise(geometry = geometry |> 
                                           sf::st_union()))

specieslink_sf_fom

ggplot() +
  geom_sf(data = grade) +
  geom_sf(data = specieslink_sf_fom)

# Matriz de composição ----

## Lista de espécies ----

specieslink_sf_fom |> 
  dplyr::pull(scientificname) |> 
  unique()

## tratando as espécies ----

specieslink_sf_fom <- specieslink_sf_fom |> 
  dplyr::mutate(scientificname = dplyr::case_match(
    scientificname,
    "Chelonoidis carbonaria" ~ "Chelonoidis carbonarius",
    "Philodryas aestivus" ~ "Philodryas aestiva",
    "Placosoma glabelum" ~ "Placosoma glabellum",
    "Bothrops alternatus neuw." ~ "Bothrops alternatus",
    "Ecpleopus gaudichaudi" ~ "Ecpleopus gaudichaudii",
    "Micrurus silvae" ~"Micrurus silviae",
    "Xenodon merremi" ~ "Xenodon merremii",
    "Rachidelus brazili" ~ "Rhachidelus brazili",
    "Paraphimophis rustica" ~ "Paraphimophis rusticus",
    "Oxyrophus rhombifer" ~ "Oxyrhopus rhombifer",
    "Philodryas olfersi" ~ "Philodryas olfersii",
    "Leposternon microcephalum" ~ "Leposternon microcephalus",
    "Tomodon dorsatum" ~ "Tomodon dorsatus",
    "Tupinambis merianae" ~ "Salvator marianae",
    "Mabuya frenata" ~ "Notomabuya frenata",
    c("Anisiolepis grilli", "Anisolepis grilli")  ~ "Urostrophus grilli",
    "Mabuya dorsivittata" ~ "Aspronema dorsivittatum",
    "Sibynomorphus neuwiedi" ~ "Dipsas neuwiedi",
    "Liotyphlops beui" ~ "Liotyphlops ternetzii",
    "Amphisbaena darwini trachura" ~ "Amphisbaena darwinii",
    "Pantodactylus schreibersii" ~ "Cercosaura schreibersii",
    "Bothrops neuwiedi diorus" ~ "Bothrops neuwiedi",
    "Mastigodryas bifossatus" ~ "Palusophis bifossatus",
    "Liophis miliaris" ~ "Erythrolamprus miliaris",
    "Sibynomorphus mikanii" ~ "Dipsas mikanii",
    "Liophis jaegeri" ~ "Erythrolamprus jaegeri",
    "Phalotris iheringii" ~ "Phalotris lemniscatus",
    "Thamnodynastes hypoconia" ~ "Dryophylax hypoconia",
    "Thamnodynastes strigatus" ~ "Mesotes strigatus",
    "Atractus taeniatus" ~ "Atractus paraguayensis",
    "Crotalus durissus terrificus" ~ "Crotalus durissus",
    "Echinanthera affinis" ~ "Dibernardia affinis",
    "Bothrops neuwiedi" ~ "Bothrops neuwiedii",
    "Amphisbaena darwini" ~ "Amphisbaena darwinii",
    "Anops kingii" ~ "Amphisbaena kingii",
    "Amphisbaena mertensi" ~ "Amphisbaena mertensii",
    c("Varanus salvator", 
      "Tupinambis teguixin",
      "Tupinambis teguixim",
      "Salvator marianae") ~ "Salvator merianae",
    "Bothrops trigemina" ~ "Bothrops alternatus",
    "Bothrops neuwiedi paranaensis" ~ "Bothrops pubescens",
    c("Anolis philopunctatus", 
      "Lygophis lineatus", 
      "Dipsas indica", 
      "Clelia plúmbea", 
      "Xenodon biligonigerus") ~ NA_character_,
    .default = scientificname
  ),
  family = dplyr::case_when(
    family == "Varanidae" ~ "Teiidae",
    .default = family
  ),
    scientificname = scientificname |> stringr::str_trim()) |> 
  dplyr::filter(!scientificname |> is.na() &
                  !scientificname |> stringr::str_detect("sp|sp.|cf|cf.") &
                  !scientificname |> 
                  stringr::str_trim() |> 
                  stringr::str_count("\\S+") == 1)

specieslink_sf_fom

## Montando a matriz de composição ----

specieslink_registros <- specieslink_sf_fom |> 
  sf::st_join(grade) |> 
  as.data.frame() |> 
  dplyr::mutate(Especies = scientificname,
                Presence =  1,
                Family = family) |> 
  dplyr::select(ID, Family, Especies, Presence) 

specieslink_registros

## Exportando ----

specieslink_registros |> writexl::write_xlsx("registros_specieslink.xlsx")

