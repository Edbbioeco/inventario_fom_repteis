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

inaturalist <- readr::read_csv("inaturalist.csv")

### Visualizando ----

inaturalist

inaturalist |> dplyr::glimpse()

# Recortar para a FOM ----

## Transformando em shapefile ----

inaturalist_sf <- inaturalist |> 
  dplyr::filter(!longitude |> is.na() &
                  !latitude |> is.na() &
                  !scientific_name |> is.na() &
                  !scientific_name |> 
                  stringr::str_detect(" sp$| sp.$| sp,$| sp | aff| cf,") &
                  !scientific_name |>
                  stringr::str_count(stringr::boundary("word")) == 1) |> 
  dplyr::mutate(latitude = latitude |> as.numeric(),
                scientific_name = scientific_name |> 
                  stringr::str_replace("^(\\S+\\s+\\S+)\\s+\\S+(.*)", 
                                       "\\1\\2")) |> 
  sf::st_as_sf(coords = c("longitude", "latitude"),
               crs = grade |> sf::st_crs())

inaturalist_sf

ggplot() +
  geom_sf(data = inaturalist_sf)

## Intersectando para a FOM ----

inaturalist_sf_fom <- inaturalist_sf |> 
  sf::st_intersection(grade |> 
                        dplyr::summarise(geometry = geometry |> 
                                           sf::st_union()))

inaturalist_sf_fom

ggplot() +
  geom_sf(data = grade) +
  geom_sf(data = inaturalist_sf_fom)

# Matriz de composição ----

## Lista de espécies ----

inaturalist_sf_fom |> 
  dplyr::pull(scientific_name) |> 
  unique()

## tratando as espécies ----

inaturalist_sf_fom <- inaturalist_sf_fom |> 
  dplyr::mutate(scientific_name = dplyr::case_match(
    scientific_name,
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
    c("Bothrops neuwiedi diorus", 
      "Bothrops newwiedi") ~ "Bothrops neuwiedi",
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
    .default = scientific_name
  ),
  taxon_family_name = dplyr::case_when(
    taxon_family_name == "Varanidae" ~ "Teiidae",
    .default = taxon_family_name),
  scientific_name = scientific_name |> stringr::str_trim()) |> 
  dplyr::filter(!scientific_name |> is.na() &
                  !scientific_name |> stringr::str_detect("sp|sp.|cf|cf.") &
                  !scientific_name |> 
                  stringr::str_trim() |> 
                  stringr::str_count("\\S+") == 1)

inaturalist_sf_fom

## Montando a matriz de composição ----

inaturalist_registros <- inaturalist_sf_fom |> 
  sf::st_join(grade) |> 
  as.data.frame() |> 
  dplyr::mutate(Especies = scientific_name,
                Presence =  1,
                Family = taxon_family_name) |> 
  dplyr::select(ID, Family, Especies, Presence) 

inaturalist_registros

## Exportando ----

inaturalist_registros |> writexl::write_xlsx("registros_inaturalist.xlsx")
