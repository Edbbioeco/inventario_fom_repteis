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

herpetohelp <- readxl::read_xlsx("herptohelp.xlsx",,
                                 sheet = 2)

### Visualizando ----

herpetohelp

herpetohelp |> dplyr::glimpse()

# Recortar para a FOM ----

## Transformando em shapefile ----

herpetohelp_sf <- herpetohelp |>
  dplyr::filter(Grupo == "Répteis") |>  
  dplyr::filter(!`Longitude no mapa (SIRGAS 2000)` |> is.na() &
                  !`Latitude no mapa (SIRGAS 2000)` |> is.na() &
                  !Espécie |> is.na() &
                  !Espécie |> 
                  stringr::str_detect(" sp$| sp.$| sp,$| sp | aff| cf,") &
                  !Espécie |>
                  stringr::str_count(stringr::boundary("word")) == 1) |> 
  dplyr::mutate(`Latitude no mapa (SIRGAS 2000)` = `Latitude no mapa (SIRGAS 2000)` |> as.numeric(),
                Espécie = Espécie |> 
                  stringr::str_replace("^(\\S+\\s+\\S+)\\s+\\S+(.*)", 
                                       "\\1\\2")) |> 
  sf::st_as_sf(coords = c("Longitude no mapa (SIRGAS 2000)", "Latitude no mapa (SIRGAS 2000)"),
               crs = grade |> sf::st_crs())

herpetohelp_sf

ggplot() +
  geom_sf(data = herpetohelp_sf)

## Intersectando para a FOM ----
herpetohelp_sf_fom <- herpetohelp_sf |> 
  sf::st_intersection(grade |> 
                        dplyr::summarise(geometry = geometry |> 
                                           sf::st_union()))

herpetohelp_sf_fom

ggplot() +
  geom_sf(data = grade) +
  geom_sf(data = herpetohelp_sf_fom)

# Matriz de composição ----

## Lista de espécies ----

herpetohelp_sf_fom |> 
  dplyr::pull(Espécie) |> 
  unique()

## tratando as espécies ----

herpetohelp_sf_fom <- herpetohelp_sf_fom |> 
  dplyr::mutate(Espécie = dplyr::case_match(
    Espécie,
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
    c("Bothrops neuwiedi diorus", 
      "Bothrops newwiedi") ~ "Bothrops neuwiedi",
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
    .default = Espécie
  ),
  Família = dplyr::case_when(
    Família == "Varanidae" ~ "Teiidae",
    .default = Família),
  Espécie = Espécie |> stringr::str_trim()) |> 
  dplyr::filter(!Espécie |> is.na() &
                  !Espécie |> stringr::str_detect("sp|sp.") &
                  !Espécie |> 
                  stringr::str_trim() |> 
                  stringr::str_count("\\S+") == 1)

herpetohelp_sf_fom

## Montando a matriz de composição ----

herpetohelp_registros <- herpetohelp_sf_fom |> 
  sf::st_join(grade) |> 
  as.data.frame() |> 
  dplyr::mutate(Especies = Espécie,
                Presence =  1,
                Family = Família) |> 
  dplyr::select(ID, Family, Especies, Presence) 

herpetohelp_registros

## Exportando ----

herpetohelp_registros |> writexl::write_xlsx("registros_herpetohelp.xlsx")
