# Pacotes ----

library(sf)

library(tidyverse)

library(sf)

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

sibbr <- readr::read_csv2("sibbr.csv", quote = ";")

### Visualizando ----

sibbr

sibbr |>  dplyr::glimpse()

### Tratando ----

sibbr_trat <- sibbr |> 
  dplyr::select(c(family, Species, decimalLongitude:decimalLatitude)) |> 
  dplyr::rename("species" = Species,
                "Longitude" = decimalLongitude,
                "Latitude" = decimalLatitude) |> 
  dplyr::filter(!species |> is.na() &
                  !Latitude |> is.na() &
                  !Longitude |> is.na()) |> 
  dplyr::mutate(Longitude = Longitude  |> 
                  stringr::str_replace(
                    "^(-?\\d{2})(\\d+)$", "\\1.\\2") |>
                  as.numeric(),
                Longitude = dplyr::case_when(
                  Longitude >= 0 ~ Longitude * -1,
                  .default = Longitude),
                Latitude = case_when(stringr::str_detect(
                  as.character(Latitude), 
                  "^(-?[1-2])") ~ str_replace(
                    as.character(Latitude),
                    "^(-?\\d{2})(\\d+)$", "\\1.\\2"),
                  stringr::str_detect(
                    as.character(Latitude), 
                    "^(-?[3-9])") ~ stringr::str_replace(
                      as.character(Latitude), 
                      "^(-?\\d{1})(\\d+)$", "\\1.\\2"),
                  TRUE ~ as.character(Latitude)) |>
                  as.numeric(),
                Latitude = dplyr::case_when(
                  Latitude >= 0 ~ Latitude * -1,
                  .default = Latitude)) |> 
  tidyr::drop_na()

sibbr_trat

sibbr_trat |>  dplyr::glimpse()

# Recortar para a FOM ----

## Transformando em shapefile ----

sibbr_sf <- sibbr_trat |> 
  dplyr::filter(!Longitude |> is.na() &
                  !Latitude |> is.na() &
                  !species |> is.na()) |> 
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = grade |> sf::st_crs())

sibbr_sf

ggplot() +
  geom_sf(data = sibbr_sf)

## Intersectando para a FOM ----

sibbr_sf_fom <- sibbr_sf |> 
  sf::st_intersection(grade |> 
                        dplyr::summarise(geometry = geometry |> 
                                           sf::st_union()))

sibbr_sf_fom

ggplot() +
  geom_sf(data = grade) +
  geom_sf(data = sibbr_sf_fom)

# Matriz de composição ----

## Lista de espécies ----

sibbr_sf_fom |> 
  dplyr::pull(species) |> 
  unique()

## tratando as espécies ----

sibbr_sf_fom <- sibbr_sf_fom |> 
  dplyr::mutate(species = dplyr::case_match(
    "Tomodon dorsatum" ~ "Tomodon dorsatus",
    "Tupinambis merianae" ~ "Salvator marianae",
    "Mabuya frenata" ~ "Notomabuya frenata",
    "Anisiolepis grilli"  ~ "Urostrophus grilli",
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
    "Bothrops newwiedi" ~ "Bothrops neuwiedi",
    "Amphisbaena darwini" ~ "Amphisbaena darwinii",
    "Anops kingii" ~ "Amphisbaena kingii",
    "Amphisbaena mertensi" ~ "Amphisbaena mertensii",
    "Leposternon microcephalum" ~ "Leposternon microcephalus",
    c("Varanus salvator", "Tupinambis teguixin") ~ "Salvator merianae",
    "Bothrops trigemina" ~ "Bothrops alternatus",
    "Bothrops neuwiedi paranaensis" ~ "Bothrops pubescens",
    c("Anolis philopunctatus", 
      "Lygophis lineatus", 
      "Dipsas indica", 
      "Clelia plúmbea", 
      "Xenodon biligonigerus") ~ NA_character_,
    .default = species
  ),
  family = dplyr::case_when(
    family == "Varanidae" ~ "Teiidae",
    .default = family,
    species = especies |> stringr::str_trim()
  )) |> 
  dplyr::filter(!species |> is.na() &
                  !species |> stringr::str_detect("sp|sp.|cf|cf.") &
                  !species |> 
                  stringr::str_trim() |> 
                  stringr::str_count("\\S+") == 1)

sibbr_sf_fom

## Montando a matriz de composição ----

sibbr_registros <- sibbr_sf_fom |> 
  sf::st_join(grade) |> 
  as.data.frame() |> 
  dplyr::mutate(Especies = species,
                Presence =  1,
                Family = family) |> 
  dplyr::select(ID, Family, Especies, Presence) 

sibbr_registros

## Exportando ----

sibbr_registros |> writexl::write_xlsx("registros_sibbr.xlsx")
