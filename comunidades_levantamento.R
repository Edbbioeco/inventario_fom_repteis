# Pacote ----

library(geobr)

library(tidyverse)

library(sf)

library(readxl)

library(parzer)

library(writexl)

# Dados ----

## Grade -----

### Importando ----

grade <- sf::st_read("grade_fom.shp")

### Visualizando ----

grade

ggplot() +
  geom_sf(data = grade)

## Registros das espécies -----

### Importando ----

sps <- readxl::read_xlsx("DADOS COPILADOS DA FOM 2026.xlsx")

### Visualizando ----

sps

sps |> dplyr::glimpse()

### Tratando ----

sps_trat <- sps |> 
  tidyr::pivot_longer(cols = dplyr::where(is.numeric),
                      values_to = "Presence",
                      names_to = "Local") |> 
  dplyr::filter(Presence == 1)

sps_trat

## Coordenadas dos locais ----

### Importar ----

coord <- readxl::read_xlsx("DADOS COPILADOS DA FOM 2026.xlsx",
                           sheet = 2)

### Visualizar ----

coord

coord |> dplyr::glimpse()

### Tratar ----

coord_sf <- coord |> 
  dplyr::mutate(Longitude = Longitude |> 
                  parzer::parse_lon(),
                Latitude = Latitude |> 
                  parzer::parse_lat()) |> 
  dplyr::select(Local, dplyr::contains("tude")) |> 
  sf::st_as_sf(coords = c("Longitude", "Latitude"),
               crs = grade |> sf::st_crs())

coord_sf

ggplot() +
  geom_sf(data = grade) +
  geom_sf(data = coord_sf)

# Recortar para a FOM ----

## Intersectando para a FOM ----

coord_sf_fom <- coord_sf |> 
  sf::st_intersection(grade |> 
                        dplyr::summarise(geometry = geometry |> 
                                           sf::st_union()))

coord_sf_fom

ggplot() +
  geom_sf(data = grade) +
  geom_sf(data = coord_sf_fom)

## Extrair as informações da grade ----

df_id_fom <- coord_sf_fom |> 
  sf::st_join(grade) |> 
  dplyr::select(Local, ID) 

df_id_fom

## Fazer o join para os dados de registro ----

sps_id <- sps_trat |> 
  dplyr::left_join(df_id_fom,
                   by = "Local") |> 
  dplyr::select(-c(geometry, Local)) |>
  dplyr::mutate(
    Especies = trimws(Especies),
    Especies = dplyr::case_match(
      Especies,
      "Bothrops alternatus neuw." ~ "Bothrops alternatus",
      "Placosoma glabelum" ~ "Placosoma glabelum",
      "Ecpleopus gaudichaudi" ~ "Ecpleopus gaudichaudii",
      "Micrurus silvae" ~"Micrurus silviae",
      "Xenodon merremii" ~ "Xenodon merremii",
      "Rachidelus brazili" ~ "Rhachidelus brazili",
      "Paraphimophis rustica" ~ "Paraphimophis rusticus",
      "Oxyrophus rhombifer" ~ "Oxyrhopus rhombifer",
      "Xenodon merremi" ~ "Xenodon merremi",
      "Philodryas olfersi" ~ "Philodryas olfersi",
      "Leposternon microcephalum" ~ "Leposternon microcephalus",
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
      "Bothrops newwiedi" ~ "Bothrops neuwiedii",
      "Amphisbaena darwini" ~ "Amphisbaena darwinii",
      "Anops kingii" ~ "Amphisbaena kingii",
      "Amphisbaena mertensi" ~ "Amphisbaena mertensii",
      c("Varanus salvator", 
        "Tupinambis teguixin",
        "Tupinambis teguixim",
        "Salvator marianae") ~ "Salvator merianae",
      "Bothrops trigemina" ~ "Bothrops alternatus",
      "Bothrops neuwiedi paranaensis" ~ "Bothrops pubescens",
      .default = Especies
    ),
    Family = dplyr::case_when(
      Especies %in% c("Acanthochelys spixii", 
                      "Hydromedusa tectifera",
                      "Phrynops geoffroanus", 
                      "Phrynops williamsi",
                      "Phrynops hilarii") ~ "Chelidae",
      
      Especies %in% c("Trachemys scripta", 
                      "Trachemys dorbigni") ~ "Emydidae",
      
      Especies == "Amerotyphlops brongersmianus" ~ "Typhlopidae",
      
      Especies %in% c("Liotyphlops beui", 
                      "Liotyphlops ternetzii") ~ "Anomalepididae",
      
      Especies %in% c("Amphisbaena darwinii", 
                      "Amphisbaena dubia",
                      "Amphisbaena mertensi", 
                      "Amphisbaena prunicolor",
                      "Amphisbaena trachura", 
                      "Amphisbaena roberti",
                      "Leposternon microcephalum") ~ "Amphisbaenidae",
      
      Especies == "Hemidactylus mabouia" ~ "Gekkonidae",
      
      Especies %in% c("Contomastix vacariensis", 
                      "Salvator merianae",
                      "Teius oculatus") ~ "Teiidae",
      
      Especies %in% c("Cercosaura schreibersii", 
                      "Colobodactylus taunayi",
                      "Mesotes strigatus", 
                      "Pantodactylus schreibersii",
                      "Placosoma glabellum") ~ "Gymnophthalmidae",
      
      Especies == "Notomabuya frenata" ~ "Scincidae",
      
      Especies %in% c("Diploglossus fasciatus", 
                      "Ophiodes fragilis",
                      "Ophiodes striatus") ~ "Diploglossidae",
      
      Especies %in% c("Enyalius iheringii", 
                      "Enyalius perditus",
                      "Urostrophus grilli", 
                      "Urostrophus vautieri") ~ "Leiosauridae",
      
      Especies %in% c("Tropidurus catalanensis", 
                      "Tropidurus torquatus") ~ "Tropiduridae",
      
      Especies %in% c("Epicrates crassus", 
                      "Eunectes murinus",
                      "Eunectes notaeus") ~ "Boidae",
      
      Especies %in% c("Bothrops alternatus", 
                      "Bothrops diporus", 
                      "Bothrops jararaca",
                      "Bothrops jararacussu", 
                      "Bothrops neuwiedi", 
                      "Bothrops cotiara",
                      "Bothrops moojeni", 
                      "Bothrops pauloensis",
                      "Crotalus durissus") ~ "Viperidae",
      
      Especies %in% c("Micrurus altirostris", 
                      "Micrurus carvalhoi",
                      "Micrurus corallinus", 
                      "Micrurus frontalis") ~ "Elapidae",
      
      Especies %in% c("Chironius bicarinatus", 
                      "Chironius exoletus", 
                      "Chironius foveatus",
                      "Chironius flavolineatus", 
                      "Chironius laevicollis",
                      "Chironius multiventris", 
                      "Chironius gouveai",
                      "Leptophis marginatus", 
                      "Spilotes pullatus",
                      "Tropidodryas serra", 
                      "Tropidodryas striaticeps") ~ "Colubridae",
      
      TRUE ~ "Dipsadidae"  
    ),
    Especies = Especies |> stringr::str_trim()
  ) |> 
  dplyr::relocate(c(ID, Family), .before = 1) |> 
  dplyr::filter(!ID |> is.na())

sps_id

## Exportando ----

sps_id |> writexl::write_xlsx("registros_levantamento.xlsx")
