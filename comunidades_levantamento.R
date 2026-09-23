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
  dplyr::mutate(Especies = Especies |> stringr::str_squish(),
                Especies = dplyr::case_match(
                  Especies,
                  "Chelonoidis carbonaria" ~ "Chelonoidis carbonarius",
                  "Uromacerina ricardinii" ~ "Cercophis auratus",
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
                  "Mabuya frenata" ~ "Notomabuya frenata",
                  c("Anisiolepis grilli", "Anisolepis grilli")  ~ "Urostrophus grilli",
                  "Mabuya dorsivittata" ~ "Aspronema dorsivittatum",
                  "Sibynomorphus neuwiedi" ~ "Dipsas neuwiedi",
                  c("Liotyphlops beui", "Liotyphlops sousai") ~ "Liotyphlops ternetzii",
                  "Taeniophallus poecilopogon" ~ "Dibernardia poecilopogon",
                  "Amphisbaena darwini trachura" ~ "Amphisbaena darwinii",
                  "Amphisbaena darwini trachura" ~ "Amphisbaena darwinii",
                  "Pantodactylus schreibersii" ~ "Cercosaura schreibersii",
                  c("Bothrops neuwiedi diorus", 
                    "Bothrops newwiedi") ~ "Bothrops neuwiedi",
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
                  "Bothrops neuwiedii" ~ "Bothrops neuwiedi",
                  "Amphisbaena darwini" ~ "Amphisbaena darwinii",
                  "Anops kingii" ~ "Amphisbaena kingii",
                  "Amphisbaena mertensi" ~ "Amphisbaena mertensii",
                  c("Varanus salvator", 
                    "Tupinambis merianae",
                    "Tupinambis teguixin",
                    "Tupinambis teguixim",
                    "Salvator marianae",
                    "Salvator rufescens") ~ "Salvator merianae",
                  "Bothrops trigemina" ~ "Bothrops alternatus",
                  "Bothrops neuwiedi paranaensis" ~ "Bothrops pubescens",
                  "Cleia rustica" ~ "Clelia rustica",
                  "Paulosophis bifossatus" ~ "Palusophis bifossatus",
                  "Oxyrophus clathratus" ~ "Oxyrhopus clathratus",
                  "Dipsas ventrimaculatus" ~ "Dipsas ventrimaculata",
                  "Dipsas neuwiedii" ~ "Dipsas neuwiedi",
                  "Bothrops neuwiedii" ~ "Bothrops neuwiedi",
                  "Phalotris iheringi" ~ "Phalotris lemniscatus",
                  "Xenodon histricus" ~ "Lystrophis histricus",
                  "Dipsas ventrimaculatus" ~ "Dipsas ventrimaculata",
                  c("Anolis philopunctatus", 
                    "Lygophis lineatus", 
                    "Dipsas indica", 
                    "Clelia plúmbea", 
                    "Xenodon biligonigerus",
                    "Caiman crocodylus",
                    "Eunectes murinus",
                    "Eunectes notaeus",
                    "Dermochelys coriacea",
                    "Amalosia queenslandia",
                    "Diploglossus fasciatus",
                    "Dipsas incerta",
                    "Hydrodynastes gigas",
                    "Pseudoboa neuwiedii",
                    "Xenopholis scalaris",
                    "Lepidodactylus lugubris",
                    "Rhinoclemmys punctularia",
                    "Arthrosaura reticulata",
                    "Heterodactylus imbricatus",
                    "Iguana iguana",
                    "Polychrus marmoratus",
                    "Chatogekko amazonicus",
                    "Gonatodes humeralis",
                    "Cnemidophorus cryptus",
                    "Cnemidophorus gramivagus",
                    "Kentropyx calcarata",
                    "Tropidurus oreadicus",
                    "Tropidurus torquatus",
                    "Uranoscodon superciliosus",
                    "Bothrops atrox",
                    "Caretta caretta",
                    "Chelonia mydas",
                    "Lepidochelys olivacea",
                    "Podocnemis expansa",
                    "Podocnemis sextuberculata",
                    "Podocnemis unifilis") ~ NA_character_,
                  .default = Especies
                ),
                Especies = Especies |> str_replace("Sibynomorphus", "Dipsas"),
    Family = dplyr::case_when(
      Especies |> 
        stringr::str_detect("Acanthochelys|Hydromedusa|Phrynops") ~ "Chelidae",
      Especies |> 
        stringr::str_detect("Trachemys") ~ "Emydidae",
      Especies |> 
        stringr::str_detect("Amerotyphlops") ~ "Typhlopidae",
      Especies |> 
        stringr::str_detect("Liotyphlops") ~ "Anomalepididae",
      Especies |> 
        stringr::str_detect("Amphisbaena|Leposternon") ~ "Amphisbaenidae",
      Especies |> 
        stringr::str_detect("Hemidactylus") ~ "Gekkonidae",
      Especies |> 
        stringr::str_detect("Contomastix|Salvator|Teius oculatus") ~ "Teiidae",
      Especies |> 
        stringr::str_detect("Cercosaura|Colobodactylus|Mesotes|Pantodactylus|Placosoma") ~ "Gymnophthalmidae",
      Especies |> 
        stringr::str_detect("Notomabuya") ~ "Scincidae",
      Especies |> 
        stringr::str_detect("Diploglossus|Ophiodes") ~ "Diploglossidae",
      Especies |> 
        stringr::str_detect("Enyalius|Urostrophus") ~ "Leiosauridae",
      Especies |> 
        stringr::str_detect("Tropidurus") ~ "Tropiduridae",
      Especies |> 
        stringr::str_detect("Epicrates|Eunectes") ~ "Boidae",
      Especies |> 
        stringr::str_detect("Bothrops|Crotalus") ~ "Viperidae",
      Especies |> 
        stringr::str_detect("Micrurus") ~ "Elapidae",
      Especies |> 
        stringr::str_detect("Chironius|Leptophis|Spilotess|Tropidodryas|Palusophis|Spilotes") ~ "Colubridae",
      .default = "Dipsadidae"  
    ),
    Family = dplyr::case_when(
      Especies |> 
        stringr::str_detect("Enyalius") ~ "Leiosauridae",
      Especies |> 
        stringr::str_detect("Liotyphlops") ~ "Anomalepididae",
      Especies |> 
        stringr::str_detect("Notomabuya") ~ "Scincidae",
      Especies |> 
        stringr::str_detect("Ophiodes") ~ "Diploglossidae",
      Especies |> 
        stringr::str_detect("Podocnemis") ~ "Podocnemididae",
      Especies |> 
        str_detect(
          "Apostolepis|Atractus|Boiruna|Clelia|Dibernardia|Dipsas|Dryophylax|Echinanthera|Erythrolamprus|Gomesophis|Oxyrhopus|Helicops|Imantodes|Mesotes|Paraphimophis|Phalotris|Philodryas|Pseudoboa|Ptychophis|Rhachidelus|Siphlophis|Taeniophallus|Thamnodynastes|Tomodon|Tropidodryas|Cercophis|Xenodon|Lygophis") ~ "Dipsadidae",
      Family == "Varanidae" ~ "Teiidae",
      Family|> stringr::str_detect("Xenodon") ~ "Dipsadidae",
      Family |> stringr::str_detect("inae") ~ Family |> 
        stringr::str_replace("inae", "idae"),,
      .default = Family
    ),
    Especies = Especies |> stringr::str_trim()
  ) |> 
  dplyr::relocate(c(ID, Family), .before = 1) |> 
  dplyr::filter(!ID |> is.na() & !Especies |> is.na())

sps_id

## Checando a matriz ----

sps_id |> 
  dplyr::filter(Especies |> is.na() | Family |> is.na())

## Exportando ----

sps_id |> writexl::write_xlsx("registros_levantamento.xlsx")

