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
  dplyr::mutate(Espécie = Espécie |> stringr::str_squish(),
                Espécie = dplyr::case_match(
                  Espécie,
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
                  "Sibynomorphus neuwiedii" ~ "Dipsas neuwiedi",
                  c("Liotyphlops beui", "Liotyphlops sousai") ~ "Liotyphlops ternetzii",
                  "Taeniophallus poecilopogon" ~ "Dibernardia poecilopogon",
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
                  c("Dipsas ventrimaculatus",
                    "Sibynomorphus ventrimaculatus") ~ "Dipsas ventrimaculata",
                  "Dipsas neuwiedii" ~ "Dipsas neuwiedi",
                  "Bothrops neuwiedii" ~ "Bothrops neuwiedi",
                  "Phalotris iheringi" ~ "Phalotris lemniscatus",
                  "Lystrophis histricus" ~ "Xenodon histricus",
                  "Anolis punctatus" ~ "Dactyloa punctata",
                  "Boa constrictor" ~ "Boa atlantica",
                  "Corallus hortulanus"  ~ "Corallus hortulana",
                  "Lystrophis histricus" ~ "Xenodon histricus",
                  "Pseustes sulphureus" ~ "Spilotes sulphureus",
                  c("Dryophylax pallidus",
                    "Thamnodynastes strigilis") ~ "Thamnodynastes pallidus",
                  "Echinanthera amoena" ~ "Amnisiophis amoenus",
                  "Echinanthera occipitalis" ~ "Echinanthera cephalostriata",
                  "Erythrolamprus macrosomus" ~ "Erythrolamprus macrosoma",
                  "Liophis poecilogyrus" ~ "Erythrolamprus poecilogyrus",
                  c("Philodryas patagoniensis",
                    "Philodryas pseudomamba") ~ "Pseudablabes patagoniensis",
                  "Philodryas arnaldoi" ~ "Pseudablabes arnaldoi",
                  "Taeniophallus affinis" ~ "Dibernardia affinis",
                  "Taeniophallus bilineatus" ~ "Dibernardia bilineatus",
                  "Taeniophallus occipitalis" ~ "Adelphostigma occipitalis",
                  "Taeniophallus persimilis" ~ "Dibernardia persimilis",
                  "Thamnodynastes nattereri" ~ "Dryophylax nattereri",
                  "Anisolepis grillii" ~ "Urostrophus grilli",
                  "Amerotyphlops bronguersmianus" ~ "Amerotyphlops brongersmianus",
                  "Philodryas agassizii" ~ "Pseudablabes agassizii",
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
                    "Podocnemis unifilis",
                    "Mabuya mabouya",
                    "Trachemys scripta") ~ NA_character_,
                  .default = Espécie
                ),
                Espécie = Espécie |> str_replace("Sibynomorphus", "Dipsas"),
                Família = dplyr::case_when(
                  Espécie |> 
                    stringr::str_detect("Enyalius") ~ "Leiosauridae",
                  Espécie |> 
                    stringr::str_detect("Liotyphlops") ~ "Anomalepididae",
                  Espécie |> 
                    stringr::str_detect("Notomabuya") ~ "Scincidae",
                  Espécie |> 
                    stringr::str_detect("Ophiodes") ~ "Diploglossidae",
                  Espécie |> 
                    stringr::str_detect("Palusophis") ~ "Colubridae",
                  Espécie |> 
                    stringr::str_detect("Podocnemis") ~ "Podocnemididae",
                  Espécie |> 
                    str_detect(
                      "Apostolepis|Atractus|Boiruna|Clelia|Dibernardia|Dipsas|Dryophylax|Echinanthera|Erythrolamprus|Gomesophis|Oxyrhopus|Helicops|Imantodes|Mesotes|Paraphimophis|Phalotris|Philodryas|Pseudoboa|Ptychophis|Rhachidelus|Siphlophis|Taeniophallus|Thamnodynastes|Tomodon|Tropidodryas|Cercophis|Xenodon|Lygophis|Adelphostigma") ~ "Dipsadidae",
                  Família == "Varanidae" ~ "Teiidae",
                  Família|> stringr::str_detect("Xenodon") ~ "Dipsadidae",
                  Família |> stringr::str_detect("inae") ~ Família |> 
                    stringr::str_replace("inae", "idae"),
                  Família == "Varanidae" ~ "Teiidae",
                  .default = Família
                ),
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

## Checando a matriz ----

herpetohelp_registros |> 
  dplyr::filter(Especies |> is.na() | Family |> is.na())

## Exportando ----

herpetohelp_registros |> writexl::write_xlsx("registros_herpetohelp.xlsx")
