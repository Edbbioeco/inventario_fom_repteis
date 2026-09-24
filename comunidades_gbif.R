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

gbif <- readr::read_tsv("gbif.csv",
                        quote = "",
                        na = "")

### Visualizando ----

gbif 

gbif |> dplyr::glimpse()

# Recortar para a FOM ----

## Transformando em shapefile ----

gbif_sf <- gbif |> 
  sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
               crs = grade |> sf::st_crs())

gbif_sf

ggplot() +
  geom_sf(data = gbif_sf)

## Intersectando para a FOM ----

gbif_sf_fom <- gbif_sf |> 
  sf::st_intersection(grade |> 
                        dplyr::summarise(geometry = geometry |> 
                                           sf::st_union()))

gbif_sf_fom

ggplot() +
  geom_sf(data = grade) +
  geom_sf(data = gbif_sf_fom)

# Matriz de composição ----

## Lista de espécies ----

gbif_sf_fom |> 
  dplyr::pull(species) |> 
  unique()

## tratando as espécies ----

gbif_sf_fom <- gbif_sf_fom |> 
  dplyr::mutate(species = species |> stringr::str_squish(),
                species = dplyr::case_match(
                  species,
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
                  "Clelia clelia" ~ "Paraphimophis rusticus",
                  "Clelia plúmbea" ~ "Clelia plumbea",
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
                    "Trachemys scripta",
                    "Anillius scytale",
                    "Dactyloa punctata",
                    "Naps ortonii",
                    "Corallus hortulana",
                    "Dendrophidion dendrophis",
                    "Leptophis ahaetulla",
                    "Mastigodryas boddaerti",
                    "Oxybelis aeneus",
                    "Spilotes sulphureus",
                    "Echinanthera melanostigma",
                    "Erythrolamprus aesculapii",
                    "Erythrolamprus breviceps",
                    "Erythrolamprus miliaris",
                    "Erythrolamprus poecilogyrus",
                    "Erythrolamprus typhlus",
                    "Helicops leopardinus",
                    "Leptodeira annulata",
                    "Phalotris lativittatus",
                    "Thamnodynastes pallidus",
                    "Micrurus frontalis",
                    "Micrurus lemniscatus",
                    "Chelonoidis carbonarius") ~ NA_character_,
                  .default = species
                ),
                species = species |> str_replace("Sibynomorphus", "Dipsas"),
                family = dplyr::case_when(
                  species |> 
                    stringr::str_detect("Enyalius") ~ "Leiosauridae",
                  species |> 
                    stringr::str_detect("Liotyphlops") ~ "Anomalepididae",
                  species |> 
                    stringr::str_detect("Notomabuya") ~ "Scincidae",
                  species |> 
                    stringr::str_detect("Ophiodes") ~ "Diploglossidae",
                  species |> 
                    stringr::str_detect("Palusophis") ~ "Colubridae",
                  species |> 
                    stringr::str_detect("Podocnemis") ~ "Podocnemididae",
                  species |> 
                    str_detect(
                      "Apostolepis|Atractus|Boiruna|Clelia|Dibernardia|Dipsas|Dryophylax|Echinanthera|Erythrolamprus|Gomesophis|Oxyrhopus|Helicops|Imantodes|Mesotes|Paraphimophis|Phalotris|Philodryas|Pseudoboa|Ptychophis|Rhachidelus|Siphlophis|Taeniophallus|Thamnodynastes|Tomodon|Tropidodryas|Cercophis|Xenodon|Lygophis|Adelphostigma") ~ "Dipsadidae",
                  family == "Varanidae" ~ "Teiidae",
                  family|> stringr::str_detect("Xenodon") ~ "Dipsadidae",
                  family |> stringr::str_detect("inae") ~ family |> 
                    stringr::str_replace("inae", "idae"),
                  family == "Varanidae" ~ "Teiidae",
                  .default = family
                ),
                species = species |> stringr::str_trim()) |> 
  dplyr::filter(!species |> is.na() &
                  !species |> stringr::str_detect("sp|sp.|cf|cf.") &
                  !species |> 
                  stringr::str_trim() |> 
                  stringr::str_count("\\S+") == 1)

gbif_sf_fom

## Montando a matriz de composição ----

## Montando a matriz ----

gbif_registros <- gbif_sf_fom |> 
  sf::st_join(grade) |> 
  as.data.frame() |> 
  dplyr::mutate(Especies = species,
                Presence =  1,
                Family = family) |> 
  dplyr::select(ID, Family, Especies, Presence) 

gbif_registros

## Checando a matriz ----

gbif_registros |> 
  dplyr::filter(Especies |> is.na() | Family |> is.na())

## Exportando ----

gbif_registros |> writexl::write_xlsx("registros_gbif.xlsx")
