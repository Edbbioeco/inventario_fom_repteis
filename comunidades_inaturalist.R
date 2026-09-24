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
  dplyr::mutate(scientific_name = scientific_name |> stringr::str_squish(),
                scientific_name = dplyr::case_match(
                  scientific_name,
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
                  "Cleia rustica" ~ "Paraphimophis rusticus",
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
                  .default = scientific_name
                ),
                scientific_name = scientific_name |> str_replace("Sibynomorphus", "Dipsas"),
                taxon_family_name = dplyr::case_when(
                  scientific_name |> 
                    stringr::str_detect("Enyalius") ~ "Leiosauridae",
                  scientific_name |> 
                    stringr::str_detect("Liotyphlops") ~ "Anomalepididae",
                  scientific_name |> 
                    stringr::str_detect("Notomabuya") ~ "Scincidae",
                  scientific_name |> 
                    stringr::str_detect("Ophiodes") ~ "Diploglossidae",
                  scientific_name |> 
                    stringr::str_detect("Palusophis") ~ "Colubridae",
                  scientific_name |> 
                    stringr::str_detect("Podocnemis") ~ "Podocnemididae",
                  scientific_name |> 
                    str_detect(
                      "Apostolepis|Atractus|Boiruna|Clelia|Dibernardia|Dipsas|Dryophylax|Echinanthera|Erythrolamprus|Gomesophis|Oxyrhopus|Helicops|Imantodes|Mesotes|Paraphimophis|Phalotris|Philodryas|Pseudoboa|Ptychophis|Rhachidelus|Siphlophis|Taeniophallus|Thamnodynastes|Tomodon|Tropidodryas|Cercophis|Xenodon|Lygophis|Adelphostigma") ~ "Dipsadidae",
                  taxon_family_name == "Varanidae" ~ "Teiidae",
                  taxon_family_name|> stringr::str_detect("Xenodon") ~ "Dipsadidae",
                  taxon_family_name |> stringr::str_detect("inae") ~ taxon_family_name |> 
                    stringr::str_replace("inae", "idae"),
                  taxon_family_name == "Varanidae" ~ "Teiidae",
                  .default = taxon_family_name
                ),
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

## Checando a matriz ----

inaturalist_registros |> 
  dplyr::filter(Especies |> is.na() | Family |> is.na())

## Exportando ----

inaturalist_registros |> writexl::write_xlsx("registros_inaturalist.xlsx")
