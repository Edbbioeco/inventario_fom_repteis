# Pacotes ----

library(tidyverse)

library(readxl)

library(flextable)

# Dados ----

## Espécies ----

### Importar ----

composicao <- purrr::map_dfr(
  c("gbif", 
    "specieslink", 
    "sibbr", 
    "levantamento",
    "inaturalist",
    "herpetohelp"), 
  \(registro){
    
    readxl::read_xlsx(paste0("./registros_", registro, ".xlsx")) |> 
      dplyr::mutate(Source = registro)
    
  },
  .progress = TRUE)

### Visualizar ----

composicao

composicao |> dplyr::glimpse()

## Dados da plataforma SALVE ----

### Importar ----

salve <- readr::read_csv("salve_criterio.csv")

### Visualizar ----

salve

salve |> dplyr::glimpse() 

# Tabela ----

## Montar a tabela ----

tabela <- composicao |> 
  dplyr::select(-c(1, 4)) |> 
  dplyr::mutate(Source = dplyr::case_match(
    Source,
    "gbif" ~ "GBIF",
    "inaturalist" ~ "iNaturalist",
    .default = Source |> stringr::str_to_title()),
    Family = Family |> stringr::str_squish(),
    Especies = Especies |> stringr::str_squish()) |> 
  dplyr::group_by(Family, Especies) |>  
  dplyr::summarise(Source = paste(unique(Source), collapse = ", "), 
                   .groups = "drop") |> 
  dplyr::rename("Species" = Especies) |> 
  dplyr::mutate(
    Order = dplyr::case_match(
      Family,
      "Alligatoridae" ~ "Crocodylia",
      c("Testudinidae", "Podocnemididae", "Chelidae", "Kinosternidae",
        "Emydidae", "Cheloniidae") ~ "Testudines",
      .default = "Squamata"),
    .before = 1) |> 
  dplyr::arrange(Order, Family, Species) |> 
  dplyr::left_join(salve |> 
                     dplyr::rename("Species" = especie,
                                   "BR" = categoria) |> 
                     dplyr::mutate(BR = dplyr::case_match(
                       
                       BR,
                       "Vulnerável" ~ "VU",
                       "Menos Preocupante" ~ "LC",
                       "Em Perigo" ~ "EN",
                       "Quase Ameaçada" ~ "NT",
                       "Dados Insuficientes" ~ "DD",
                       "Criticamente em Perigo" ~ "CR"
                       
                       )) |> 
                     dplyr::select(Species, BR),
                   by = "Species")

tabela

## Criar a tabela flextable ----

tabela_flex <- tabela |> 
  flextable::flextable() |> 
  flextable::align(align = "center", part = "all") |> 
  flextable::italic(j = 3, part = "body") |> 
  flextable::width(width = 1.5)

tabela_flex

## Exportar tabela ----

tabela_flex |> flextable::save_as_docx(path = "tabela_riqueza.docx")

## Estatísticas descritivas ----

### Riqueza por ordem ----

tabela |> 
  dplyr::summarise(riqueza = dplyr::n(),
                   .by = Order)

### Riqueza por família ----

tabela |> 
  dplyr::summarise(riqueza = dplyr::n(),
                   .by = Family) |> 
  dplyr::arrange(riqueza |> dplyr::desc())

### Riqueza por família ----

tabela |> 
  dplyr::summarise(riqueza = Family |> dplyr::n_distinct(),
                   .by = Order) |> 
  dplyr::arrange(riqueza |> dplyr::desc())

### Riqueza de espécies por gênero ----

tabela |> 
  dplyr::mutate(Genus = Species |> stringr::word(1)) |> 
  dplyr::summarise(riqueza = dplyr::n(),
                   .by = Genus) |> 
  dplyr::arrange(riqueza |> dplyr::desc())
