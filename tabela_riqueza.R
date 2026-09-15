# Pacotes ----

library(tidyverse)

library(readxl)

library(flextable)

# Dados ----

## Importar ----

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

## Visualizar ----

composicao

composicao |> dplyr::glimpse()

# Tabela ----

## Montar a tabela ----

tabela <- composicao |> 
  dplyr::select(-c(1, 4)) |> 
  dplyr::mutate(Source = dplyr::case_match(
    Source,
    "gbif" ~ "GBIF",
    "inaturalist" ~ "iNaturalist",
    .default = Source |> stringr::str_to_title())) |> 
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
    .before = 1)

tabela
