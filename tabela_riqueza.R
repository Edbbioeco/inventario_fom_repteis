# Pacotes ----

library(tidyverse)

library(readxl)

library(flextable)

# Dados ----

## Importar ----

comp <- purrr::map_dfr(
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

comp

comp |> dplyr::glimpse()

# Tabela ----

## Montar a tabela ----

comp |> 
  dplyr::select(-c(1, 4)) |> 
  dplyr::mutate(Source = dplyr::case_match(
    Source,
    "gbif" ~ "GBIF",
    "inaturalist" ~ "iNaturalist",
    .default = Source |> stringr::str_to_title())) |> 
  dplyr::group_by(Family, Especies) |>  
  dplyr::summarise(Source = paste(unique(Source), collapse = ", "), 
                   .groups = "drop") |> 
  as.data.frame() 
