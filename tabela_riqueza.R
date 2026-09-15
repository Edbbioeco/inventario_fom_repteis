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