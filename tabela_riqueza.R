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

comp
