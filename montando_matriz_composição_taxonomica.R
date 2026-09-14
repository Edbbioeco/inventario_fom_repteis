# Pacotes ----

library(tidyverse)

library(readxl)

library(writexl)

# Dados ----

## Importando ----

comunidades <- purrr::map_dfr(
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

## Visualizando ----

comunidades

comunidades |> dplyr::glimpse()

# Matriz de composição taxonomica ----

## Montando a matriz ----

comunidades_trat <- comunidades |> 
  dplyr::select(ID, Especies, Presence) |> 
  tidyr::pivot_wider(names_from = Especies,
                     values_from = Presence,
                     values_fill = 0,
                     values_fn = max)

comunidades_trat

## Exportando ----

comunidades_trat |> writexl::write_xlsx("comunidades_taxonomicas.xlsx")
