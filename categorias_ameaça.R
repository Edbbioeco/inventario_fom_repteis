# Pacotes ----

library(readxl)

library(tidyverse)

# Dados ----

## Importar ----

iucn <- readxl::read_xlsx("lista_repteis.xlsx")

## Visualizar ----

iucn

iucn |> dplyr::glimpse()
