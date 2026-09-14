# Pacotes ----

library(readxl)

library(tidyverse)

library(flextable)

# Dados ----

## Importar ----

comp <- readxl::read_xlsx("comunidades_taxonomicas.xlsx")

## Visualizar ----

comp

comp |> dplyr::glimpse()
