# Pacotes ----

library(geobr)

library(tidyverse)

library(sf)

library(ggview)

# Shapefile dos Estados do Brasil ----

## Importar ----

br <- geobr::read_state(year = 2025)

## Visualizar ----

br

ggplot() +
  geom_sf(data = br, color = "black")

# Shapefile da Mata Atlântica ----

## Importar ----

ma <- geobr::read_biomes(year = 2025) |> 
  dplyr::filter(name_biome == "Mata Atlântica")

## Visualizar ----

ma

ggplot() +
  geom_sf(data = ma, color = "green", fill = "green") +
  geom_sf(data = br, color = "black", fill = "transparent")

# Shapefile das Florestas Ombrófilas Densas ----

## Importar ----

AMF <- sf::st_read("fom.shp")

## Visualizar ----

AMF

ggplot() +
  geom_sf(data = ma, color = "green", fill = "green") +
  geom_sf(data = AMF, color = "darkgreen", fill = "darkgreen") +
  geom_sf(data = br, color = "black", fill = "transparent")

# Shapefile da grade ----

## Importar ----

grade <- sf::st_read("grade_fom.shp")

## Visualizar ----

grade

ggplot() +
  geom_sf(data = ma, color = "green", fill = "green") +
  geom_sf(data = AMF, color = "darkgreen", fill = "darkgreen") +
  geom_sf(data = grade, color = "black", fill = "transparent", 
          linewidth  = 0.05) +
  geom_sf(data = br, color = "black", fill = "transparent")

# Mapa ----

ggplot() +
  geom_sf(data = br, 
          aes(color = "Brazil", fill = "Brazil"),
          linewidth = 1) +
  geom_sf(data = ma, 
          aes(color = "Atlantic Forest", fill = "Atlantic Forest")) +
  geom_sf(data = AMF, 
          aes(color = "AMF", fill = "AMF")) +
  geom_sf(data = br, color = "black", fill = "transparent", 
          linewidth = 1) +
  geom_sf(data = grade, 
          aes(color = "Grid", fill = "Grid"), 
          linewidth = 1) +
  geom_sf(data = br, color = "black", fill = "transparent", 
          linewidth = 1) +
  coord_sf(xlim = c(-54.04717, -48),
           ylim = c(-30, -23.5)) +
  scale_color_manual(values = c("Brazil" = "black",
                                "Atlantic Forest" = "green",
                                "AMF" = "darkgreen",
                                "Grid" = "orange"),
                     breaks = c("Brazil", 
                                "Atlantic Forest",
                                "AMF",
                                "Grid")) +
  scale_fill_manual(values = c("Brazil" = "white",
                               "Atlantic Forest" = "green",
                               "AMF" = "darkgreen",
                               "Grid" = "transparent"),
                    breaks = c("Brazil", 
                               "Atlantic Forest",
                               "AMF",
                               "Grid")) +
  labs(color = NULL,
       fill = NULL) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.position = "bottom",
        panel.border = element_rect(color = "black", linewidth = 2)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "mapa_grade_AMF.png",
       height = 10, width = 12)
