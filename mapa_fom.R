# Pacotes ----

library(geobr)

library(tidyverse)

library(sf)

library(ggview)

library(cowplot)

# Shapefile dos estados do Brasil ----

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

# Mapa ----

## Mapa principal ----

mapa_principal <- ggplot() +
  geom_sf(data = br, 
          aes(color = "Brazil", fill = "Brazil"),
          linewidth = 1) +
  geom_sf(data = ma, 
          aes(color = "Atlantic Forest", fill = "Atlantic Forest")) +
  geom_sf(data = AMF, 
          aes(color = "AMF", fill = "AMF")) +
  geom_sf(data = br, color = "black", fill = "transparent", 
          linewidth = 1) +
  coord_sf(xlim = c(-54.04717, -44.25722),
           ylim = c(-30.36529, -22.56086)) +
  scale_color_manual(values = c("Brazil" = "black",
                                "Atlantic Forest" = "green",
                                "AMF" = "darkgreen"),
                     breaks = c("Brazil", 
                                "Atlantic Forest",
                                "AMF")) +
  scale_fill_manual(values = c("Brazil" = "white",
                               "Atlantic Forest" = "green",
                               "AMF" = "darkgreen"),
                    breaks = c("Brazil", 
                               "Atlantic Forest",
                               "AMF")) +
  labs(color = NULL,
       fill = NULL) +
  theme_bw() +
  theme(axis.text = element_text(color = "black", size = 20),
        legend.text = element_text(color = "black", size = 20),
        legend.position = "bottom",
        panel.border = element_rect(color = "black", linewidth = 2)) +
  ggview::canvas(height = 10, width = 12)

mapa_principal

## Inset map ----

inset_map <- ggplot() +
  geom_sf(data = br, color = "black", fill = "white") +
  geom_sf(data = ma, color = "green", fill = "green") +
  geom_sf(data = AMF, color = "darkgreen", fill = "darkgreen") +
  geom_sf(data = br, color = "black", fill = "transparent",
          linewidth = 1) +
  geom_rect(aes(xmin = -54.04717, xmax = -44.25722,
                ymin = -30.36529, ymax = -22.56086),
            color = "darkred",
            fill = "red",
            alpha = 0.5,
            linewidth = 1) +
  theme_void() +
  ggview::canvas(height = 10, width = 12)

inset_map

## Mapa final ----

cowplot::ggdraw(mapa_principal) +
  cowplot::draw_plot(inset_map,
                     x = 0.565,
                     y = 0.2,
                     height = 0.45,
                     width = 0.45) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "mapa_AMF.png",
       height = 10, width = 12)
