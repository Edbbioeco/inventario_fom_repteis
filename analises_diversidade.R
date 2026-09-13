# Pacotes ----

library(readxl)

library(tidyverse)

library(sf)

library(vegan)

library(ggview)

library(spdep)

library(spatialreg)

library(betapart)

library(reshape2)

library(patchwork)

# Dados ----

## Composição ----

### Importar ----

comp <- readxl::read_xlsx("comunidades_taxonomicas.xlsx")

### Visualizar ----

comp

comp |> dplyr::glimpse()

## Grade das Florestas Ombrófilas Mistas ----

### Importar ----

grade <- sf::st_read("grade_fom.shp")

### Visualizar ----

grade

ggplot() +
  geom_sf(data = grade, color = "black")

# Diversidade ----

## Riqueza ----

### Calcular riqueza ----

riq <- comp |> 
  tibble::column_to_rownames(var = "ID") |> 
  vegan::specnumber() |> 
  as.data.frame() |> 
  tibble::rownames_to_column() |> 
  dplyr::rename("ID" = 1,
                "Richness" = 2)

riq

### Adicionar as informações de riqueza na grade ----

grade <- grade |> 
  dplyr::left_join(riq, by = "ID") |> 
  dplyr::mutate(Richness = dplyr::case_when(
    
    Richness |> is.na() ~ 0,
    .default = Richness
    
    ))

grade

### Visualizar ----

ggplot() +
  geom_sf(data = grade, 
          aes(color = Richness |> log(),
              fill = Richness |> log())) + 
  scale_color_viridis_c(na.value = "#440154FF",
                        guide = guide_colourbar(
                          title = "Log Richness",
                          title.position = "top",
                          title.hjust = 0.5,
                          barwidth = 30,
                          barheight = 2,
                          frame.colour = "black",
                          ticks.colour = "black"
                        )) +
  scale_fill_viridis_c(na.value = "#440154FF",
                       guide = guide_colourbar(
                         title = "Log Richness",
                         title.position = "top",
                         title.hjust = 0.5,
                         barwidth = 30,
                         barheight = 2,
                         frame.colour = "black",
                         ticks.colour = "black"
                       )) +
  theme_bw() +
  theme(axis.text = element_text(size = 20, color = "black"),
        legend.text = element_text(size = 20, color = "black"),
        legend.title = element_text(size = 20, color = "black"),
        legend.position = "bottom",
        panel.border = element_rect(color = "black", linewidth = 1)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "riqueza_fom.png",
       height = 10, width = 12)

## Calcular teste I de Moran para agrupamento espacial ----

### Calcular janela ----

janela <- grade |> 
  dplyr::distinct(grade |> sf::st_geometry(), 
                  .keep_all = TRUE) |> 
  spdep::poly2nb() |> 
  spdep::nb2listw(style = "W")

janela

### Calcular teste I de Moran ----

spdep::moran.mc(grade |> 
                  dplyr::distinct(grade |> sf::st_geometry(), 
                                  .keep_all = TRUE) %>%
                  .$Richness, 
                janela, nsim = 999)

## Calcular a regressão geográfica ----

### Calcular coordenadas ----

grade_modelos <- grade |> 
  dplyr::distinct(grade |> sf::st_geometry(), 
                  .keep_all = TRUE) |> 
  dplyr::bind_cols(grade |> 
                     dplyr::distinct(grade |> sf::st_geometry(), 
                                     .keep_all = TRUE) |> 
                     sf::st_centroid() |> 
                     sf::st_coordinates() |>
                     as.data.frame() |> 
                     dplyr::rename("Longitude" = 1,
                                   "Latitude" = 2))

grade_modelos

### Testar multicolinearidade ----

cor(grade_modelos$Longitude, grade_modelos$Latitude)

cor(grade_modelos$Longitude, grade_modelos$Longitude * grade_modelos$Latitude)

cor(scale(grade_modelos$Longitude, scale = FALSE)[,1], 
    scale(grade_modelos$Longitude, scale = FALSE)[,1] * scale(grade_modelos$Latitude, scale = FALSE)[,1])

### Calcular Modelo de Erro Espacial ----

modelo_riqueza <- spatialreg::errorsarlm(
  Richness ~ Longitude * Latitude, 
  data = grade_modelos |> 
    dplyr::mutate(dplyr::across(
      .cols = dplyr::contains("tude"),
      .fns = ~scale(.x, scale = FALSE)[,1])), 
  listw = janela
)

modelo_riqueza

modelo_riqueza |> summary()

## Dissimilaridade das comunidades ----

### Calcular a dissimilaridade global ----

dis_global <- purrr::map_vec(
  1:3,
  \(id){
    
    indice <- comp |> 
      tibble::column_to_rownames(var = "ID") |> 
      betapart::beta.multi(index.family = "jaccard")
    
    indice[[id]]
    
    },
  .progress = TRUE)

dis_global

### Calcular a dissimilaridade par-a-par ----

dis_par <- purrr::map2_dfr(
  1:3,
  c("Turnover", "Nestdeness", "Jaccard"),
  \(id, indice){
    
    dis <- comp |> 
      tibble::column_to_rownames(var = "ID") |> 
      betapart::beta.pair(index.family = "jaccard")
    
    dis_matrix <- dis[[id]] |> 
      as.matrix()
    
    dis_matrix[upper.tri(dis_matrix)] <- NA
    
    dis_matrix |> 
      reshape2::melt() |> 
      tidyr::drop_na() |> 
      dplyr::filter(Var1 != Var2) |> 
      dplyr::rename("Mean dissimilarity" = 3) |> 
      dplyr::mutate(`Mean dissimilarity` = `Mean dissimilarity` |> 
                      round(2),
                    Index = indice)
    
    },
  .progress = TRUE) |> 
  tidyr::pivot_wider(names_from = Index,
                     values_from = `Mean dissimilarity`)

dis_par

### Calcular a média por cada grid ----

dis_par_trat <- dis_par |> 
  dplyr::summarise(dplyr::across(.cols = dplyr::where(is.numeric),
                                 .fns = ~.x |> mean()),
                   .by = Var1) |> 
  dplyr::rename("ID" = 1)

dis_par_trat

### Adicionar os valores de dissimilaridade ao shapefile da grade ----

grade <- grade |> 
  dplyr::left_join(dis_par_trat,
                   by = "ID") |> 
  dplyr::mutate(dplyr::across(.cols = c(Turnover:Jaccard),
                              .fns = ~dplyr::case_when(
                                
                                .x |> is.na() ~ 0,
                                .default = .x
                                
                              )))

grade

### Mapas ----

mapas_dis <- purrr::map(
  c("Jaccard", "Turnover", "Nestdeness"),
  \(indice){
    
    ggplot() +
      geom_sf(data = grade,
              aes(color = .data[[indice]],
                  fill = .data[[indice]])) +
      scale_color_viridis_c(na.value = "#440154FF",
                            guide = guide_colourbar(
                              title = paste0(indice, 
                                             " mean dissimilarity"),
                              title.position = "top",
                              title.hjust = 0.5,
                              barwidth = 25,
                              barheight = 2,
                              frame.colour = "black",
                              ticks.colour = "black"
                            )) +
      scale_fill_viridis_c(na.value = "#440154FF",
                           guide = guide_colourbar(
                             title = paste0(indice, 
                                            " mean dissimilarity"),
                             title.position = "top",
                             title.hjust = 0.5,
                             barwidth = 25,
                             barheight = 2,
                             frame.colour = "black",
                             ticks.colour = "black"
                           )) +
      labs(title = indice) +
      theme_bw() +
      theme(axis.text = element_text(size = 20, color = "black"),
            legend.text = element_text(size = 20, color = "black"),
            legend.title = element_text(size = 20,  color = "black"),
            legend.position = "bottom",
            panel.border = element_rect(color = "black", linewidth = 1),
            plot.title = element_text(size = 30, color = "black", 
                                      hjust = 0.5)) +
      ggview::canvas(height = 10, width = 12)
    
    },
  .progress = TRUE) |> 
  patchwork::wrap_plots() +
  ggview::canvas(height = 10, width = 20)

mapas_dis

ggsave(filename = "dissimilaridade_fom.png",
       height = 10, width = 20)

## Compartilhamento de espécies ----

### Calcular ----

spe_comp <- comp |> 
  tibble::column_to_rownames(var = "ID") |> 
  betapart::betapart.core() %>%
  .$shared |> 
  as.matrix() |> 
  reshape2::melt() |> 
  dplyr::summarise("Max count of shared species" = value |> max(),
                   .by = Var1) |> 
  dplyr::rename("ID" = 1)

spe_comp

### Adicionar os valores de dissimilaridade ao shapefile da grade ----

grade <- grade |> 
  dplyr::left_join(spe_comp,
                   by = "ID") |> 
  dplyr::mutate(`Max count of shared species` = dplyr::case_when(
    
    `Max count of shared species` |> is.na() ~ 0,
    .default = `Max count of shared species`
    
    ))

grade

### Mapas ----

ggplot() +
  geom_sf(data = grade,
          aes(color = `Max count of shared species` |> log(),
              fill = `Max count of shared species` |> log())) +
  scale_color_viridis_c(na.value = "#440154FF",
                        guide = guide_colourbar(
                          title.position = "top",
                          title.hjust = 0.5,
                          barwidth = 25,
                          barheight = 2,
                          frame.colour = "black",
                          ticks.colour = "black"
                        )) +
  scale_fill_viridis_c(na.value = "#440154FF",
                       guide = guide_colourbar(
                         title.position = "top",
                         title.hjust = 0.5,
                         barwidth = 25,
                         barheight = 2,
                         frame.colour = "black",
                         ticks.colour = "black"
                       )) +
  labs(fill = "Log max count of shared species",
       color = "Log max count of shared species") +
  theme_bw() +
  theme(axis.text = element_text(size = 20, color = "black"),
        legend.text = element_text(size = 20, color = "black"),
        legend.title = element_text(size = 20,  color = "black"),
        legend.position = "bottom",
        panel.border = element_rect(color = "black", linewidth = 1),
        plot.title = element_text(size = 30, color = "black", 
                                  hjust = 0.5)) +
  ggview::canvas(height = 10, width = 12)

ggsave(filename = "spe_comp_fom.png",
       height = 10, width = 12)
