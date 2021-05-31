#' ---
#' Título: Introdução ao R com aplicações em biodiversidade e conservação
#' Capítulo: 15 Dados geoespaciais no R
#' Autores: Maurício Vancine
#' Data: 2021-05-28
#' ---

# Carregar pacotes
library(tidyverse)
library(here)
library(sf)
library(raster)
library(rgdal)
library(spData)
library(rnaturalearth)
library(geobr)
library(ggplot2)
library(ggspatial)
library(tmap)
library(tmaptools)
library(grid)
library(mapview)
library(leaflet)
library(viridis)
library(knitr)
library(sidrar)
library(fasterize)

# Importar locais
aa_locais <- readr::read_csv(here::here("dados", "tabelas", "ATLANTIC_AMPHIBIANS_sites.csv"), col_types = cols()) %>%
  dplyr::select(id, longitude, latitude, species_number)

aa_especies_locais_ve_ma <- aa_locais %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) %>%
  dplyr::mutate(id = as.factor(id),
                dentro_bioma = as.numeric(sf::st_intersects(., mata_atlantica))) %>%
  dplyr::filter(dentro_bioma == 1) %>%
  dplyr::select(-dentro_bioma) %>%
  dplyr::slice_sample(n = 50)

# Importar raster do GlobCover
ra_globcover <- raster::raster(here::here("dados", "raster", "GLOBCOVER_L4_200901_200912_V2.3.tif"))

# Ajustar para o limite do bioma da Mata Atlântica
ra_globcover_ma <- ra_globcover %>%
  raster::crop(mata_atlantica) %>%
  raster::mask(mata_atlantica)
ra_globcover_ma

# Mapa
tm_shape(mata_atlantica, bbox = aa_especies_locais_ve_ma) +
  tm_polygons() +
  tm_shape(aa_especies_locais_ve_ma) +
  tm_dots(size = .1, col = "forestgreen")

aa_especies_locais_ve_ma_buffer10km <- sf::st_buffer(aa_especies_locais_ve_ma, dist = 0.08333)

aa_especies_locais_ve_ma_buffer10km_e <- raster::extract(x = ra_globcover_ma,
                     y = aa_especies_locais_ve_ma_buffer10km,
                     fun = function(i,...) table(i),
                     na.rm = TRUE) %>%
  dplyr::bind_rows() %>%
  dplyr::mutate(across(everything(), ~replace_na(.x, 0))) %>%
  dplyr::rename_with(~paste0("cover_", .)) %>%
  dplyr::mutate(across(everything(), as.numeric)) %>%
  dplyr::bind_cols(id = aa_especies_locais_ve_ma_buffer10km$id, .) %>%
  janitor::adorn_totals("col") %>%
  janitor::adorn_percentages("row") %>%
  janitor::adorn_pct_formatting(rounding = "half up", digits = 1)
aa_especies_locais_ve_ma_buffer10km_e$

aa_especies_locais_ve_ma_buffer10km_pe  <- dplyr::left_join(aa_especies_locais_ve_ma_buffer10km,
                                                            aa_especies_locais_ve_ma_buffer10km_e)
aa_especies_locais_ve_ma_buffer10km_pe

head(st_drop_geometry(aa_especies_locais_ve_ma_buffer10km_pe))
