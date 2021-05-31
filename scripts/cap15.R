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

# -------------------------------------------------------------------------

# 1.2 Vetor ----

# 1.2.1 sf: principal pacote no R para dados vetoriais ----

# Dados vetoriais de polígonos do mundo
data(world)
world

# Plot dos polígonos do mundo
plot(world[1], col = viridis::viridis(100), main = "Mapa do mundo")

# 1.3 Raster ----

# 1.3.1 raster: principal pacote no R para dados raster ----

# Dados de altitude de um vulcão
head(volcano)

# Rasterlayer
raster_layer <- raster::raster(volcano)
raster_layer

# Plot raster layers
plot(raster_layer, col = viridis::viridis(n = 100))

# Raster layers
raster_layer1 <- raster_layer
raster_layer2 <- raster_layer * raster_layer
raster_layer3 <- sqrt(raster_layer)
raster_layer4 <- log10(raster_layer)

# Raster brick
raster_brick <- raster::brick(raster_layer1, raster_layer2, raster_layer3, raster_layer4)
raster_brick

# Plot raster brick
plot(raster_brick, col = viridis::viridis(n = 25))

# Raster layers
raster_layer1 <- raster_layer
raster_layer2 <- raster_layer * raster_layer
raster_layer3 <- sqrt(raster_layer)
raster_layer4 <- log10(raster_layer)

# Raster stack
raster_stack <- raster::stack(raster_layer1, raster_layer2, raster_layer3, raster_layer4)
raster_stack

# Plot raster stack
plot(raster_stack, col = viridis::viridis(n = 25))

# 1.4 Sistema de Referência de Coordenadas e Unidades ----

# 1.4.4 Sistema de Referência de Coordenadas (CRS) no R ----

# Listagem dos Sistemas de Referências de Coordenadas no R
crs_data <- rgdal::make_EPSG()
head(crs_data)

# 1.5 Principais fontes de dados geoespaciais ----



# 1.6 Importar e exportar dados geoespaciais ----

# 1.6.2 Importar dados ----

# 1.6.2.1 Vetor ----

# Formatos vetoriais importados e exportados pelo pacote sf
head(sf::st_drivers())

# 1.6.2.1.1 Importar dados vetoriais existentes ----

# Criar um diretório
dir.create(here::here("dados"))
dir.create(here::here("dados", "vetor"))

# Aumentar o tempo de download
options(timeout = 600)

# Download
for(i in c(".dbf", ".prj", ".shp", ".shx")){

  # Pontos de nascentes
  download.file(
    url = paste0("http://geo.fbds.org.br/SP/RIO_CLARO/HIDROGRAFIA/SP_3543907_NASCENTES", i),
    destfile = here::here("dados", "vetor", paste0("SP_3543907_NASCENTES", i)), mode = "wb")

  # Linhas de hidrografia
  download.file(
    url = paste0("http://geo.fbds.org.br/SP/RIO_CLARO/HIDROGRAFIA/SP_3543907_RIOS_SIMPLES", i),
    destfile = here::here("dados", "vetor", paste0("SP_3543907_RIOS_SIMPLES", i)), mode = "wb")

  # Polígonos de cobertura da terra
  download.file(
    url = paste0("http://geo.fbds.org.br/SP/RIO_CLARO/USO/SP_3543907_USO", i),
    destfile = here::here("dados", "vetor", paste0("SP_3543907_USO", i)), mode = "wb")
}

# Importar nascentes
rc_nas <- sf::st_read(here::here("dados", "vetor", "SP_3543907_NASCENTES.shp"), quiet = TRUE)

# Plot
plot(rc_nas[1], pch = 20, col = "blue", main = NA, axes = TRUE, graticule = TRUE)

# Importar hidrografia
rc_hid <- sf::st_read(here::here("dados", "vetor", "SP_3543907_RIOS_SIMPLES.shp"), quiet = TRUE)

# Plot
plot(rc_hid[1], col = "steelblue", main = NA, axes = TRUE, graticule = TRUE)

# Importar cobertura da terra
rc_cob <- sf::st_read(here::here("dados", "vetor", "SP_3543907_USO.shp"), quiet = TRUE)

# Plot
plot(rc_cob[5], col = c("blue", "orange", "gray30", "forestgreen", "green"),
     main = NA, axes = TRUE, graticule = TRUE)
legend(x = .2, y = .2, pch = 15, cex = .7, pt.cex = 2.5,
       legend = (rc_cob$CLASSE_USO),
       col = c("blue", "orange", "gray30", "forestgreen", "green"))

# 1.6.2.1.2 Importar utilizando pacotes

# Listar todos os dados do geobr
geobr::list_geobr()

# Polígono de Rio Claro
rc_2019 <- geobr::read_municipality(code_muni = 3543907, year = 2019, showProgress = FALSE)

# Plot
plot(rc_2019[1], col = "gray", main = NA, axes = TRUE, graticule = TRUE)

# Polígono do Brasil
br <- rnaturalearth::ne_countries(scale = "large", country = "Brazil", returnclass = "sf")

# Plot
plot(br[1], col = "gray", main = NA, axes = TRUE, graticule = TRUE)

# 1.6.2.1.3 Criar um objeto espacial de uma tabela de coordenadas ----

# Criar um diretório
dir.create(here::here("dados", "tabelas"))

# Download
download.file(url = "https://esajournals.onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1002%2Fecy.2392&file=ecy2392-sup-0001-DataS1.zip",
              destfile = here::here("dados", "tabelas", "atlantic_amphibians.zip"), mode = "wb")

# Unzip
unzip(zipfile = here::here("dados", "tabelas", "atlantic_amphibians.zip"),
      exdir = here::here("dados", "tabelas"))

# Importar tabela de locais
aa_locais <- readr::read_csv(
  here::here("dados", "tabelas", "ATLANTIC_AMPHIBIANS_sites.csv")
)
aa_locais

# Converter dados tabulares para sf
aa_locais_ve <- aa_locais %>%
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)
aa_locais_ve

# Plot
plot(aa_locais_ve[1], pch = 20, col = "black", main = NA, axes = TRUE, graticule = TRUE)

# 1.6.2.1.4 Converter dados espaciais sp para sf ----

# Polígonos países sp
co110_sp <- rnaturalearth::countries110
class(co110_sp)

# Polígonos países sf
co110_sf <- sf::st_as_sf(co110_sp)
class(co110_sf)

# Polígonos países sp
co110_sp <- sf::as_Spatial(co110_sf)
class(co110_sp)

# 1.6.2.2 Raster ----



















# 1.7 Descrição de objetos espaciais ----



# 1.8 Reprojeção de dados geoespaciais ----



# 1.9 Principais operações com dados geoespaciais ----



# 1.10 Visualização de dados geoespaciais ----



# 1.11 Exemplos de aplicações de análises geoespaciais para dados ecológicos ----



# fim ---------------------------------------------------------------------
