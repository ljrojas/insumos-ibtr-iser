# objetivo
# llevar a shp puntos de fonasa


# conf --------------------------------------------------------------------

library(readr)
library(sf)
library(stringr)
library(stringi)
library(tidyr)
library(dplyr)
library(ggplot2)

# data ---------------------------------------------------------------------

fonasa <- read_csv('data/fonasa-scrap.csv')


# rescatar lat lon --------------------------------------------------------

# ejemplo
ej <-
  fonasa %>%
  slice(1) # primera fila

ej$href %>%
  str_extract('(?<=dir/).+(?=/@)') %>% # regex rescatar lat lon
  tibble(latlon = .) %>% # crear tibble
  separate(latlon,c('lat','lon'), ',')# usar tidyr::separate

# cuenta NA
fonasa %>%
  mutate(latlon = str_extract(href,"(?<=dir/).+(?=/@)")) %>%
  mutate(na=is.na(latlon)) %>%
  filter(na) %>%
  nrow()

# conceptos mas repetidos en NA

fonasa %>%
  mutate(latlon = str_extract(href,"(?<=dir/).+(?=/@)")) %>%
  mutate(na=is.na(latlon)) %>%
  filter(na) %>%
  pull(nombre) %>%
  stri_trans_general("Latin-ASCII") %>%
  str_to_lower() %>%
  str_split(pattern = " ") %>%
  unlist() %>%
  tibble(palabra = .) %>%
  group_by(palabra) %>%
  summarise(n= n()) %>%
  arrange(-n)

# ips, municipalidad, clinicas y suc => poi (points of interest)

# geo ---------------------------------------------------------------------

# geo_fonasa <-
  fonasa %>%
  mutate(latlon = str_extract(href,"(?<=dir/).+(?=/@)")) %>%
  separate(latlon,c("lat","lon"), ",") %>%
  mutate(across(c('lat','lon'),as.numeric)) %>% # corrige latitud norte
  mutate(lat = ifelse(lat>0,lat*-1,lat)) %>%
  st_as_sf(
    coords = c('lon','lat'),
    crs = 4326,
    na.fail = FALSE,
    agr = "identity") %>%
  ggplot() + geom_sf()

# error en municipalidad de doñihue
fonasa %>%
  mutate(latlon = str_extract(href,"(?<=dir/).+(?=/@)")) %>%
  separate(latlon,c("lat","lon"), ",") %>%
  mutate(across(c('lat','lon'),as.numeric)) %>%
  filter(lat>0)

geo_fonasa <-
  fonasa %>%
  mutate(latlon = str_extract(href,"(?<=dir/).+(?=/@)")) %>%
  separate(latlon,c("lat","lon"), ",") %>%
  mutate(across(c('lat','lon'),as.numeric)) %>% # corrige latitud norte
  mutate(lat = ifelse(lat>0,lat*-1,lat)) %>%
  st_as_sf(
    coords = c('lon','lat'),
    crs = 4326,
    na.fail = FALSE,
    agr = "identity")


# exportar ----------------------------------------------------------------


if (!dir.exists('data/shp')) dir.create('data/shp')

st_write(geo_fonasa,'data/shp/fonasa_geo.shp', delete_layer = TRUE)

