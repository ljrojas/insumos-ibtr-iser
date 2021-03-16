# objetivo consolidar info descargada como informacion geografica


# conf --------------------------------------------------------------------

library(readr)
library(sf)
library(stringr)
library(tidyr)
library(dplyr)

# data --------------------------------------------------------------------

ips <- read.csv('data/resultado-ips.csv')

# rescatar lat lon --------------------------------------------------------

# ejemplo
ej <-
  ips %>%
  slice(1) # primera fila

ej$latlot %>%
  str_extract('(?<=\\?ll=).+(?=&z)') %>% # regex rescatar lat lon
  tibble(latlon = .) %>% # crear tibble
  separate(latlon,c('lat','lon'), ',')# usar tidyr::separate

# tbl to sf ---------------------------------------------------------------

geo_ips <-
  ips %>%
  select(-X) %>%
  mutate(xy = str_extract(latlot,'(?<=\\?ll=).+(?=&z)')) %>%
  separate(xy, c('lat', 'lon'), ',') %>%
  st_as_sf(
    coords = c('lon','lat'),
    crs = 4326,
    na.fail = FALSE,
    agr = "identity")


# export ------------------------------------------------------------------

if (!dir.exists('data/shp')) dir.create('data/shp')

st_write(geo_ips,'data/shp/ips_geo.shp', delete_layer = TRUE)
