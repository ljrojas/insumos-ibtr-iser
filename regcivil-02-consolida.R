# objetivo
# llevar informacion levantada a puntos en el espacio


# conf --------------------------------------------------------------------

library(readr)
library(sf)
library(stringr)
library(stringi)
library(dplyr)
library(tidyr)
library(ggplot2)

# data --------------------------------------------------------------------

data <- read_csv("data/registro-civil-scrap.csv")

# construye regex  -----------------------------------------------------------

data %>%
  pull(href) %>%
  str_extract("-\\d{2}\\..*")

# sin comas (un elemento)
comas <-
  data %>%
  pull(href) %>%
  str_extract("-\\d{2}\\..*") %>%
  str_detect(",",negate = T)

# revicion
data %>% filter(comas) %>% pull(href) # primera coordenada sin punto

# NA
na_which <-
  data %>%
  pull(href) %>%
  str_extract("-\\d{2}\\..*") %>%
  is.na()
# inspeccion
data %>% filter(na_which) %>% pull(href) # coordenada separa por espacio, o direccion

# corrige regex
data %>%
  pull(href) %>%
  str_extract("-\\d{2}\\.?.*,.*")

na_which <-
  data %>%
  pull(href) %>%
  str_extract("-\\d{2}\\.?.*,.*") %>%
  is.na()

# inspeccion
data %>% filter(na_which) %>% pull(href) # sin coordenada

data %>%
  filter(na_which) %>%
  pull(href) %>%
  str_extract("(?<=google\\?).*") %>%
  urltools::url_decode() %>%
  stri_enc_toutf8()

data %>%
  pull(href) %>%
  str_extract("-\\d{2}\\.?.*,.*") %>% # rescate inicial
  str_replace("^-\\d{2},","") %>% # corrige numero inicial antes de coordenadas
  str_replace_all("(?<=\\d)%2C(?=\\d)","\\.") %>% # url a punto
  str_replace_all(" ","") %>% # elimina espacios
  str_replace("(?<=\\d)-(?=\\d)","") %>%  # elimina menos rodeado de numeros
  str_replace("(?<=-\\d{2})(?!\\.|%|\\d\\.)","\\.") %>%   # añade punto a numero sin punto (asume que punto deberia ir des pues de dos numeros... evitando que haya un numero seguido de punto ojo isla de pascula)
  str_extract("-\\d{2}\\.\\d{4,},-\\d{2,3}\\.\\d{4,}$") # forma final

# rescata coord -----------------------------------------------------------

geo_data <-
  data %>%
  mutate(latlon = href %>%
           str_extract("-\\d{2}\\.?.*,.*") %>% # rescate inicial
           str_replace("^-\\d{2},","") %>% # corrige numero inicial antes de coordenadas
           str_replace_all("(?<=\\d)%2C(?=\\d)","\\.") %>% # url a punto
           str_replace_all(" ","") %>% # elimina espacios
           str_replace("(?<=\\d)-(?=\\d)","") %>%  # elimina menos rodeado de numeros
           str_replace("(?<=-\\d{2})(?!\\.|%|\\d\\.)","\\.") %>%   # añade punto a numero sin punto (asume que punto deberia ir des pues de dos numeros... evitando que haya un numero seguido de punto ojo isla de pascula)
           str_extract("-\\d{2}\\.\\d{4,},-\\d{2,3}\\.\\d{4,}$") # forma final
  ) %>%
  separate(latlon,c("lat","lon"), ",") %>%
  st_as_sf(
    coords = c('lon','lat'),
    crs = 4326,
    na.fail = FALSE,
    agr = "identity")

geo_data %>% ggplot() + geom_sf()

st_write(geo_data, 'data/shp/reg-civil_geo.shp', delete_layer = TRUE)

