# Objetivo: rescate datos sucursales fonasa

# 398 sucursales
# tipo sucursal en el nombre al inicio (IPS, Municipalidad, etc)
# No siempre direccion guarda lat lon (marker position)
#


# conf --------------------------------------------------------------------

# library(rvest)
library(RSelenium) # requiere chrome y java
library(dplyr)
library(stringr)
library(readr)

# url de busqueda zona censal y localidad rural
url_fonasa <- "https://www.fonasa.cl/sites/fonasa/beneficiarios/tramites/sucursales"


# fun ---------------------------------------------------------------------

limpiar_texto <- function(x) {
  x <-
    x %>%
    stri_trans_general("Latin-ASCII") %>%
    str_to_lower() %>%
    str_trim()
  return(x)
}

# setup selenium ----------------------------------------------------------
# debiese abrir un navegador chro,e
rD <- rsDriver(browser = "firefox", port = 4446L)
# rD <- rsDriver(version = "3.141.59", port = 8888L)
remDr <- rD[["client"]]

remDr$navigate(url_fonasa)
sys.sleep(5)

# box sucursales
LISTA <- remDr$findElements("class","box-sucursal-selector")

box <- remDr$findElement("class","box-sucursal-selector")
box$highlightElement()

# sucursales
botones <- box$findChildElements("tag","button")
length(botones)

# acumulador --------------------------------------------------------------

resultado <-
  tibble(
    nombre = as.character(),
    direccion = as.character(),
    href = as.character()
  )

# ciclo -------------------------------------------------------------------


for (i in seq_len(length(botones))) {
  nombre <- botones[[i]]$findChildElements("class","ng-binding")[[1]]$getElementText()[[1]]
  cat(nombre,"\n")
  direccion <- botones[[i]]$findChildElements("class","ng-binding")[[2]]$getElementText()[[1]]
  cat(direccion, "\n")
  # href <- botones[[i]]$findChildElement("xpath","//a[contains(@href,'maps')]")$getElementAttribute("href")[[1]]

  href <-
    botones[[i]]$findChildElement("tag","a")$getElementAttribute("href")[[1]]

  cat(href, "\n")
  resultado <- add_row(resultado, nombre = nombre, direccion = direccion, href = href)
  # resultado[i,] <- c(nombre, direccion, href)
}

# PENDIENTE
# click elemento
# - horario de atencion
# - foto de direccion -> ¿rescatar coordenada del mapa?
# - servicios


write_csv(resultado, 'data/fonasa-scrap.csv')
# write_csv(resultado, 'data/isp-resultado-webscraping.csv')
# cerrar selenium
remDr$close()
