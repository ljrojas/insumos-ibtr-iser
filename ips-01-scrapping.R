# Objetivo
# Levantar información dentro de sucursales ISP (instituto prevision social)

# Antecedentes

# Datos aparecen luego de hacer click en distintas secciones
# Niveles
# - region
# - apartado region (numero de paginas)
# - comuna localidad (boton mapa)

# - mapa dentro de iframe (al entrar al frame se pierden referencia a los elementos) -> reiniciar
# - link dentro de mapa: lat lon
# - direccion

# opcional
# - horario de atencion
# - modulo chile atiente express -> tramites que se pueden realizar

# ojo Punto vecino (¿incluir? tambien tiene lat lon)

# ref
# - pseudo elementos (::before) https://stackoverflow.com/questions/59689722/how-locate-the-pseudo-element-before-using-selenium-python

# conf --------------------------------------------------------------------

# library(rvest)
library(RSelenium) # requiere chrome y java
library(stringr)
library(stringi)
library(readr)
library(purrr)
library(readr)

# url de busqueda zona censal y localidad rural
url_isp <- "https://www.ips.gob.cl/servlet/internet/inicio/red-de-atencion/sucursales"

# util --------------------------------------------------------------------

limpiar_texto <- function(x) {
  x <-
    x %>%
    stri_trans_general("Latin-ASCII") %>%
    str_to_lower() %>%
    str_trim()
  return(x)
}

rescatar_elementos <- function(remDr) {
  # declarar elementos
  buscar <- webElement
  selector <- webElement
  sel_region <- webElement
  sel_comuna <- webElement

  # asignar elementos
  buscar <- remDr$findElement('id','buscar')

  # 2. selector region y comuna
  selector <- remDr$findElements("tag", "select")
  # selector %>% length()
  # selector[[2]]$getElementText()

  # asignar selectores region y comuna
  for (j in seq_len(length(selector))) {
    texto <-
      selector[[j]]$getElementText() %>%
      stringi::stri_trans_general("Latin-ASCII") %>%
      stringr::str_to_lower()
    # print(texto)
    if (str_detect(texto,'region')) {
      sel_region <- selector[[j]]
    }
    if (str_detect(texto,'comuna')) {
      sel_comuna <- selector[[j]]
    }
  }
  return(list(sel_region = sel_region, sel_comuna = sel_comuna,  buscar =buscar))
}


# setup selenium ----------------------------------------------------------

# debiese abrir un navegador chrome
# rD <- rsDriver()
rD <- rsDriver(browser = "firefox", port = 4445L)
remDr <- rD[["client"]]

# remDr$setTimeout(type = "page load", milliseconds = 5000)

# visistar url busqueda
remDr$navigate(url_isp)

# rescatar elementos  -----------------------------------------------------

elementos <- rescatar_elementos(remDr)

# elementos$sel_region$highlightElement()
# elementos$sel_comuna$highlightElement()
# elementos$buscar$highlightElement()

# acumulador --------------------------------------------------------------

resultado <-
  tibble(
    region = as.character(),
    comuna = as.character(),
    mod_comuna = as.character(),
    direccion = as.character(),
    iframe_info = as.character(),
    latlot = as.character(),
    horario = as.character(),
    chileatiende = as.character()
  )

comunas_completadas <- c()

# proceso -----------------------------------------------------------------

# click selector region
elementos$sel_region$clickElement()

# rescatar opciones
opt_region <- elementos$sel_region$findChildElements("tag","option")
# length(opt_region)

elementos$sel_region$clickElement()

# i<- 7
# iterar en cada region
for (i in seq_len(length(opt_region))) {
  cat('i:',i,'\n')

  # click selector region
  elementos$sel_region$clickElement()

  # rescata opciones
  text_opt_reg <-
    opt_region[[i]]$getElementText()[[1]] %>%
    limpiar_texto()

  # descartar texto "seleccione"
  if (str_detect(text_opt_reg,'seleccione')) {
    cat('Boton sin region\n')
    next
  }

  # click region
  cat('Click region:', text_opt_reg, '\n')
  opt_region[[i]]$clickElement()

  # rescatar comunas
  elementos$sel_comuna$clickElement()

  opt_comunas <- elementos$sel_comuna$findChildElements('tag','option')

  # j<-2

# ciclo comunas -----------------------------------------------------------------
  num_comunas <- length(opt_comunas)
  for (j in seq_len(length(opt_comunas))) {
    cat('j:',j,'\n')
    cat('num comunas:',num_comunas,'\n')

    # texto nombre comuna
    text_comuna <-
      opt_comunas[[j]]$getElementText()[[1]] %>%
      limpiar_texto()

    # saltarse texto instruccion
    if (str_detect(text_comuna,'seleccionar comuna')) next

    # nombre y modificador comuna
    mod_comuna <-
      str_extract(text_comuna,'(?<=- ).+')
    if (is.na(mod_comuna)) mod_comuna <- 'sin modificador'
    nom_comuna <- text_comuna
    if (str_detect(text_comuna,' - ')) nom_comuna <- str_extract(text_comuna, '.+(?= -)')

    # salterse comuna completada
    if (nom_comuna %in% comunas_completadas) next

    # programa avisa en que comuna va
    cat(text_comuna,'\n')

    # busqueda ----------------------------------------------------------------

    # click comuna
    opt_comunas[[j]]$clickElement()

    # click buscar
    cat('click buscar\n')
    elementos$buscar$clickElement()

    # panel donde esta la informacion
    panel <- remDr$findElements("xpath","//div[contains(@class,'panel-body')]")
    length(panel) # en general son 16 (regiones)

    # posicion de panel (iterador region considera "1: Seleccionar Region")
    p <- i - 1

    # al filtrar por selectores deberia el primero ser operativo
    panel[[p]]$highlightElement() # checkea panel activo

    # direccion e informacion general
    info_gral <- panel[[p]]$findChildElements("class","minirecuadro")
    length(info_gral)

    # mini ciclo rescatar info panel
    for (x in seq_len(length(info_gral))) {
      text_info_gral <- info_gral[[x]]$getElementText()[[1]] %>% str_trim()
      if (text_info_gral=="") next
      text_info <- limpiar_texto(text_info_gral)
      cat(text_info,"\n")
      # checkear:
      # - direccion
      if (str_detect(text_info,'direccion')) direccion <- text_info
      # - horario
      if (str_detect(text_info,'horario')) horario <- text_info
      # - chileatiende
      if (str_detect(text_info,'chileatiende')) chileatiende <- text_info
      # TODO: profundizar tramites chile atiende
    }

    # boton mapa
    btn_gm <- panel[[p]]$findChildElements("class","alternar-respuesta")
    length(btn_gm)

    b<-1
    # para cada candidato a boton de mapa
    for (b in seq_len(length(btn_gm))) {
      # btn_gm[[1]]$highlightElement()

      # rescatar texto
      text_btn <- btn_gm[[b]]$getElementText()[[1]] %>% str_trim()
      # saltar texto vacio
      if (text_btn=="") next
      # mostrar texto boton
      cat(text_btn,"\n")

      # buscar info preliminar iframe
      data_iframe <- btn_gm[[b]]$getElementAttribute("data-iframe")[[1]]
      length(data_iframe)

      # click boton iframe (abre mapa)
      btn_gm[[b]]$clickElement()
      Sys.sleep(1)

      # seleccionar iframe
      iframe <- panel[[p]]$findChildElements("tag","iframe")[[1]]

      # cargar mapa interior ----------------------------------------------------
      remDr$switchToFrame(iframe)
      Sys.sleep(2)

      # alternativa: text "Ampliar el Mapa"
      ll <- remDr$findElements("xpath","//*[contains(@href,'?ll=')]")
      # check error (ll vacio)
      if (length(ll)==0) {
        href <-'error google maps'
        break
      }
      # rescata info link que contiene latitud y longitud
      href <- ll[[1]]$getElementAttribute('href')[[1]]
      # terminar ciclo cuando checkea mapa
      if (text_btn == "Ver mapa") break

      cat('fin ciclo boton')
    } # fin ciclo boton

    # guardar info -----------------------------------------

    resultado <-
      add_row(
        resultado,
        region = text_opt_reg,
        comuna = nom_comuna,
        mod_comuna = mod_comuna,
        direccion = direccion,
        iframe_info = data_iframe,
        latlot = href,
        horario = horario,
        chileatiende = chileatiende
      )

    # actualizar comunas completas  --------------------
    comunas_completadas <- c(comunas_completadas, nom_comuna)
    # reiniciar pagina
    cat('recarga pagina\n')
    remDr$navigate(url_isp)
    # Sys.sleep(1)
    # buscar elementos
    elementos <- rescatar_elementos(remDr)


    opt_region <- elementos$sel_region$findChildElements("tag","option")

    # click region
    # cat('Click region:', text_opt_reg, '\n')
    opt_region[[i]]$clickElement()

    # texto region
    text_opt_reg <-
      opt_region[[i]]$getElementText()[[1]] %>%
      stri_trans_general("Latin-ASCII") %>%
      str_to_lower() %>%
      str_trim()

    if (j == num_comunas) {
      cat('fin ciclo comuna\n-----------\n')
      next
    }
    # rescatar comunas
    elementos$sel_comuna$clickElement()

    opt_comunas <- elementos$sel_comuna$findChildElements('tag','option')

    cat('fin ciclo comuna\n-----------\n')

  } # fin ciclo comuna

cat('fin ciclo region\n************\n')
}
# guardar resultado
# write.csv(resultado,'data/resultado-isp.csv')

write_csv(resultado, 'data/ips-resultado-webscraping.csv')
# cerrar selenium
remDr$close()
