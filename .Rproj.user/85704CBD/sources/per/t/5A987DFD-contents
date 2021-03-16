# https://www.sii.cl/ayudas/asistencia/oficinas/3048-3049.html
# necesario: listado de direcciones

# conf --------------------------------------------------------------------

# library(rvest)
library(RSelenium) # requiere chrome y java
library(stringr)
library(stringi)
library(readr)
library(purrr)
library(readr)
library(dplyr)

# url de busqueda zona censal y localidad rural
url_data <- "https://www.sii.cl/ayudas/asistencia/oficinas/3048-3049.html"

# util --------------------------------------------------------------------

limpiar_texto <- function(x) {
  x <-
    x %>%
    stri_trans_general("Latin-ASCII") %>%
    str_to_lower() %>%
    str_trim()
  return(x)
}

rescatar_botones <- function(remDr) {
  btns <- webElement
  sel_reg <- webElement
  sel_com <- webElement
  btns <- remDr$findElements("tag", "button")
  # for (i in seq_len(length(btns))) btns[[i]]$highlightElement()
  sel_reg <- btns[[1]]
  sel_com <- btns[[2]]
  return(list(sel_reg = sel_reg, sel_com = sel_com))
}

# recibe botones de f(x) rescatar botones
rescatar_regiones <- function(remDr, btns) {
  # inicializa elementos
  opt_reg <- webElement
  reg <- webElement
  # botones recibidos de f(x) rescatar_botones()
  btns$sel_reg$clickElement()
  # rescata opciones
  opt_reg <- remDr$findElements("xpath","//*[contains(@class,'dropdown-menu open')]")[[1]]
  # opt_reg$highlightElement()

  # rescata opciones individuales de region
  reg <- opt_reg$findChildElements("tag","span")
  # rescata texto de opciones individuales
  txt_reg <- lapply(reg,function(x) {x$getElementText()}) %>% unlist() %>%  limpiar_texto()
  # recuperar elementos con texto : iterador region
  it_reg <- txt_reg %>% str_detect("region \\w+") %>% which()
  # click para cerrar selector
  btns$sel_reg$clickElement()
  # retorna
  # - opt_reg : opciones region
  # - reg : region
  # - txt_reg : texto region
  # - it_reg : iterador region (numeros donde hay texto relacionado a region)
  return(list(opt_reg = opt_reg, reg = reg, txt_reg = txt_reg, it_reg = it_reg))
}

rescatar_comunas <- function(remDr,btns) {
  # inicializa elementos
  opt_com <- webElement
  com <- webElement
  # primero rescata botones
  # btns <- rescatar_botones(remDr)
  # click comuna
  btns$sel_com$clickElement()
  # rescatar comunas
  opt_com <- remDr$findElements("xpath","//*[contains(@class,'dropdown-menu open')]")[[2]]
  opt_com$highlightElement()
  # rescatar elementos de comuna
  com <- opt_com$findChildElements("tag","span")
  txt_com <- lapply(com,function(x) {x$getElementText()}) %>% unlist() %>% limpiar_texto()
  it_com <- txt_com %>% str_detect("\\w") %>% which()
  # se omite click final porque este se realiza en el elemento comuna que se rescatara
  # btns$sel_com$clickElement()

  return(list(opt_com = opt_com, com = com, txt_com = txt_com, it_com = it_com))
}

# setup selenium ----------------------------------------------------------

# debiese abrir un navegador chrome
# rD <- rsDriver()
rD <- rsDriver(browser = "firefox", port = 4448L)
remDr <- rD[["client"]]

remDr$setTimeout(type = "page load", milliseconds = 5000)

# visistar url busqueda
remDr$navigate(url_data)
# acumulador --------------------------------------------------------------
resultado <-
  tibble(
    region = as.character(),
    comuna = as.character(),
    titulo = as.character(),
    comunas_atiende = as.character(),
    direccion  = as.character(),
    horario = as.character()
    # href = as.character(),
  )
grupo_comunas <- c()

# elementos de busqueda  --------------------------------------------------------
btns <- rescatar_botones(remDr)
elem_reg <- rescatar_regiones(remDr,btns)

Sys.sleep(2)
for (i in elem_reg$it_reg) {
  cat("Iterador region posicion:",i,"\n")
  # click opcion
  btns$sel_reg$clickElement()
  elem_reg$reg[[i]]$clickElement()
  # nombre region
  nom_reg <- elem_reg$txt_reg[[i]]
  cat(str_to_title(nom_reg),"\n")
  cat("---------------------------------\n")
  # rescatar comunas
  elem_com <- rescatar_comunas(remDr,btns)
  Sys.sleep(2)
  # last_com <- max(elem_com$it_com)
  # Iterador por comuna
  for (j in elem_com$it_com) {
    cat("Iterador comuna posicion:",j,"\n")
    nom_com <- elem_com$txt_com[[j]]
    if (str_detect(nom_com,"seleccione")) {
      cat("## Slot Seleccionar Comuna, salta a siguiente posicion iterador\n")
      next
    }

    if (nom_com %in% grupo_comunas) {
      cat("## Comuna",str_to_title(nom_com),"ya servida por oficina rescatada, salto siguiente comuna\n")
      next
    }
    cat("# # # # # # # # # # # #\n")
    cat("Comuna",str_to_title(nom_com),"\n")
    elem_com$com[[j]]$clickElement()

    # contenedor info comuna --------------------------------------------------
    infocom <-remDr$findElement("class","info-comunas")
    infocom$highlightElement()
    # titulo
    titulo <- remDr$findElement("class","bloque-title")$getElementText()[[1]] %>% limpiar_texto()
    # items
    items_info <- infocom$findChildElements("class","items-info-comunas")
    items_info_txt <- lapply(items_info, function(x) {x$getElementText()}) %>% unlist() %>% limpiar_texto()
    items_info_txt <- items_info_txt %>% str_replace_all('\n',' ')
    # direccion siempre es el segundo elemento
    direccion <- items_info_txt[[2]]
    horario_atencion <- str_subset(items_info_txt,'horario de atencion:')[[1]]
    # hay oficinas que sirven a mas de una comuna, lo cual se describe en Comunas: nombres
    # Para evitar visitar dos veces la misma oficina
    # - guardar Comuna: nombres
    comunas_atiende <- items_info_txt %>% str_subset('comunas:')
    # - crear listado de comunas a no visitar
    comunas_atiende_vect <-
      comunas_atiende %>%
      str_replace('comunas:','') %>%
      str_replace_all("\\.","") %>%
      str_replace_all(" y ",",") %>% # experimental
      str_split(",") %>%
      flatten() %>%
      str_trim()

    # - al iniciar el checkeo comunal se revisa si el nombre de la comuna esta en este listado
    # - añadir elementos al listado a medida que se recupere esta informacion (no actualizar)
    grupo_comunas <- c(grupo_comunas,comunas_atiende_vect)
    # llenar info
    resultado <-
      add_row(
        resultado,
        region = nom_reg,
        comuna = nom_com,
        titulo = titulo,
        comunas_atiende = comunas_atiende,
        direccion = direccion,
        horario = horario_atencion
      )

    # volvera buscar elementos
    btns <- rescatar_botones(remDr)
    # sys.sleep(2)
    elem_reg <- rescatar_regiones(remDr, btns)
    Sys.sleep(2)
    elem_com <- rescatar_comunas(remDr, btns)
    Sys.sleep(2)
    cat("# # # # # # # # # # # #\n")

  } # fin ciclo comuna
  # vaciar listado comunas
  grupo_comunas <- c()
  cat("---------------------------------\n")
} # fin ciclo region
readr::write_csv(resultado,"data/sii-direcciones-webscraping.csv")

remDr$closeServer()
remDr$close()
