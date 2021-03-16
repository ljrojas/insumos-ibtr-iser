
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
url_regcivil <- "https://www.registrocivil.cl/principal/paginas-frecuentes/nuestras-oficinas"

# util --------------------------------------------------------------------

limpiar_texto <- function(x) {
  x <-
    x %>%
    stri_trans_general("Latin-ASCII") %>%
    str_to_lower() %>%
    str_trim()
  return(x)
}

# no es necesario en esta pagina
# rescatar_elementos <- function() {
#   buscar <- remDr$findElement('id','buscar')
#
#   # 2. selector region y comuna
#   selector <- remDr$findElements("tag", "select")
#   selector %>% length()
#   selector[[2]]$getElementText()
#
#   # asignar selectores region y comuna
#   for (j in seq_len(length(selector))) {
#     texto <-
#       selector[[j]]$getElementText() %>%
#       stri_trans_general("Latin-ASCII") %>%
#       str_to_lower()
#     # print(texto)
#     if (str_detect(texto,'region')) {
#       sel_region <- selector[[j]]
#     }
#     if (str_detect(texto,'comuna')) {
#       sel_comuna <- selector[[j]]
#     }
#   }
#   return(list(sel_region, sel_comuna, buscar))
# }

# setup selenium ----------------------------------------------------------

# debiese abrir un navegador chrome
# rD <- rsDriver()
rD <- rsDriver(browser = "firefox", port = 4445L)

remDr <- rD[["client"]]

remDr$setTimeout(type = "page load", milliseconds = 5000)

# visistar url busqueda
remDr$navigate(url_regcivil)

# primera busqueda --------------------------------------------------------

# todo el contenido esta dentro iframe
iframes <- remDr$findElements("tag","iframe")
remDr$switchToFrame(iframes[[1]])

# rescatar selectores

select <- remDr$findElements("tag","select")
sel_reg <- select[[1]]
sel_of <- select[[2]]
buscar <- remDr$findElement("id","BUSCAR")

# opciones de regiones

opt_reg <- sel_reg$findChildElements("tag","option")

# acumulador --------------------------------------------------------------

resultado <-
  tibble(
    region = as.character(),
    oficina = as.character(),
    direccion  = as.character(),
    horario = as.character(),
    href = as.character(),
  )

for (i in seq_len(length(opt_reg))) {
  cat("region",i,"\n")
  # rescata text region
  reg <- opt_reg[[i]]
  reg_txt <- reg$getElementText()[[1]] %>% limpiar_texto()
  # saltarse "seleccionar"
  if (str_detect(reg_txt,"seleccione")) next
  # click
  reg$clickElement()
  # rescatar oficinas
  opt_of <- sel_of$findChildElements("tag","option")
  ofs_txt <- unlist(lapply(opt_of, function(x) x$getElementText()))
  for (j in seq_len(length(opt_of))){
    cat("oficina",j,"\n")
    of <- opt_of[[j]]
    of_txt <- of$getElementText() %>% limpiar_texto()
    if (str_detect(of_txt,"seleccione")) next
    of$clickElement()
    buscar$clickElement()
    Sys.sleep(1)
    # rescata info
    cont <- remDr$findElement("class","ContenedorCentro")
    # test
    text_att <- cont$findChildElements("class","gx-attribute")
    max_min <- 1 # posicion maxima del iterador donde la informacion es minima
    for (z in seq_len(length(text_att))) {
      if (length(text_att[[z]]$findChildElements("tag","span"))==0) {
        max_min <- z # actualiza posicion
        next
        }
      # cat(
      #   z, "gx-label:",
      #   text_att[[z]]$findChildElement("class","gx-label")$getElementText()[[1]],
      #   "span: ",
      #   text_att[[z]]$findChildElement("tag","span")$getElementText()[[1]],
      #   "\n"
      # )
      info <- text_att[[z]]$findChildElement("tag","span")$getElementText()[[1]] %>% limpiar_texto()
      # horario
      if (str_detect(info,'lunes|martes|miercoles|jueves|viernes|sabado|domingo')) {
        horario <- info
        next
      }
      # oficial
      if (str_detect(info, 'oficial civil')) {
        oficial <- info
        next
      }
      # direccion | primera info visible
      if (z == max_min+1) {
        direccion <- info
        next
      }
    }

    text_block <- remDr$findElements("class","TextBlock")
    text_block[[2]]$highlightElement()

    ver <- text_block[[2]]$findChildElement("tag","a")
    ver$clickElement()
    Sys.sleep(1.5)

    remDr$findElements("tag","iframe") %>% length()

    frame <- remDr$findElement("tag","iframe")
    href <- frame$getElementAttribute("src")[[1]]
    Sys.sleep(1)
    cerrar <- remDr$findElement("name","CERRAR")
    cerrar$clickElement()

    # llenar info

    resultado <-
      add_row(resultado,
              region = reg_txt,
              oficina = of_txt,
              direccion = direccion,
              horario = horario,
              href = href
            )

  } # fin ciclo oficina

} # fin ciclo region
remDr$close()
write_csv(resultado,'data/registro-civil-scrap.csv')
