library(stringr)
library(stringi)
library(dplyr)

limpiar_texto <- function(x) {
  x <-
    x %>%
    stri_trans_general("Latin-ASCII") %>%
    str_to_lower() %>%
    str_trim()
  return(x)
}

vector <- c("Chillán, Provincia de Diguillín, Región de Ñuble, Chile",
                "Bulnes, Provincia de Diguillín, Región de Ñuble, 3930000, Chile",
                "Chillán Viejo, Provincia de Diguillín, Región de Ñuble, 3820000, Chile",
                "El Carmen, Provincia de Diguillín, Región de Ñuble, Chile",
                "Pemuco, Provincia de Diguillín, Región de Ñuble, Chile",
                "Pinto, Provincia de Diguillín, Región de Ñuble, 3880000, Chile",
                "Quillón, Provincia de Diguillín, Región de Ñuble, Chile",
                "San Ignacio, San Carlos, Provincia de Punilla, Región de Ñuble, Chile",
                "Yungay, Provincia de Diguillín, Región de Ñuble, Chile",
                "Quirihue, Provincia de Itata, Región de Ñuble, Chile",
                "Cobquecura, Provincia de Itata, Región de Ñuble, Chile",
                "Coelemu, Provincia de Itata, Región de Ñuble, 3970000, Chile",
                "Ninhue, Provincia de Itata, Región de Ñuble, Chile",
                "Portezuelo, Provincia de Itata, Región de Ñuble, Chile",
                "Ránquil, Provincia de Itata, Región de Ñuble, Chile",
                "Treguaco, Provincia de Itata, Región")

vector2 <- str_extract(vector,"^\\w+( \\w+)?(?=,)") %>% limpiar_texto()

for (i in seq_len(length(vector))) {
  print(vector[i])
  # print(vector2[i])

  txt_limpio <- vector[i] %>% str_extract("^\\w+( \\w+)?(?=,)") %>% limpiar_texto() %>% print()
  nombre_archivo <- paste0('data/shp/',txt_limpio,'.shp')
}
