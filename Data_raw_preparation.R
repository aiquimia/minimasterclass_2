
# 1. Importamos datos
# 1.1. Preparamos entorno y librerías
library(pdftools)
library(stringr)
library(tidyverse)

# 1.2. Creamos un variable temporal
temp_file<-tempfile()

# 1.3. Extraemos los datos del pdf de una URL "Repositorio del INS"
temp_file = pdf_text("https://repositorio.ins.gob.pe/xmlui/bitstream/handle/INS/1034/tablas-peruanas-QR.pdf") 

# 2. Creamos el dataframe para una plana (páginas derecha e izquierda)

# 2.1. Filtrar Páginas Derecha

n_pag = 16

data_frame_derecha = function(temp_file, n_pag){
  
  pag_list = temp_file[n_pag]
  tab = pag_list[[1]] %>% str_split("\n")
  tab = tab[[1]]
  len = length(tab)
  n_table <- function(tab){
    len = length(tab)
    for(i in seq(len)){
      bool=tab[i] %>% str_detect("Grasa Carbohidratos")
      if(bool == TRUE){
        aux=i
      }
    }
    return(aux)
  }
  n_tab = n_table(tab)
  # 2.1.1. Filtrar nombres
  
  nombres_1 <- tab[n_tab] %>% 
    str_replace_all("Carbohidratos","CHO") %>%
    str_trim() %>%
    str_split("\\s* ",simplify = TRUE)
  
  nombres_2 <- tab[n_tab+1] %>% 
    str_replace_all("í","i") %>%
    str_replace("dietaria"," ") %>%
    str_trim() %>%
    str_split("\\s+",simplify = TRUE)
  
  nombres_3 <- tab[n_tab+2] %>% 
    str_trim() %>%
    str_split("\\s+",simplify = TRUE)
  
  nombres_4 <- tab[n_tab+4] %>%
    str_replace_all("<[A-Z]*>\\s*<[A-Z]*>\\s*<[A-Z]*>\r$","") %>%
    str_replace("CÓDIGO","ID") %>%
    str_replace("NOMBRE DEL ALIMENTO","Alimento") %>%
    str_trim() %>%
    str_split("\\s+",simplify = TRUE)
  
  nombres_5 <- tab[n_tab+5] %>% 
    str_trim() %>%
    str_split("\\s+",simplify = TRUE)
  
  mod_nombres <- str_c(nombres_1,nombres_3,sep = "_")
  mod_nombres <- c(nombres_2[-5],mod_nombres,nombres_2[5])
  mod_nombres <- str_c(mod_nombres,nombres_5,sep = "_")
  mod_nombres <- c(nombres_4, mod_nombres)
  # 2.1.2. Filtrar datos
  
  data_derecha <- tab[n_tab+11:len-5] %>%
    str_replace_all("([A-Z])\\s+(\\d*)","\\1_\\2") %>%
    str_replace_all("(\\d?),(\\d?)","\\1.\\2") %>%
    str_trim() %>%
    str_split("\\s{2,}", simplify = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    setNames(mod_nombres) %>%
    mutate_at(-1:-2, parse_number)
  return(data_derecha)
  
}
#-----------------------------------------------------------------------

pag_list = temp_file[n_pag]
tab = pag_list[[1]] %>% str_split("\n")
tab = tab[[1]]
len = length(tab)
n_table <- function(tab){
  len = length(tab)
  for(i in seq(len)){
    bool=tab[i] %>% str_detect("Grasa Carbohidratos")
    if(bool == TRUE){
      aux=i
    }
  }
  return(aux)
}
n_tab = n_table(tab)
# 2.1.1. Filtrar nombres

nombres_1 <- tab[n_tab] %>% 
  str_replace_all("Carbohidratos","CHO") %>%
  str_trim() %>%
  str_split("\\s* ",simplify = TRUE)

nombres_2 <- tab[n_tab+1] %>% 
  str_replace_all("í","i") %>%
  str_replace("dietaria"," ") %>%
  str_trim() %>%
  str_split("\\s+",simplify = TRUE)

nombres_3 <- tab[n_tab+2] %>% 
  str_trim() %>%
  str_split("\\s+",simplify = TRUE)

nombres_4 <- tab[n_tab+4] %>%
  str_replace_all("<[A-Z]*>\\s*<[A-Z]*>\\s*<[A-Z]*>\r$","") %>%
  str_replace("CÓDIGO","ID") %>%
  str_replace("NOMBRE DEL ALIMENTO","Alimento") %>%
  str_trim() %>%
  str_split("\\s+",simplify = TRUE)

nombres_5 <- tab[n_tab+5] %>% 
  str_trim() %>%
  str_split("\\s+",simplify = TRUE)

mod_nombres <- str_c(nombres_1,nombres_3,sep = "_")
mod_nombres <- c(nombres_2[-5],mod_nombres,nombres_2[5])
mod_nombres <- str_c(mod_nombres,nombres_5,sep = "_")
mod_nombres <- c(nombres_4, mod_nombres)
# 2.1.2. Filtrar datos

data_derecha <- tab[n_tab+6:len-3] %>%
    str_replace_all("([A-Z])\\s+(\\d*)","\\1_\\2") %>%
    str_replace_all("(\\d?),(\\d?)","\\1.\\2") %>%
    str_trim() %>%
    str_split("\\s{2,}", simplify = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    setNames(mod_nombres) %>%
    mutate_at(-1:-2, parse_number)

data_derecha

# 2.2. Filtrar Páginas Izquierda

n_pag = 15

data_frame_izquierda = function(temp_file, n_pag){
  pag_list = temp_file[n_pag]
  tab = pag_list[[1]] %>% str_split("\n")
  tab = tab[[1]]
  len = length(tab)
  n_table_izq <- function(tab){
    len = length(tab)
    for(i in seq(len)){
      bool=tab[i] %>% str_detect("Vitamina A")
      if(bool == TRUE){
        aux=i
      }
    }
    return(aux)
  }
  n_tab = n_table_izq(tab)
  n_tab
  
  # Nombres
  nombres_1 <- tab[n_tab] %>% 
    str_replace_all(" caroteno","_caroteno") %>% 
    str_replace_all(" A","_A") %>% 
    str_trim() %>%
    str_split("\\s* ",simplify = TRUE)
  
  nombres_2 <- tab[n_tab+1] %>%
    str_replace_all("C\\s+","") %>%
    str_replace_all("Vitamina","Vitamina_C") %>%
    str_replace_all("Ácido","Acido_Folico") %>%
    str_trim() %>%
    str_split("\\s+",simplify = TRUE)
  
  nombres_3 <- tab[n_tab+5] %>% 
    str_trim() %>%
    str_split("\\s+",simplify = TRUE)
  
  mod_nombres <- str_c(nombres_1, nombres_2[6:7], sep="_")
  mod_nombres <- c(nombres_2[1:5],mod_nombres,nombres_2[8:14])
  mod_nombres <- str_c(mod_nombres,nombres_3,sep = "_")
  mod_nombres <- c(mod_nombres, "ID")
  
  # Data izquierda
  
  data_izquierda <- tab[n_tab+12:len-6] %>%
    str_replace_all("([A-Z])\\s+(\\d*)","\\1_\\2") %>%
    str_replace_all("(\\d?),(\\d?)","\\1.\\2") %>%
    str_trim() %>%
    str_split("\\s+", simplify = TRUE) %>%
    data.frame(stringsAsFactors = FALSE) %>%
    mutate_at(-15, parse_number) %>%
    setNames(mod_nombres) 
  
  return(data_izquierda)
}






#line[[1]][4] %>% str_trim()
  # Lo descargamos en un documento temporal
  #download.file(url, temp_file)
  
  # Convertimos de pdf a txt
  #txt <- pdf_text(temp_file)
