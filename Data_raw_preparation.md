1. Importando librerías de trabajo
----------------------------------

    library(pdftools)
    library(stringr)
    library(tidyverse)

2. Importamos el pdf de una URL
-------------------------------

    temp_file<-tempfile()
    temp_file = pdf_text("https://repositorio.ins.gob.pe/xmlui/bitstream/handle/INS/1034/tablas-peruanas-QR.pdf") 

3. Creamos los dataframes para las páginas de derecha
-----------------------------------------------------

### 3.1. Creando la función para extracción de datos de la derecha

    # Creando función de apoyo
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
      
      n_inicio <- function(tab){
        len = length(tab)
        for(i in seq(len)){
          bool=tab[i] %>% str_detect("kcal")
          if(bool == TRUE){
            aux=i
          }
        }
        return(aux+1)
      }
      n_inicio = n_inicio(tab)
      n_final <- function(tab){
        len = length(tab)
        for(i in seq(len)){
          bool_1=tab[i] %>% str_detect("\\s?Imputado\\s?")
          if(bool_1 == TRUE){
            aux=i
            break
          } else{
            bool_2=tab[i] %>% str_detect("^\\d{2,}\r$")
            if(bool_2 == TRUE){
              aux=i
            }
          }
        }
        return(aux-1)
      }
      n_final = n_final(tab)
      data_derecha <- tab[n_inicio:n_final] %>%
        str_replace_all("([A-Z])\\s+(\\d*)","\\1_\\2 ") %>%
        str_replace_all("(\\d?),(\\d?)","\\1.\\2") %>%
        str_trim() %>%
        str_split("\\s{2,}", simplify = TRUE) %>%
        data.frame(stringsAsFactors = FALSE) %>%
        setNames(mod_nombres) %>%
        mutate_at(-1:-2, parse_number)
      return(data_derecha)
      
    }

### 3.2. Probando la función

    n_pag = 22
    data_frame_derecha(temp_file, n_pag) %>% nrow()

    ## [1] 14

4. Creamos los dataframes para las páginas de la izquierda
----------------------------------------------------------

### 4.1. Creando la función para extracción de datos de la derecha

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
      
    # Datos 
      n_inicio <- function(tab){
        len = length(tab)
        for(i in seq(len)){
          bool=tab[i] %>% str_detect("^\\s*g\\s*mg")
          if(bool == TRUE){
            aux=i
          }
        }
        return(aux+1)
      }
      n_inicio = n_inicio(tab)
      n_final <- function(tab){
        len = length(tab)
        for(i in seq(len)){
          bool=tab[i] %>% str_detect("\\d{2,}\r$")
          if(bool == TRUE){
            aux=i
          }
        }
        return(aux-1)
      }
      n_final = n_final(tab)
      data_izquierda <- tab[n_inicio:n_final] %>%
        str_replace_all("([A-Z])\\s+(\\d*)","\\1_\\2") %>%
        str_replace_all("(\\d?),(\\d?)","\\1.\\2") %>%
        str_trim() %>%
        str_split("\\s+", simplify = TRUE) %>%
        data.frame(stringsAsFactors = FALSE) %>%
        mutate_at(-15, parse_number) %>%
        setNames(mod_nombres) 
      
      return(data_izquierda)
    }

### 4.2. Probando la función

    n_pag = 23
    data_frame_izquierda(temp_file, n_pag) %>% nrow()

    ## [1] 14

5. Juntando los dataframe derecha e izquierda en una función
------------------------------------------------------------

    n_hojas=14

    data_completa_n <- function(n){
      for(i in seq(n)){
        idx=14 + 2*(i-1)
        df_completo = cbind(data_frame_derecha(temp_file, idx), data_frame_izquierda(temp_file, idx+1))
        if(i == 1){
          aux=df_completo
          } else {
            aux = rbind(aux,df_completo)
          }
      }
      return(aux)
      }

    # Cantidad de filas totales

    data_completa_n(n_hojas) %>% nrow()

    ## [1] 462

6. Exportar los datos en un CSV
-------------------------------

    datos_exportar <- data_completa_n(n_hojas)
    write.csv(datos_exportar,"INS_ALIMENTOS_datos_no_tratados.csv", row.names = FALSE)
