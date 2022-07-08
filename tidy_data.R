##### TIDY DOCUMENTOS #####

## creamos un objeto con todos los documentos 
#(f <- file.path("/cloud/project/degrab/Capital", ## carpeta con los archivos
#               c("CAPITAL GRUPO 4.docx", ## seleccion de archivos
#                 "CAPITAL GRUPO 5.docx",
#                 "CAPITAL GRUPO 6.docx",
#                 "CAPITAL GRUPO 7.docx",
#                 "CAPITAL GRUPO 8.docx",
#                 "CAPITAL GRUPO 9.docx",
#                 "G1 - JxC - Jóvenes- C3D1 21.03.docx",
#                 "G2 - JxC - Adultos - C3D1 21.03.docx",
#                 "G3 - Mix.docx", 
#                 )))


(f <- file.path("/cloud/project/degrab/", ## carpeta con los archivos
                c("Capital/CAPITAL GRUPO 4.docx", ## seleccion de archivos
                  "Capital/CAPITAL GRUPO 5.docx",
                  "Capital/CAPITAL GRUPO 6.docx",
                  "Capital/CAPITAL GRUPO 7.docx",
                  "Capital/CAPITAL GRUPO 8.docx",
                  "Capital/CAPITAL GRUPO 9.docx",
                  "Capital/G1 - JxC - Jóvenes- C3D1 21.03.docx",
                  "Capital/G2 - JxC - Adultos - C3D1 21.03.docx",
                  "Capital/G3 - Mix.docx", 
                  "Rio cuarto/G6 - JxC - 35 a 50 años - alto_medio.docx",
                  "Rio cuarto/RIO CUARTO GRUPO 1.docx",
                  "Rio cuarto/RIO CUARTO GRUPO 3.docx",
                  "Rio cuarto/RIO CUARTO GRUPO 4.docx",
                  "Rio cuarto/RIO CUARTO GRUPO 5.docx",
                  "Rio cuarto/RIO CUARTO GRUPO 7.docx",
                  "Rio cuarto/RIO CUARTO GRUPO 8.docx",
                  "Rio cuarto/RIO CUARTO GRUPO 9.docx",
                  "San francisco/SAN FRANCISCO GRUPO 1.docx",
                  "San francisco/SAN FRANCISCO GRUPO 2.docx",
                  "San francisco/SAN FRANCISCO GRUPO 3.docx",
                  "San francisco/SAN FRANCISCO GRUPO 4.docx",
                  "San francisco/SAN FRANCISCO GRUPO 5.docx",
                  "San francisco/SAN FRANCISCO GRUPO 6.docx",
                  "San francisco/SAN FRANCISCO GRUPO 7.docx",
                  "San francisco/SAN FRANCISCO GRUPO 8.docx",
                  "San francisco/SAN FRANCISCO GRUPO 9.docx"
                )))


  
## aplicamos la función para leer los documentos a todos los archivos

d <- lapply(f, read_document)

## vemos cómo se compone
str(d)

## Contruimos los df

d[[1]][2] ## tomamos la línea 2 del objeto 1 de la lista d

text_1 <- tibble(text = d[[1]]) %>% ## seleccion del primer documento
  cbind(n_focus = "focus1", ## asignacion categorica 
        ciudad = "Cordoba", ## asignacion de la ciudad
        NSE = "medio - alto", ## asignacion Nivel socio econ?mico
        Voto = "HxC", ## voto 2019
        Edad = "20 - 30 años") %>%  ## rango etareo
        mutate(linea = row_number()) 

text_2 <- tibble(text = d[[2]]) %>% ## seleccion del primer documento
  cbind(n_focus = "focus2", ## asignacion categorica 
        ciudad = "Cordoba", ## asignacion de la ciudad
        NSE = "medio - alto", ## asignacion Nivel socio econ?mico
        Voto = "JxC", ## voto 2019
        Edad = "20 - 30 años") %>%  ## rango etareo
  mutate(linea = row_number())

text_3 <- tibble(text = d[[3]]) %>% ## seleccion del primer documento
  cbind(n_focus = "focus3", ## asignacion categorica 
        ciudad = "Cordoba", ## asignacion de la ciudad
        NSE = "medio - bajo", ## asignacion Nivel socio econ?mico
        Voto = "HxC", ## voto 2019
        Edad = "20 - 30 años") %>%  ## rango etareo
  mutate(linea = row_number())

d[[4]][2]
text_4 <- tibble(text = d[[4]]) %>% ## seleccion del primer documento
  cbind(n_focus = "focus4", ## asignacion categorica 
        ciudad = "Cordoba", ## asignacion de la ciudad
        NSE = "medio - bajo", ## asignacion Nivel socio econ?mico
        Voto = "HxC", ## voto 2019
        Edad = "35 - 50 años") %>%  ## rango etareo
  mutate(linea = row_number())

d[[5]][2]
text_5 <- tibble(text = d[[5]]) %>% ## seleccion del primer documento
  cbind(n_focus = "focus5", ## asignacion categorica 
        ciudad = "Cordoba", ## asignacion de la ciudad
        NSE = "medio - alto", ## asignacion Nivel socio econ?mico
        Voto = "JxC", ## voto 2019
        Edad = "20 - 30 años") %>%  ## rango etareo
  mutate(linea = row_number())

d[[6]][2]
text_6 <- tibble(text = d[[6]]) %>% ## seleccion del primer documento
  cbind(n_focus = "focus6", ## asignacion categorica 
        ciudad = "Cordoba", ## asignacion de la ciudad
        NSE = "medio - alto", ## asignacion Nivel socio econ?mico
        Voto = "JxC", ## voto 2019
        Edad = "35 - 50 años") %>%  ## rango etareo
  mutate(linea = row_number())

d[[7]][2]
text_7 <- tibble(text = d[[7]]) %>% ## seleccion del primer documento
  cbind(n_focus = "focus7", ## asignacion categorica 
        ciudad = "Cordoba", ## asignacion de la ciudad
        NSE = "medio - bajo", ## asignacion Nivel socio econ?mico
        Voto = "JxC", ## voto 2019
        Edad = "20 - 30 años") %>%  ## rango etareo
  mutate(linea = row_number())

d[[8]][2]
text_8 <- tibble(text = d[[8]]) %>% ## seleccion del primer documento
  cbind(n_focus = "focus8", ## asignacion categorica 
        ciudad = "Cordoba", ## asignacion de la ciudad
        NSE = "medio - bajo", ## asignacion Nivel socio econ?mico
        Voto = "JxC", ## voto 2019
        Edad = "35 - 50 años") %>%  ## rango etareo
  mutate(linea = row_number())

d[[9]][2]
text_9 <- tibble(text = d[[9]]) %>% ## seleccion del primer documento
  cbind(n_focus = "focus9", ## asignacion categorica 
        ciudad = "Cordoba", ## asignacion de la ciudad
        NSE = "mixto", ## asignacion Nivel socio econ?mico
        Voto = "mixto", ## voto 2019
        Edad = "mixto") %>%  ## rango etareo
  mutate(linea = row_number())

d[[10]][2]
text_10 <- tibble(text = d[[10]]) %>% ## seleccion del primer documento
  cbind(n_focus = "focus10", ## asignacion categorica 
        ciudad = "Rio Cuarto", ## asignacion de la ciudad
        NSE = "medio - alto", ## asignacion Nivel socio econ?mico
        Voto = "JxC", ## voto 2019
        Edad = "35 - 50 años") %>%  ## rango etareo
  mutate(linea = row_number())

d[[11]][2]
text_11 <- tibble(text = d[[11]]) %>% ## seleccion del primer documento
  cbind(n_focus = "focus11", ## asignacion categorica 
        ciudad = "Rio Cuarto", ## asignacion de la ciudad
        NSE = "medio - alto", ## asignacion Nivel socio econ?mico
        Voto = "HxC", ## voto 2019
        Edad = "20 - 30 años") %>%  ## rango etareo
  mutate(linea = row_number())

d[[12]][2]
text_12 <- tibble(text = d[[12]]) %>% ## seleccion del primer documento
  cbind(n_focus = "focus12", ## asignacion categorica 
        ciudad = "Rio Cuarto", ## asignacion de la ciudad
        NSE = "medio - bajo", ## asignacion Nivel socio econ?mico
        Voto = "HxC", ## voto 2019
        Edad = "20 - 30 años") %>%  ## rango etareo
  mutate(linea = row_number())

d[[13]][2]
text_13 <- tibble(text = d[[13]]) %>% ## seleccion del primer documento
  cbind(n_focus = "focus13", ## asignacion categorica 
        ciudad = "Rio Cuarto", ## asignacion de la ciudad
        NSE = "medio - bajo", ## asignacion Nivel socio econ?mico
        Voto = "HxC", ## voto 2019
        Edad = "35 - 50 años") %>%  ## rango etareo
  mutate(linea = row_number())

d[[14]][2]
text_14 <- tibble(text = d[[14]]) %>% ## seleccion del primer documento
  cbind(n_focus = "focus14", ## asignacion categorica 
        ciudad = "Rio Cuarto", ## asignacion de la ciudad
        NSE = "medio - alto", ## asignacion Nivel socio econ?mico
        Voto = "JxC", ## voto 2019
        Edad = "20 - 30 años") %>%  ## rango etareo
  mutate(linea = row_number())

d[[15]][2]
text_15 <- tibble(text = d[[15]]) %>% ## seleccion del primer documento
  cbind(n_focus = "focus15", ## asignacion categorica 
        ciudad = "Rio Cuarto", ## asignacion de la ciudad
        NSE = "medio - bajo", ## asignacion Nivel socio econ?mico
        Voto = "JxC", ## voto 2019
        Edad = "20 - 30 años") %>%  ## rango etareo
  mutate(linea = row_number())

d[[16]][2]
text_16 <- tibble(text = d[[16]]) %>% ## seleccion del primer documento
  cbind(n_focus = "focus16", ## asignacion categorica 
        ciudad = "Rio Cuarto", ## asignacion de la ciudad
        NSE = "medio - bajo", ## asignacion Nivel socio econ?mico
        Voto = "JxC", ## voto 2019
        Edad = "35 - 50 años") %>%  ## rango etareo
  mutate(linea = row_number())

d[[17]][2]
text_17 <- tibble(text = d[[17]]) %>% ## seleccion del primer documento
  cbind(n_focus = "focus17", ## asignacion categorica 
        ciudad = "Rio Cuarto", ## asignacion de la ciudad
        NSE = "mixto", ## asignacion Nivel socio econ?mico
        Voto = "mixto", ## voto 2019
        Edad = "mixto") %>%  ## rango etareo
  mutate(linea = row_number())

d[[18]][2]
text_18 <- tibble(text = d[[18]]) %>% ## seleccion del primer documento
  cbind(n_focus = "focus18", ## asignacion categorica 
        ciudad = "San Francisco", ## asignacion de la ciudad
        NSE = "medio - alto", ## asignacion Nivel socio econ?mico
        Voto = "HxC", ## voto 2019
        Edad = "20 - 30 años") %>%  ## rango etareo
  mutate(linea = row_number())

d[[19]][2]
text_19 <- tibble(text = d[[19]]) %>% ## seleccion del primer documento
  cbind(n_focus = "focus19", ## asignacion categorica 
        ciudad = "San Francisco", ## asignacion de la ciudad
        NSE = "medio - alto", ## asignacion Nivel socio econ?mico
        Voto = "HxC", ## voto 2019
        Edad = "35 - 50 años") %>%  ## rango etareo
  mutate(linea = row_number())

d[[20]][2]
text_20 <- tibble(text = d[[20]]) %>% ## seleccion del primer documento
  cbind(n_focus = "focus20", ## asignacion categorica 
        ciudad = "San Francisco", ## asignacion de la ciudad
        NSE = "medio - bajo", ## asignacion Nivel socio econ?mico
        Voto = "HxC", ## voto 2019
        Edad = "20 - 30 años") %>%  ## rango etareo
  mutate(linea = row_number())

d[[21]][2]
text_21 <- tibble(text = d[[21]]) %>% ## seleccion del primer documento
  cbind(n_focus = "focus21", ## asignacion categorica 
        ciudad = "San Francisco", ## asignacion de la ciudad
        NSE = "medio - bajo", ## asignacion Nivel socio econ?mico
        Voto = "HxC", ## voto 2019
        Edad = "35 - 50 años") %>%  ## rango etareo
  mutate(linea = row_number())

d[[22]][2]
text_22 <- tibble(text = d[[22]]) %>% ## seleccion del primer documento
  cbind(n_focus = "focus22", ## asignacion categorica 
        ciudad = "San Francisco", ## asignacion de la ciudad
        NSE = "medio - alto", ## asignacion Nivel socio econ?mico
        Voto = "JxC", ## voto 2019
        Edad = "20 - 30 años") %>%  ## rango etareo
  mutate(linea = row_number())

d[[23]][2]
text_23 <- tibble(text = d[[23]]) %>% ## seleccion del primer documento
  cbind(n_focus = "focus23", ## asignacion categorica 
        ciudad = "San Francisco", ## asignacion de la ciudad
        NSE = "medio - alto", ## asignacion Nivel socio econ?mico
        Voto = "JxC", ## voto 2019
        Edad = "35 - 50 años") %>%  ## rango etareo
  mutate(linea = row_number())

d[[24]][2]
text_24 <- tibble(text = d[[24]]) %>% ## seleccion del primer documento
  cbind(n_focus = "focus24", ## asignacion categorica 
        ciudad = "San Francisco", ## asignacion de la ciudad
        NSE = "medio - bajo", ## asignacion Nivel socio econ?mico
        Voto = "JxC", ## voto 2019
        Edad = "20 - 30 años") %>%  ## rango etareo
  mutate(linea = row_number())

d[[25]][2]
text_25 <- tibble(text = d[[25]]) %>% ## seleccion del primer documento
  cbind(n_focus = "focus25", ## asignacion categorica 
        ciudad = "San Francisco", ## asignacion de la ciudad
        NSE = "medio - bajo", ## asignacion Nivel socio econ?mico
        Voto = "JxC", ## voto 2019
        Edad = "35 - 50 años") %>%  ## rango etareo
  mutate(linea = row_number())

d[[26]][2]
text_26 <- tibble(text = d[[26]]) %>% ## seleccion del primer documento
  cbind(n_focus = "focus26", ## asignacion categorica 
        ciudad = "San Francisco", ## asignacion de la ciudad
        NSE = "mixto", ## asignacion Nivel socio econ?mico
        Voto = "mixto", ## voto 2019
        Edad = "mixto") %>%  ## rango etareo
  mutate(linea = row_number())

####### data frame y stop words #######

Names <- ls(pattern = "^text_[1-99]")
L <- mget(Names)
df_text <- do.call("rbind", L)

head(df_text)


df_sin_var_control <- data.frame(matrix(unlist(d)))

focus_tokenizado <- df_text %>% unnest_tokens(word, text) # tokenización

stop <- stopwords("es") %>%  # palabras comunes para eliminar del análisis
  as.tibble() ## importante para que sea el mismo tipo de objeto que focus_tokenizado

## la función stopwords() viene en el paquete tm y devuelve un data frame con 
## un diccionario de stop words

## el único argumento es el idioma, en este caso español "es"


## otra opción para descargar los datos
stop2 <- read.csv("https://bitsandbricks.github.io/data/stopwords_es.csv",
                         stringsAsFactors = FALSE)

## le cambiamos el nombre a la columna para hacer el anti_join
colnames(stop) <- "word"

# Sacamos las stop words del DF
focus_tokenizado <- focus_tokenizado %>% 
                    anti_join(stop) 

# Mostramos las principales palabras #
  
focus_tokenizado %>% 
  count(word, sort = TRUE) %>% 
  head(10)


# Vemos que aparecen muchas veces las iniciales de los nombres y otra palabras no clave
# creamos otro objeto para hacer anti_join

stop2 <- c("p", "si", "n", "osea", "como", "digamos", "masc",
           "tambien", "okey") %>% 
  as.tibble()
colnames(stop2) <- "word"

focus_tokenizado <- anti_join(focus_tokenizado, stop2)

focus_tokenizado %>% 
  count(word, sort = TRUE) %>% 
  head()


focus_tokenizado$word <- focus_tokenizado$word %>% 
  str_replace_all(c("á"="a", "é"="e","í"="i","ó"="o", "ú"="u")) 

focus_tokenizado <- focus_tokenizado %>% 
  filter(str_length(word) > 3)



