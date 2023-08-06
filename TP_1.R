#Instrumentos de Análisis Urbanos II - TP1 

#Para este trabajo analizaremos la cantidad de puntos verdes por km2 que posee cada barrio de la Ciudad Autónoma de Buenos Aires. 
#Se tomaron datos públicos generados, guardados y publicados por organismos de gobierno de la Ciudad Autónomas de Buenos Aires descargados desde https://data.buenosaires.gob.ar/dataset/

#Instalamos las librerias necesarias para llevar adelante el Trabajo

library(tidyverse)
library(dplyr)

#Cargamos las bases de datos con la que vamos a trabajar, para poder responder la cantidad de puntos verdes por km2 que posee cada barrio. En este caso utilizaremos:
# 1- Base de Puntos verdes existentes en la CABA, nos brinda información y ubicación geográfica sobre los centros de recepción de materiales reciclables.
# 2- Base de Barrios de la CABA, que posee los límites y ubicación geográfica de los barrios de la Ciudad.

puntos_verdes <- read.csv(file = "https://cdn.buenosaires.gob.ar/datosabiertos/datasets/agencia-de-proteccion-ambiental/puntos-verdes/puntos-verdes.csv") 
                          
barrios <- read.csv(file = "https://cdn.buenosaires.gob.ar/datosabiertos/datasets/ministerio-de-educacion/barrios/barrios.csv", sep = ";")

#Haremos un summary para conocer cómo se estructuran ambas bases de datos.

summary(barrios)

summary(puntos_verdes)

#Ahora que sabemos la información que tienen ambos dataset, procederemos a limpiar y transformar los datos para obtener la información que necesitamos para resolver nuestra pregunta.

barrios_limp <- barrios %>%
  select(BARRIO, AREA, COMUNA) %>%
  rename(AREA_M2 = AREA) %>% 
  mutate(AREA_KM2 = AREA_M2 / 1000000) %>% 
  select(BARRIO, COMUNA, AREA_KM2) %>% 
  rename_all(tolower) %>% 
  mutate(barrio = ifelse(barrio == "BOCA", "LA BOCA", barrio)) %>% 
  mutate(barrio = ifelse(barrio == "VILLA GRAL. MITRE", "VILLA GENERAL MITRE", barrio))

puntos_verdes_limp <- puntos_verdes %>%
  select(nombre, direccion, barrio, comuna) %>% 
  mutate(barrio = ifelse(barrio == "VILLA OTUZAR", "VILLA ORTUZAR",
                        ifelse(barrio == "LUGANO", "VILLA LUGANO",
                        ifelse(barrio == "POMPEYA", "NUEVA POMPEYA", barrio))))

unificado_final <- puntos_verdes_limp %>% 
  group_by(barrio) %>%
  summarise(cant_pv=n()) %>% 
  right_join(barrios_limp, by= "barrio") %>% 
  replace_na(list(cant_pv = 0)) %>% 
  mutate(pv_x_km2= round(cant_pv/area_km2, 2)) %>% 
  mutate(terciles = ntile(pv_x_km2, 3))
 
unificado_final <- unificado_final %>% 
  arrange(pv_x_km2) %>% 
  print()

#Podemos observar que hay 8 de los 48 barrios de la ciudad que no poseen puntos verdes y que los barrios ubicados en el sur de la ciudad (comuna 4 y 8) están dentro del 30% de los barrios que menos cantidad de puntos verdes por Km2 poseen a excepción de Parque Patricios que se encuentra dentro del 30% de los barrios con mayor cantidad de puntos verdes por Km2.
#Versalles y Monte Castro son los únicos 2 barrios que poseen más de 1 punto verde por Km2.

#*(Aclaración: La primera vez que se corrieron las líneas de código, se detectaron problemáticas a la hora de correr la función “right_join”, por incongruencias en los nombres de 5 barrios porteños, las cuales fueron subsanadas para correr nuevamente los códigos).
