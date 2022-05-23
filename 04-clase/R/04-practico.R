
# Práctico 4: Importar, seleccionar y exportar datos ----------------------


# 0. Instalar paquetes ----------------------------------------------------

pacman::p_load(sjmisc, #explorar datos
               sjPlot,
               tidyverse, #colección de paquetes, del cuál utilizaremos `dplyr` y `haven`
               haven, #cargar y exportar bases de datos en formatos .sav y .dta
               readxl, #para cargar y exportar bases de datos en formato .xlsx y .xls
               writexl)#para cargar y exportar bases de datos en formato .xlsx y .xls


# 1. Importar datos -------------------------------------------------------

# En este práctico, se trabajó con CASEN 2020. 
# Antes ¿dónde están nuestros datos? Por lo general, nuestros datos los dejaremos en 
# la carpeta `input/data`. En el [siguiente enlace](https://drive.google.com/drive/folders/1Orgb3Qb9LcjTfjYMdIdy7SWd3xDMrTbG?usp=sharing) 
# podrán descargar el archivo .zip que contiene la base de CASEN. Si aún no sabes como descomprimir datos, por favor revisa [aquí]
# (https://learn-r-udp.netlify.app/resource/unzipping/).
# Luego de que hayas **descargado y descomprimido los datos** asegurate de dejar el
# archivo `.sav` y `.dta` en la carpeta de tu proyecto `input/data`. Los datos se
# llamarán en formato SPPS `Casen en Pandemia 2020 SPSS.sav` o en STATA `Casen en 
# Pandemia 2020 SPSS.dta`.

datos <- read_sav("input/data/Casen en Pandemia 2020 SPSS.sav")

## 1.1. Importar datos en diversos formatos --------------------------------

## a) .RData y .rds

load(file = "input/data/CASEN.RData")
readRDS(file = "input/data/CASEN.rds")

datos <- readRDS(file = "input/data/CASEN.rds")

## b) .dta

datos <- read_dta("input/data/Casen en Pandemia 2020 SPSS.dta") 

## c) .csv

datos <- read.csv("input/data/CASEN.csv", sep=",", 
                  encoding = "UTF-8", stringsAsFactors = F)

datos <- read.csv("input/data/CASEN.csv", sep=";", 
                  encoding = "Latin-1", stringsAsFactors = F, na.strings = c("No sabe", NA))

## d) .xlsx

datos <- readxl::read_excel(path = "input/data/CASEN.xlsx")

datos <- readxl::read_excel(path = "input/data/CASEN.xlsx", sheet = "Hoja1", skip = 1)

datos <- readxl::read_excel(path = "input/data/CASEN.xlsx", sheet = "Hoja1", skip = 1, na = "NA")

# 2. Explorar datos ------------------------------------------------

View(datos) # Ver datos
names(datos) # Nombre de columnas
dim(datos) # Dimensiones
str(datos) # Estructura de los datos (las clases y categorias de repuesta)

sjPlot::view_df(datos)

find_var(datos, "pobreza")
find_var(datos, "salario")

frq(datos$pobreza)
frq(datos$y1) #¡Qué feo!

descr(datos$y1)

#### Sobre las clases de las variables

class(datos$ypchtotcor)

# 3. Selección de datos ----------------------------------------------------

datos_proc <- select(datos, ypchtotcor,v13,v29,p6)

# 4. Guardar y exportar datos ---------------------------------------------

save(datos_proc, file = "output/data/datos_proc.RData") #Guardamos el objeto datos_proc en la ruta de trabajo actual, bajo el nombre de datos_proc.RData. 

saveRDS(datos_proc, file= "output/data/datos_proc.rds") #Guardamos el objeto datos_proc en la ruta de trabajo actual, bajo el nombre de datos_proc.rds. 

write_sav(datos_proc, "output/data/datos_proc.sav") #Guardamos el objeto datos_proc en la ruta de trabajo actual, bajo el nombre de datos_proc.sav.

write_dta(datos_proc, "output/data/datos_proc.dta") #Guardamos el objeto datos_proc en la ruta de trabajo actual, bajo el nombre de datos_proc.dta.

write.csv(datos_proc, "output/data/datos_proc.csv") #Guardamos el objeto datos_proc en la ruta de trabajo actual, bajo el nombre de datos_proc.csv. 

writexl::write_xlsx(datos_proc, "output/data/datos_proc.xlsx") #Guardamos el objeto datos_proc en la ruta de trabajo actual, bajo el nombre de datos_proc.xlsx. 

