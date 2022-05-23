
# Práctico 5: Transformar y seleccionar variables ----------------------


# 0. Instalar paquetes ----------------------------------------------------

pacman::p_load(tidyverse, #Universo de librerías para manipular datos
               haven, #Para cargar datos
               dplyr,#Para manipular datos 
               sjmisc,#Para explorar datos
               magrittr) #Para el operador pipeline (%>%)

# 1. Importar datos -------------------------------------------------------

# En este práctico, se trabajó con CASEN 2020. 

datos <- read_dta("input/data/Casen en Pandemia 2020 STATA.dta") 

## 1.1. Explorar los datos  -----------------------------------------------

head(datos, 5)

# De sjmisc

# find_var(datos, "concepto") # se utiliza para encontrar variables: primer argumento es el set de datos, concepto a buscar en "". En este sentido, la función lee las etiquetas de las variables.
# cuando seleccionemos variables trabajaremos con el número de la variable o su nombre, no su etiqueta

# 2. Operadores relacionales -------------------------------------------------

# Se usan para hacer comparaciones. Cuando en la *Tabla 1* nos referimos a `un valor`, esto refiere tambien a `variables`
# 
#   | Símbolo  | Función |
#   |---------:|:--------|
#   | `<`      |  Un valor es menor que otro |
#   | `>`      |  Un valor es mayor que otro |
#   | `==`     |  Un valor es igual que otro [^1] |
#   | `<=`     |  Un valor es menor o igual que otro |
#   | `>=`     |  Un valor es mayor o igual que otro |
#   | `!=`     |  Un valor es distinto o diferente que otro|
#   | `%in%`   |  Un valor pertenece al conjunto designado [^2] | para filtrar por datos específicos dentro del set de datos
#   | `is.na()`|  El valor es perdido o `NA` |
#   | `!is.na()`| El valor es distinto de  `NA` |


# 3. Operadores aritméticos -----------------------------------------------

# Realizan operaciones, como la suma, resta, división, entre otros.
# 
#   | Símbolo  | Función |
#   |---------:|:--------|
#   | `+`      |  Suma |
#   | `-`      |  Resta|
#   | `*`     |  Multiplicación |
#   | `/`     |  División |
#   | `^`     |  Elevado |

# 4. Operadores de asignación ---------------------------------------------


# Hay dos formas de asignar `objetoA <- objetoB` o `objetoA = objetoB`. Ambas 
# implican que lo que se este realizando en el *objetoB* implica que eso va a 
# producir o generar al *objetoA*


# 5. Operadores booleanos -------------------------------------------------

# Describen relaciones **lógicas** o **condicionales**
# 
# | Símbolo  | Función |
# |---------:|:--------|
# | `&`      |  Indica un *y* lógico |
# | `|`      |  Indica un *o* lógico |
# | `xor()`  |  Excluye la condición  |
# | `!`      |  Distinto de ... |
# | `any`    |  Ninguna de las condiciones serán utilizadas |
# | `all`    |  Todas las condiciones serán ocupadas |


# 6. Operador pipeline (%>%) ----------------------------------------------

# ¡Aquí mucha atención! Este operador `%>%` (llamado `pipe`) no es un operador que este contenido en las funciones base del lenguaje R. Este operador proviene de la función `magrittr` de `tidyverse`, y es de los operadores más útiles y utilizados en R.
# 
# ¿Para qué sirve? Para concatenar múltiples funciones y procesos. *Imagina que quieres filtrar una base de datos a partir de tramos etarios. Pero no tienes esa variable creada. ¿Que hacer? La respuesta: concatenar el proceso de creación de variables y luego filtrar.* Eso se puede hacer gracias a ` %>% ` (ya mostraremos como utilizar esta herramienta), que por lo demás es muy fácil de ejecutar.
# 
# - `Ctrl + shift + M` Para Windows
# - `⌘ + shift + M` Para Mac


# 7. Transformación de variables ---------------------------------------------

## 7.1. Seleccionar variables ------------------------------------

### Con R base -------------------------------------------------------------------------

datos[, 1] #Seleccionamos primera columna

datos[, c(1,2,3)] #Seleccionamos columnas 1, 2 y 3

datos[, 1:5] #Seleccionamos columnas de la 3 a la 5

datos[, c('sexo', 'metodologia_entrev')] #Seleccionamos columnas sexo y metodologia_entrev


### Con dplyr ---------------------------------------------------------------

### Selección por nombre de columna

# select(datos, variable1, variable2, variable3) #Incluir variables

# select(datos, -variable1) #Para excluir una variable, anteponer un signo menos (-)

### Selección por indexación

select(datos, 1, 2) # la primera y la segunda columna

select(datos, 1:4) # la primera hasta la cuarta columna

select(datos, c(1, 4, 5)) # la primera, la cuarta y la quinta columna columna

### Selección renombrando

select(datos, edad, sexo, o1)

select(datos, edad, sexo, ocupacion = o1) #Renombrarmos

### Para reordenar variables
# Empleamos el argumento everything() para agregar todo el resto de variables.
# El data frame creado ordenará las variables según se asigne con select.

select(datos, id_persona, sexo, edad, everything())

### Con patrones de texto

# - `starts_with()`: prefijo | que las variables partan con
# - `ends_with() `:  sufijo | que la variable termine con 
# - `contains()` : contiene una cadena de texto literal | que la variable contenga
# - `matches()` : coincide con una expresión regular | que la variable coincida

select(datos, starts_with("a"), ends_with("preg")) # aquí se difrencian variables

# También se pueden combinar con operadores logicos

select(datos, starts_with("y1")&ends_with("preg")) 
select(datos, contains("pobre")|contains("vivienda")) # es aquí necesario fijarse en el operador (&, |)
select(datos, matches("pobreza_|vivienda"))

### Con condiciones lógicas

select(datos, where(is.character)) # 

## 7.2. Selección de variables para el ejercicio ---------------------------

# Seleccionamos las siguientes variables
# - `edad`
# - `sexo`
# - `s13`: previsión de salud
# - `tot_per`: número de personas en el hogar
# - `ytoth`: ingresos totales del hogar
# - `o1`: ocupación
# - `y26d_total`: Monto del IFE
# - `y26d_hog`: ¿Alguien recibió el IFE?

datos_proc <- select(datos, folio, edad, sexo, prev = 592, ocupacion = o1, 
                     tot_per, ytoth, starts_with("y26d_")&matches("total|hog"),
                     o2, o3, o4, o6)


## 7.3. Filtrar datos  -----------------------------------------

### Con R base -------------------------------------------------------------------------

datos_proc[datos_proc$sexo == 2,] #Seleccionamos filas con sexo == 2
datos_proc[datos_proc$sexo == 2 & datos_proc$edad >= 33,] #Seleccionamos filas con sexo == 3 y edad mayor o igual a 33
datos_proc[datos_proc$prev %in% c(1,3,4),] # Seleccionamos personas con prevision 1, 3 o 4



### Con dplyr ---------------------------------------------------------------

# filter(datos, condicion_para filtrar)
# Esta condición para filtrar podría ser, por ejemplo
# variable1 >= 3

### a) Con números

filter(datos_proc, edad >= 15) # A tibble: 151,315 x 13
filter(datos_proc, edad >= 15 & tot_per <7) # en ambos casos se agregan condiciones para filtrar. En este caso, que sean mayores de 15 y además que vivan en hogares con menos de 7 personas

filter(datos_proc, ytoth == max(ytoth))

### b) Con caracteres

# cuando una variable double (que tenga números etiquetados, por ej, sexo) Es en el caso donde existen categorías dentro de las respuestas ()

datos_proc$sexo <- as_factor(datos_proc$sexo) # a mis datos, la variable sexo, le voy a aplicar que sea un factor y este proceso lo sobreescribiré dentro de mis datos procesdos a mi variable sexo. Cuando se asignan objetos e 


filter(datos_proc, sexo == "Mujer")
filter(datos_proc, sexo != "Hombre")

#### Dos condiciones con caracter

datos_proc$prev <- as_factor(datos_proc$prev) # cómo se qué significa cada número? hay que convertir a factor y así se conserva la etiquta de la variable 
# Nico explica este proceso de conversión en el práctico de la clase 5
filter(datos_proc, prev %in% c("Sistema Público FONASA", "ISAPRE") & edad >= 65)



# 8. Tratamiento de casos perdidos -------------------------------------------

## Revisar valores == NA

is.na(datos_proc) #Revisamos si hay casos perdidos en el total del set de datos 
is.na(datos_proc$ytoth) #Revisamos si hay casos perdidos en Ingresos per cápita

## Contar cuántos NA hay en df

sum(is.na(datos_proc)) #Contamos los valores nulos del set de datos en general, que suman un total de 180.148
sum(is.na(datos_proc$ytoth)) #Contaremos los valores nulos de la variable Ingresos per cápita, que alcanzan un total de 98


## Eliminar NA

nrow(datos_proc)
x <- na.omit(datos_proc) #Eliminamos las filas con casos perdidos
nrow(x)
nrow(datos_proc) #La nueva base de datos tiene 5.387 filas y 4 columnas


# 9. Resumen de procesamiento ------------------------------------------------

datos_proc %>% 
  filter(edad >= 15 & tot_per <7) %>%
  select(folio, sexo, edad, ocupacion, ytoth, tot_per, ife = y26d_hog, o2, o3, o4, o6) %>% 
  na.omit() # aqui se prueba y luego

datos_proc <- datos_proc %>% 
  filter(edad >= 15 & tot_per <7) %>%
  select(folio, sexo, edad, ocupacion, ytoth, tot_per, ife = y26d_hog, o2, o3, o4, o6) %>% 
  na.omit() # se crea el objeto

sjPlot::view_df(datos_proc)


# 10. Unir datos ----------------------------------------------------------

## Crear sets de prueba

proc_1 <- datos_proc %>% select(folio, sexo, ocupacion, ytoth)
proc_1 <- proc_1[1:908,] #Seleccionamos la mitad de las filas
proc_2 <- datos_proc %>% select(folio, sexo, edad, tot_per, ife)
proc_2 <- proc_2[909:1816,] #Seleccionamos la mitad de las filas

### merge() -----------------------------------------------------------------

merge <- merge(proc_1, proc_2, #Especificamos data frames a unificar. Debe haber una variable común 
               by = c("folio", "sexo"), #Especificamos la variable a partir de la cual se realiza la unificación (puede ser más de una, como folio y sexo)
               all = T) #Especificamos que queremos mantener total de filas, sumando las de x (proc_1) e y (proc_2)

head(merge)

### bind_cols() -------------------------------------------------------------

bind_columnas <- bind_cols(proc_1, proc_2) # bind == unir (columnas o filas) cuando tenga dos set de datos 

head(bind_columnas)

### bind_rows() -------------------------------------------------------------

bind_filas <- bind_rows(proc_1, proc_2)

head(bind_filas)


# 11. Exportar datos ------------------------------------------------------

saveRDS(datos_proc, file = "output/data/datos_proc.rds")

# cómo usar estos archivos?





















