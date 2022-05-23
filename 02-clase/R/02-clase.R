# Codigo clase: Herramientas para el uso de R - II -----------------------------

# A. Partes de un script -------------------------------------------------------------------------

# Los gatos ( # ) son comentarios
# Para hacer una division en el código CTRL+ Shift + R

# Para guardar el script CTRL + S (archivo.R)

# ¿Qué pasa si me aparece el codigo con símbolos raros?  Reopen with encoding

# 1. Cargar paquetes ------------------------------------------------------

# 2. Cargar datos ---------------------------------------------------------

# 3. Procesar datos -------------------------------------------------------

# 4. Guardar datos --------------------------------------------------------

# B. Elementos del lenguaje de R ------------------------------------------

# Calculando una media
(25 + 24 + 22)/3
media <- (25 + 24 + 22)/3
media

# Caracteres
c("Valentina", "Nicolás", "Dafne")
equipo <- c("Valentina", "Nicolás", "Dafne")

# Numeric (numeros)
c(25, 22, 24)
edad <- c(25,22,24)
mean(edad)

# Integer (longitudes)
1:3
orden <- 1:3

# Crear data frame
?data.frame()
datos <- data.frame(equipo, edad, orden)

# Y ahora ¿calculamos la media?
mean(datos$edad)

# C. Instalar paquetes ----------------------------------------------------
# Informacion de la sesion
sessionInfo()

# Instalar paquetes
?install.packages()

install.packages("tidyverse")
remove.packages("tidyverse")

install.packages(c("tidyverse", "sjPlot", "sjmisc")) # Primera vez

# Llamar paquetes
library(tidyverse)

# Ocupemos una funcion
frq(datos$equipo)

# ¡Hay que llamar! (u obligar)
library(sjmisc)
sjmisc::frq(datos$equipo)

# Una forma fácil de hacerlo: pacman

install.packages("pacman")
library(pacman)

# La funcion p_load de pacman instala y carga a la vez
pacman::p_load(tidyverse,
               sjPlot,
               sjmisc)


