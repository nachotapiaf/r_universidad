
# Práctico 8: Análisis descriptivo bivariado ------------------------------


# 1. Cargar librerías -----------------------------------------------------

pacman::p_load(sjmisc,
               sjPlot,
               tidyverse,
               magrittr)

# 2. Cargar datos ---------------------------------------------------------

datos_proc <- readRDS("input/data/datos_proc.rds")


# 3. Explorar datos -------------------------------------------------------

sjPlot::view_df(datos_proc,
                encoding = "UTF-8")


# 4. Visualización --------------------------------------------------------


## a) Gráficos de dispersión -----------------------------------------------

#Con dos variables
datos_proc %>%  
  dplyr::filter(ing_pc <= 2000000) %>% # hay que forzarle desde dplyr
  plot_scatter(., tot_per, ing_pc) # relacionar una variable X con una variable Y 

# cómo se distribuyen dos variables.
# para hacer análisis de variables continuas
# variables: total personas x hogar / ingreso per capita
# siempre es X (argumento primero) Y (argumento segundo)
# ES UN MAL GRÁFICO, POR QUÉ: EL TOTAL DE LA RAZÓN DE INTEGRANTES DEL HOGAR DISTA DE SER UNA VARIABLE CONTINUA, ES DISCRETA.
# para hacer un modelo de regresión lineal se debe hacer con variables continuas. La profe menciona que es algo difícil encontrar v. continuas porque si se piensa quizás todo podría ser una v. discreta. 
# 
?plot_scatter


#Con tercera variable categórica
datos_proc %>%  
  dplyr::filter(ing_pc <= 2000000) %>%
  plot_scatter(., ytoth, ing_pc, sexo)

# uno ocupa los scatter plot como primera instancia para hacer un modelo de regresión
# mirar gráficamente cómo se relacionan las variables
# cómo se ve la relación entre esas variables. si los datos están todos dispersos entonces quizás hablamos de que no hay relación, visualmente hablanod
# es mejor partir con esto (no para reportar en informe) podemos notar que si bien hay una relación baja, cuando coloreamos los grupos por edad y se ve que x ej los hombres están agrupados en un sector y las mujeres en otro (jóvenes)


#Incorporar recta de regresión (OLS)

datos_proc %>%  filter(ing_pc <= 2000000) %>%
  plot_scatter(., tot_per, ing_pc, sexo,
               fit.grps = "lm", grid = TRUE)
# quiero ponerle una grilla (grids) lo que hará es separarlos (masc fem) y si pongo fits grps mostrará la recta

## b) Frecuencias agrupadas para variables categóricas ---------------------

#Grafico de barras 
plot_grpfrq(datos_proc$sexo, datos_proc$o4,
            type = "bar", title = "Gráfico de barras")

#Barras con porcentaje y barra de totales
plot_xtab(datos_proc$sexo_edad_tramo, datos_proc$o4, title = "Gráfico de barras")


#Barras horizontales
plot_xtab(datos_proc$o4, datos_proc$sexo_edad_tramo, margin = "row", 
          bar.pos = "stack", # que las variables estén apiladas "how can I stack barplot"?
          title = "Gráfico de barras horizontales",
          show.summary = TRUE, coord.flip = TRUE)

# hay que sacar los N

plot_xtab(datos_proc$o4, datos_proc$sexo_edad_tramo, margin = "row", 
          bar.pos = "stack", # que las variables estén apiladas "how can I stack barplot"?
          show.n = F,
          title = "Gráfico de barras horizontales",
          show.summary = TRUE,
          coord.flip = TRUE)

#Barras para diversas variables con mismas categorías de respuesta

datos_proc %>% select(ocupacion, o2, o3, o4, o6) %>% 
  plot_stackfrq(., title = "Gráfico de barras proporcional")

#Escalas Likert
datos_proc %>% select("ocupacion","o2", "o3", "o4", "o6") %>% 
  sjPlot::plot_likert(., title = "Gráfico de escalas Likert")

#Proporciones cruzadas 
plot_gpt(datos_proc, o4, o6, sexo,
         shapes = c(15, 21), 
         title = "Gráfico de proporción agrupada")

# as factor para conservar la etiqueta de la categoría de respuesta. Para conservar las etiquetas en la recodificación se debe ocupar car package

#Gráfico de puntos 
plot_grpfrq(datos_proc$sexo, datos_proc$o4,
            title = "Gráfico de puntos",
            type = "dot")

#Gráfico de líneas
plot_grpfrq(datos_proc$sexo_edad_tramo, datos_proc$o4,
            title = "Gráfico de línea",
            type = "line")


## c) Variables categóricas y continuas ---------------------------------------


#Gráfico de cajas
plot_grpfrq(datos_proc$tot_per, datos_proc$sexo_edad_tramo,
            title = "Gráfico de caja",
            type = "boxplot")

#Incorporar tercera variable
plot_grpfrq(datos_proc$tot_per, datos_proc$sexo_edad_tramo, intr.var = datos_proc$o4, 
            title = "Gráfico de cajas",
            type = "box")

#Gráfico de violín
plot_grpfrq(datos_proc$tot_per, datos_proc$sexo_edad_tramo,
            title = "Gráfico de violín",
            type = "violin")



## 5. Tablas de contingencia --------------------------------------------------

# hay que tener cuidado en cuál es la variable X e Y

sjt.xtab(datos_proc$sexo, datos_proc$o4,
         show.col.prc=TRUE,
         show.summary=FALSE, 
         encoding = "UTF-8", 
         title = "Tabla de contingencia")

#Incorporar tercera variable
tab_stackfrq(as.data.frame(datos_proc %>% select(sexo, "o4", "o6")),
             value.labels=c('1'='Si', '2'='No'),
             encoding = "UTF-8",
             show.n = TRUE, show.total = T)
# por qué ocupar data frame: puede pasar que esta selección no sea necesariamente un data frame
# entonces tengo, 

## 6. Test de independencia chi cuadrado -------------------------------------

data.frame(as_factor(sample(datos_proc$sexo, replace = TRUE)),
           as_factor(sample(datos_proc$o4, replace = TRUE)),
           as_factor(sample(datos_proc$o6, replace = TRUE))) %>% 
  sjp.chi2(., 
           title = "Gráfico de Chi2",
           axis.labels  = c("Ha trabajado alguna vez", "Busco empleo"))

# en sjPlot encontraremos un gráfico para chi2. Necesita que el data frame sea con variables dicotómicas o dummy
# lo primero es que los datos estén procesados y en vriables dummy
# se comprueba que sea dummy con str()
# variables dummy: presencia o ausencia (0,1)
# si lanza el error: "ambas variables deben tener la misma longitud" aunque sean variables dicotómicas, no están trabajadas como variables dummu
str(datos_proc$sexo)

## 7. Correlaciones -----------------------------------------------------------

datos_proc %>%
  select(ing_pc, tot_per) %>% 
  tab_corr(.,
           triangle = "lower",   
           title = "Tabla de correlación",
           encoding = "UTF-8")
# muestra la relación entre personas del hogar y el ingreso per capita
# lo importante aquí es que las variables sean numéricas


# Test ANOVA --------------------------------------------------------------

sjp.aov1(datos_proc$ing_pc, datos_proc$sexo, title = "Anova")
# anova es para la comparación de y entre grupos
# primer argumento es la variable dependiente (ingreso) 
# tomaré una variable dependiente continua para compararla entre grupos
# sirve para obtener valores 
# en el día a día, para explorar los datos y su asociación 






