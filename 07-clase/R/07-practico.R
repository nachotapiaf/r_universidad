
# 7: Descriptivos univariados ---------------------------------------------


# 1. Cargar librerías -----------------------------------------------------


pacman::p_load(sjmisc, # explorar datos
               sjPlot, # hacer gráficos
               tidyverse, # mundo de paquetes
               magrittr) # operadores 

set_theme()

# 2. Cargar datos ---------------------------------------------------------

datos_proc <- readRDS("input/data/datos_proc.rds")


# 3. Explorar datos -------------------------------------------------------

names(datos_proc)
head(datos_proc)
sjPlot::view_df(datos_proc,
                encoding = "UTF-8")


# 4. Descriptivos ---------------------------------------------------------


## 4.1 Medidas de tendencia central ----------------------------------------


### a) Media -------------------------------------------------------------

mean(datos_proc$ing_pc, na.rm=TRUE) # na.rm = T sirve para remover NA de la muestra. Es bueno ponerlo para que no se distorcsionen los datos

#### Recortada

mean(datos_proc$ing_pc, na.rm=TRUE, trim = 0.025) # trim recorta la media, los valores extremos 
# na.rm = T para hacer cálculos con mutate aplicando rowise también sirve

### b) Mediana -----------------------------------------------------------

median(datos_proc$ing_pc, na.rm =TRUE)

### c) Estadísticos resumen de variables cuantitativas con sjmisc::descr() ----------------------------------

sjmisc::descr(datos_proc$ing_pc)

#### Interactuando con dplyr::select()

datos_proc %>% 
  select(ing_pc, ytoth, tot_per) %>% # aplicar a las 3 variables un mismo descr() y concatenar a través de %>% Solo se puede hacer porque no hay conflicto entre sjmisc y dplyr
  sjmisc::descr()


## 4.2 Análisis de frecuencias ----------------------------------------------


### a) table() para tablas de frecuencia absoluta ---------------------------------

table(datos_proc$sexo) 

### b) flat_table() para tablas de contingencia ------------------------------------

flat_table(datos_proc, sexo, ocupacion, ife) # para presentar datos

### c) Frecuencias para variables categóricas con sjmisc::frq() -------------------------------------

sjmisc::frq(datos_proc$sexo,
            out = "viewer",
            title = "Frecuencias",
            encoding = "UTF-8")


# 5. Gráficos univariados -------------------------------------------------


## a) Gráfico de barras ---------------------------------------------------

plot_frq(datos_proc,sexo_edad_tramo,
         title = "Gráfico de frecuencias, barras",
         type = "bar", show.values = F) + theme_classic()


### Exportar gráfico --------------------------------------------------------

save_plot("output/figures/figure1.png", fig = last_plot())


## b) Gráfico de puntos ----------------------------------------------------

plot_frq(datos_proc, sexo,
         title = "Gráfico de frecuencias, puntos",
         type = "dot") + theme_classic()

### Personalizando

plot_frq(datos_proc$sexo_edad_tramo,
         type = "dot", 
         show.ci = TRUE, 
         sort.frq = "desc", # orden
         coord.flip = TRUE, # para que cambie el orden de los ejes
         expand.grid = TRUE, vjust = "bottom", hjust = "left", 
         title = "Gráfico de frecuencias, puntos cambiado")


## c) Histograma  ---------------------------------------------------------------------

datos_proc %>%  filter(ing_pc <= 2000000) %>% 
  plot_frq(., ing_pc,
           title = "Histograma",
           type = "histogram")


## d) Gráfico de densidad -------------------------------------------------------------

datos_proc %>%  filter(ing_pc <= 2000000) %>%
  plot_frq(., ing_pc,
           title = "Gráfico de densidad",
           type = "density")



## e) Gráfico de líneas ---------------------------------------------------------------

plot_frq(datos_proc, tot_per,
         title = "Gráfico de líneas",
         type = "line")


## f) Gráfico de cajas -----------------------------------------------------
# cuando quieran comparar sexo con personas, por ej


datos_proc %>%  
  group_by(sexo)
  filter(ing_pc <= 2000000) %>%
  plot_frq(., ing_pc,
           title = "Gráfico de caja",
           type = "boxplot")


## g) Gráfico de violín ----------------------------------------------------


datos_proc %>%  filter(ing_pc <= 2000000) %>%
  plot_frq(., ing_pc,
           title = "Gráfico de violín",
           type = "violin")
# ejemplo
  
plot_scatter(datos_proc, tot_per, ytoth)
sjPlot::plot_scatter()