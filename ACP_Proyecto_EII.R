
if (!require(FactoMineR)) install.packages("FactoMineR")
if (!require(factoextra)) install.packages("factoextra")
if (!require(foreign)) install.packages("foreign")
if (!require(ggplot2)) install.packages("ggplot2")
if (!require(psych)) install.packages("psych")
if (!require(dplyr)) install.packages("dplyr")
if (!require(tidyr)) install.packages("tidyr")

library(FactoMineR)
library(factoextra)
library(foreign)
library(ggplot2)
library(psych)
library(dplyr)
library(tidyr)

# Limpiar el entorno
rm(list = ls())

# Pelotita y cargar los datos
datos <- read_excel("Base_EII")

#-------Modificar (las variables, ojo con la nomenclatura)-----------------

variables_seleccionadas <- c(
  "pumabus", "taxi", "microbus", "camion", "combi", "trolebus", "mexibus", 
  "stc_metro", "metrobus", "escuela", "fines_recreativos", "trabajo",
  "costo", "ruta_corta","tiempo", "accesibilidad", "seguridad")



#-------No modificar -----------------
# Filtrar el dataframe para incluir solo las variables seleccionadas
datos_filtrados <- datos[, variables_seleccionadas]


#-------Modificar (si lo consideran, el número de factores)-----------------
poly_model <- fa(datos_filtrados, nfactors = 3, cor = "poly", fm = "mle", rotate = "none")

# Mostrar las cargas factoriales y diagrama del modelo
print(poly_model$loadings)
fa.diagram(poly_model)




