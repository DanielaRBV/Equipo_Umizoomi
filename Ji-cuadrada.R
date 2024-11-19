
library(readxl)

datos <- read_excel("EDAD.TIPOT.xlsx", sheet = "Hoja1")

# Crear tabla de contingencia
  tabla_contingencia <- table(datos$Edad_codigo, datos$metrobus)
  print(tabla_contingencia)

chi_prueba <- chisq.test(tabla_contingencia)
chi_prueba

tabla_contingencia2 <- table(datos$Edad_codigo, datos$stc_metro)
print(tabla_contingencia2)

chi_prueba <- chisq.test(tabla_contingencia2)
chi_prueba

tabla_contingencia3 <- table(datos$Edad_codigo, datos$taxi)
print(tabla_contingencia3)

chi_prueba <- chisq.test(tabla_contingencia3)
chi_prueba
