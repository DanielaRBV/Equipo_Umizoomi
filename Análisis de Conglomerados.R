#--------------------------------------------------------------------------------
# Tema:       Analisis de conglomerados: k-medias
# Autor:      Julio Cesar <jcms2665@gmail.com>
# Fecha:      septiembre 2022
# Datos:      Latinobarometro_2018_Esp_R_v20190303.Rds
# Github:     https://github.com/jcms2665/UNAM-2021-Multivariado


#               CONTENIDO

#     0. Entorno de trabajo
#     1. Cargar base
#     2. Arreglo de la base de datos
#     3. Algoritmo de k-modes con 3 grupos
#     4. Pegar grupos a la base original
#     5. Interpretacion
#--------------------------------------------------------------------------


#0.  Entorno de trabajo

rm(list=ls())     
graphics.off()    

library(foreign)
library(ggplot2)
library(psych)
library(dplyr)
library(psych)
library(tidyr)
library(htmltools)
library(klaR)
install.packages("klaR")

#1. Cargar base

##Set as working directory
Equipo_Geimizoomi<-read_excel("Equipo Geimizoomi.xlsx")
Base <- Equipo_Geimizoomi
Base

# Variables:

# Edad
# Genero_codigo --- Género masculino (1), femenino (2) y Otros (3)
# Lugar_de_residencia_codigo --- CDMX (1), EdoMex (2), Hidalgo (3) y Otros (4)
# Uso_frecuente_codigo --- Sí (1) y Casi no lo utilizo (2)
# Tipo_transporte
# Dia_semana
# Motivos
# Razon_de_transporte
# Tiempo_trayectos --- 1-30 min (1), 31-59min (2), 1-2horas (3) y Más de 2 horas (4)A

# Quitar respuestas inv?lidas --> Ya se logró con el tratamiento de la base de datos
##dat1[dat1 <=0] <- NA
##dat1<-dat1%>%drop_na()


# Etiquetar variables (para identificar a la unidad de analisis: personas)
dat1<-Equipo_Geimizoomi
dat1$Edad_codigo<-factor(dat1$Edad_codigo,levels = c(1,2,3,4,5,6,7,8,9,10), labels = c("18-22 anos","23-27 anos","28-32 anos","33-37 anos","38-42 anos","43-47 anos","48-52 anos","53-57 anos","58-60 anos","60 o mas anos"))
dat1$Genero_codigo<-factor(dat1$Genero_codigo,levels = c(1,2,3), labels = c("Masculino","Femenino","Otros"))
dat1$Lugar_de_residencia_codigo<-factor(dat1$Lugar_de_residencia_codigo,levels = c(1,2,3,4), labels = c("CDMX","EdoMex","Hidalgo","Otros"))
View(dat1)

# Filtrar variables para el analisis --> SÍ es necesario, porque tenemos que dejar solo las que son con código
dat2<-dat1[,5:40]
View(dat2)

#Gráfica de sedimentación para decidir el número de conglomerados
corr<-round(cor(dat2),2)
aucor=eigen(corr)
plot(5:40,aucor$values,type="l",xlab="conglomerados",ylab="Autovalores")

#3. Algoritmo de k-medias con 3 grupos
fit <-kmodes(dat2, 3)


#4. Pegar grupos a la base original
dat.grupos <- data.frame(dat1, fit$cluster)

names(dat.grupos)

#5. Interpretacion
View(dat.grupos)
table(dat.grupos$fit.cluster,dat.grupos$Edad_codigo)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$Genero_codigo)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$Lugar_de_residencia_codigo)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$Uso_frecuente_codigo)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$Tiempo_trayectos)%>%prop.table(1)%>%`*`(100)%>%round(1)

table(dat.grupos$fit.cluster,dat.grupos$metrobus)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$pumabus)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$taxi)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$microbus)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$camion)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$combi)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$trolebus)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$stc_metro)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$teleferico_cablebus)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$mexibus)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$bicitaxi_mototaxi)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$tren_ligero)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$suburbano)%>%prop.table(1)%>%`*`(100)%>%round(1)

table(dat.grupos$fit.cluster,dat.grupos$escuela)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$fines_recrativos)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$trabajo)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$familiares)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$diversion)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$tramites)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$compras)%>%prop.table(1)%>%`*`(100)%>%round(1)

table(dat.grupos$fit.cluster,dat.grupos$costo)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$ruta_corta)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$tiempo)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$accesibilidad)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$areas_designadas)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$seguridad)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$unico_medio)%>%prop.table(1)%>%`*`(100)%>%round(1)

table(dat.grupos$fit.cluster,dat.grupos$lunes)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$martes)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$miercoles)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$jueves)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$viernes)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$sabado)%>%prop.table(1)%>%`*`(100)%>%round(1)
table(dat.grupos$fit.cluster,dat.grupos$domingo)%>%prop.table(1)%>%`*`(100)%>%round(1)


