##### P7#####

# 1) Crea un nuevo proyecto llamado "p7"
# ir a menú y pulsar File-> new project-> escoger nuevo proyecto vacío.
# 2) Crea un script y carga las librerías necesarias.
#Instala previamente los archivos si es necesario.
library(tidyverse)
library(dplyr)
library(plyr)
# 3) Crea una función que dado un número calcule su módulo 4 
#  Aplícalo a tu número de dni.

mod4<-function(n) {
  mod4<-n%%4
  }
mimod<-mod4(3093835)

# 4) Carga el dataset proporcionado "datos_XXX.csv" correspondiente a:  
# A para mod4 igual a 0, B para mod4 igual a 1, 
# C para mod4 igual a 2 y D para mod4 igual a 3.  
datos<-read.csv('datos_XXC.csv', 
             encoding = 'UTF-8', 
             sep = ',',
             dec = '.')

# 5) Echa un vistazo a los datos para familiarizarte con ellos. 
# Escribe directivas para ello. ¿Cuántas variables tiene? 
# ¿Cuántos registros tiene? Visualiza en tipo de las variables y 
# la estructura del dataset.
#head(datos, 4)  poco recomendable por que es un dataset muy grande
#tail(datos, 4)
str(datos) #129 obs. of  180 variables

# 6)Elimina todas las variables que tengan más de un 80% de NAs. 
# Para ello, debes contar el número de NAs usando código R. 
# Almacena los resultados en "datos_1". 
# Guardar el dataset en un archivo csv con el mismo nombre.
contarNA<- function(lista) {
  return(sum(is.na(lista)))
}

#Esta función calcula el porcentaje de NA:

porcentajeNA<-function(lista) {
  return(contarNA(lista)/length(lista))
}

elimina<-c(0) #En este vector se almancenarán los resultados:
datos_1<-datos
for (i in 1:length(datos)){
  porcentaje<-porcentajeNA(datos[,i])
  if (porcentaje>=0.8) elimina[i]=TRUE
  else elimina [i]=FALSE
} #Aplicando el filtro que se ha definido se obtiene el dataset deseado.
datos_1 <- datos_1[,-elimina==0] 

write.csv(datos_1, "datos_1.csv") 

# 7) Crea un nuevo dataset llamado datos_2" con las primeras 13 variables

datos_2<-select(datos, fecha:lentitud)

#Después se renombran las variables al inglés para evitar problemas con las tildes y las ñ:




# 8)Renombra en inglés las variables para evitar tildes y otros símbolos.
nombres<-c("Date","Animus","Anxiety","Irritability","Concentration","Tabacco","Caffeine","Sleep","Waking_up","Motivation","Sleep_quality", "Slowness")
names(datos_2)<-nombres
head(datos_2) #para comprobar

# 9) Utiliza "ggplot2"con  boxplot, etc.  para calcular la mediana, los 
# cuartiles y los datos atípicos de 3 variables de tu elección. Comenta 
# en cada caso tus observaciones. Guarda las imágenes en archivos png 
# dentro de tu proyecto.

#ANIMUS---------------------------
ggplot(datos_2, aes(x=Animus))+geom_bar(fill="grey")
summary(datos_2$Animus)

#MOTIVATION---------------------------

ggplot(data=datos_2, aes(x=Motivation)) + geom_bar(fill="pink")
summary(datos_2$Motivation)

#SLEEP--------------------------------

ggplot(datos_2, aes(x=Sleep)) + geom_histogram(binwidth = 0.5)
summary(datos_2$Sleep)


#A continuación se calcula el tiempo dormido por cada individuo:

total_sleep<-function(dormir,despertar) {
  a=dormir*60
  b=despertar*60
  if (dormir>despertar) { #Hay cambio de día 
    dormido=(1440-a)+b
  } else #No hay cambio de día:
    dormido=b-a
  return(dormido)
}


datos_2<-mutate(datos_2, sleep_time=total_sleep(Sleep, Waking_up))

modelo <- lm(sleep_time ~ Date+Animus  + Anxiety  + Irritability  + Concentration  + Tabacco  + Caffeine  + Sleep  + Waking_up  + Motivation  + Sleep_quality  +  Slowness, data=datos_2 )
summary(modelo)
ggplot(datos_2,aes(sleep_time, Waking_up))+
  geom_smooth(method='lm',formula=y~x) + xlab('Horas de sueño') + ylab('Hora del despertar') + ggtitle('Horas de sueño - Hora del despertar')
#-----------------------------------------------------------------
ggsave('SleepTime.png')




