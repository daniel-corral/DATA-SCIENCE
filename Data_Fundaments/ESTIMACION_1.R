#####################  INFERENCIA PARAMETRICA #######################

###########################  ESTIMACION  ###########################
# Por metodo de maxima verosimilitud 
# Por metodo de momentos
# METODO DE MAXIMA BONDAD DE AJUSTE: uno de los mejores

library(MASS) ##analisis actuarios?


set.seed(123)
x<- rnorm(250, mean=1, sd=0.45)
x
# Simulacion de 250 valores normales y vamos a estimar con estos datos por maxima verosimilitud (el que mas max credibilidad)
##maximiza la funcion del mismo nombre

#Ajuste maxima verosimilitud de distribuciones univariables
fit <- fitdistr(x, densfun="normal")
fit ##fijas la distribucion para valores x y decimos que es una normal

##la media es 0.9961... esa media es una ariable aleatoria (cuando estableces el seed)
##DESVIACION TIPICA MUESTRAL 
                #SEGUNDA LINEA: DESVIACION DE ESTOS PARAMETROS- VALORES NEGATIVOS

hist(x, pch=20, breaks=25, prob=TRUE, main="HISTOGRAMA") # forma campaniforme y suporponer la funcion
curve(dnorm(x, fit$estimate[1], fit$estimate[2]), col="red", lwd=2, add=T) 
##superponer la funcion de densidad
##que el valor medio coja el estimador de la media y como desv tipica coja ese estimador
##util para gestion de riesgo






# ESTIMACION POR EL METODO DE MOMENTOS
fitMME <- fitdistr(x, "normal", method = "mme")
fitMME
curve(dnorm(x, fitMME$estimate[1], fitMME$estimate[2]), col="blue", lwd=2, add=T)


#Estimacion maxima bondad del ajuste
#Diversos estudios destacan la mejor estimacion parametrica 
##METODO DE LA MAXIMA BONDAD Y ES EL MEJOR AJUSTE Y UNO DE LOS MEJORES METODOS


fitMGE <-fitdistr(x, "normal", method="mge", gof="CvM")
fitMGE



##METODO DE LOS MOMENTOS 
##SI EN fit no decimos nada nos da directamente máxima verosimilitud

##si queremos comprobar cual es mejor que otra tenemos que estimar el mejor metodo 

##PARA INTERVALOS VAMOS A HIPOTESIS

# PROFESORA:-----------------------------
#####################  INFERENCIA PARAMETRICA #######################

###########################  ESTIMACION  ###########################


library(MASS)



set.seed(1)
x<- rnorm(250, mean=1, sd=0.45)     


#Ajuste m?xima verosimilitud de distribuciones univariables
fit <- fitdistr(x, densfun="normal") 
fit

hist(x, pch=20, breaks=25, prob=TRUE, main="HISTOGRAMA")
curve(dnorm(x, fit$estimate[1], fit$estimate[2]), col="red", lwd=2, add=T)



#Estimaci?n por el m?todo de los momentos 
fitMME <- fitdistr(x, "normal", method="mme")
fitMME
curve(dnorm(x, fitMME$estimate[1], fitMME$estimate[2]), col="blue", lwd=2, add=T)



#Estimaci?n m?xima bondad del ajuste
#Diversos estudios destacan la mejor estimaci?n param?trica 

fitMGE <-fitdistr(x, "normal", method="mge", gof="CvM")
fitMGE

