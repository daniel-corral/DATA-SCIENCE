#################################################
########### SERIES TEMPORALES APPLE #############
#################################################


### IMPORTACION DE DATOS
apple <- read.csv("apple.csv", sep = ",", dec = ".")
View(apple)

### CARGA DE LIBRERIAS
library(zoo)
library(ggfortify)
require(forecast)
library(forecast)
require(xts)
library(xts)
require(ggplot2)
library(ggplot2)
require(lubridate)
library(lubridate) #manipular fechas
library(magrittr)

### VISUALIZACION DE DATOS
apple[is.na(apple)] <- 0 ##ponemos 0 en los datos de NA
class(apple$Time) #es un factor con 70 niveles, quiero convertirlo a fecha

class(apple) #dataframe
names(apple)
summary(apple[,3:6])

##################################  PARTE 1 ##################################
#VENTAS DE TODOS LOS PRODUCTOS: VENTAS AGREGADAS 
ventas.suma <- apply(apple[ , 3:6], 1, sum)
View(ventas.suma)
apple <- cbind(apple, ventas.suma)

#quitamos columnas que no nos hacen falta
apple$Period = NULL
apple$iPhone = NULL
apple$iPod = NULL
apple$iPad = NULL
apple$Mac = NULL

ventas.apple <- apple

#Convertir fecha a formato numerico
ventas1 <- ventas.apple$venta
date.apple <- seq(as.Date("1998/10/01"), as.Date("2016/01/01"), by = "quarter")

#apple$Time <- date.apple ##llamamos a Time los datos apple

#Convertimos a xts para poder hacer una SERIE TEMPORAL
ventas.apple <- xts(ventas1, order.by = date.apple)

#Utilizamos una periodicidad trimestral
ventas.apple <- to.quarterly(ventas.apple)

#Convertimos el objeto en zoo
ventas.zoo = as.zoo(ventas.apple$ventas.apple.Close)

#Cambiamos el nombre de ventas
names(ventas.zoo) <-  "Ventas"

####################################### PARTE 2 ##################################################

##VENTAS POR CADA PRODUCTO

              ######################## IPHONE #########################
apple <- read.csv("apple.csv", sep = ",", dec = ".")
apple[is.na(apple)] <- 0

ventas.iphone <- apple$iPhone
datos2 <- seq(as.Date("1998/10/01"), as.Date("2016/01/01"), by = "quarter")

ventas.iphone <- xts(ventas.iphone, order.by = datos2)

ventas.iphone <- to.quarterly(ventas.iphone)

ventas.iphone = as.zoo(ventas.iphone$ventas.iphone.Close)

names(ventas.iphone) <- 'Ventas iPhone'
View(ventas.iphone)

              ######################## IPAD #########################

ventas.ipad <- apple$iPad
datos3 <- seq(as.Date("1998/10/01"), as.Date("2016/01/01"), by = "quarter")

ventas.ipad <- xts(ventas.ipad, order.by = datos3)

ventas.ipad <- to.quarterly(ventas.ipad)

ventas.ipad = as.zoo(ventas.ipad$ventas.ipad.Close)

names(ventas.ipad) <- 'Ventas iPad'
View(ventas.ipad)
              ######################## MAC #########################
ventas.mac <- apple$Mac
datos4 <- seq(as.Date("1998/10/01"), as.Date("2016/01/01"), by = "quarter")

ventas.mac <- xts(ventas.mac, order.by = datos4)

ventas.mac <- to.quarterly(ventas.mac)

ventas.mac = as.zoo(ventas.mac$ventas.mac.Close)

names(ventas.mac) <- 'Ventas Mac'
View(ventas.mac)


################################### PARTE 3: MODELIZACION #########################################
 
          ###########################################################################
          ################################### ETS ################################### 
          ###########################################################################

#### GRAFICAS : le puedes añadir todo lo que quieras de ggplot
autoplot(ventas.zoo) + ggtitle("Ventas Trimestrales Apple") + xlab("Trimestres") + ylab("Ventas") ##ventas por trimestres

#Grafico Estacionario: no hay estacionariedad
ggfreqplot(as.ts(ventas.zoo),freq = 4,nrow = 1,facet.labeller = c("1T","2T","3T","4T")) + ggtitle("Ventas Trimestrales")

#Numero de observaciones para comparar predicciones 
cOmit = 4

#Tamaño de los datos
numob = length(ventas.zoo)

#Se saca una submuestra 
#oVentas=zVentas[1:(nObs-cOmit),]
oVentas <- window(ventas.zoo,start = index(ventas.zoo[1]),end = index(ventas.zoo[numob - cOmit]))
View(oVentas)

###PRIMER MODELO
##Hacemos los 4 fit sobre la submuestra
#Fit Suavizado de Alisado Exponencial (parametro alpha)
fit1 <- ses(oVentas)
round(accuracy(fit1),2)
#Fit Holt(paramtros alpha y beta)
fit2 <- holt(oVentas)
round(accuracy(fit2),2)
#Fit Holt- exponential
fit3 <- holt(oVentas,exponential = TRUE)
#Fit Holt - damped
fit4 <- holt(oVentas,damped = TRUE)
#Fit Holt - (exponential+damped)
fit5 <- holt(oVentas,exponential = TRUE,damped = TRUE)
# RESULTADOS PRIMER MODELO
fit1$model

#GRAFICOS PRIMER MODELO de Suavizado Exponencial
plot(fit3, type = "o", ylab = "Ventas",  flwd = 1, plot.conf = FALSE)
lines(window(ventas.zoo),type = "o")
lines(fit1$mean,col = 2)
lines(fit2$mean,col = 3)
lines(fit4$mean,col = 5)
lines(fit5$mean,col = 6)
legend("topleft", lty = 1, pch = 1, col = 1:6,
       c("datos","SES","Holt's","Exponencial",
         "Tendencia Amortiguada Aditiva","Tendencia Amortiguada Multiplicativa"))


#SEGUNDO MODELO:Modelos Estacionales (Holt-Winters)
fit6 <- hw(oVentas,seasonal = "additive")
fit7 <- hw(oVentas,seasonal = "multiplicative")

#GRAFICO MODELO
plot(fit7,ylab = "Ventas",
     plot.conf = FALSE, type = "o", fcol = "white", xlab = "Year")
lines(window(ventas.zoo),type = "o",col = "blue")
lines(fitted(fit6), col = "red", lty = 2)
lines(fitted(fit7), col = "green", lty = 2)
lines(fit6$mean, type = "o", col = "red")
lines(fit7$mean, type = "o", col = "green")
legend("topleft",lty = 1, pch = 1, col = 1:3, 
       c("datos","Holt Winters' Aditivo","Holt Winters' Multiplicativo"))



#Calculamos  los componentes
states <- cbind(fit6$model$states[,1:3],fit7$model$states[,1:3])
colnames(states) <- c("level","slope","seasonal","level","slope","seasonal")
plot(states, xlab = "Tiempo(anual)")
fit6$model$state[,1:3]
fitted(fit6)
fit6$mean


## Seleccionamos el Modelo ETS 
etsfit <- ets(oVentas)
#prediccion del modelo
fventas.ets = forecast(etsfit)
#Resultados
summary(fventas.ets)
#Resumimos la precisión del pronóstico.
accuracy(fventas.ets)

#GRAFICO MODELO ETS
plot(fventas.ets)
lines(window(ventas.zoo),type = "o")#window: extrae los valores de una TS

#Estado Actual y Prediccion
matrix(c(fventas.ets$mean[1:cOmit],ventas.zoo[(numob - cOmit + 1):numob]),ncol = 2)

#Seleccion del modelo ETS
etsfit2 <- ets(oVentas,damped = TRUE)
#Prediccion del modelo
fventas.ets2 = forecast(etsfit2)
#Resultados
summary(fventas.ets2)

#GRAFICO
plot(fventas.ets2)
lines(window(ventas.zoo),type = "o")
#Situacion Actual y Prediccion
matrix(c(fventas.ets2$mean[1:cOmit],fventas.ets$mean[1:cOmit],ventas.zoo[(numob - cOmit + 1):numob]),ncol = 3)

#GRÁFICO DE TODOS LOS MODELOS
plot(fventas.ets2)
lines(window(ventas.zoo),type = "o")
lines(fventas.ets$mean,type = "o",col = "red")

##Prediccion de datos más recientes
modelo2 <- ets(ventas.zoo , damped = T)
f.modelo2 <- forecast(modelo2)
plot(f.modelo2)


############### IPHONE ETS ##################

## Seleccionar ETS automatico
etsfit <- ets(ventas.iphone)
#Predecir el modelo
fventas.ets <- forecast(etsfit)
#Resultado
summary(fventas.ets)

#GRAFICO
plot(fventas.ets)
lines(window(ventas.iphone),type = "o")

############### IPAD ETS ##################

## Seleccionar ETS automatico
etsfit2 <- ets(ventas.ipad)
#Predecir el modelo
fventas.ets2 <- forecast(etsfit2)
#Resultado
summary(fventas.ets2)

#GRAFICO
plot(fventas.ets2)
lines(window(ventas.ipad),type = "o")


############### MAC ETS ##################

## Seleccionar ETS automatico
etsfit3 <- ets(ventas.mac)
#Predecir el modelo
fventas.ets3 <- forecast(etsfit3)
#Resultado
summary(fventas.ets3)

#GRAFICO
plot(fventas.ets3)
lines(window(ventas.mac),type = "o")


##################################################################################### 
################################### MODELOS ARIMA ################################### 
##################################################################################### 
#Se hace sobre la suma de todos los productos: sobre las ventas agregadas

require(forecast)
library(forecast)
require(xts)
library(xts)
require(ggplot2)
library(ggplot2)


#Construyo un dataframe
apple.dfnuevo <- data.frame(value = as.vector(ventas.zoo),
                     time = time(ventas.zoo))
ggplot(apple.dfnuevo) + geom_point(aes(x = time,y = value)) +
  geom_line(aes(x = time,y = value)) +
  ylab("Ventas") + ggtitle("Ventas Trimestrales Apple") + xlab("Trimestres")


#Log transformation:se hace para obtener ESTACIONARIDAD en varianza
ventas.log = log(ventas.zoo) 

##Antes la varianza se distribuía diferente, ahora con log es igual en todos los puntos
apple.dfnuevo.log <- data.frame(value = as.vector(ventas.log),
                      time = time(ventas.log))
ggplot(apple.dfnuevo.log) +
  geom_point(aes(x = time,y = value)) + 
  geom_line(aes(x = time,y = value)) + ylab("Ventas") + 
  ggtitle("Ventas Trimestrales LOG Appple") + xlab("Trimestres")


#Diferencia
  ##ACF: calcula la función de autocorrelación simple de una serie temporal
  #PACF: la función de autocorrelación parcial
  ##Por defecto el intervalo de confianza: 95%
  ##Observar cualquier variación atípica

ggtsdisplay(ventas.log)
ggtsdisplay(diff(ventas.log)) ##Tasa de Variación TRIMESTRAL
ggtsdisplay(diff(ventas.log,4)) ##Diferencia de lo de HOY con lo de hace 4 trimestres: Tasa de Variación ANUAL
ggtsdisplay(diff(diff(ventas.log,4),1)) ##Hago diferencia adicional: sobrediferenciada: no es problema, solo analisis exploratorio

#Numero de observaciones para comparar con las predicciones
cOmit2 = 6

#Tamaño datos
numob2 = length(ventas.zoo)

#sub_muestra
oVentas2 <- window(ventas.zoo,start = index(ventas.zoo[1]),end = index(ventas.zoo[numob2 - cOmit]))

#datos reales para predecir el comportamiento del modelo
pVentas2 <- window(ventas.zoo,start = index(ventas.zoo[numob2 - cOmit + 1]),end = index(ventas.zoo[numob2]))


#ARIMA MODEL: lambda 0- es aplicar el logaritmo
fit.arima = auto.arima(oVentas2,lambda = 0)
summary(fit.arima)
###PRIMERO PARTE REGULAR Y SEG ES LA ESTACIONAL Y 4 ES LA ESTACIONALIDAD
##HA ELEGIDO DE FORMA AUT LA TASA DE VARIACION ANUAL
#Depende de mi error anterior: hace 4 TRIMESTRES O UN AÑO


#ANALISIS DE RESIDUOS : tenemos que hacer la prueba de que loserrores del modelo(residuos), sean ruido blanco
ggtsdisplay(fit.arima$residuals)

#box-Ljung Test: para comprobar que todas las corr son iguales, hay ruido blanco y es correcto
Box.test(fit.arima$residuals,lag = 4, fitdf = 3, type = "Lj")
Box.test(fit.arima$residuals,lag = 8, fitdf = 3, type = "Lj")
Box.test(fit.arima$residuals,lag = 12, fitdf = 3, type = "Lj")
##UTILIZO 4 8 Y 12 PQ SON MULTIPLOS DE LA ESTACIONALIDAD
fventas.arima = forecast(fit.arima)

ggplot(apple.dfnuevo) + geom_point(aes(x = time,y = value)) + geom_line(aes(x = time,y = value))
+geom_forecast(fventas.arima,alpha = 0.4) + ggtitle("ARIMA: Predicción Apple")

fventas.arima

autoplot(forecast(fventas.arima)) #predicciones de arima



############### ARIMA IPHONE ##################
#submuestra
oiPhone <- window(ventas.iphone,start = index(ventas.iphone[1]),end = index(ventas.iphone[numob2 - cOmit]))


df_iphone <- data.frame(value = as.vector(ventas.iphone),
                        time = time(ventas.iphone))

ggplot(df_iphone) + geom_point(aes(x = time,y = value)) +
geom_line(aes(x = time,y = value)) +
ylab("Ventas") + ggtitle("Ventas Trimestrales iPhone") + xlab("Trimestres")

#Actuacion 
iphone.perf <- window(ventas.iphone,start = index(ventas.iphone[numob2 - cOmit + 1]),end = index(ventas.iphone[numob2]))

##MODELO ARIMA
fit.iphone <- auto.arima(oiPhone,lambda = "auto")
summary(fit.iphone)

#Analisis residual
ggtsdisplay(fit.iphone$residuals)

#box-Ljung Test
Box.test(fit.iphone$residuals,lag = 4, fitdf = 3, type = "Lj") 
Box.test(fit.iphone$residuals,lag = 8, fitdf = 3, type = "Lj") #pvalor mas pequeño
Box.test(fit.iphone$residuals,lag = 12, fitdf = 3, type = "Lj")

fventas.iphone <- forecast(fit.iphone)

ggplot(df_iphone) +
  geom_point(aes(x = time,y = value)) +
  geom_line(aes(x = time,y = value)) +
  geom_forecast(fventas.iphone,alpha = 0.4) +
  ggtitle("ARIMA: Predicción iPhone")



############### ARIMA IPAD ##################
oiPad <- window(ventas.ipad,start = index(ventas.ipad[1]),end = index(ventas.ipad[numob2 - cOmit]))


df_ipad <- data.frame(value = as.vector(ventas.ipad),
                        time = time(ventas.ipad))

ggplot(df_ipad) + geom_point(aes(x = time,y = value)) +
  geom_line(aes(x = time,y = value)) +
  ylab("Ventas") + ggtitle("Ventas Trimestrales iPad") + xlab("Trimestres")

#Actuacion
ipad.perf <- window(ventas.ipad,start = index(ventas.ipad[numob2 - cOmit + 1]),end = index(ventas.ipad[numob2]))

##MODELO ARIMA
fit.ipad <- auto.arima(oiPad,lambda = "auto")
summary(fit.ipad)

#Analisis residual
ggtsdisplay(fit.ipad$residuals)

#box-Ljung Test
Box.test(fit.ipad$residuals,lag = 4, fitdf = 3, type = "Lj") 
Box.test(fit.ipad$residuals,lag = 8, fitdf = 3, type = "Lj") #pvalor mas pequeño
Box.test(fit.ipad$residuals,lag = 12, fitdf = 3, type = "Lj")

fventas.ipad <- forecast(fit.ipad)

ggplot(df_ipad) +
  geom_point(aes(x = time,y = value)) +
  geom_line(aes(x = time,y = value)) +
  geom_forecast(fventas.ipad,alpha = 0.4) +
  ggtitle("ARIMA: Predicción iPad")

############### ARIMA MAC ##################
oMac <- window(ventas.mac,start = index(ventas.mac[1]),end = index(ventas.mac[numob2 - cOmit]))


df_mac <- data.frame(value = as.vector(ventas.mac),
                      time = time(ventas.mac))

ggplot(df_mac) + geom_point(aes(x = time,y = value)) +
  geom_line(aes(x = time,y = value)) +
  ylab("Ventas") + ggtitle("Ventas Trimestrales Mac") + xlab("Trimestres")

#Actuacion
mac.perf <- window(ventas.mac,start = index(ventas.mac[numob2 - cOmit + 1]),end = index(ventas.mac[numob2]))

##MODELO ARIMA
fit.mac <- auto.arima(oMac,lambda = "auto")
summary(fit.mac)

#Analisis residual
ggtsdisplay(fit.mac$residuals)

#box-Ljung Test
Box.test(fit.mac$residuals,lag = 4, fitdf = 3, type = "Lj") 
Box.test(fit.mac$residuals,lag = 8, fitdf = 3, type = "Lj") #pvalor mas pequeño
Box.test(fit.mac$residuals,lag = 12, fitdf = 3, type = "Lj")

fventas.mac <- forecast(fit.mac)

ggplot(df_mac) +
  geom_point(aes(x = time,y = value)) +
  geom_line(aes(x = time,y = value)) +
  geom_forecast(fventas.mac,alpha = 0.4) +
  ggtitle("ARIMA: Predicción MAC")
