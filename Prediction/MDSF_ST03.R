 
# Forecast Models I
# ETS Models
# Forecast KO sales 
#(c)Ricardo A. Queralt
# 

require(forecast)
library(forecast)
require(xts)
library(ggplot2)
require(ggplot2)
library(ggfortify) #Plot Monthplot

#read data from CSV file
rawData <- read.csv("~/Desktop/DATASCIENCE/PRACTICASR/ko.csv", sep = ";", dec = ",")

#Create a XTS object
#Convert data to XTS
mData = xts(rawData$Ingresos, order.by = as.Date(as.character(rawData$Fecha),"%Y%m%d"),frequency = 4)
##as character lo hemos metido- sin esto fallaba 
xVentas = xts((rawData$Ingresos),order.by = as.POSIXct(strptime(rawData$Fecha,"%Y%m%d")),frequency = 4)
##en este con as.POSI es el indice para la posicion de la fecha
View(xVentas) ##el indice son las fechas, le faltan dias y las voy a transormar en TRIMESTRAL en el sigiente

#Generate quarterly data
xVentas = to.quarterly(xVentas)
View(xVentas) ##con esto lo tengo como un FLOW- se puede cambiar y puedes construir el trimestre como quieras

#Transform to zoo data (forecast package)
zVentas = as.zoo(xVentas$xVentas.Close) ##aqui esto no es teorico del palo para la prediccion sino para ver cosas 
#tengo xts pero lo voy a convertire en zoo- voy a quitarle lo que me sobra
View(zVentas)

#Change name
names(zVentas) = "Ventas" ##le cambio el nombre


##Plot Serie
autoplot(zVentas) + ggtitle("Ventas Trimestrales CocaCola") + xlab("Trimestres") + ylab("Ventas")
#tendencia y estacionalidad
##fusion y consolidacion de balances en 2010 por eso pegan un salto 

#Seasonal Plot: grafico para estudiar la estacionariedad
ggfreqplot(as.ts(zVentas),freq = 4,nrow = 1,facet.labeller = c("1T","2T","3T","4T")) + ggtitle("Ventas Trimestrales")
 #aumenta la eetacionariedad: los duenios anticipan y empizan a comprar
#salen todos los primeros trimestres con las ventas y la raya azul es la media y vemos que en 
  #el segunto trimestre hay mas por lo indicado anteriormente

#Select number of observation to compare forecast: voy a omitir 4 datos para ver como es de bueno mi modelo predictivo
#cuento num de varuables y me quedo con las submuestras
cOmit = 4 #omit omitir y c constantante

#Data Size
nObs = length(zVentas)

#sub_sample
#oVentas=zVentas[1:(nObs-cOmit),] ##ventas en formato zoo: esto es mas general xts te da hasta fecha y hora con mil de seg
oVentas <- window(zVentas,start = index(zVentas[1]),end = index(zVentas[nObs - cOmit]))

index(zVentas) ##te dice lo que necesitas coger

#Fit Simple Exponential Smoothing ##esto no se necesita para la teoria
fit1 <- ses(oVentas)

#Fit Holt
fit2 <- holt(oVentas)

#Fit Holt- exponential
fit3 <- holt(oVentas,exponential = TRUE)

#Fit Holt - damped
fit4 <- holt(oVentas,damped = TRUE)

#Fit Holt - (exponential+damped)
fit5 <- holt(oVentas,exponential = TRUE,damped = TRUE)

# Results for first model:
fit1$model

#Plot models fitted - grafico modelos pero no son validos pq no estan estimando el componente estacional
plot(fit3, type = "o", ylab = "Ventas",  flwd = 1, plot.conf = FALSE)
lines(window(zVentas),type = "o")
lines(fit1$mean,col = 2)
lines(fit2$mean,col = 3)
lines(fit4$mean,col = 5)
lines(fit5$mean,col = 6)
legend("topleft", lty = 1, pch = 1, col = 1:6,
       c("Data","SES","Holt's","Exponential",
         "Additive Damped","Multiplicative Damped"))

##salen los CI de 80 y 85% y el verde que es el multiplicativo es el mejor modelo

#seasonal model Holt-winters
fit6 <- hw(oVentas,seasonal = "additive")
fit7 <- hw(oVentas,seasonal = "multiplicative")

#Plot models
plot(fit7,ylab = "Ventas",
     plot.conf = FALSE, type = "o", fcol = "white", xlab = "Year")
lines(window(zVentas),type = "o",col = "blue")
lines(fitted(fit6), col = "red", lty = 2)
lines(fitted(fit7), col = "green", lty = 2)
lines(fit6$mean, type = "o", col = "red")
lines(fit7$mean, type = "o", col = "green")
legend("topleft",lty = 1, pch = 1, col = 1:3, 
       c("data","Holt Winters' Additive","Holt Winters' Multiplicative"))



#Calculate Components ##nivel tendencia y componente estacional
states <- cbind(fit6$model$states[,1:3],fit7$model$states[,1:3])
colnames(states) <- c("level","slope","seasonal","level","slope","seasonal")
plot(states, xlab = "Year")
fit6$model$state[,1:3]
fitted(fit6)
fit6$mean
#en el multiplicativo (derech) en el componente estacional parece que se hace mas pequeño
  #en la multiplicativa estas en porcentaje y en la primera tienes nivel entonces tienes nums
## Select automatic ETS no necesita ninguna coma
etsfit <- ets(oVentas)
#forecast model
fventas.ets <- forecast(etsfit)
#Results
summary(fventas.ets)
summary(etsfit) ##para ver que modelo ha seleccionado :la M es multipicativa: para la E 
  #ETS(M,A,M) : el error crece cuando crece la tendencia
  #tambien tienes los errores de medida: cuando tienes varios modelos puedes compararlo: hace prediccion en cada uno de los instantes
#Plot

plot(fventas.ets) ##la predicc y los int de confianza de las predicciones
lines(window(zVentas),type = "o")

#Actual and Forecast: ver las reales con las predicciones
matrix(c(fventas.ets$mean[1:cOmit],zVentas[(nObs - cOmit + 1):nObs]),ncol = 2)


## Select automatic ETS: prediccion es la primera y la otra la segunda
##la seleccion automatica puede estar sesgada: en el graf anterior hay tendencia aditiva: puede que sea el mejor AIC pero es mejor meterle una amortiguada
#parece que esta amortiguada pero ha cogido una aditiva: es como si se estuviera moviendo la serie temporal sobre una linea recta
##le voy a decir que me elija el mejor modelo amortiguado
etsfit2 <- ets(oVentas,damped = TRUE)
#forecast model
fventas.ets2 <- forecast(etsfit2)
#Results
summary(fventas.ets2)

#Plot
plot(fventas.ets2)
lines(window(zVentas),type = "o")

#Actual and Forecast
matrix(c(fventas.ets2$mean[1:cOmit],fventas.ets$mean[1:cOmit],zVentas[(nObs - cOmit + 1):nObs]),ncol = 3)

#Plot all models##la azzul es la amortiguada y la roja es la lineal: no hay dif muy grandes pero se ve en los maximos
##lo ultimo seria hacer prediccion hacia el futuro
plot(fventas.ets2)
lines(window(zVentas),type = "o")
lines(fventas.ets$mean,type = "o",col = "red")


###PREDICCION EN TIEMPO REAL
model2 <- ets(zVentas, damped = TRUE )
f.model2 <- forecast(model2)
plot(f.model2)

