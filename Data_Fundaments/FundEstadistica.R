####### PRÁCTICA ESTADISTICA I #########
##funciones de las distribuciones de probabilidad 
#raiz: foo (nombre de la distribucion)

##dfoo: funcion de densidad
##pfoo: funcion de distribucion (prob acumulada)
##qfoo: funcion inversa F^-1(x)
##rfoo: generador (pseudo)aleatorio de valores
library(stats)

###1. BINOMIAL
#INFO: P(precio VIERNES)<P(precioLUNES)= 0.8 
#comprar viernes y vender lunes - 4 semanas 
##probabilidad(rdtos + 3/4 semanas) = probabilidad(rdtos + 4 weeks)
##definimos n como el num de veces que se repite, es decir, n=4 y el num de veces que se quiere conseguir n=3
##p es la probabilidad de que el evento ocurra, o sea, 0.8
##POR TANTO
m <- dbinom(3,4,0.8)
m
p <- dbinom(4,4,0.8)
p
##ambas generan la misma funcion de densidad
sum(m,p) ##esta funcion no nos dice nada relacionado con la probabilidad que buscamos 

##2. NORMAL
##P(380<x<1200 mill€) pero la prob de ventas superiores a 1 millon 
##al ser una variable continua, se utiliza la funcion para conocer la distribucion uniforme del intervalo
##si el max y el min no se definen se asume 1 y 0
b <- punif(1000000, min=380000, max=1200000, log.p = FALSE)
b
##la probabilidad de que las ventas sean mayores a 1m € es del 75% aproximadamente

##3. Prob 1 acuerdo: 0.6%
#se cierran 250 de 400
s <- dbinom(250,400,0.06)
s
###la probabilidad de que se cierren 250 de los 400 acuerdos segun la 
#probabilidad de que se cierre un acuerdo, tiende a 0

