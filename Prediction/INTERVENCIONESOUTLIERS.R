####
#### HTML NUEVO CLASE 21 nov
#A veces nuestros datos tienen datos atipicos: outliers: ver si tienen algun tipo de epxlicacion economica para ver el valor cuantitativo de ellos

#ej: como afecto el 9-11 a la venta de billetes de aerolineas <- este valor atipico genera INTERVENCION

#campañas de publicdad, salida de nuevos productos(hay intervenciones)

#objetivo: valorarloy a la vez eliminarlo

#vamos acoger la serie temporal que queremos predecir: yt: compuesta de arima(Nt) y una variable: la de intervencion que tiene elmodelo
#modelo dinamico: determinar que son polinomios y cual es el orden de los polinomios
  #variable de intervencion: escalon o pulso (donde se produceel salto)

#nivel de ventas en el que reduces el 10% o baja el iva entonces tienes modelo nuevo; el pulso es un efecto puntual(ej: huelga)
  #al escalon le podemos alica un polinomio de retardo
  #b retarda un periodo la lserie temporal
  #pmega: parametro del efecto: si es negativo lo disminuye y viceversa


#pega un salto y se van disminuyendo: ej: aumento de costes fijos te provoca que va subiendo hasta que se genera una situacion(subida de IVa): no es inmediato: se genera una subida y luego permanece
  #si se va reduciendo (impuestos) nunca llevas al nivel que tenias antes

#rampa: al principio tienes un efecto impulso muy grande y luego 


# GOAL: como afectan los SHOCKS a mi variable de serie temporal!!!!!!!!!!!!!!!

#cuando trabajamos sobre escalones
  #efecto retardo dice que si aumento mis ventaas este mes por ejemplo (aparece la a) hasta el mes que viene no vere el efecto; sino aparece la b

#w1: y w2: dos saltos: uno que disminuye y el otro que es una rampa sobre el impulso- si haces la dif de un escalon tienes un impulso


####MODELOS:: solo los podemos hacer sabiendo el SHOCK que lo ha causado: si poner rampas steps o saltos

##ejeplo: coges el paquete de la libreria 

library(TSA)
library(ggplot2)
library(forecast)

data(airmiles) #series de millas aereas
myData <- as.zoo(airmiles)
plot(log(airmiles),ylab = 'Log(airmiles)',xlab = 'Year') #el efecto en 2000 es gradual (no puntual, ya que le falta para llegar al nivel de antes_ le sigue afectando el 9-11)

autoplot(log(myData)) + ylab('Log(airmiles)') + xlab('Year')

tsdisplay(myData) #no es ruido blanco sino AUTOCORRELACIONES SIGNIFICATIVAS
autoplot(diff(diff(myData,12)))

tsdisplay(diff(diff(myData,12))) #diferencia de orden 12 pq hayuna correlacion muy grande en 12: pq hay una autocorrelacion muy grande
#me aparecen 2 valores atipicos pero son el mismo

##siempre que tengas un atipico apareceran tantos atipicos como valores tenga: de 12 es 1 

##el ruido blanco siempre se mira sobre la serie original: si ya es ruido blanco no haces los modelos: media 0 varianza constante: que esten dentro de las estas y miramos que los residuos sean ruido blanco

acf(as.vector(diff(diff(window(log(airmiles),end=c(2001,8)),
                        12))),lag.max=48)

# Additive outliers are incorporated as dummy variables in xreg.
# Transfer function components are incorporated by the xtransf and transfer
# arguments.
# Here, the transfer function consists of two parts omega0*P(t) and 
# omega1/(1-omega2*B)P(t) where the inputs of the two transfer
# functions are identical and equals the dummy variable that is 1 at September
# 2001 (the 69th data point) and zero otherwise.
# xtransf is a matrix whose columns are the input variables.
# transfer is a list consisting of the pair of (AR order, MA order) of each
# transfer function, which in this examples is (0,0) and (1,0).

  
  #voy a hacer un omega 0... en el primer mes se deja de volar
  #omega partido de un omega 2 b a partir de este momento empieza a reducirse

#el modelo que se utiliza es el arimax
#seleccion del modelo automatica
  #orden: es un arima 0,1,1 0,1,1 en 12: estacional
  #creo un DF: le llamo intervencion en el 9-11 I911:me hace una sec que desde 1 y va a ver cuando es igual a 69: le dato 69: creo un TRUE FALSE que va a ser FALSE en todo execpto en 69
air.m1=arimax(log(airmiles),order=c(0,1,1),
              seasonal=list(order=c(0,1,1),period=12),
              xtransf=data.frame(I911=1*(seq(airmiles)==69), #69 lo veomanual en la grafica: necesito 2 col iguales pq necesito 2 impulsos
                                 I911=1*(seq(airmiles)==69)),
              transfer=list(c(0,0),c(1,0)), #fncion de transferencia: 0,0 y 1,0 #se one el den y luego el num- es decir el pol para la primera variable y el de la segunda variable
              xreg=data.frame(Dec96=1*(seq(airmiles)==12), #que datos voy a meter en la regresion de la a <- dic del 96 es igual a 12
                              Jan97=1*(seq(airmiles)==13),Dec02=1*(seq(airmiles)==84)), ###estos datos son atipicos puntuales: son un efecto impulso
              method='ML') #estime por maxima verosimilitud
air.m1
#3 atipicos puntuales

##el efecto puntual:e s de casi un 10% en la caida de los aviones lo miras en I911-MAOs
##hay una recuperacion del 0.2... esta en la ultuma de los coeffi

#el orden del polinomio en b: es 0 pq es un cocientes pq el grado e la bb en w es 0
  #el orden del polinomio en el den: 




plot(log(airmiles),ylab='Log(airmiles)')
points(fitted(air.m1)) ###mirar el ajuste: es bastante bueno

air.m1$coef

###GRAFICO EL EFECTO

Nine11p=1*(seq(airmiles)==69)
plot(ts(Nine11p*(-0.0949)+
          filter(Nine11p,filter=.8139,method='recursive',side=1)*(-0.2715), ##se ha hecho el filtro para hacer una regresion aritmetica (con metodo recursivo)
        frequency=12,start=1996),type='h',ylab='9/11 Effects')
abline(h=0)

##en la grafica se va reduciendo pq es un alisado exponencial: por lo de arriba del metodo recursivo al final 0,algo al cuadrado acaba dando 0: es decir al principio vas recuperandote poco a poco 


##PUTLOERS
data(co2)
m1.co2=arima(co2,order=c(0,1,1),seasonal=list(order=c(0,1,1),period=12))
m1.co2
