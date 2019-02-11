#### MODELOS OUTLIERS ######

#Carga de librerias
library(TSA) ##detect
library(ggplot2)
library(forecast)
data(airmiles) #esto para que te funcione air.m2

data(co2)
autoplot(co2)
m1.co2 = sarima(co2, order = c(0,1,1), seasonal = list(order = c(0,1,1), period = 12))
m1.co2
##no vemos outliers 

##modelo de lineas aereas: lo pruebo sobre el modelo nosobre la serie temporal

detectAO(m1.co2) 
#detectamos a ver si hay outliers aditivos: sobre la y: dice que no hay 

detectIO(m1.co2) #outliers innovativos: encuentra 1 que esta en el dato 57

m4.co2 = arimax(co2,order = c(0,1,1),seasonal = list(order = c(0,1,1),period = 12), io = c(57))
m4.co2 #que me estime cual es el efecto
# el arimax: modelo arima con una x: le estas  poniendo una variable de regresion
#le has añadido un modelo de regresion: es decir se convierte en esto: variables economicas.. ventas....
#va a tomar 0 en todos menos un 1 en el valor 57
#me da en el 57 lo que vale el valor innovativo

#volvemos a la serie de venta de lineas aereas
#la transferencia del efecto de intervencion:modelo de millas en el 69 -- esto solo son transferencias si introducir OUTLIERS
#la intervencion si que la conozco pero los outliers no, los tengo que conicer a partir del modelo
air.m2 = arimax(log(airmiles),order = c(0,1,1),
              seasonal = list(order = c(0,1,1),period = 12),
              xtransf = data.frame(I911 = 1*(seq(airmiles) == 69),
                                 I911 = 1*(seq(airmiles) == 69)),
              transfer = list(c(0,0),c(1,0)),
              method = 'ML')

detectAO(air.m2)  #aqui nos da el 25- el 25 es aditivo
detectIO(air.m2) #aqui te dice el innovativo: el 25 y 81

#en el m3 metemos los dos como innovativo: c(25, 81)

#aqui hago la correcion: detectame si hay outliers y ahora me dice que no hay


air.m3=arimax(log(airmiles),order=c(0,1,1),
              seasonal=list(order=c(0,1,1),period=12),
              xtransf=data.frame(I911=1*(seq(airmiles)==69),
                                 I911=1*(seq(airmiles)==69)),
              transfer=list(c(0,0),c(1,0)),
              io=c(25,81),
              method='ML')

air.m3 #-0.1346 nuestro modelo es significativo: tenemos que hacer contraste t student(en summary si): como no aparece 
# si t student es mayor 2 es sign: divvidir parametro entre lo de s.e(se o desv tipica del estimador)

#-0.4728/0.0667 = mayor que 2: ES SIGNITFICO : entomces si te sale alguno que no es asi

detectAO(air.m3)
detectIO(air.m3) #aqui queda un outlier nuevo: introducir el 84: antes no sale y ahora si: como disminuye la desv tipica y antes habia outliers, un dato que antes no era outlier ahora lo es pq disminuyen las lineas

#vamos a estimar el modelo pero en lugar de poner 1,0 voy a poner 3,0: en el denominador delta hasta elevado a 3

air.m3.mod3=arimax(log(airmiles),order=c(0,1,1),
              seasonal=list(order=c(0,1,1),period=12),
              xtransf=data.frame(I911=1*(seq(airmiles)==69),
                                 I911=1*(seq(airmiles)==69)),
              transfer=list(c(0,0),c(3,0)),
              io=c(25,81),
              method='ML')
air.m3.mod3 #aqui ves que no es significativo: al dividir las cosas entonces no es el modelo correcto

air.m3.84=arimax(log(airmiles),order=c(0,1,1),
                   seasonal=list(order=c(0,1,1),period=12),
                   xtransf=data.frame(I911=1*(seq(airmiles)==69),
                                      I911=1*(seq(airmiles)==69)),
                   transfer=list(c(0,0),c(1,0)),
                   io=c(25,81,84),
                   method='ML')
air.m3.84

detectAO(air.m3.84)
detectIO(air.m3.84)
#lambda: procedimiento que utiliza para detectar como es el procedimiento: es como si fuera la t de significacion
##YA NO QUEDAN MAS OUTLIERS!!!!!!!!!!!!!! en algun momento quedan todos dentros
#una serie con muchos outliers..


#sale que el 0,0 no es significativo entonces quito una variable y no hay outliers nuevos
air.m3.sig=arimax(log(airmiles),order=c(0,1,1),
                 seasonal=list(order=c(0,1,1),period=12),
                 xtransf=data.frame(I911=1*(seq(airmiles)==69)),
                 transfer=list(c(1,0)),
                 io=c(25,81,84),
                 method='ML')
air.m3.sig

detectIO(air.m3.sig)
detectAO(air.m3.sig)
##ya esta todo bien


########## TEORIA SOBRE LOS ARMAX Y REGRESION CON ERRORES ARIMA ########
  #ARMA: EL ERROR LLAMADO Z
  #DECIMES: ARIMAX (NO ESTACIONARIO )- AÑADIENDO BXT: LO EXPRESO EN FOORMA DE RETARDO
#LA RELACION CAUSAL: XT Y Y DE T: ES EN EL INSTANTE: LO QUE LE OCURRE A UNA EN EL INSTANTE AFECTA A LA OTRA
#EN LA OPERACION RETARDO AFECTA XT , XT-1.... CON RETARDO

#es decir, afecta de forma dinamica: MODELO DINAMICO
#el ruido es el arima aqui

#una regresion con modelos ARMA- nt sigue un ARMA: los errores tienen este problema

#la diferencia es q con arma no estas inplicitanmente introduciendo DINAMISMO en la xt(es decir no le pones xt-1...)

#en armax: efecto es contemporaneo e historico

#cuando tengo una variable y explicada por una x: quiero que exista un dinamismo: generalizo un modelo armax: el polinomio solo me sale BETA
#la dif entre reg dinamica o funcion de transferencia con otro modelo: permites que haya un polinomio en el numerador

#la regresion con residuos: polinomio 0 y grado 0- tienes una regresion

##vamos a estimar funciones de transferencia: mo solo dice si la variable x explica a la y sino cual es el DINAMISMO DE ESTO
  #modelo de negocio en el que pase esto-el gasto en publicidad quw me gasto todos los meses para anunciar FORMULA 1
#si anuncions en movistar plus aunque lo pongan aqui: coste de oportunidad: es un gasto
#X: gasto en publicidad
#Y: num de personas que lo ven

#esto vaa afectar a mas meses... es decir a la gente que visualiza esto hace que el resultado sea que lo siga viendo: cual es el dinamismo? VER LOS POLINOMIOS(hay una tecnica quepermite ver las ordenes de los polinomios)
#el problema: IDENTIFICAR LAS FUNCIONES DE TRANSFERENCIA
#yt : constante mas nuestro polinomio(hay que determinar la b): gasto de publicidad y hasta el mes siguiente no tiene el efecto
  #b: retardo con el que el efeto de x empieza a afecta la a la y


##PARA DETERMINAR b, s y r:
#1. estimas el modelo con un num de retardos suf mente largos: xt-1..... yestimo un AR pequeño para el ruido
#si no estacionarios: diferenciar las R
#dead time o retardo: numero de pesos vsi es 0 en los dos primeros es q hay un retardo de 2no empieza hasta el tiempo 2

#dibujo los pesos: y veo la forma de este decaimiento de los pesos
#si tengo muchos retardos esq el denominador espoco importante

##s: numero de retardos que no son 0 hasta que empieza el decaimiento

#cal errores, identificamos ARIMA y estimamos modelo de forma conjunta

##mirar el 1.4..0.7 (no signifivativo y empiez aa ser significativo)
#3 valores no significativos: pesos iniciales que son 0, por tanto s es 0 no hay numeros antes del decaimiento
#r=1(tipo de decaimiento- si es exponencial 1 si es mas complicado pones 2), y que b=3

library(TSA)
library(Hmisc)
library(astsa)

sales_140 <- window(sales,end = 140) #ventas
lead_140 <- window(lead,end = 140) ##variable lider: publicidad y eso que lo explica
#hacemos un window para quedarnos con los 140 primeros


lead_140_Z <- lead_140 - mean(lead_140) #le quitamos la media
sales_140_D <- diff(sales_140) #hacemos las diferrencias: PARA HACER LAS ESTACIONARIAS
lead_140_D <- diff(lead_140_Z)

library(dynlm)
###MODELO DE ESTIMACION
mod0 <- dynlm(sales_140 ~ L(lead_140, 0:15) + L(sales_140, 1)) 
mod0
summary(mod0)

#0,1,2: b=3
#decaimiwnto empieza en 4 no en 3 y queempieza a ser mas o menos exponencial
#s: vale 1: el decaimineto empieza despues de 3(en 4 por tanto hay uno antes de 3)
#r=1 o 2 para probar a ver si es significativo y sino lo eliminas
#los significativos importan solo para los primeros 3
#LM REGRESION LINEAL DINAMICA: LAG: DE RETARDO - variablea explica:
  #ventas en funcion de la publicidad: cuantos retardos del 0 al 15 publi a la -1 
  #hasta la -15 + las ventas retardas es solo 1 pq es el autorregresivo de orden 1


mod0
###esto es xactamente lo mimso pero con ARIMAX
mod0 <- arimax(sales_140,
               order = c(1,0,0), #autorregresivo de orden 1: el modelo de transf: 0 (denominador) y 15 que tienes 15 retardos
               include.mean = TRUE,
               xtransf = lead_140,
               transfer = list(c(0,15)),
               method = "ML")

mod0
#DIFERENCIA ENTRE PRIMERO Y SEGUNDO, en el segundo hems hecho una funcion de transferencia: 
  #yt=w0+w1b+w2b+...+.. -- pq es una r1
  ##at tiene polinomio en el segundo: si lo quieres quitar 

#apliques AUTORREGRESIVO SOBRE EL ERROR(ruido blanco) O SOBRE LA SERIE TEMPORAL:: hay está la diferencia

#estamos estimando lo mismo pero de forma difernte: tenemos un ARMA y un ARIMAX o funcion de transferencia

###impte: IDENTIFICAR EN EL SEGUNDO MODELO Y NO EN EL PRIMERO

#sale que el moelo NO es ruido blanco: voy a trabajar con la diferencia(la estacionaria) en e de con la original

tsdisplay(mod0$residuals)

mod1 <- arimax(sales_140_D,
               order=c(1,0,0),
               include.mean=TRUE,
               xtransf=lead_140_D,
               transfer=list(c(0,15)),
               method="ML")


mod1
plot(mod1$coef[3:18],type="h") # a parti del 4 se ve el decaimieto: el decaimiento es infinito aunque parezca que en 15 para

modelolm <- lm(sales_140_D~lead_140_D) ##la publicidad no explica las ventas pq no estamos haciendo dinamismo en el modelo sino incluyendo funciion de transferencia
##esto solo se aplica sobre modelos de regresion
summary(modelolm)

#####MODELO DE FUNCION DE TRANSFERENCIA LA X ES EL IMPUOSO O EL ESCALON
mod <- arimax(sales_140_D,
              order=c(0,0,1),
              include.mean=TRUE,
              fixed=c(NA,NA,NA,0,0,0,NA),
              xtransf=lead_140_D,
              transfer=list(c(1,3)), ##EL 1, 3 DE LA FUNCION DE TRANSFERENCIA- EXPLICADO ABAJP
              method="ML")

mod


########################
##EL 1 ES POR EL ORDEN DEL DENOMINADOR? 
  #constante: media movi, luego viene el 1

 #IMPORTANTISISIISIISISMO
 #parametros que estimamos: media movil, constante (no autoregresiva), el delta 1, w (0,3)= sabemos que w0, w1 y w2= son 0 y los ponemos a priori
#la media NA pq no la fijo, ... asi iguales.. las w0 a 2 las he fijado en 0



##con este modelo de transferencia vamos a hacer prediccion
  #ma1: media movil, intercept esla constante,  T1-AR1 es el delta 1... y asi sucesivamente

##LOS MODELOS SE HACEN con causalidad de efecto de una sola x

#CONSLUION: ST y tenemos q hacer modelo de relacion causal: funcion de transferencia



##ANALISIS CAUSAL DE IMPACTOS
############### AHORA LA IDEA: como Google: oplamntea libreria de modelos de funcion de transferencia
  #causalImpact
#vendemos publicidad y tenemos alguna forma de darle herramienta a un cliente: detectar si estan recibiendo algun tipo de beneficio por la publicidad
#####https://github.com/google/CausalImpact
#estamos haciendo un modelo de transferencia
#analisis microeconomico: varian: jefe Big Data google

#como se ve afectada la pag web ccon los clicks: intervenciones cuando no hay anuncios cuando hay anuncios y cuando dejas de hacerlo
#el modelo <- se estima: y apartir de este modelo se hace prediccion (intervention)
  #google: si mi modelo es correcto pq modeliza bien los clicks que se hacen: teoricamente la prediccion es el comportamiento que tiene si no hago nada
#la diferencia entre prediccion y lo que hace es el EFECTO DE INTERVENCION

#azul clarito: intervalo de confianza: hasta que no se salga el efecto no es importante pq no se sale de lo q establecemos

#cuando dja de hacer ubli se estima perfectamente

#seg grafica: si 0 esta dentro del intervalo esq no hay efecto pq la serie azul es el EFECTO y luego se hace casi significativo hasta el post intervention que deja de ser significativo

#puedes trabajar con datos de todo pero ESTA MAS AJUÇSTADO CON DATOS CON MAS ESTACIONALIDAD: diarios
  #sino el modelo da una prediccion PLANA

library(devtools)
library(CausalImpact)
library(tseries) #vamos a probar el efecto del BREXIT sobre el banco santander

cotizaciones <- get.hist.quote(instrument = "san.mc", start = as.Date("2017/01/01") - 500,
                                #Sys.Date() - 500,#-500 no nos coje la fecha del Brexit
                               end = as.Date("2017/01/01") + 100 , quote = "AdjClose",
                               provider = "yahoo", #me bajo los datos directamente de yahoo
                               compression = "d", retclass = "zoo")
cotizaciones


brexit <- as.Date("2016-06-23") #fecha en la que empieza el impacto
#aqui le indico cual es el preriodo en el que es pre hasta el brexit-1 le quito un dia, luego es el brexit y lo que hay más
impact <- CausalImpact(cotizaciones,
                       c(min(index(cotizaciones)), brexit - 1),
                       c(brexit, max(index(cotizaciones))))
plot(impact, metrics = c("original", "pointwise","cumulative"))
#esto: vemos que baja pero el efecto del brexit es lo que sale FUERA DEL INTERVALO

#tipica prediccion de una ST financieroa: plana- es mejor que el anterior 

#cuando empieza a estar en el intervalo es 0 cuando esta fuera no es 0 y el efecto existe pq hay una caida

##para Santander fue bueno pq es positiva- se ve que es plana que es lo tipico

#MONTECARLO: genera numeros aleatorios de una distribucion(ya sea una normal, uniforme.....) y le das una serie de num por ej entre 10 y 20 entonces y la generamos mediante una distribucion MONTECARLO
summary(impact)
summary(impact, "report") ##hace el report automatico
