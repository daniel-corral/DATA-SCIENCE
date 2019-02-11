
################### EJERCICIO CLIMA #####################################

library("markovchain")


###3 estados y las prob de las matrices de transicion
estados<- c("soleado", "nublado", "lluvioso")
byRow <- TRUE
mt <- matrix(data = c(0.60, 0.3, 0.1,
                      0.4, 0.4, 0.2,
                      0.25, 0.45, 0.30), byrow = byRow,
             nrow = 3,dimnames = list(estados, estados))
##las prob deben seguir el mismo oerden que los estados definidos anteriormente

##0.6 es la prob de que si ghoy es soleado mañana tambien
##0.4 es de estando 2(nublado), mañana sea soleado
##el 0.3 es que si es lluvioso hoy mañana tambien

cmclima <- new("markovchain", states = estados,
               byrow = byRow,
               transitionMatrix = mt, name = "clima")
cmclima ##probabilidades de la matriz de transicion
##vamos a plotear esto
plot(cmclima)

sinicial <- c(0, 1, 0)
dosdespues <- sinicial * (cmclima * cmclima)  
sietedespues <- sinicial * (cmclima ^ 7) ##elevo la mat de transicion a 7
dosdespues ##la prob de que pasados 2 dias este nublado es 0.37
sietedespues

#Estado estacionario
steadyStates(cmclima) ##el comportamiento a largo plazo cuando n tiene infinito
##es decir el limite: cual seria el comportamiento a largo plazo

#Simulacion aleatoria del estado del clima durante 100 d?as (comprobar que >n... <DS) 
#distribucion prob a largo plazo
#el 0.4680 para los soleados, nublados...
simulacionclima <- rmarkovchain(n = 1000, object =cmclima , t0 = "nublado")
simulacionclima365 <- rmarkovchain(n = 365, object =cmclima , t0 = "nublado")

##simulacion: desarrollar una realidad virtual: con unas info historicas: crear un futuro
  #jhacer evaluaciones de medidas de efectividad..
##simulando cual seria el clima en 1000 dias partiendo de una simulacion en nublado con una matriz de transicion y nuestra cadena
#simulacion del estado del tiempo proxima semana 
simulacionclima[1:7]
simulacionclima365[1:7]

#Estimador maximo verosimilitud (mle=maximum likelihood estimator)
emvclima <- markovchainFit(data = simulacionclima, method = "mle", name = "EMVclima")                                 
#markovchainFit, devuelve una CM de una secuencia dada.

emvclima$estimate

emvclima$standardError

