####################################################################
##################    Cadenas de Markov    #########################
####################################################################
#Def: un proceso de markov es un proceso aleatorio con la probabilidad de que el valor actual
    #y los valores futuros son independientes de los valores pasados
#Es decir, que si tenemos la informacíon presente del proceso,saber como llegó alestado actual
    #no afecta las probabilidades de pasar a otro estado en el futuro.

#################    EJERCICIO 1       ################
#X:"Siniestralidad, durante el año t"


#vemos un FENOMENO DICOTOMICO


library(markovchain)

###Definimos el estado inicial:
  ##No presentado en el año 0 y presentado en el 1: 20% (0)
  ##Presentado en el año 0 y presentado en el año 1: 5% (1)


##CARATC GENERALES: ESTADOS Y MATRIZ DE TRANSICION
#Define la matriz de transición
ms <-matrix(c(0.05,0.95,
              0.2,0.8),nrow = 2,byrow = TRUE)
ms


#crea la Cadena de Markov en tiempo discreto
cmms1 <- new("markovchain",transitionMatrix = ms,states = c("D","ND"), ##D O ND ES QUE SE PRESENTE PARTE O NO SE PRESENTE PARTE DEPENDIENDO DE SI SE HA PRESENTADO O NO ESTE AÑO
            name = "CADENA MARKOV")

cmms1

###LA PROB DENTRO DE DOS AÑOS ES P AL CUADRADO
##dibujamos la matriz de transición
plot(cmms1)

##VECTOR INIDICAL DE ESTADOS TIENE QUE TENER EL MISMO ORDEN QUE LA MATRIZ DE TRANSICION ES DECIR D, ND
#prob de que no se presento por tanto vector inciial --> c(0,1) el 0 corresponde a D pq no hay declaracion de siniestro entonces ponemos el 1 donde no hay declaracion de siniestro


#Simular distribucion de estados despues de 2 etapas
vectorinicial <- c(0,1)
etapas <- 2
estadofinal2 <- vectorinicial*cmms1^etapas
estadofinal2
##aqui calculo en vector en 2 etapas: prob 0,17 de que se presente y 0.83 de que no se presente
estadofinal2[1,1] #esta es la solucion que extraigo de lo anterior: posicion 1, 1

#Prob.ND acabo de n años- probabilidad a largo plazo: comportamiento estacuibario a largo plazo si las matrices de transicion NO CAMBIAN: son homogeneas
steadyStates(cmms1) ##esto es SI NADA CAMBIA



####################   EJERCICIO 2 ###########################

##### Definir el estado inicial
  #### Si prenss en 0, prensa en 1: 70% y TV en 1: 30% (0)
  ####Si TV en 0, TV en 1: 60% y prensa en 1: 40% (1)
##medimos en MESES

#Define la matriz de transición: proceso estocastico: caracterizar la probabilidad de sucesos aleatorios
ms1 <- matrix(c(0.7,0.3,
              0.4,0.6),nrow = 2,byrow = TRUE) #los elemenetos de la diagonal principal suelen ser los mayores pq tenemos aversion al cambio
ms1

###Def de Cadena de Markov
cmms1.1 <- new("markovchain",transitionMatrix = ms1,states = c("Prensa","TV"), ##D O ND ES QUE SE PRESENTE PARTE O NO SE PRESENTE PARTE DEPENDIENDO DE SI SE HA PRESENTADO O NO ESTE AÑO
            name = "CADENA MARKOV")

cmms1.1

###plotear esto
plot(cmms1.1)


###Si prensa, prensa en 2 meses 
vectorprensa <- c(1, 0) ##como se anuncio en prensa entonces ahora es al reves
meses <- 2
estado <- vectorprensa*cmms1.1^meses
estado 
estado[1,1] ##la probabilidad despues de 2 meses es de 0.66 --> esto es lo que buscamos, fila1 col1

###Si prensa, TV en 3 meses y TV en 2 meses consecutivos (mes 1 y mes 2)
vectorTV_prensa <- c(1,0)
mesesTV <- 3
estadoTV <- vectorTV_prensa*cmms1.1^mesesTV
estadoTV
estadoTV[1,2] ##la probabilidad despues de 3 meses es de 0.417


#0,417: esto es empezando en presnsa terminemos en TV en 3 meses
  #ahora partiendo de estos datos  es decir partiendo de esa probabilidad quieres volver a aplicar la matriz de transicion, es decir el nuevo vector inicial es la pprobanilidad 0,417
#pq dice calcula en esas mismas condiciones
#despues de 2 meses consecutivos: es decir: VECTOR EN 3 por la matriz de transicion
estadoTV*cmms1.1

######ESTO NO
##la probabilidad de TV en 2 meses consecutivos si prensa
vectorcons <- c(0,1)
mesescons <- 1
estadocons <- vectorcons*cmms1.1^mesescons
estadocons1 <- estadocons^mesescons
estadocons1
estadocons1[1,2] ##la probabilidad en 2 meses consecutivos es de 0.4
####ESTE NOOOOOO



##comportamiento a largo plazo
steadyStates(cmms1.1)


####################   EJERCICIO 3 ###########################

###definimos el estado inicial
  ##Si simiestro en 0: siniestro en 1: 20% y no siniestro en 1:80% (0)
  ##Si no siniestro en 0: siniestro en 1: 30% y no siniestro en 1: 70% (1)
###medimos en AÑOS

###esto es un comportamiento aleatorio por tanto la diag principal no a a ser mayor: si suben precio de mi seguro entonces prefieres no darlo

##Matriz de transicion
ms2 <- matrix(c(0.2,0.8,
               0.3,0.7),nrow = 2,byrow = TRUE)
ms2

##Def de Cadena de Markov
cmmssiniestro <-new("markovchain",transitionMatrix=ms2,states=c("Siniestro","NoSiniestro"), ##D O ND ES QUE SE PRESENTE PARTE O NO SE PRESENTE PARTE DEPENDIENDO DE SI SE HA PRESENTADO O NO ESTE AÑO
              name="CADENA MARKOV")

cmmssiniestro

###Si no siniestro, siniestro en el 7 año
vectorsin <- c(0,1)
anio <- 7
estadosin <- vectorsin*cmmssiniestro^anio
estadosin
estadosin[1,1] ##que haya un siniestro en el 7 año: 0.2727

steadyStates(cmmssiniestro)

#####ESTO NO
##probabilidad a largo plazo de no si no tuvo
vectorno <- c(0,1)
aniosno <- 1
estadono <- vectorno*cmmssiniestro^aniosno
estadono
#steadyStates(estadono)
#####ESTO NO

##despues de que hayan pasado 5 años no tenga accidentes durante 3 años consecutivos: es decir, ya tenemos una situacion inicial que cuenta como 1 no siniestro entonces calcular ese y otros 2
vector_cinco <- c(0,1)
anios_cinco <- 5
estado_cinco <- vector_cinco*cmmssiniestro^anios_cinco
estado_cinco[1,2] ##hemos visto que se mantiene el comportamiento y que es es estaionario entonce s no hay que calcular nada ya conocemos las probabilidades

#anios_conse <- 3
#estado_cinco^anios_conse ##la probabilidad durante 3 años consecutivos es de 0.3826

##probabilidad a largo plazo #segun la situacion/estados actuales
steadyStates(cmmssiniestro) ##0.2727 de tenerlo frente a 0.7272 de no tenerlo
