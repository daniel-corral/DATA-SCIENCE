####################################################################
##################    Cadenas de Markov    #########################
####################################################################
#Def: un proceso de markov es un proceso aleatorio con la probabilidad de que el valor actual 
  # y los valores futuros son independientes de los valores pasados
#Es decir, que si tenemos la informacíon presente del proceso,saber como llegó alestado actual
    #no afecta las probabilidades de pasar a otro estado en el futuro.

#################    EJERCICIO 1       ################
#X: "Siniestralidad, durante el año t"

library (markovchain)

# Definimos el ESTADO INICIAL:
#  No presentado en el año 0 y presentado en el 1: 20% (0)
#  Presentado en el año 0 y presentado en el año 1: 5% (1)
estado_inicial<- c(1,0)


#Define la matriz de transición
ms <-matrix(c(0.2,0.8,
              0.05,0.95),nrow = 2,byrow = TRUE)
ms

#crea la Cadena de Markov en tiempo discreto
cmms1 <-new("markovchain",transitionMatrix=ms,states=c("D","ND"), ##D O ND ES QUE SE PRESENTE PARTE O NO SE PRESENTE PARTE DEPENDIENDO DE SI SE HA PRESENTADO O NO ESTE AÑO
            name="CADENA MARKOV")

cmms1

### LA PROB DENTRO DE DOS AÑOS ES P AL CUADRADO
# Dibujamos la matriz de transición
plot(cmms1)



#Simular distribucion de estados despues de 2 etapas
vectorinicial<-c(0,1)
etapas<-2
estadofinal2<-vectorinicial*cmms1^etapas
estadofinal2

estadofinal2[1,1]

#Prob.ND acabo de n años- probabilidad a largo plazo
steadyStates(cmms1)



####################   EJERCICIO 2 ###########################

##### Definir el estado inicial
  #### Si prenss en 0, prensa en 1: 70% y TV en 1: 30% (0)
  ####Si TV en 0, TV en 1: 60% y prensa en 1: 40% (1)
##medimos en MESES

#Define la matriz de transición: proceso estocastico: caracterizar la probabilidad de sucesos aleatorios
ms1 <-matrix(c(0.7,0.3,
              0.6,0.4),nrow = 2,byrow = TRUE)
ms1

###Def de Cadena de Markov
cmms1.1 <-new("markovchain",transitionMatrix=ms1,states=c("Prensa","TV"), ##D O ND ES QUE SE PRESENTE PARTE O NO SE PRESENTE PARTE DEPENDIENDO DE SI SE HA PRESENTADO O NO ESTE AÑO
            name="CADENA MARKOV")

cmms1.1

###plotear esto
plot(cmms1.1)


###Si prensa, prensa en 2 meses 
vectorprensa<-c(0,1)
meses <- 2
estado <- vectorprensa*cmms1.1^meses
estado 
estado[1,1] ##la probabilidad despues de 2 meses es de 0.66 --> esto es lo que buscamos, fila1 col1

###Si prensa, TV en 3 meses y TV en 2 meses consecutivos (mes 1 y mes 2)
vectorTV_prensa <- c(0,1)
mesesTV <- 3
estadoTV <- vectorTV_prensa*cmms1.1^mesesTV
estadoTV
estadoTV[1,2] ##la probabilidad despues de 3 meses es de 0.334

##la probabilidad de TV en 2 meses consecutivos si prensa
vectorcons <- c(0,1)
mesescons <- 1
estadocons <- vectorcons*cmms1.1^mesescons
estadocons1 <- estadocons^mesescons
estadocons1
estadocons1[1,2] ##la probabilidad en 2 meses consecutivos es de 0.4

##comportamiento a largo plazo
steadyStates(cmms1.1)


####################   EJERCICIO 3 ###########################

###definimos el estado inicial
  ##Si simiestro en 0: siniestro en 1: 20% y no siniestro en 1:80% (0)
  ##Si no siniestro en 0: siniestro en 1: 30% y no siniestro en 1: 70% (1)
###medimos en AÑOS

##Matriz de transicion
ms2 <-matrix(c(0.2,0.8,
               0.3,0.7),nrow = 2,byrow = TRUE)
ms2

##Def de Cadena de Markov
cmmssiniestro <-new("markovchain",transitionMatrix=ms2,states=c("Siniestro","NoSiniestro"), ##D O ND ES QUE SE PRESENTE PARTE O NO SE PRESENTE PARTE DEPENDIENDO DE SI SE HA PRESENTADO O NO ESTE AÑO
              name="CADENA MARKOV")

cmmssiniestro

###Si no siniestro, siniestro en el 7 año
vectorsin <- c(1,0)
anio <- 7
estadosin <- vectorsin*cmmssiniestro^anio
estadosin
estadosin[1,1] ##que haya un siniestro en el 7 año: 0.2727

##probabilidad a largo plazo de no si no tuvo
vectorno <- c(0,1)
aniosno <- 1
estadono <- vectorno*cmmssiniestro^aniosno
estadono
#steadyStates(estadono)

##despues de que hayan pasado 5 años no tenga accidentes durante 3 años consecutivos
vector_cinco <- c(0,1)
anios_cinco <- 5
estado_cinco <- vector_cinco*cmmssiniestro^anios_cinco
estado_cinco

anios_conse <- 3
estado_cinco^anios_conse ##la probabilidad durante 3 años consecutivos es de 0.3826

##probabilidad a largo plazo #segun la situacion/estados actuales
steadyStates(cmmssiniestro) ##0.2727 de tenerlo frente a 0.7272 de no tenerlo
