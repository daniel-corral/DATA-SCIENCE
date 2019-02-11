######################### Cadenas de Markov ##########################

library (markovchain)

# Definimos el estado inicial
estados <- c("a","b","c")
#Define la matriz de transicion
mt <-matrix(c(0.5,0.25,0.25,
              0,1,0,
              0.25,0.25,0.5),nrow = 3,byrow = TRUE)
mt

#crea la Cadena de Markov en tiempo discreto
cmmt1 <-new("markovchain",transitionMatrix=mt,states=estados,
            name="MarkovChain A")
cmmt1

#Crea el diagrama de transicion
plot(cmmt1)

#probabilidades de transicion
cmmt1[2,3]
cmmt1[1,1]
cmmt1[1,4]  # esta fuera

transitionProbability(cmmt1,"b","c")
transitionProbability(cmmt1,"b","b")


#distribucion condicionada
conditionalDistribution(cmmt1,"b")

#Distribucion de estados despues de n etapas
estadoinicial<-c(1,0,0)
etapas<-7
estadofinal<-estadoinicial*cmmt1^etapas
estadofinal

steadyStates(cmmt1)
