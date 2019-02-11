####CALCULO DE PROBABILIDADES######
# Poisson y binomial son discretas
# Uniforme y normal son continuas

##funciones de las distribuciones de probabilidad 
#raiz: foo (nombre de la distribucion)

##dfoo: funcion de densidad
##pfoo: funcion de distribucion (prob acumulada)
##qfoo: funcion inversa F^-1(x)
##rfoo: generador (pseudo)aleatorio de valores

##Variable discreta 
#dado E=(1,2,3,4,5,6) con pb=1/6

#probabilidad de en 7 tiradas sacar 3 veces 5 -- LANZO 7 VECES EL DADO ENTONCES N=7 ES DECIR UN NUM DE REPETICCIONES
##B(7;1/6)
##tenemos que conocer los parametros de la distribucion <- si tengo una binomial n,p tenemos que conocer n y p
##la binomia es un fenomeno dicotomico
dbinom(3,7,1/6) ##determiname la probabilidad de una binomial

##probabilidad de sacar como mucho dos veces el 5
dbinom(c(2,1,0),7,1/6) ##ES DECIR, LA PROBABILIDAD DE QUE SALGA 0 VECES, 1 VEZ O 2 VECES

sum(dbinom(c(2,1,0),7,1/6)) ##conocer la suma de esas 3 probabilidades

pbinom(c(2),7,1/6)  ##funcion de distribucion F(2)

qbinom(c(0.90),7) ##el valor que acumula el 0.90 de pb

pbinom(c(2),7,1/6, lower.tail = F) ##ofrece la probabolidad complementaria es decir 1-F(2) --> trabajamos con la cola, vemos esa parte que antes no analizabamos

##  BINOMIAL QUE SALGA EL 5 O QUE NO SALGA: BAYES

sum(dbinom(2:5,7,1/6)) ##Pb de que salgan entre 2 y 5 --> otra forma de decirlo es decir la probabilidad entre 2 y 5


##DISTRIBUCION BINOMIAL
#simulacion genera la muestra de 100 alores con distribucion binomial n=5 y p=0.5

set.seed(123)
n=5
p=0.25
x= rbinom(100,n,p)##simulame una binomial
rbinom(100,n,p)
hist(x, probability=T) ##histograma

mean(x)
var(x)
sd(x)
sqrt(var(x))
median(x)
quantile(x,probs=c(0.05,0.95)) ##importante para la gestion de riesgos(es impte los percentiles mayores) --> para ver el 5% extremo donde queda
quantile(x, seq(0,1,0.20)) ##quintiles <- los valores que me deja el 5% de los extremos
quantile(x,seq(0.9,1,0.01)) ##calculo de los percentiles del 90-100 <- que me de los valores del 90 al 100 podemos ver que sigue una tendencia normal y de repente si te aparece un valor que sale 3 veces la tendencia pues o bien se estudia el caso o bien se depuran los datos
summary(x)

##Para superponer al histograma la funcion de cuantia

##al histograma le salen las barritas mas gordas pq es la probabilidad real representada <- hay ligeros cambios- es impte tener el histograma
##la probabilidad de que de 5 es 1/6 elevado a 5


##VARIABLE CONTINUA

#distribucion uniforme
set.seed(123)##para que todos tengamos lo mismo
x=runif(100) ##simulacion de 100 u(0,1) muy util para simulacion montecarlo y si lo cambias por 1000 las barritas se van unificando y se hacen más homogeneas
runif(100,2,5)
hist(x, probability = T,col=grey(0.9), main="Histograma distribución
     #Uniforme[0,1]")
curve(dunif(x,0,1), add=T) ##es una uniforme 0,1- superponga la funcion de una uniforme <-  distribucion de desnsidad  con parametros 0 y 1
mean(x) ##esta es la linea que sale en el histograma
sd(x)
summary(x)


##DISTRIBICION NORMAL
rnorm(n,mean=0,sd=1) ##genera obs distr
x=rnorm(100)
hist(x,probability=T, col=gray(0.9), main="Histograma de una muestra
     #N(0,1)") ##esto es simplemente testo: datos, colores, ejes y titulo
##para suponer al histograma la funcion de densidad 

curve(dnorm(x), add=T) ##funcion para que fije la funcion de densidad o de cuantia sobre el histograma

##plots de probabilidad normal:
#representa los cuantiles de los datos 

x=rnorm(10)
qqnorm(x,main = "N(0,1)")
qqline(x)

##N(0,1) Calculo de las probabilidades
pnorm(0)
pnorm(1.645) ##da que es el percentil 95%
1-pnorm(1.645) ##pb complementaria
pnorm(1.645, lower.tail = F)

qnorm(0.95) ##calculo de los valores; cual es el valor que acumula el 95% de probabilidad

##N(1,1)
pnorm(1,1,1)
pnorm(2,1,1)
pnorm(2,1,1)-pnorm(1,1,1) ##probabilidad de un intervalo

##Calculo de probabilidades

##B(5,0.25)

##DISTRIBUCION POISSON

#HGENERAR MUESTRA DE B=10

x=rpois(1000,1) ##simulacion de 10 valores con el parametro lambda = 1
rpois(1000,10) ##la poisson solo toma valores entre 0 y finitos valores
hist(x,probability = T, col=gray(0.9), main="Histograma") ##Histograma

