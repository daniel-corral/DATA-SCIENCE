##PRACTICA FUNDAMENTOS DEL ALGEBRA MATRICIAL

#A,B,C,D... nombres de las matrices
## x,y,z... elnombre de los vectores(fila/columna)

#Vectores

x<-c(1,2,3,4)
length(c) #DEvuelve la dimension (Nº Elementos)
x #Devuelve un vector fila

#MAtrices
A<-matrix(c(1,2,3,4,5,6), nrow=2,ncol=3, byrow=F) #byrow-si metes 3filas y 3 columnas, no sepuede pq si tengo una matriz de 3 por 3 necesito 9 elelmentos no 6

B<-matrix(c(1,2,3,4,5,6),2,3,byrow=T)
A
B

dim(A) #Devuelve la dimension
length(A) #Devuelve el numero de elementos

A[1,2] #Devuelve de la primera fila la segunda posicion
B[2,2]
A[1,]#Devuelve la primera fila
B[,4] #Devuelve la segunda columna

#Calculos

A+B

A-B

A*2

t(A) #MAtriz TRaspuesta
t(B)
t(x) #Matriz de x(dimension 1*4)

dim(t(B)) #Devuelve la dimension de la traspuesta
matrix(x) #MAtrix de x(dimension 4*1)
dim(matrix(x))

A*B #Multiplica elemento por elemento- R no hace la multiplicacion de valores como es, multoplica primer elemento por sprimer elemento sin conocer las dimensiones?



##Juntando matrices- (despues de la multiplicación de matrices)
A
C<-matrix(c(1,2,3,4,5,6),2,3,byrow=F) ##el 2 y 3 fuera de la matriz indican la dimensión de la matriz
C
cbind(A,C) #junta las columnas
rbind(A,C) #junta las filas

t(rbind(t(A),t(C)))
t(cbind(t(A),t(C)))


##Diagonales y trazas 
D<-matrix(c(1,2,3,4,5,6,7,8,9),3,3, byrow=T)
D
diag(D) #diagonal
sum(diag(D))##traza; es decir esto es la suma de los valores de la diagonal

diag(c(1,2,3)) ##matriz diagonal

diag(diag(D)) ##devuelvela matriz diagonal con los elementos de la diagonal D
##determinantes
D<-matrix(c(1,-2,2,2,0,1,1,1,-2),3,3,byrow=T)
D
det(D) ##calculamos la determinante

##Inversas

E<-matrix(c(2,3,3,4),2,2,byrow=T)
E
E; solve(E) ##Calcula las inversas de las matricesno singulares
##solo funciona cuando las matrices tienen determinantes distintos de 0


H<-matrix(c(1.3,9.1,1.2,8.4),2,2,byrow=T) ##MATRIZ SINGULAR el det es 0 por eso no funciona el código y da error
H; solve(H)
det(H)

###DIAGONALIZACION DE MATRICES 
G<-matrix(c(1,4,9,1),2,2,byrow=T)
G
eigen(G) ##proporciona autovalores y autovectores asociados

##APLICACION EN PROCESOS ESTOCASTICOS 

PE<-matrix(c(1,2,1,-1),2,2,byrow=T)
PE
PE(solve(PE))

##Matriz de transicion
PE<-matrix(c(0.5, 0.5,0.25,0.75),2,2,byrow=T)
PE
a<-eigen(PE) ##autovalores y autovectores asociados
a

##Matriz H de autovectores 
AV<-matrix(c(-0.7071068,-0.8944272, -0,70771068,0.4472136),2,2,byrow=F)
AV

##Matriz H de autovectores (otra forma)
##cadauno de los autovectores es la solucion para uno de los autovalores
autovec<-a$vectors
AV<-matrix(autovec,2,2)
AV

##Calculo la inversa
AV; solve(AV)
AVI<-matrix(c(-0.4714045,-0.942809,-0.7453560,0.745356),2,2,byrow=T)
AVI

##Matrix J de autovalores 
J<-matrix(c(1,0,0,0.25),2,2,byrow=T)
J

##Calculo de PN=HJ^nH^(-1)donde en J^n hacemos n=100
AV%*%J^100%*%AVI
##calcular la potencia enesima - primero multiplico por AV y luego por AVI

##El proceso se estabiliza en DP=1/3 y en RV=2/3


####### Distribuciones Discretas y Continuas
#Ejercicio 1: P(E)= 0,30; P(contraria E)=0,70 dicotimico; P(no especuladores)= 0,40
##para resolver el primer ejercicio
##poder distribuir las probabilidadesen diferentes escenarios e ir calculamdp

##cn varios escenarios es habitla que digamos si hay 2 escenarios 1/2 y 1/2.. pero hau que tener cuidado


