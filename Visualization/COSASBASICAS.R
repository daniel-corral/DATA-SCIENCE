##1.Vector

miVector <- c(1, 2, 3)
str(miVector) ##dice que es un vector num de longitud...

##otra forma de vector- va más lento por lo que sea entonces haces un VECTOR VACIO
numeric(100) ##double es otra forma de decir numerico
character(100)
integer(100)
logical(100)

rep(1, 100) ##repetir 1s 100 veces

##Qué pasa con los vectores? siempre estan bien pq en R se utilizan 

c("Alejandro", 10)##te está haciendo un cambio de variable sin decir nada pq convierte el 10 en texto

##los vetores SOLO pueden ser de un tipo

##2. Listas - se crearon para tener mas tipos- mas completo

list()
list(1)
list(1, "alejandro")
list(nota=1, nombre="alejandro")
milistarara <- list(nota=1, nombre="alejandro", "me aburro")
##cuando te descargas algo de youtube viene en listas- son jsones 

milistarara$nombre
milistarara$nota
milistarara[[1]]

##listas: algunos tienen nombre otros no tendrán nombre
names(milistarara)
##pq los df se acceden como listas? PQ SON LISTAS 

##pueden hacer cosas como un json
list(a=list(1,2), b=3, c="A") ##asi se representan los jsones
##mirar los resultados para ver como se accede
##despues del b hay un [1]- esto es pq el $b es un vector- LO QUE HAY DENTRO DE UNA LISTA ES UN VECTOR
str(3) ##te dice que es un vector numerico de 3
##todo en r son vectores c(3) == 3 ::: es lo mismooooooo

##dentro de la lista hay un elemento que se llama b que tiene un vector que es un 3



##tambien se puede acceder a columnas asi
milistarara["nombre"] ##para que usar el corchete si el dolar es mas facil?

##si no pones comillas dice que todo lo que venga despues lo busca
##en las comillas le das una cadena de texto

##confusion tipica
mivariable <- "nombre"
milistarara$mivariable ##NULL
milistarara[mivariable]

str(milistarara["nombre"]) ##te dice lo que es
milistarara[["nombre"]]

##doble corchete es para acceder al segundo nivel, es lo mismo que un $

#3. Matrices 
matrix()
#df : pensado para tablas y una matriz para el concepto matematico de matriz

#4.Dataframe
##de cero:

miDF <- data.frame(hola=c(1,2), caracola=c(3,4)) ##lista con vect que son col cuya restriccion esq todas las columnas tienen que tener la misma longitud o num de elementos
list(hola=c(1,2), caracola=c(3,4))

as.list(miDF)
##para pasar de un tipo a otro 
as.XXXXX
as.list()
as.data.frame()

#5. Factores - cuando tienes un vector con datos
sexo <- c("Hombre", "Mujer")
str(sexo)
sexfact <- as.factor(sexo) ##se convierten en factores pq es mejor calcular todo con numeros: traduccion de num a palabras
sexfact
as.integer(sexfact)
c(sexfact, "Hombre") ##cuando quieres concatenar factores, los 2 deben ser factores
c(sexfact, 1)
