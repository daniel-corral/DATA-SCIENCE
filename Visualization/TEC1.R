##TECNICAS DE VISUALIZACION

library(ggplot2) ##libreria de visualizacion de datos o install.packages("ggplot2")
##f1+fn da directo a la ayuda
##usar o no la c() da igual ya que todo en R es un vector

##GGPLOT2
####Aesthetics

##Dataset mpg- se carga con la libreria de ggplot
##cuando cargas dos librerias a veces te salen errores que siempre obviamos- dice que esta libreria esta enmascarando a otras funciones de la libreria anterior-los está sobreescribiendo
##si quiero la funcion de la libreria anterior ponemos dplyr::lag 

ggplot(mpg, aes(class)) + geom_bar() ##aes es la conexion entre la variable y lo que ploteo
##no le hemos dicho cual es la variable y pero ha hecho un CONTEO
##dataset mapping 

ggplot(cars, aes(class)) + geom_...
##dentro del parentesis pongo la x aunque ya sale, a la vriable x le doy la plus
ggplot(mpg,aes(x=class)) + geom_point() ##da lo mismo pq por defecto sale la x

##falla porque nos falta la y entonces

ggplot(mpg,aes(x=class, y=hwy)) + geom_point()
##por defecto se hace un conteo pq si observo el dataset y pongo por clase se ve que hay muchas filas con la misma clase. 
##usa esta variable para ponerla en la x, ggplot necesita alguna forma de resolver esto; la diferencia con la antigua esq no hay suma ni media

View(mpg)
#alpha=1 - muy opaco, fill es el color del relleno, color es el borde; shape=forma, tamaño y stoke(borde que forma tiene, continuo discontinuo..)
##tu puedes asignar a una propiedad: una variable o una constante

##ejercicio 2: corregid el ejer anterior cambiando los mappings 
ggplot(mpg, aes(x=class, y=displ)) + geom_point() ##ggplot nombre de la funcion, despues de las comas siempre va un espacio para leer el codigo)

###IMPTE: cuando hay un +significa que no se ha acabado la sentencia; le das a enter para ver que sale y te dice el error
#no siempre funciona el ES, o es mejor utilizar el punto y coma

##ejercicio 3: conseguid grafico en el  que mappings tiene tamaño color x e y 
##y=dspl en la x=cty, color class, y tamaño hwy
ggplot(mpg, aes(x=cty, y=displ, color=class, size=hwy)) + geom_point()
##tambien se podria utilizar el fill
##si quiero codificar una constante- que todos los circulos sean cuadrados por ej. Cada punto tiene un valor distinto en cada variable
##si pones class entre "" te da una leyenda diciendo que en vez de cada cosa es class, al df sin querer le he añadido que para color es class para todos los coches
##cuando codifico esto como una variable te lo pone con leyenda- el texto class corresponde al color rojo
##la cadena de texto que yo he introducido se lee como color

##si quiero hacer algo con una constante no poner dentro de aes

#si quiero poner un color constante
ggplot(mpg, aes(x=cty, y=displ, color=class, size=hwy)) + geom_point(color="blue") ##si pongo las "" lo hago siempre FUERA DE AES

ggplot(mpg, aes(x=cty, y=displ, color=class, size=hwy)) + geom_point(shape=2)
##el numero en shape indica la forma geometrica quele estamos dando

##AES es cuando varía y fuera de AES es cuando es constante
##una capa: hago una grafica y voy poniendo encima de otra una capa- un diag de puntos que tiene una corr de Pearson encima, por ej.

##como añadir multiples capas -- el orden de lectura: primero se pinta lo de abajo
ggplot(data, 
       aes(x=variableuno))+
  geom_bar()+
  geom_point()

##ejercicio 4
##el aes arriba aplica a todas las capas, es decir la x va a serlo para el geom bar y para el geom point
#si lo pones en geom point- aes de la geometria, solo esta capa lo utilizará
##si no pongo nada en geom bar como esta arriba aplica a geom bar
ggplot(mpg,
       aes(x=cty))+
  geom_bar(fill="grey")+ ##color es el borde no lo de dentro; luego geom bar ya tiene la x=. no hace falta ponerla
  geom_point(aes(y=displ, color=class))
##una barra no tiene Y pq la barra contea- que estadisitico tengo que hacer?
##como las barras no tienen tamaño no funciona 

##ejercicio 5-overplotting(sobrepintado- pasa cuando tienes un diag de dispersion y tienes tantos puntos que todo se ve negro)
#estimacion de densidades: resolucion de problemas- diagrama de profundidades que te dice cuanta distribucion hay en cada barra- no es más que una interpolación
#utilizando opacity alpha de 0.2 o jittering(añadir ruido a los datos para que se muevan)
##hay un problema de OVERPLOTTING
#-- ponerle un alpha a los puntos 0.1=10%, alpha es una constante 

ggplot(mpg,
       aes(x=cty))+
  geom_bar(fill="grey")+
  geom_point(aes(y=displ, color=class), alpha=0.1)

##DOS OPCIONES DE UTILIZAR JITTER o bien con geom_jitter o con geom_point y cambias alpha por position="jitter"
ggplot(mpg,
       aes(x=cty))+
  geom_bar(fill="grey")+
  geom_jitter(aes(y=displ, color=class))

ggplot(mpg,
       aes(x=cty))+
  geom_bar(fill="grey")+
  geom_point(aes(y=displ, color=class), position ="jitter")

##jitter tiene un parametro que es el ancho y el alto, si ponngo: 
ggplot(mpg,
       aes(x=cty))+
  geom_bar(fill="grey")+
  geom_jitter(aes(y=displ, color=class), width=2, height=2)
##se mueven más