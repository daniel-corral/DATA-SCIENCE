####Mini curso de R en 20 mins

##Expresiones R
#{} <- operador 
##lo que la llave hace es agrupar en un grupo devolviendo la ultima fila de codigo
{
  "hola"
  9
  3+3
  rnorm(1)
  33
  1:10
}

{
  mivariable <- 3
  mivariable+34
}


##se utiliza en los if elses...

##estas son equivalentes
if(5>3) "Hola"

if(5>3){
  "Hola"
  "Blah"
}  

rnorm({ ##las llaves es una expresion que devuelve la ult linea
  "Hola"
  1
})

vector <- 1:10

vector[{
  1
  3
}]

#vector con 10 posiciones 
vector[if(5>3) 5 else 3 ] ##me da el 5 pq la posicion sigue siendo igual


##return solo sirve dentro de una funcion
a <- function (){
  1
  2
  3
  return(3)
}
sapply(1:10, function(x) {x+3}) ##cada valor de la cadena definido como x se le suma 3

mifuncion <- function(x) {x+1}

