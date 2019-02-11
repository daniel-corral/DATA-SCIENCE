#FILTER TAREA
library(tidyverse)
library(nycflights13)
#Todos los vuelos con un arrival delay de 2 o más horas
delay<-filter(flights, arr_delay>=120)

#Los vuelos que volaron a Houston a IAH y HOU 
houston<-filter(flights, dest =="IAH"| dest=="HOU")

#Los vuelos operados por United, American Airlines y Delta
# Primero queremos consultar aerolineas para ver cuales son los codigos de cada una
airlines<- nycflights13::airlines
# Carriers 
#1 9E      Endeavor Air Inc.          
#2 AA      American Airlines Inc.     
#3 AS      Alaska Airlines Inc.       
#4 B6      JetBlue Airways            
#5 DL      Delta Air Lines Inc.       
#6 EV      ExpressJet Airlines Inc.   
#7 F9      Frontier Airlines Inc.     
#8 FL      AirTran Airways Corporation
#9 HA      Hawaiian Airlines Inc.     
#10 MQ      Envoy Air                  
#11 OO      SkyWest Airlines Inc.      
#12 UA      United Air Lines Inc.      
#13 US      US Airways Inc.            
#14 VX      Virgin America             
#15 WN      Southwest Airlines Co.     
#16 YV      Mesa Airlines Inc. 

#luego consultumos según el carrier las reestricciones (no parámetros)
aerolineas<- filter(flights, carrier=="UA"|carrier=="AA"|carrier=="DL")
aerolineas 

#aquellos vuelos que salieron en verano, ya sea Julio, Septiembre u Octubre
#summer
summer<-filter(flights, month==7|month==8|month==9)
summer

#Aquellos vuelos que llegaron más de dos horas tarde, pero no tarde
late<-filter(flights, arr_delay>120, dep_delay <=0)
late

#Aquellos vuelos que tenian retraso de 1 hora y recuperado en más de 30 minutos
makeup<- filter(flights, dep_delay>=60,!(dep_delay-arr_delay)>30)
makeup

#Aquellos vuelos que salieron entre midnight y las 6am inclusive
midnight<- filter(flights, dep_time<= 600 |dep_time == 2400)
midnight

#ACTIVIDAD 2
#La funcion between() se utiliza para comparar lo que hay entre dos valores llamados right or left.La podemos utilizar con el último ejemplo, para saber los vuelos entre midnight y 6am
entre<-filter(flights, between(dep_time,600, 2400))
entre

#Encontrar los vuelos con NA en dep_time
nodispo<-filter(flights, is.na(dep_time))
nodispo
#faltan datos de arrival, lo cual puede significar que o bien el vuelo se ha cancelado o aun esté sin info, considerando que son en 2013, fueron cancelados

#¿Por qué NA ^ 0 no está missing?
NA ^ 0
#Porque cualquier número elevado a 0 siempre te va a dar 1, es decir, es == a 1--> NA ^ 0 ==1

#¿Por qué NA | TRUE no está missing?
NA | TRUE
#

#¿Por qué FALSE & NA no está missing?
NA & FALSE
#Cualquier cosa y falso parece que siempre va a ser FALSO, prima este último.

#TRUCO?
NA * 0
#Esto quiere decir que no sabemos si realmente NA * 0 == 0 ya que NA puede tomar cualquier valor y si le asociamos un numero infinito, ya sea positivo o negativo, el resultado no es 0,por tanto depende de inf
Inf * 0
help(NaN)
#Lo cual significa que las matematicas son limitadas al igual que los ordenadores y que el numero no está disponible


#ARRANGE TAREA
# 1a. 10 vuelos más retrasados (arrange) en el arrival
retrasados<-arrange(flights,desc(arr_delay))
retrasados

# 1b. departure con menor retraso o menos delay, los que se han adelantado más-número negativo; indica que el vuelo que más pronto sale, son 43 minutos más esperado de lo normal-07, 2013 at 21:23 
pronto<-arrange(flights,dep_delay)
pronto

# 2.Vuelo más rapido: espacio/tiempo * 60
tiempovuelo<-arrange(flights,air_time)
tiempovuelo
distancia<-arrange(flights, distance/air_time * 60)
distancia

# 3. Vuelos: travelled longest y shortest?

#comandos adicionales
eliminarna<-arrange(flights,desc(is.na(dep_time)), dep_time)
eliminarna
dept<-arrange(flights,dep_time)
dept

##Ejercicios práctica 2 dia 2 octubre
##1. 
##Para empezar de 0 otra vez primero cargamos library(tidyverse) y luego library(nycflights13), seguido de nycflights13::flights
##Seleccionar todas las formas posibles-brainstorm
select(flights,"dep_time","dep_delay", "arr_time", "arr_delay")

##Cuando sabemos el num decolums de lo que queremos ponemos select(flights, 4,5,6,9)- haciendo View(flights); str(flights)
select(flights, starts_with("dep"), starts_with("arr"))

##select(flights, matches("^(dep|arr)_(time|delay)$")) esto es otra forma de decirlo

##2.En una variable calculada con mutate, calcular arrival time - departure time seleccionando de flights estas 3 variables y almacenarlo en un nuevo objeto
datos<- select(flights, "air_time", "arr_time", "dep_time")
##Esta es la estructura de mutate para que quede más visual y estructurado, esto es de mejor utilidad cuando hay más variables
##la variable en el tiempo en el aire no es una variable calcula, hay que buscar que hacer para que sean iguales
mutate(datos,
       diferencia=arr_time-dep_time
)
##hacemos una transformacion- la hora de salida en minutos %% divide (100)*60... y le hace elmodulo 1440 que son los mins de un día
flights_airtime <- 
  mutate(flights,
         dep_time_min = (dep_time %/% 100 * 60 + dep_time %% 100) %% 1440,
         arr_time_min = (arr_time %/% 100 * 60 + arr_time %% 100) %% 1440,
         air_time_diff = air_time - arr_time + dep_time)
##vamos a filtrar de todos los datos air_time en los que la diferencia sea 0 minutos y hace un plot con un histograma y encuentra que hay algunos datos que son discrepantes
#y podemos hacer un plot entre air_time y arr_time;

##3
##si la unidad de ,edoda fuera la misma el sched-deptime seria el dep delay; hay que hacerla conversion para hacer la cuenta correctamente y no tiene porque ser inmediata
##Con esto de arriba queda muy normalizado -- con ggplot


#1. Seleccionar todas las variables que acaben en ‘time’ de la base de datos flights de nycflights13
select(flights, ends_with("time"))

#2. Renombrar la variable arr_delay con retraso_llegada de flights y almacenar en un nuevo objeto llamado Vuelos
Vuelos<-rename(flights,retraso_llegada=arr_delay)
Vuelos

##3.Ordenar Vuelos de menor a mayor distancia recorrida- en ascendente
tabladist<-arrange(flights,distance)
tabladist$distance
tabladist1<-select(tabladist,distance,origin,dest)
tabladist1

##4.Encontrar el origen y el destino del vuelo de mayor distancia recorrida y el de menor distancia recorrida
head(tabladist1,1)
tail(tabladist1,1)

##5.Anadir a la base de datos vuelo la velocidad media del vuelo como una variable calculada a partir del espacio y el tiempo
velocidadmedia<-arrange(flights, VelMed=distance / air_time)
velocidadmedia

##ACTIVIDAD PIPE
##Sin Pipe- el símbolo %>% significa hago una cosa y luego ejecuta la siguiente y luego...

by_dest<- group_by(flights,dest)
delay<- summarize(by_dest,
                  count= n(),
                  dist=mean(distance, na.rm=TRUE),
                  delay=mean(arr_delay,na.rm=TRUE)
                  )
delay<-filter(delay, count > 20, dest!= "HNL")
delay

##Con PIPE
##Se utiliza esto porque solo tiennes una variable que te almacena los datos y asi no creas variables en bucle, por cuestion de espacio

delays <- flights %>%
  group_by(dest) %>%
  summarize(
    count= n(),
    dist =mean(distance, na.rm=TRUE),
    delay=mean(arr_delay, na.rm=TRUE)
  ) %>%
  filter(count >20, dest!="HNL")
delays

#Quitamos los NAs con el !
not_cancelled <- flights %>%
  filter(!is.na(dep_delay), !is.na(arr_delay))
not_cancelled %>%
  group_by(year,month,day)%>%
  summarize(mean=mean(dep_delay))

#Interpretacion de la solucion: 1 de enero 2013, el retraso medio diario en la salida de los vuelos no cancelados ha sido de 11.4 min.

