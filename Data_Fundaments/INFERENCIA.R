####MODELO GLM####

##Carga del archivo CSV
loan=read.csv("LOAN.csv",sep = ",", dec=".", header = TRUE, skip = 1)
View(loan)

##Carga de librería
library(ISLR) ##Data para introduccion estadistica para aplicaciones en R
