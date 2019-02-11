#################### FUNDAMENTOS DE ESTADISTICA ############################

########################### INFERENCIA  ###################################

# Contraste igualdad medias 


library("dplyr")
library("ggpubr")
library("PairedData")

#EL ejemplo consiste en analizar la variable "peso" al inicio y final de un periodo:


#valores medidos al comienzo del periodo "c", creamos random 20 datos, de media 100, sd de 16
set.seed(123)
c=rnorm(20, 100, 16) 
c

#Valores medidos al final del periodo "f"
f=rnorm(20, 120, 20)  #creamos otros datos que no tienen media igual, para hacer luego un contraste
f

# Datos de dos vectores 

# antes del tratamiento
Comienzo <-c
# final del tratamiento
Final <-f

# Crear data.frame
BD <- data.frame( 
        Grupo = rep(c("Comienzo", "Final"), each = 20),
        Peso = c(Comienzo,  Final))
       

# Los datos
View(BD)        
     
        

#install.packages("dplyr")

library("dplyr")



#Ofrece el resumen estadistico para los distintos grupos
group_by(BD, Grupo) %>%
        summarise(
                count = n(),
                mean = mean(Peso, na.rm = TRUE),
                sd = sd(Peso, na.rm = TRUE))
      

#VISUALIZACION DE DATOS

#install.packages("devtools") #No requerido 



# Grafico por peso y grupo

library("ggpubr")
ggboxplot(BD, x = "Grupo", y= "Peso", 
          color = "Grupo", palette = c("#00AFBB", "#E7B800"),
          order = c("Comienzo", "Final"),
          ylab = "Peso", xlab = "Grupos") #se interpreta que al final del periodo las medias han aumentado

# COMENTAR RESULTADOS
library(PairedData)

# Subgrupo de pesos inicio periodo
Comienzo <- subset(BD,  Grupo == "Comienzo", Peso,
                 drop = TRUE)
# Subgrupo de pesos final periodo
Final <- subset(BD,  Grupo == "Final", Peso,
                drop = TRUE)

# Grafico de grupos

pd <- paired(Comienzo, Final)
plot(pd, type = "profile") + theme_bw()
#nos da idea de la evolucion de los datos de comienzo a final. si todas estas rectas son ascendentes, nos dirian que todos los valores han aumentado en el tiempo
#aqui vemos que hay dos lineas que no son ascendientes de comienzo a final, por lo que concluimos que todos los valores han aumentado excepto dos


# Verificar la normalidad con Test Shapiro-Wilk
d <- with(BD, 
          Peso[Grupo == "Comienzo"] - Peso[Grupo == "Final"])

shapiro.test(d) 
#se admite la normalidad, con p mayor a 0.05

# Contraste
res <- t.test(Peso ~ Grupo, data = BD, paired = TRUE) #paired=TRUE; con esto le decimos que ambos datos son numericos
res
#no se puede admitir que las dos medias sean iguales, porque p-valor es mas peque??o a 0.05


#La media es menor al comienzo
t.test(Peso ~ Grupo, data = BD, paired = TRUE,
       alternative = "less")

#La media es mayor al comienzo
t.test(Peso ~ Grupo, data = BD, paired = TRUE,
       alternative = "greater")

# p-valor
res$p.value

# Media de las diferencias
res$estimate

# Intervalo de confianza
res$conf.int

