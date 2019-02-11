
#############################################
############ Los COCHES del JEFE ###############
#############################################


###Primero importamos el dataset de sav y con library foreign y reescribimos a CSV
library(haven)
library(foreign)
library(MVA)
library(ape) #hclust plotting
library(seriation) ##para hacer dissplot
library(factoextra)
library(ggplot2)
library(NbClust)

terreno <- read_sav("tterreno.sav")
write.csv(terreno, 'terreno.csv')
View(terreno)

#quitamos los NAs
terreno <- na.omit(terreno)
View(terreno)
#nos asegurmaos de su conversion a un DATAFRAME
#terreno <- as.data.frame(terreno)

#### 2. Metodología ACP  ####

## Seleccion de variables relevantes para el estudio mediante un ACP ##
#Nombres
terreno <- terreno[c(1,3,6,8,9,13,14)] ##nos quedamos con 7 variables que pueden ser explicativas
nombres <- c("marca","precio","potencia","peso","plazas","velocidad","aceleracion")
names(terreno) <- nombres
View(terreno)


#Matriz de Correlaciones: para evitar problemas de escala ya que las obs están tipificadas así
mat.cor <- cor(terreno)
mat.cor ##altas correlaciones generalmente

eigen.terr <- eigen(mat.cor)
eigen.terr ##combinaciones lineales que max la varianza

eigen.terr$values[1]/7 ##el primer componente explica el 44% de la varianza explicativa
(eigen.terr$values[1] + eigen.terr$values[2])/7 #con1 y 2 componente explicamos casi 70% varianza explicativa
(eigen.terr$values[1] + eigen.terr$values[2] + eigen.terr$values[3])/7 #1,2,3 compt explican el 83% var explic

ter.vec <- eigen.terr$vectors[1:7] ##la diagonal principal
ter.vec

#Conjunto Matricial
x <- as.matrix(terreno)
y <- as.matrix(ter.vec)
x %*% y
terr.datos <- as.matrix(x %*% y)
terr.final <- cbind(terreno, terr.datos)
terr.final #coches mas potentes tienen un valor menor que los menos potentes y una menor aceleracion en general y mas caros



#### 1. Analisis Más Exploratorio de datos   ####
##Identificacion del numero de clusters

set.seed(1234)
k.max <- 10 ##numero max de clusters es 10
data <- terreno[,-1:-2]
wss <-  sapply(1:k.max,
               function(k){kmeans(data, k, nstart = 10 )$tot.withinss})
plot(1:k.max, wss,
     type = "b", pch = 19, frame = FALSE,
     xlab = "Número K de clusters",
     ylab = "Suma total de cuadrados intra-clusters")
abline(v = 4, lty = 2) 
abline(v = 6, lty = 3)

##el numero optimo de clusters se encuentra entre 4 a 6

#Método Kmeans
fviz_nbclust(data, kmeans, method = "wss") +
  geom_vline(xintercept = 6, linetype = 3) +
  geom_vline(xintercept = 4, linetype = 6) +
  ggtitle("Número óptimo de clusters - kmeans") +
  labs(x = "Número k de clusters",y = "Suma total de cuadrados intra grupos")



#Metodo de particion alrededor de los medioides (PAM)
fviz_nbclust(data, cluster::pam, method = "wss") +
  geom_vline(xintercept = 4, linetype = 4) +
  geom_vline(xintercept = 6, linetype = 2) +
  ggtitle("Número óptimo de clusters - PAM") +
  labs(x = "Número k de clusters",y = "Suma total de cuadrados intra grupos")

##Sugiera más bien 6 grupos

#Método jerarquico
#Metodo jerárquico
fviz_nbclust(data, hcut, method = "wss") +
  geom_vline(xintercept = 6, linetype = 6) +
  ggtitle("Número óptimo de clusters - jerárquico") +
  labs(x = "Número k de clusters",y = "Suma total de cuadrados intra grupos")

##Por tanto, nos decantamos por 6 grupos, por ahora

set.seed(1234)
clus.nb = NbClust(data, distance = "euclidean",
                  min.nc = 2, max.nc = 10,
                  method = "complete", index = "gap")
clus.nb ##numero de clusters es de 3

#valores estadisticos del corte
clus.nb$All.index

#numero de clusters optimo: 3
clus.nb$Best.nc

#mejor particion
clus.nb$Best.partition

####CONCLUSION SEGUN REGLA DE MAYORIA: HUBERT INDEX
nb.todos = NbClust(data, distance = "euclidean", 
                   min.nc = 2, max.nc = 10, 
                   method = "complete", index = "all")

nb.todos

#### 2. Metodología clúster  ####
##Realizar sobre valor que toman 3 componentes para cada observación un análisis cluster
x <- as.matrix(terreno)
y <- as.matrix(ter.vec)
terr.observ <- x %*% y
plot(terr.observ)

mat.dist <- dist(terr.observ)#con esta calc la distancia euclidea entre observ
dissplot(mat.dist) #Cuanto más oscuro, menor la distancia entre las observaciones

#Aplicamos prueba sobre el conjunto real y anadimos etiquetas
set.seed(123)
prueba.km <-  kmeans(terreno, 3)
dissplot(mat.dist, labels = prueba.km$cluster)

cluster1 <- hclust(mat.dist, method = "complete") #metodo Ward
summary(cluster1)

attach(cluster1) ##una idea de como se van haciendo los cluster
merge ##aqui decimos se une la obs 20 con la obs 21 en cluster 1, la observ 72 con la 74 formando el cluster 2
#cuando son positivos son los cluster que se han hecho que se unen

#idea intuitiva de como con los datos ordenados de menor a mayor se van organizando en clusters
terr.ord <- sort(terr.observ)
distterr.ord <- dist(terr.ord)

##segundo cluster
cluster2 <- hclust(distterr.ord)
attach(cluster2)
merge #de igual forma de como ocurre arriba, los coches de gama media se 


##DENDOGRAMAS: determinar el num de cluster
plot(cluster2,labels = NULL, hang = -1, cex = 0.6)

cluster3 <- hclust(distterr.ord, method = "centroid")
plot(cluster3, hang = -1, cex = 0.6)


##gran cantidad de observaciones, se han minimizado disimiliridades y referencia visual no informativa
#Tecnicas de metodos divisorios
library(cluster)
clusterA <- pam(terreno,3)
clusterA
#ploteamos esto
par(mfrow = c(2,2))
plot(clusterA)

#regla de funcionamiento para agrupacion de observacions
summary(clusterA)


###Validacion interna de clusters: 

library(factoextra)

km_clusters <- eclust(x = data, FUNcluster = "kmeans", k = 3, seed = 123,
                      hc_metric = "euclidean", nstart = 50, graph = FALSE)
fviz_silhouette(sil.obj = km_clusters, print.summary = TRUE, palette = "jco",
                ggtheme = theme_classic()) 

View(km_clusters$cluster) ##hay 44 observaciones en el primer grupo, 21 en el segundo y 11 en el tercero

View(terreno$modelo)
