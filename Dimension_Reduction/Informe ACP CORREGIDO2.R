###### INFORME ACP ########
##########################

##Comprobación empírica -planteamiento lineal y exploración de lo que me piden
##ACP: reestructurarlo en el informe

library(factoextra)
library(FactoMineR)

TIUSD=read.csv(file="ACPTIUSD.csv", sep=";")
View(TIUSD)

head(TIUSD) ##trato como observaciones activas las 950 - hecho un vistazo 
tail(TIUSD) ##aqui nohay filtros; observo que aparant en el 95 los tipos a un mes segun aumenta el plazo hasta el 4Y el tipo de interes y la rtbilidad asociada va aumentando
##del 5 al 10 esta mas o menos estabe 
##3 años largos despies no aparecedeposito a un mes y los tipos en general han caido- sigue ocurriendo que hasta 6 meses pasa algo raro 
##tenngo una modificacion en la curva de tipos


##PREGUNTA: esto tiene sentido? 

##tiene sentido que los tipos a corto sean < largo? riesgo derivado del emisor y coste de oportunidad
#desde un punto de vista de pol monteria tradicional 
##si los cortos > largos puede deberse a SHOCKS, INESTABILIDADES: no es lo normal; hay una preferencia temporal por el dinero 

##bonos que el Estado me lo paga a "este" tipo y dif a largo que a corto 
#hacambiado desde la intervencion del BCE desde Leehman y la estructura de tipos se ha hundido
#las personas que han intervenido en la crisis:org internacionales (FED, BCE) han creado una distorsion en el mercado por las inyecciones de liquidez para reestructura
##roblema de los bancos comerciales

TIUSD.act=TIUSD[1:949, 1:9] ##me quiero cargar/tratar la X
str(TIUSD.act) ##$X es un factor aunque realmente no; le dices a R que el df la primera columna 
##mis obs son fechas definidas por precios en distintos bonos
##dado que tenemos una columna de fechas, procedemos 
##estoy asumiendo 

###VECTOR DE FECHAS
Dates=as.Date(TIUSD.act$X, format="%d/%m/%y") ##Creacion del Vector de fechas 
TIUSD.act=TIUSD.act[,-1] ##Extraccion de la primera columna de fechas del objeto
head(Dates)

##frenas la velocidad del dinero subiendo los tipos... la gente como le van a cobrar mas por dinero dejan de endeudarse
#falacia: ceteris paribus- voy a recibir mas dinero 

##dan mas dinero a la gente pero tambien tienen que subir gastos



###----ANALISIS EXPLORATORIO-----######
#######------------####################

#Empiezo pidiendo un resumen de las variables: podemos meter imo en el data frame del Min Q1 Med MEan Q3...
summary(TIUSD.act)
####resumen personalizado para obtener más información
TIUSD.act_stats = data.frame( 
  Min = apply(TIUSD.act, 2, min, na.rm=TRUE), # mín
  Q1 = apply(TIUSD.act, 2, quantile, 1/4, na.rm=TRUE), # 1er cuartil
  Med = apply(TIUSD.act, 2, median, na.rm=TRUE), # mediana
  Mean = apply(TIUSD.act, 2, mean, na.rm=TRUE), # media
  Q3 = apply(TIUSD.act, 2, quantile, 3/4, na.rm =TRUE), # 3er cuartil
  Max = apply(TIUSD.act, 2, max, na.rm=TRUE) # Máx
)
TIUSD.act_stats=round(TIUSD.act_stats, 1)
TIUSD.act_stats


#####ANALISIS DE LA MATRIZ DE CORRELACIONES########
####Asociaciones entre variables: determinante para decision de reducción de dimensión
##PAra que el ACP tenga sentido es alto tener un alto nivel de reduccion

cor.mat=round(cor(TIUSD.act),2) ##redondea a dos decimales la salida que calcule las corr del data frame activo pero tiene probpara calcularlas por los NA
cor.mat

cor.mat=round(cor(TIUSD.act, use="complete.obs"),2)
cor.mat ##miro en la tabla las asoaciaciones, los num que estan iguales y pq pueden salir bonos iguales
##pensar: puedo llegar a pensar que son indepent: si lo son su coef cor es 0 si el coef cor es 0 no sig que sean independientes
###que el coef de cor sea 0 puede pasar-un 0.26 es muy bajo: pero esto son valores brutos por tanto necesito niveles de sig: se contrasta la hipotesis

##cual es la dispersion dentro de la muestra??? 

##pero si el coef de corr es 0.26 cual es el p-value(nivel de sig del estadistico de contraste)? 
require(Hmisc)

##CONTRASTE DE HIPOTESIS

##puedo decir que el precio medio del deposito es 5.25? por ejemplo. 
##Tendre salidas qyue se alejen y tendre excesos por encima del 5.25... HACER UN HISTOGRAMA DE LA DISTRIBUCION DE PRECIOS DEL BONO para ver si tiene o no la forma de campana
#hipotesis alternativa cuando no la planteo 

###a menor tamaño muestral para tener el mismo intervalo de confianza.. necesitare mayor espacio?
#no fijo el nivel y calculo el estadístico de contraste

##el p valor es la probabilidad de superarlo: el nivel de significacion del estadistico de contraste
##si son independientes el coef cor es 0 entonces la hipotesis nula ya me la conozco
##la independencia es muy critica y se calcula el nivel de sig del est de contraste (pvalor)

##el estadistico de contraste es el que me da la tabla P: da el p valor
#pvalor: valor de la prob y varia entre 0 y 1 pq es una probabilidad
##rechazare la h sea cual sea elp valor cuando este tienda a 0!!!! IMPORTANTE

cor.mat.nds =rcorr(as.matrix(TIUSD.act))
cor.mat.nds ##le estoy diciendo que lo trate como una matriz y me da la misma mat corr y las observaciones que ha utilizado
##para calc las corr del coef un mes 

##pvalor:hhipotesis: prob de rechazarla siendo 0 siendo falsa

require(corrplot)
##ERROR TIPO I y ERROR TIPO 2: el primero es rechazar la Hipotesis cierta y la seg es aceptar la hipotesis que es falsa

corrplot(cor.mat, type="lower", order="original", 
         tl.col="black", tl.cex=0.7, tl.srt=45)  # las correlaciones positivas en azul, las negativas en rojo
##los cortos plazos con los largos va cayendo la intensidad y me da una asociacion

##OTRA FORMA DE PLOTEAR con order hclust(rectangulos)
corrplot(cor.mat, type="full", order="hclust", addrect = 3,
         tl.col="black", tl.cex=0.7, tl.srt=45)

library(PerformanceAnalytics)

chart.Correlation(TIUSD.act, histogram=TRUE, pch=19)

col = colorRampPalette(c("red", "white", "blue"))(20)

##MAPA DE CALOR
heatmap(x = cor.mat, col = col, symm = TRUE)

##cuantas menos estrellas menor es el pvalor:tengo 3:puedo rechazar la H0 y p valor 0
#los histogramas con multiples modas, los demas con colas largas

##INDICE KMO yBArtlett
invR=solve(cor.mat) ##o con ppcor
invR
##matriz de correlaciones parciales
require(ppcor)
#obtención de la matriz sin NA's a partir del complete cases
TIUSD.act.C=TIUSD.act[complete.cases(TIUSD.act),]

##genero una de observacionescompletas y como le sale singular la mat de corr no podria por tanto invertirla y hace una aprox y trabajada con ella y p.cor tal es la mat de corr  en este casola inversa
##kmo de la mat cor obtengo los cuadrados y le resto los 1 de la diag principal(son absubdos meteros)
##resto los 1s de la diag principal : razonamiento: el coef de cor parcial(relacion de 2 variables ELIMINANDO LA DEPENDENCIA DE TODASLAS DEMAS)
##en el bono a 3 años-computo de precio- está influyendo todos y en el de 5 tambien influyen todos 
#cuando calculo alguno los demas están influyendo: el coef parcial mediante un sistema de residuos: en el precio bono-asoacion 1y 2 voy a quitar la influencia de todos los demas bonos
#todo lo que influye en el bono 1 que no es el nbono 2 se quita y viceversa y se mide
#mediante un sistema de eliminacion de todo lo que influye pero que no quiero que este incorporado lo puedo quitar
#lo que me queda esq el bono 1 y 2 puros donde su asociacion vendra medida por lo queuno influye en otro
#sist de analasis factoral: cuanta mayor asociacion con todas las variables

p.cor.mat=pcor(TIUSD.act.C)
#The inverse of variance-covariance matrix is calculated using Moore-Penrose generalized matrix invers due to its determinant of zero.
#para así conseguir la matriz de corrleaciones parciales a partir de “$estimate”


p.cor.mat2=as.matrix(p.cor.mat$estimate)
p.cor.mat2

##calcular el KMO global
kmo.num = sum(cor.mat^2) - sum(diag(cor.mat^2))
kmo.denom = kmo.num + (sum(p.cor.mat2^2) - sum(diag(p.cor.mat2^2)))
kmo = kmo.num/kmo.denom

##MSAO o KMO global para variables parciales
p.cor.mat2=data.frame(p.cor.mat2)
rownames(p.cor.mat2) = c(rownames(cor.mat))
colnames(p.cor.mat2)=c(colnames(cor.mat))
for (j in 1:ncol(TIUSD.act)){
  kmo_j.num <- sum(cor.mat[,j]^2) - cor.mat[j,j]^2
  kmo_j.denom <- kmo_j.num + (sum(p.cor.mat2[,j]^2) - p.cor.mat2[j,j]^2)
  kmo_j <- round(kmo_j.num/kmo_j.denom,4)
  print(paste(colnames(TIUSD.act)[j],"=",kmo_j))
}

##todas las prubeas tienen que ver las unas con las otras
#matriz antiimagen: inversa de la matriz de cor parciales-estan incorporadas en el KMO no necesito estudiarla

##test de esfericidad de Bartlett: test de independencia
#Este no será valido si se utilizan más de 100 observaciones, por tanto muestreamos
cor.mat
n = nrow(TIUSD.act)
p = ncol(TIUSD.act)
chi2 = -(n-1-(2*p+5)/6)*log(det(cor.mat))
ddl = p*(p-1)/2
##muestreamos a 70 observaciones, rebajando 25 el resultado es el mismo
set.seed(1234)
TIUSD.mas=TIUSD.act[sample(nrow(TIUSD.act), 70), ]
n = nrow(TIUSD.mas)
p = ncol(TIUSD.mas)
chi2 = -(n-1-(2*p+5)/6)*log(det(cor.mat))
ddl = p*(p-1)/2
print(chi2)

print(ddl)
print(pchisq(chi2,ddl,lower.tail=F))
#cual pongo en abscisas y cual en ordenadas, cual es la dependiente y la independiente?

##el punto donde se cortan es lamedia de las x y lamedia de las y's
#las variables son indep: coef cor = 0 desde un punto de vista geometrico: cos angulo tiene que ser = 0 y  las variables son ortogonales y la rep es ortogonal
#la varianza mide la dispersion cuadratica en torno a la media

##fuerza centrifuga

####EL DETERMINANTE DE LA MAT DE CORR#######diag princ-diag sec
#si son indep en la diagonal principal son todo 1 y en el resto 0 por tanto el det es 1

#p.cor.mat2= matriz de corr parciales - diag  de esa matriz: 0.84 es el resultado: es un buen KMOW

#KMO medio de cada bono 

##test de Bartlett: demostrar que el log del det de la mat corr multiplic por cambiado de sig(-) por el num de observaciones -1 -1/6.... tuviera el comportamiento estocastico como chisquare
#problema es la n pq si le meto mas variables Bartlett crece tambien con el 
##sisolo incremento el tamaño muestral el est de Bartlet crece con el 

#en el estadistico de bartlett no debe meterse mas de 100 observaciones

#el estadistico de contraste toma un nivel de 1698.146: p value 
#se calcula la prob de que una chisquare 
#es imposible que me equivoque 


######OTRA FORMA#######
library(psych)

##la rotacion me va a cambiar los ejes por tanto tendre unas variables muy asoaciadasa un eje y otras asociadas a otro eje
#el largo de la flecha: indica la impt de la asoc de la variable con el COMPONENTE
#las 2 dimensiones explican un 98% con solo dos plazos subyacentes 
print(cortest.bartlett(cor.mat, n=nrow(TIUSD.mas)))

##identificación de los componentes principales
TIUSD.acp = PCA(TIUSD.act, scale.unit = TRUE, ncp = ncol(TIUSD.act), graph = TRUE)
print(TIUSD.acp)

#autovalores de los CCPP y el tanto por ciento de la varianza explicada
autoval= TIUSD.acp$eig #devuelve 
round(autoval, 2)
##rep grafica de los autovalores
barplot(autoval[, 2], names.arg=1:nrow(autoval), 
        main = "Varianza explicada por los CCPP",
        xlab = "Componentes Principales",
        ylab = "Porcentaje explicado de la varianza",
        col ="steelblue",
        ylim=c(0,105))

##linea con conexion de barrs y con porcentaje acumulado
barplot(autoval[, 2], names.arg=1:nrow(autoval), 
        main = "Varianza explicada por los CCPP",
        xlab = "Componentes Principales",
        ylab = "Porcentaje explicado de la varianza",
        col ="steelblue",
        ylim=c(0,105))
lines(x = 1:nrow(autoval), autoval[, 2], 
      type="b", pch=19, col = "red")

lines(x = 1:nrow(autoval), autoval[, 3], 
      type="o", pch=21, col = "blue", bg="grey")

##1er CP explica más del 80% de la varianza, los dos primeros el 98%. La reducción de dimensión es enorme.

##Gráfico de sedimentacion con FACTOEXTRA
fviz_screeplot(TIUSD.acp)+
  labs(title="Scree plot / Gráfico de sedimentación", x="Factores / Dimensiones /Ejes", y="% Varianza explicada")+
  geom_line(aes( y = autoval[,3]), linetype="dashed", color = "red")+
  geom_point(y = autoval[,3], color="red")+
  theme_minimal()
##variables representadas en un mapa de componentes
plot.PCA(TIUSD.acp, axes = c(1,2), choix=c("ind"))

plot.PCA(TIUSD.acp, axes = c(1,2), choix=c("var")) ##componentes que queremos utilizar


####LAS COORDENADAS#####de las variables en el espacio de CCPP 
TIUSD.acp$var$coord 
head(TIUSD.acp$ind$coord) 
TIUSD.acp$var$cos2

#####LAS COMUNALIDADES#####---estamos comprobando que cada una de las variables: 8 vari con 8 componentes- parte explicada de cada variable con compt
apply(as.matrix(TIUSD.acp$var$cos2), 1, sum)##la suma de las comunalidades es uno en cada variable
##autovalor
CP1=TIUSD.acp$var$coord[,1]
CP1
CCP2=CP1^2
CCP2 ##para que no haya negativos
sum(CCP2)

##contribución de los factores extraídos en la explicación de cada variable, es decir su comunalidad
fviz_pca_var(TIUSD.acp, col.var="cos2") +
  scale_color_gradient2(low="white", mid="blue", 
                        high="red", midpoint=0.5) + theme_minimal()

##contribucion de las variables a los CCPP.
TIUSD.acp$var$contrib
apply(as.matrix(TIUSD.acp$var$contrib), 2, sum) ##suma 100 por columnas
fviz_contrib(TIUSD.acp, choice = "var", axes = 1) ##contribucon de cada variable- 1/8 variables son 12.5% si una variable tiene masde esto es pq tiene poder explicativo
##Compruébese como para el eje 2 los depósitos a corto plazo son esenciales


##contribución de las variables a la explicación de un número concreto de CCPP, simplemente sumamos el producto de la contribución de la vble en cada uno de ellos por el valor propio de cada uno de ellos
A=as.matrix(TIUSD.acp$var$contrib[,1:2])/100  # lo expresamos en tanto por uno
B=as.matrix(TIUSD.acp$eig[1:2,1])
A%*%B # producto matricial 


##VARIABLES QUE CONTRIBUYEN AL EJE 1
fviz_contrib(TIUSD.acp, choice = "var", axes = 1, top = 3)
##VARIABLES QUE CONTRIBUYEN AL EJE 2
fviz_contrib(TIUSD.acp, choice = "var", axes = 2, top = 3) 
##Representacion en escala de color
fviz_pca_var(TIUSD.acp, col.var="contrib")
##modificada
fviz_pca_var(TIUSD.acp, col.var="contrib") +
  scale_color_gradient2(low="steelblue", mid="white", 
                        high="darkblue", midpoint=50) + 
  theme_minimal()

##Contribución de las observaciones degradadas por importancia en la explicación
fviz_pca_ind(TIUSD.acp, alpha.ind="contrib") +
  theme_minimal() 

###metiendo el cos2: la parte explicada de la variab de la variable por las partes comunes?

##la contribucion de cada una de las variables a cada dimension en porcentaje
#los que menos contribuyen a la 1 son los plazos cortos
#mientras que a la dim 2 los que menos contribuyen son los 3 plazos mas cortos

##ROTACIONES FACTORIALES
##1. Normalizamos
TIUSD.norm = data.frame (scale (TIUSD.act.C)) 
summary(TIUSD.norm)
round(sapply(TIUSD.norm, mean, na.rm = T), 2)
sapply(TIUSD.norm, sd, na.rm=T)
##aplicamos el ACP
TI.acp=prcomp(TIUSD.norm )
summary(TI.acp)
TI.acp$sdev
(TI.acp$sdev)^2
sum((TI.acp$sdev)^2)

##generamos gráfico de saturacion o screeplot
screeplot(TI.acp, type="lines")
require(qcc)##graficos de control de calidad
varianzas = TI.acp$sdev^2 ##varianzas
pareto.chart (varianzas, ylab="Varianzas") ##grafico de PAreto, otra forma de ver las varianzas explicadas

##Rotacion
TI.acp$rotation
TI.acp$rotation[,1]  ##orden con cargas en el primer eje
sum(TI.acp$rotation[,1]^2) ##se ha normalizado, la suma de los cuadrados de las cargas debe ser 1
TI.acp$rotation[1,] ##cargas de la primera variable
sum(TI.acp$rotation[1,]^2)

TI.rot = principal(TIUSD.norm, nfactors=8, rotate="varimax") ##rotacion varimax
TI.rot$communality ##todas dan 1 pq openemos las comunalidades
TI.rot$loadings 


###COMPONENTES SUPLEMENTARIOS: como he hecho un PCA y he extraido cargas fact(los coef de un modelo de regresion. si y es = 2x+3 el 2 es el coef que equivale a la carga)
#representar graficamente en el mapa de 2 dimensiones
#PCA(X, ind.sup = NULL, quanti.sup = NULL, quali.sup = NULL, graph = TRUE)
#x, un data frame donde los individuos aparecen por filas y las varibles por columnas;
#ind.sup es un vector numérico que especifica los valores de los individuos suplementarios;
#quanti.sup, quali.sup son vectores numéricos que especifican los índices de las variables suplementarias, respectivamente numéricas o categóricas;
#graph es la orden para señalar si queremos o no una representación gráfica.

supl.acp= PCA(TIUSD[,-1], ind.sup= 950:978,  quanti.sup = 10, graph=FALSE)

##El bono a 10 años, su resultados previstos en términos de coordenadas, correlaciones y cos2 viene dados por
supl.acp$quanti.sup

##el bono a 10 años junto a las variables activas
fviz_pca_var(supl.acp)
##o bien mejorado
fviz_pca_var(supl.acp,
             col.var = "black",     #  variables activas
             col.quanti.sup = "red" #  variables suplementarias métricas
)
##visualizacion solo de las suplementarias
fviz_pca_var(supl.acp, invisible = "var")


##valores previstos vienen dados pro
supl.acp$ind
##visualizamos
fviz_pca_ind(supl.acp, col.ind= "cos2", col.ind.sup = "red", repel = FALSE, 
             pointsize=1,
             labelsize = 3,
             label = "sup"
             #jitter = list(what = "label", width = NULL, height = NULL)
)
##seleccionar a los 40 con mayor cos2
fviz_pca_ind(supl.acp, col.ind= "cos2", col.ind.sup = "red", repel = FALSE, 
             pointsize=1,
             labelsize = 3,
             label = "sup",
             select.ind = list(cos2=40)
             #jitter = list(what = "label", width = NULL, height = NULL)
)



######FORMA DE HACERLO MAS RAPIDO#######
#PCA(X, ind.sup = NULL, quanti.sup =... )
##con el df original los individuos sup de la matriz x 950-978 variables cuantitativas en esc metrica sup, qualitativas y el grafico si lo quiero o no)
#le digo que me haga un PCA que utilice el pca que ya he hecho 
##supl.acp=PCA...

##donde se posicionan las nuevas observaciones respecto de las anteriores
###PONER LA COLUMNA X COMO IDENTIFICADOR FILA