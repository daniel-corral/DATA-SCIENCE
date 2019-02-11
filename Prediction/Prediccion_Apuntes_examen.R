## PREDICCION ## 

### Ejemplo regresion lineal simple s1

(edad <- c(56, 42, 72, 36, 63, 47, 55, 47, 38, 42))  # vector con los datos de la edad de las mujeres
(presion <- c(148, 126, 159, 118, 149, 130, 151, 142, 114, 141))  # presion sanguinea de las mujeres
plot(edad,presion)  # observamos el grafico y vemos que existe una relacion lineal 
abline(reg_lineal)  # recta de regresion
# determinar la edad de una mujer a partir de su presion sanguinea, la variable dependiente es EDAD

# como existe una relacion lineal en tre ambas variables podemos establecer un modelo de regresion lineal simple
# procede al ajuste lineal simple

(reg_lineal <- lm(edad~presion))  # edad = -43.5440 + 0.6774 presion
# –43.5440  es el valor de la edad para una persona de  presión sanguínea 0, lo cual no tiene sentido. 
# De hecho, en multitud de ocasiones la interpretación del parámetro  B0 no es relevante y todo el interés recae sobre la interpretación del resto de parámetros.
# B1: 0.6774  indica que, por término medio, cada mmHg (milímetros de mercurio) de incremento en la presión sanguínea de una persona supone un incremento de 0.6774. en su edad.

summary(reg_lineal)  # informacion completa del analisis
# residuos: cuanto mas pequeños mejor:  dif entre valor predicho y el valor real de la vriable dependiente
# coeficientes: valores de los parametros.si hay significacion (es decir, el t student es menor que 0.05)
# entonces rechazamos la hipotesis nula asi supone que los parametros B0 y B1 son diferentes de 0
#Mediante este contraste se comprueba si, de forma global, el modelo lineal es apropiado para modelizar los datos. En nuestro ejemplo, el p-valor asociado a este contraste es inferior
#a 0.05 por lo que, al 5% de significación podemos rechazar la hipótesis nula y afirmar que, efectivamente, el modelo lineal es adecuado para nuestro conjunto de datos.


ks.test(reg_lineal$residuals, "pnorm")


# Que se suele utilizar: 
summary(reg_lineal)
coefficients(reg_lineal) # enumera los parametros del modelo para el modelo ajustado
confint(reg_lineal)  # intervalos de confianza para los parametros del modelo 95% por defecto
fitted(reg_lineal)  # enumera los valores predichos en el modelo ajustado
residuals(reg_lineal)  # residuos
anova(reg_lineal)  # genera una tabla ANOVA para comparar 2 o mas modelos ajustados (cuando se comparan 2 o mas grupos)


# Funciones para el diagnostico de regresion(car package)
library(car)
library(carData)
qqplot() # grafico de comparaciones de cuantiles
durbinWatsontest()  # test para errores autocorrelacionados
crPlots()  # grafico de componentes y residuales juntos
ncvTest()  # nota para errores de la varianza no constantes
spreadLevelPlot()  # spread level plots
outlierTest()  # Bonferroni outolier test
avPlots()  # Added variable plots
influencePlot()  # graficos de influencia de regresion
scatterplot()  # grafico de dispersion de puntos
vif()  # inflacion de la varianza graficos

# Advertising:
getwd()
(advertising <- read.csv("./advertising.csv", header = TRUE))  # lectura de archivos
depend <- advertising$Sales  # creo la variable dependiente o la Y
# construyo la matriz de variables independientes con 1s en la columna de la constante para que la tome en cuenta
matrix_X <- cbind(1, advertising[,2:4])
head(depend)  # muestreo de la variable dependiente
head(matrix_X)  # muestre de las independientes



### Ejemplo s2: Regresion 2
advertising <- read.csv("./Advertising2.csv", header= TRUE)
(regresion1 <- lm(Sales~TV+Radio+Newspaper, data = advertising))
summary(regresion1)  # hay mas variables indepes con un p valor muy pequeño por lo tanto significativo.

library(car)
qqPlot(regresion1, labels = row.names(advertising), id.method = "identify",
       simulate = TRUE, main = "Q-Q Plot")  # graficamos el QQPLOT

# Funcion para crear un grazfico con histograma, densidad, normal y rug
residplot <- function(fit, nbreaks=10) {
  z <- rstudent(fit)
  hist(z, breaks=nbreaks, freq=FALSE,
       xlab="Studentized Residual",
       main="Distribution of Errors")
  rug(jitter(z), col="brown")
  curve(dnorm(x, mean=mean(z), sd=sd(z)),
        add=TRUE, col="blue", lwd=2)
  lines(density(z)$x, density(z)$y,
        col="red", lwd=2, lty=2)
  legend("topright",
         legend = c( "Normal Curve", "Kernel Density Curve"),
         lty=1:2, col=c("blue","red"), cex=.7)
}

residplot(regresion1)

# Jarque Bera: test para detectar NORMALIDAD en los modelos de prediccion (importe que sean normales para predecir)
  # mirar en el summary el ajuste del modelo: r2, cuanto mas cercano a1 mejor se ajusta el modelo
# realizar un histograma para ver que NO hay asimetria

(variable_resid <- resid(regresion1))
library(fBasics)  # para Jarque Bera instalar esta libreria
jbTest(variable_resid)  # se hace sobre los residuos de la regresion
# Como el p valor es inferior a 0.05 entomnces los errores no cumplen el supuesto de normalidad
# Luego el estadistico cuando mas cercano a 0 mejor.. en este caso se aleja

# Shapiro-Wilk: comprobar si la muestra ha sido generada por una distribucion normal
shapiro.test(variable_resid) 
# la W: es la region critica, si variamos el valor y obtenmos datos fuera de esta region, aceptamos H0
# pvalor: tbb vemos que el pvalor es muy pequeño, por tanto, se rechaza la H0

# Linealidad:
crPlots(regresion1)  # la estimacion suavizada (rosa) debe aproximarse a la azul.

# Varianza no constante (ncvtest) (Breusch-Pagan)
  # La H0 es que la varianza es constante(homosteceidad)
# heterosteceidad: varianza no constante.

ncvTest(regresion1)
# También se puede representar los residuos estandarizados absolutos versus los valores ajustados
# se superpone la mejor linea recta que ajusta los datos. (Si se cumple la hipótesis de homocedasticidad se espera que la linea sea horizontal)
spreadLevelPlot(regresion1) # disminuye entonces los residuos se estan haciendo mas pequeños a 
# medida que los valores ajustados se aumentan

# validacion global
library(gvlma)
(gvlmmodel <- gvlma(regresion1))  # te dice la decision


# Multicolinealidad: alta correlacion en las variables, intervalos de confianza muy anchos y 
  # se tiende a no rechazar la hipotesis nula
# se mide con VIF: Método del Factor de Inflación de la Varianza
# Para cualquier regresor la raíz del VIF indica cuantas veces es la varianza del estimador es mayor que la que se obtendría si no hubiera correlación entre los regresores. 
# Cuando √VIF>2 hay PROBLEMAS DE MULTICOLINEALIDAD

vif(regresion1)
sqrt(vif(regresion1)) > 2  # comprobamos que no haya problemas de multicolinealidad
# no hay problema

# Observaciones anomalas: OUTLIERS con  Bonferroni p-values
outlierTest(regresion1)

#Identificacion de HIGH leverage points
hat.plot <- function(fit) {
  p <- length(coefficients(fit))
  n <- length(fitted(fit))
  plot(hatvalues(fit), main="Index Plot of Hat Values")
  abline(h=c(2,3)*p/n, col="red", lty=2)
  identify(1:n, hatvalues(fit), names(hatvalues(fit)))
}
hat.plot(regresion1)  

# Identificacion de los factores influyentes: 2 metodos: Distancia de Cook o con Added Variable

# Cooks Distance D
# identify D values > 4/(n-k-1) 
cutoff <- 4/(nrow(advertising)-length(regresion1$coefficients)-2)
plot(regresion1, which=4, cook.levels=cutoff)
abline(h=cutoff, lty=2, col="red")

# Added variable plots
# add id.method="identify" to interactively identify points
avPlots(regresion1, ask=FALSE, id.method="identify")

# Grafico de influencia
influencePlot(regresion1, id.method="identify", main="Influence Plot", 
              sub="Circle size is proportial to Cook's Distance" )

# Seleccion de variables: Comparando modelos
regresion2 <- lm(Sales~TV+Radio,data=advertising)
anova(regresion2, regresion1)  # el p valor es alto por tanto el modelo 1 no se puede rechazar
# observar el AIC y BIC
  # se selecciona el modelo con menor BIC
AIC(regresion1, regresion2)
BIC(regresion1, regresion2)  # el modelo 2 es mejor en este caso

# Metodos de seleccion: https://rpubs.com/davoodastaraky/subset

# 1. Best Subset:menor RSS y mayor R2
library(leaps)
regfit.full <- regsubsets(Sales~.-X,data = advertising )
(reg.summary <- summary(regfit.full))

reg.summary$rss
reg.summary$cp
reg.summary$aic  # eliminar el NA
reg.summary$bic


# 2. Forward Stepwise: empieza sin regresores y se van añadiendo poco a poco 
library(MASS)
regfit.fwd <- regsubsets(Sales~.-X,data = advertising ,method = "forward")
summary(regfit.fwd )

# 3. Backward Stepwise: empieza con todos los regresores y se van eliminando poco a poco
library(MASS)
stepAIC(regresion1, direction="backward")
 # ni el 2 ni el 3 garantizan la seleccion del mejor modelo

regfit.bwd <- regsubsets(Sales~.-X,data = advertising ,method ="backward")  # backward echo como el forward
summary(regfit.bwd )

stepAIC(regresion1, direction="both")  # modelo mixto para asegurar que se escoja el mejor  #el 2 menor AIC


# Validacion CRUZADA: importante separar la muestra en 2 partes
library(ISLR)
set.seed(250)
(numerodatos <- nrow(advertising))  # 200 filas
train <- sample(numerodatos ,numerodatos/2)  # hacemos el train dividiendolo en 2

regres.train <- lm(Sales~.-X,advertising ,subset =train )
attach(advertising)
mean((Sales-predict(regres.train ,Auto))[-train ]^2)

set.seed(251)
regres.train2 <- lm(Sales~.-X-Newspaper,advertising,subset =train )
mean((Sales-predict(regres.train2 ,Auto))[-train ]^2)

# Leave One Out CROSS VALIDATION: se cogen todos los datos excepto 1 y se predice sobre el que se ha dejado fuera
glm.fit1 <- glm(Sales~.-X,data = advertising,family = gaussian())
coef(glm.fit1)

library (boot)
cv.err <- cv.glm(data = advertising,glm.fit1)
cv.err$delta

glm.fit2 <- glm(Sales~.-X-Newspaper,data = advertising ,family = gaussian()) 
# cv.glm: Esta función calcula el error de predicción de validación cruzada K-fold estimado para modelos lineales generalizados.
(cv.err2 <- cv.glm(data = advertising,glm.fit2))  # la K son las 200 filas que utilizamos
# este es mejor pq los deltas son mas pequeños
cv.err2$delta  # son los ajustes del error (el segundo) de no utilizar LEAVE ONE OUT CV

# K fold CV

glm.fit1 <- glm(Sales~.-X,data = advertising ,family = gaussian())

library (boot)
cv.err <- cv.glm(data = advertising,glm.fit1,K = 10)
cv.err$delta

glm.fit2 <- glm(Sales~.-X-Newspaper,data = advertising, family = gaussian())
cv.err2 <- cv.glm(data = advertising ,glm.fit2,K = 10)
cv.err2$delta

# Importancia RELATIVA: calcular los estimadores estandarizados
zData <- data.frame(scale(advertising))  #transforma los datos
zlm.fit <- lm(Sales~.-X-Newspaper,data = zData)
coef(zlm.fit) # Un aumento en una desv.tipica de gastos en publicidad en TV supone incremento 
# de ventas en 0.752 desviaciones típicas.

# Datos AUSENTES: si hubiera no hay que hacer esto
library(rminer)
nelems <- function(d) paste(nrow(d),"x",ncol(d))
#missing Data
# missing data example
# since sale does not include missing data, lets synthetically create such data:

set.seed(12345) # set for replicability
mData3 <- advertising
N = 20 # randomly assign N missing values (NA) to 2nd and 3rd attributes
srow1 <- sample(1:nrow(advertising),N) # N rows
srow2 <- sample(1:nrow(advertising),N) # N rows
mData3[srow1,2] = NA # tv
mData3[srow2,3] = NA # radio
print("Show summary of sales attributes (with NA values):")
print(summary(mData3[,1:2]))
cat("mData3:",nelems(mData3),"\n")
cat("NA values:",sum(is.na(mData3)),"\n")

# Primer metodo: Borrar casos
print("-- 1st method: case deletion --")

mData4 <- na.omit(mData3)
cat("mData4:",nelems(mData4),"\n")

cat("NA values:",sum(is.na(mData4)),"\n")  # ahora hay 0 NAS

# Segundo metodo: average imputation for tv, mode imputation for radio:
# sustitucion de NAs por la media
print("-- 2nd method: value imputation --")
print("original tv summary:")
print(summary(mData3$TV))  # mean 147.30

meanTV <- mean(mData3$TV,na.rm=TRUE)
mData5 <- imputation("value",mData3,"TV",Value=meanTV)
print("mean imputation TV summary:")
print(summary(mData5$TV))

# sustituir NA valores por la mediana (most common value of mData$radio):
print("original Radio summary:")
print(summary(mData3$Radio))

mData5 <- imputation("value",mData5,"Radio",Value=(which.max(table(advertising$Radio))))
print("mode imputation Radio summary:")
print(summary(mData5$Radio))

# tercer metodo: hot deck
# sustituir los NAs por los valores que tengan caracteristicas mas parecidas (1-nearest neighbor):
print("-- 3rd method: hotdeck imputation --")
print("original Radio summary:")
print(summary(mData3$TV))
mData6=imputation("hotdeck",mData3,"TV")
print("hot deck imputation age summary:")
print(summary(mData6$TV))
# substitute NA values by the values found in most similar case:
print("original Radio summary:")
print(summary(mData3$Radio))
mData6=imputation("hotdeck",mData6,"Radio")
print("hot deck imputation Radio summary:")
print(summary(mData6$Radio))
cat("mData6:",nelems(mData6),"\n")  # se queda con todas las observaciones pq ha eliminado los NAs
cat("NA values:",sum(is.na(mData6)),"\n")


# COMPARACION DE LAS DENSIDADES con las distintas formas de deshacer de los NAs
# comparison of age densities (mean vs hotdeck):
library(ggplot2)
meth1 <- data.frame(length=mData4$TV)
meth2 <- data.frame(length=mData5$TV)
meth3 <- data.frame(length=mData6$TV)
meth0 <- data.frame(length=mData3$TV)
meth1$method="omit"
meth2$method="average"
meth3$method="hotdeck"
meth0$method="Original"
all=rbind(meth1,meth2,meth3,meth0)
ggplot(all,aes(length,fill=method))+geom_density(alpha = 0.2)

# parece que el mejor es hotdeck

# Ejemplos : s3
# Variables  binarias

library(faraway)
curve(ilogit(x),-6,6, xlab = expression(eta), ylab = "p")  # Sigmoide de clasificacion binaria
# Ejemplo: Tarjetas de crédito: entrar en mora o no
#Los datos son del libro ISLR
#A data frame with 10000 observations on the following 4 variables.

#default: A factor with levels No and Yes indicating whether the customer defaulted on their debt
#student: A factor with levels No and Yes indicating whether the customer is a student
#balance: The average balance that the customer has remaining on their credit card after making their monthly payment
#income: Income of customer

library(ISLR)  # Introduction to Statistical Learning in R
data("Default")  # Carga de datos de una libreria interna
summary(Default)  # hay 333 en default


# ANALISIS EXPLORATORIO DE LAS VARIABLES BINARIAS
sapply(Default, sd)  # aplicamos la desviacion tipica a cada variable de la biblioteca Default
cor(Default)  # no se puede pq la x no es numerica

# Tabla de contingencia entre los predictores y el outcome categorico
xtabs(~default + student, data = Default)  # Matriz de confusion

library(ggplot2)
ggplot(data = Default,aes(default,balance)) +  # boxplots donde se ven los outliers
  geom_boxplot() 

ggplot(data = Default,aes(default,income)) +
  geom_boxplot() 

ggplot(data = Default, aes(default, student)) + 
  geom_boxplot()  # cuando comparas 2 variables binarias el BOXPLOT no tiene sentido


# Funcion GLM
  # estimamos utilizando regresion lineal con la funcion glm
modelo01 <- glm(default~student+balance+income, data = Default, family = "binomial")  # en binario indicar familia y GLM
summary(modelo01)  # version larga de summary
sumary(modelo01)  # version corta del summary

# Las variables no tienen valores 0 o 1, por tanto, si se quiere aplicar se utiliza relevel (ya que se aplica por orden alfabetico)
# Relevel: reordenar los niveles de los factores

# ejemplo
warpbreaks$tension <- relevel(warpbreaks$tension, ref = "M")
summary(lm(breaks ~ wool + tension, data = warpbreaks))


# ¿Cómo se interpretan los coeficientes del modelo?
# Podemos o bien: * Calcular la probabilidad de default de la tarjeta 
# * Ver como cambia la probabilidad cuando varia el valor de un predictor.

# ¿Que ocurre con la tarjeta de los estudiantes? sbemos que ser estudiante: MUY SIGNIFICATIVO
datos_nuevos <- with(data = Default,
                 data.frame(balance = mean(balance), income = mean(income), student = c("Yes","No")))
predict(modelo01, newdata = datos_nuevos)
# with is a generic function that evaluates expr in a local environment constructed from data
# sirve para evaluar una expresion y predecir sobre esos nuevos datos
?predict

predict(modelo01, newdata = datos_nuevos, type = 'response')  # response: devuelve probabilidades predichas

# ¿Que ocurre con los saldos?
datos_nuevos2 <- with(Default,
                 data.frame(balance = mean(balance)*c(0.75,1.25), income = mean(income), student = c("Yes")))
predict(modelo01, newdata = datos_nuevos2)

predict(modelo01,newdata = datos_nuevos2, type = "response")

# ¿Que ocurre con los ingresos?

datos_nuevos3 <- with(Default,
                 data.frame(balance = mean(balance), income = mean(income)*c(0.75,1.25), student = c("Yes")))
predict(modelo01, newdata = datos_nuevos3)

predict(modelo01, newdata = datos_nuevos3, type = "response")

# ¿Que ocurre con las tarjetas de los estudiantes?

modelo02 <- glm(default~student, family= "binomial", data = Default)
summary(modelo02)$coef  # estos son los coeficientes
summary(modelo01)$coef

ggplot(data = Default,aes(student,balance))+ 
  geom_boxplot() 

seq.balance <-seq (min(Default$balance), max(Default$balance), 50)

newdata4 <- with(Default,
                 data.frame(balance=rep(seq.balance,2) ,
                            income = mean(income), 
                            student =c(rep(c("Yes"),length(seq.balance)),rep(c("No"),length(seq.balance))))
)

pred.default <- predict(modelo01, newdata=newdata4,type="response")

df.glm <- data.frame(x=rep(seq.balance,2),y=pred.default,stu = newdata4$student)

ggplot()+ 
  geom_point(aes(Default$balance,as.numeric(Default$default)-1,color=Default$student),position=position_jitter(height=0.05, width=0),alpha=0.7)+
  geom_line(aes(df.glm$x,df.glm$y, color=df.glm$stu),alpha=0.4,size=2)+
  xlab("Balance") + geom_point()+
  ylab("Pr (Default)") 

# https://rpubs.com/emilopezcano/logit

# Interpretacion de los ODDS: numero de casos en los que el evento ocurre vs. numero de casos en los que no ocurre
# un aumento de variable xi, supone un aumento de odds en la exponencial

  # la interpretacion de los coeficientes cambia: ya que el modelo GLM no ajusta una variable respuesta
    #sino una funcion de enlace LOGIT

# ir a la significativa en z valor: coeficiente de la variable
summary(modelo01)  # para un estudiante el logaritmo del odds ratio indica que disminuirá entrar
# en default 2.738 unidades por cada unidad que aumenta el numero de estudiantes

# Para la interpretacion de coeficientes: vamos a exponenciar
exp(coef(modelo01))  # es lo mismo coef que coefficients

# indican que al aumentar el numero de estudiantes en un punto, se reduce x% 
# la probabilidad de entrar en default con un porcentaje elevado (correspondiente a 1 - lo que vaya detras de la coma)
# (lo que va despues de la coma corresponde al %)

exp(coef(modelo01))/(1+exp(coef(modelo01)))
exp(cbind(OR = coef(modelo01), confint(modelo01)))  # los odds de default aumentan en un 0,5% por cada UNIDAD que aumente de balance

# El efecto de tener un saldo, por ej, de 500 será:
exp(coef(modelo01)[3]*100)  # de la 3 que corresponde a balance, multiplicamos el coef por 100
exp(coef(modelo01)[3]*100)/(1+exp(coef(modelo01)[3]*100))

# diagnostico de toda la informacion: test ANOVA utilizando CHISQUARE y obs. p valor
anova(modelo01, test = "Chisq")

# realizar un analisis de residuos
library(ggfortify)
autoplot(mod01, which = 1:6, label.size = 3)

# otra posibilidad para el diagnostico y seleccion del modelo
drop1(modelo01,test = "Chi")


# clasificacion BINARIA
#La elección de una gran probabilidad de corte resultará en pocos casos
# se predijo como 1, y elegir una pequeña probabilidad de corte resultará en muchos casos se está prediciendo como 1.

predict.modelo1 <- predict(modelo01, type = "response")
(glm.predict <- rep("No", 10000))  # replica elementos de vectores o listas
# replicar los valores no y las veces que se han replicado
glm.predict[predict.modelo1 > 0.5] = "Yes"  # a las predicciones por encima 0.5
# en el glm le asignamos Yes
(mean(glm.predict == Default$default))  # sacar la media de la predicion aquellos
# que correspondan a default (se apilan alrededor del 0.9732) # encima de 0.5

# probamos con otro cutoff probability
glm.pred <- rep("No", 10000)
glm.pred[predict.modelo1  > .6] = "Yes"
mean(glm.pred == Default$default)

# Aumento el cutoff
glm.pred <- rep("No", 10000)
glm.pred[predict.modelo1  > .7] = "Yes"
mean(glm.pred == Default$default)  # la prob disminuye cuanto mas aumento el cutoff
# en ete caso 

# disminuyo el cutoff
glm.pred <- rep("No", 10000)
glm.pred[predict.modelo1  > .4] = "Yes"
mean(glm.pred == Default$default)  # la prob es mayor pero no mayor que 0.5

glm.pred <- rep("No", 10000)
glm.pred[predict.modelo1  > .999] = "Yes"
mean(glm.pred == Default$default)   # la prob disminuye si ponemos el cutoff tan alto

# table() mean(glm.pred == defaultVector) table(predict(mod01,type=“response”) > 0.2) table(predict(mod01,type=“response”) > 0.0001)

# Validacion cruzada del modelo en variables binarias
# PRIMERO: set de validacion
set.seed(1)  # establecemos una semilla

train <- sample(10000,5000)  # Muestra de 5000 como train
Defaultx <- Default[-train,]  # Defaultx no incluye train data = test data

glm.fit <- glm(default~ balance + student, data = Default, family = binomial, subset = train)
# modelo ajustado con el vector de observaciones de  train para los datos Default

# Con el modelo ya construido, se hace la prediccion sobre el TEST
glm.probs <- predict(glm.fit, data = Defaultx, type = "response")  # con los datos de test

# Crear un vector para guardar los resultados (No default) 
glm.pred <- rep("No", 5000)  # hacemos 5000 NO

# Reemplazar NO por YES cuando la probabilidad es mayor del 50%
glm.pred[glm.probs > .5] = "Yes"

# Crear un vector con los resultados de test
defaultVector <- Defaultx$default 

# Calculo de la media 
mean(glm.pred == defaultVector)  # hay un 95.12% bien clasificado

## Ahora hago K-fold CV
#Aqui se utiliza boot
library(boot)
# Semilla
set.seed(2)

# Estimar el modelo
glm.fit1 <- glm(default~balance + student, data = Default, family = binomial)

# Crear un vector para guardar los resultados
cv.error <- rep(0,3)

# Guardar los resultados para cada K  validation set. K= {3,5,10} 
cv.error[1] <- cv.glm(Default, glm.fit1, K=3)$delta[1]
cv.error[2] <- cv.glm(Default, glm.fit1, K=5)$delta[1]
cv.error[3] <- cv.glm(Default, glm.fit1, K=10)$delta[1]
cv.error # del vector creado, los resultados de cada Kfold : 3,5 y 10 se guardan aqui

1- mean(cv.error) # se clasifican mejor con el KFOLD
# Consiste en repetir y calcular la media aritmética obtenida de las 
    # medidas de evaluación sobre diferentes particiones. 

# Seleccion de los parametros
  # CV con TODAS LAS VARIABLES (ya que antes solo se ha hecho con las significativas)
set.seed(1)
train <- sample(10000,5000)

Defaultx <- Default[-train,]
glm.fit <- glm(default~income + balance + student, data = Default, family = binomial, subset = train)
glm.probs <- predict(glm.fit, Defaultx, type = "response")
glm.pred <- rep("No", 5000)
glm.pred[glm.probs > .5] = "Yes"

defaultVector <- Defaultx$default 
mean(glm.pred == defaultVector)  # hay 95.12% bien clasificadas, menos que con las variables significativas

# Ahora con KFOLDS con TODAS LAS VARIABLES
# Semilla
set.seed(2)

# Estimar el modelo
glm.fit1 <- glm(default~income +balance + student, data = Default, family = binomial)

# Crear un vector para guardar los resultados
cv.error <- rep(0,3)


# Guardar los resultados para cada K  validation set. K= {3,5,10} 
cv.error[1] <- cv.glm(Default, glm.fit1, K=3)$delta[1]
cv.error[2] <- cv.glm(Default, glm.fit1, K=5)$delta[1]
cv.error[3] <- cv.glm(Default, glm.fit1, K=10)$delta[1]

cv.error
1- mean(cv.error)   # con las variables sig: 0.9786227 y con esta 0.978607 lo cual es menor y es mejor con las significativas solo


#### DESPUES DE LA TEORIA, ejemplo credito alemania

# El objetivo de este caso es conseguir que se entienda la regresión logística (clasificación binaria)
# y algunas ideas importantes,tales como la validación cruzada, la curva ROC, la probabilidad de corte.

credit.data <- read.csv("./credit0.csv", header=TRUE)  # carga de los datos

# Division de los datos en TRAINING y TEST para entrenar el modelo
set.seed(1234)
n <- nrow(credit.data)  # filas
id_train <- sample(1:n , 0.90*n)  # de los dato que hay en credit data le damos el 90% del peso al train
credit.train <-  credit.data[id_train,]
credit.test <- credit.data[-id_train,]

nrow(credit.train)  # train tiene 4500 observaciones o filas
ncol(credit.train)  # train tiene 63 columnas o variables
colnames(credit.train)  # nombres de las columnas o variables

# Tras el analisis exploratorio, REGRESION LOGISTICA O CLASIFICACION BINARIA
credit.glm0<-glm(Y~.-id, family=binomial, data = credit.train) # modelo (excepto id)
summary(credit.glm0)  # el error es por x9, por tanto, se puede tratar de ajustar el modelo sin x9

# PREDICCION Y REGRESION LOGISTICA: vemos las variables influyentes y vamos a construir ahora un modelo 
# con distintas combinaciones
# Ahora supongamos que hay 2 modelos que queremos probar, uno con todas las variables X (credit.glm0),
# y uno con X3, X8 y X11_2 (credit.glm1).

credit.glm1<-glm(Y~X3+X8+X11_2,family=binomial,data = credit.train) # con los datos de training
AIC(credit.glm0)  # AIC de este modelo: 1723.159
AIC(credit.glm1)  # AIC: 1908.961
BIC(credit.glm0)  # 2120.692
BIC(credit.glm1)  # 1934.608

# Ahora cual cogemos? el de menor AIC o menor BIC? Deicision de clasificacion
# nos decantamos por el BIC


# Toma de decisiones en la clasificacion binaria
hist(predict(credit.glm1))

# Prediccion de probabilidad para CADA observacion
hist(predict(credit.glm1,type="response"))

# Estas tablas ilustran el impacto de la elección de diferentes probabilidad de corte.
# La elección de una gran probabilidad de corte resultará en pocos casos se predijo como 1, 
# y elegir una pequeña probabilidad de corte resultará en muchos casos se está prediciendo como 1.

table(predict(credit.glm1,type="response") > 0.5)
table(predict(credit.glm1,type="response") > 0.2)
table(predict(credit.glm1,type="response") > 0.0001)  # Se clasifican bien con una probabilidad de corte muy pequeña


# PREDICCION FUERA DE LA MUESTRA Y DENTRO DE LA MUESTRA

# 1. Dentro de la muestra: es decir, ver el RENDIMIENTO del conjunto de entrenamiento
# Supongamos que la probabilidad de corte se elige es 0,2.
# La segunda sentencia genera un vector lógico (VERDADERO o FALSO) para observación del conjunto (convertir a numerico)
# de entrenamiento que tiene una probabilidad mayor de 0,2. La tercera transforma el vector 
# lógico numérico (0 o 1).

prob.glm1.insample <- predict(credit.glm1,type="response")
predicted.glm1.insample <- prob.glm1.insample > 0.2 # establecemos la prob de corte en 0.2
(predicted.glm1.insample <- as.numeric(predicted.glm1.insample))  

table(credit.train$Y, predicted.glm1.insample, dnn=c("Truth","Predicted"))  # Matriz de Confusion
# Calculamos la tasa de error para los datos dentro del conjunto de entrenamiento
mean(ifelse(credit.train$Y != predicted.glm1.insample, 1, 0)) 

# 2. Fuera de la muestra (el RENDIMIENTO en el conjunto de prueba o test)
  # damos los mismos pasos
# OPCION 1:

prob.glm1.outsample <- predict(credit.glm1,credit.test,type="response")
predicted.glm1.outsample <-  prob.glm1.outsample> 0.2
predicted.glm1.outsample <- as.numeric(predicted.glm1.outsample)
table(credit.test$Y, predicted.glm1.outsample, dnn=c("Truth","Predicted"))
mean(ifelse(credit.test$Y != predicted.glm1.outsample, 1, 0))  # la tasa de erros es practicamente la misma

# OPCION 2: mismos resultados que la opcion 1
glm1.outsample.logodds <- predict(credit.glm1,credit.test)
predicted.glm1.outsample <- exp(glm1.outsample.logodds)/(1 + exp(glm1.outsample.logodds)) > 0.2
predicted.glm1.outsample <- as.numeric(predicted.glm1.outsample)
table(credit.test$Y, predicted.glm1.outsample, dnn=c("Truth","Predicted"))
mean(ifelse(credit.test$Y != predicted.glm1.outsample, 1, 0))

# IMPORTANTE: la tasa de error fuera de la muestra por norma general suele ser mayor
library('verification')  # libreria curva ROC

# Para trazar la curva ROC, el primer argumento de roc.plot es el vector con los VALORES REALES
# “Una observación binario (codificado {0, 1})”. 
# El segundo argumento es el vector CON PROBABILIDAD PREDICHA
    # Es decir, vemos el ajuste de los modelos, si es bueno o no

roc.plot(credit.test$Y == '1', prob.glm1.outsample)  # los que corresponden a uno en la realidad y los 
# que se han predicho

roc.plot(credit.test$Y == '1', prob.glm1.outsample)$roc.vol  #OBTENER AREA DEBAJO DE LA CURVA (AUC)

# Poner todo en el grafico ROC curve
prob.glm0.outsample <- predict(credit.glm0,credit.test,type="response")
roc.plot(x= credit.test$Y == '1', pred=cbind(prob.glm0.outsample,prob.glm1.outsample), legend=TRUE, leg.text=c("Full Model","X_3, X_8, and X_11_2"))$roc.vol


# Libreria 2 de la curva ROC
library(ROCR)
pred <- prediction(prob.glm1.outsample, credit.test$Y)  # comparamos la prediccion y los datos reales
perf <- performance(pred, "tpr", "fpr")  # hacemos un plot del performance
plot(perf, colorize=TRUE)

# Para obtener el area debajo de la curva: AUC
unlist(slot(performance(pred, "auc"), "y.values"))

# Validacion CRUZADA o CV
pcut = 0.2
#Symmetric cost
cost1 <- function(r, pi){
  mean(((r==0)&(pi>pcut)) | ((r==1)&(pi<pcut)))
}
#Asymmetric cost
cost2 <- function(r, pi){
  weight1 = 2
  weight0 = 1
  c1 = (r==1)&(pi<pcut) #logical vector - true if actual 1 but predict 0
  c0 = (r==0)&(pi>pcut) #logical vector - true if actual 0 but predict 1
  return(mean(weight1*c1+weight0*c0))
}

library(boot)
credit.glm3<-glm(Y~X3+X8+X11_2,family=binomial,credit.data);  
cv.result <-  cv.glm(credit.data, credit.glm3 , cost1, 10)
cv.result$delta

# Buscando la probabilidad de corte optima

# El siguiente código hace una búsqueda de rejilla (grid search) pcut = 0,01 a pcut = 0,99
# con el objetivo de minimizar el coste total en el conjunto de entrenamiento. Estoy utilizando una 
# función de coste asimétrica suponiendo que dar una mal préstamo cuesta: 10 veces más que el coste 
# de rechazar la solicitud de alguien que puede pagar.

#define the searc grid from 0.01 to 0.99
searchgrid = seq(0.01, 0.99, 0.01)
#result is a 99x2 matrix, the 1st col stores the cut-off p, the 2nd column stores the cost
result = cbind(searchgrid, NA)
#in the cost function, both r and pi are vectors, r=truth, pi=predicted probability
cost1 <- function(r, pi){
  weight1 = 10
  weight0 = 1
  c1 = (r==1)&(pi<pcut) #logical vector - true if actual 1 but predict 0
  c0 = (r==0)&(pi>pcut) #logical vector - true if actual 0 but predict 1
  return(mean(weight1*c1+weight0*c0))
}
credit.glm1<-glm(Y~X3+X8+X11_2,family=binomial,credit.train); 
prob <- predict(credit.glm1,type="response")
for(i in 1:length(searchgrid))
{
  pcut <- result[i,1]
  #assign the cost to the 2nd col
  result[i,2] <- cost1(credit.train$Y, prob)
}
plot(result, ylab="Coste del Set de Entranamiento")   # el coste se minimiza con un cutoff de 0.08

# buscar el resultado minimo de la grafica del coste
result[which.min(result[,2]),]


# Tambien se puede buscar el coste con CV
searchgrid = seq(0.01, 0.6, 0.02)
result = cbind(searchgrid, NA)
cost1 <- function(r, pi){
  weight1 = 10
  weight0 = 1
  c1 = (r==1)&(pi<pcut) #logical vector - true if actual 1 but predict 0
  c0 = (r==0)&(pi>pcut) #logical vector - true if actual 0 but predict 1
  return(mean(weight1*c1+weight0*c0))
}
credit.glm1<-glm(Y~X3+X8+X11_2,family=binomial,credit.train); 
for(i in 1:length(searchgrid))
{
  pcut <- result[i,1]
  result[i,2] <- cv.glm(data=credit.train,glmfit=credit.glm1,cost=cost1, K=3)$delta[2]
}
plot(result, ylab="CV Cost")

result[which.min(result[,2]),]  # el coste se minimiza con un cutoff de 0.09

##  Sesion 6: Modelos NO lineales

# 1. Regresiones polinomicas y funciones de paso
library(ISLR)  # libreria default con librerias con datos
attach(Wage)
require(knitr)  # para que funcione kable
library(knitr)

################ OPCION 1 ######################

# Funciones polinomicas: con poly() donde especificamos la variable y el grado del polinomio
fit = lm(wage~poly(age, degree = 4), data = Wage)
kable(coef(summary(fit)))  # esto es, nos devuelve para age: desde el 1 grado al 4
# La función devuelve una matriz de polinomios ortogonales, lo que significa que cada columna
  # es una combinación lineal de las variables en las variables age, age^2, age^3 y age^4

# crear un vector de años si vamos a predecir y hacemos un plot de los datos y de la funcion polinomica
(ageLims <- range(age))  # devuelve desde donde a donde esta el rango de edades
(age.grid <- seq(from = ageLims[1], to = ageLims[2]))  # secuencia de todas las edades que hay

pred <- predict(fit, newdata = list(age = age.grid),
                se = TRUE)  # predecir sobre el modelo polinomico con la variable de todas las edades
se.bands <- cbind(pred$fit + 2*pred$se.fit,
                  pred$fit - 2*pred$se.fit) 

plot(age,wage,xlim = ageLims ,cex = .5,col = "darkgrey")  # grafico
title("Degree -4 Polynomial ",outer = T)  # titulo
lines(age.grid,pred$fit,lwd=2,col="blue")  # lineas 
matlines(age.grid,se.bands,lwd=2,col="blue",lty=3)  # bandas de limites

# ANOVA TEST: podríamos usarlo ya que realiza un análisis de varianza para probar la hipótesis nula
# de que un modelo M es suficiente para explicar los datos, en contra de la hipótesis alternativa 
# de que se requiere un modelo M2 más complejo.

fit.1=lm(wage~age,data=Wage)
fit.2=lm(wage~poly(age,2),data=Wage)
fit.3=lm(wage~poly(age,3),data=Wage)
fit.4=lm(wage~poly(age,4),data=Wage)
fit.5=lm(wage~poly(age,5),data=Wage)

anova(fit.1, fit.2, fit.3, fit.4, fit.5)  # los mas significativos son el de grado 2 y 3
# por tanto, se rechaza la H0 y por tanto no todo el modelo es suficiente para explicar los datos
# Son el modelo cubico y el cuadratico suficientes para "fit the data" : hay preferencia de modelos simples

################ OPCION 2 ######################

# OTRA OPCION: tambien podemos escoger un grado polinomico con VALIDACION CRUZADA 
library(boot)
set.seed(17)

(cv.errors <- data.frame(degree = seq(1,5,1), 
                        error = rep(NA, 5)))  # creamos un df con 2 columnas, una con nums del 1-5
# y otra que se llame error con NAS repetidos 5 veces

for (i in 1:5) {  # hacer un bucle through 1-5 degree polynomials
  glm.fit <- glm(wage~poly(age, i), data = Wage)
  cv.errors$error[i] <- cv.glm(Wage, glm.fit, K = 10)$delta[1]
  
}

kable(cv.errors)  # generador de tablas
# Aquí realmente vemos que el error cruzado más bajo es para un polinomio de 4 grados, sin embargo, 
# no perdemos mucho terreno al elegir un modelo de tercer o segundo orden.

# Prediccion:  si los individuos ganan mas de $250 000 con la expresion I(wage>250) para crear una 
  # respuesta booleana variable; type='response' in the prediction to extract the probabilities.

fit <- glm(I(wage > 250)~poly(age, 4), data=Wage,
           family=binomial)  # al ser booleana ,la family es binomial
pred <- predict(fit, newdata=list(age=age.grid), 
                se=TRUE, type='response')
# Sin embargo, los intervalos de confianza para las probabilidades no serían sensibles, 
# ya que terminamos con algunas probabilidades negativas. 
# Para generar intervalos de confianza tiene más sentido transformar las predicciones logit.

# MEJOR: predicciones logit (para no tener probabilidades negativas)
pred <- predict(fit, newdata=list(age=age.grid), 
                se=TRUE)

pfit <- exp(pred$fit) / (1+exp(pred$fit)) # Convert logit
se.bands.logit<- cbind(pred$fit + 2*pred$se.fit,
                       pred$fit - 2*pred$se.fit)

se.bands <- exp(se.bands.logit) / (1+exp(se.bands.logit))

# Graficamos esto
plot(age,I(wage>250),xlim=ageLims ,type="n",ylim=c(0,.2))
points(jitter(age), I((wage>250)/5),
       cex=.5,pch="|", col =" darkgrey ")
lines(age.grid,pfit,lwd=2, col="blue")
matlines(age.grid,se.bands,lwd=1,col="blue",lty=3)


# Funcion de paso: utilizamos la funcion de cut() para dividir los datos 
table(cut(age, 4))  # dividimos en 4
fit <- lm(wage~cut(age, 4), data=Wage)
coef(summary(fit))


# SPLINES: Aquí haremos uso de la biblioteca de splines. La función bs ()
# genera la matriz completa de funciones básicas con el conjunto especificado de tejidos.
# Por defecto utiliza una spline cúbica.

library(splines)

fit <- lm(wage~bs(age, knots=c(25,40,60)), data=Wage)
pred <- predict(fit, newdata=list(age=age.grid), se=T)

plot(age, wage, col='gray')
lines(age.grid, pred$fit, lwd=2)
lines(age.grid, pred$fit+2*pred$se, lty='dashed')
lines(age.grid, pred$fit-2*pred$se, lty='dashed')

# Ya que estamos usando una SPLINE CUBICAcon tres nudos, esto produce una spline con seis funciones básicas.
# Esto equivale a 7 GRADOS DE LIBERTAD (incluida la intersección). También podemos usar el parámetro 
# df = para especificar una spline con nudos de cuantiles. Tenga en cuenta que la función también tiene 
# una opción de degree = si queremos especificar un tipo diferente de spline.

dim(bs(age, knots=c(25,40,60)))  
dim(bs(age, df=6))  # te dice lo mismo que arriba
attr(bs(age, df=6), 'knots') # cuantiles de "age"  # donde se encuentran los 3 nodos


# Para meter un spline NATURAL utilizamos ns() o natural spline
fit2 <- lm(wage~ns(age, df=4), data=Wage)
pred2 <- predict(fit2, newdata=list(age=age.grid), se=T)

# re-plot the data
plot(age, wage, col='gray')
lines(age.grid, pred$fit, lwd=2)
lines(age.grid, pred2$fit, col='red', lwd=2)

# este spline natural no sufre por las varianzas en las colas
# También podemos ajustar una spline de suavizado sin más que smooth.spline ().
# Aquí encajamos una spline con 16 grados de libertad, luego la spline elegida por validación cruzada,
# que produce 6.8 grados de libertad.
plot(age, wage, xlim=ageLims, col='gray')
title('Smoothing Spline')
fit <- smooth.spline(age, wage, df=16)  # grados de libertidad en el smooth spline
fit2 <- smooth.spline(age, wage, cv=TRUE)  # validacion cruzada
fit2$df  # sacamos los grados de libertad de la validacion cruzada

lines(fit, col='red', lwd=2)
lines(fit2, col='blue', lwd=1)
legend('topright', legend=c('16 DF', '6.8 DF'),  # indicamos en la leyenda la DF de arriba
       col=c('red','blue'), lty=1, lwd=2, cex=0.8)

# Regresion local: es decir, hacemos vencidarios de 20-50% de los datos 

# Para realizar una regresión local usamos la función loess ().
# Esta función también está integrada en ggplot2. Aquí ajustamos una función de loess con un rango de
# 0.2 y 0.5: es decir, cada vecindario consiste de 20% o 50% de las observaciones 
# Cuanto mayor sea el tramo, más suave será el ajuste.

library(ggplot2)

ggplot(data=Wage, aes(x=age, y=wage))+geom_point(color='gray') +
  geom_smooth(method='loess', span=0.2) +
  geom_smooth(method='loess', span=0.5, color='red')+
  theme_bw()

# GAM: para predecir la variable "SALARIO" usando un spline natural de "year, age and education"
# Esto se hace con un moedlo de regresion simple 
(gam1 <- lm(wage ~ ns(age, 5) + education, 
           data=Wage))  # se predice sobre wage con un spline natural y distintos grados variable edad

# Para meter splines mas complejos utilizamos la libreria gam() y la s() se utiliza para un spline de suavizado
library(gam)
library(foreach)  # computacion en paralelo

gam2 <- gam(wage~s(year, 4) + s(age, 5) + education, 
            data=Wage)
# Graficamos ambos modelos, el lineal simple con un spline natural y el que utiliza splines de suavizado
par(mfrow=c(1, 3))
plot(gam2, se=TRUE, col='blue')  # modelo 2
#library(mgcv)
plot.gam(gam1, se = TRUE, col = 'red')    #modelo 1

# vemos que la variable YEAR es lineal. 
# Vamos a crear un nuevo modelo y utilizar ANOVA test para decidir entre ellos 
gam3 <- gam(wage~year + s(age, 5) + education, data=Wage)
anova(gam1, gam3, gam2, test='F')  # esto indica: que el mejor modelo es el 2, sin meter YEAR como 
# lineal, ya que pierde significacion (p valor del modelo 3 es 0.35)
summary(gam2)
# Within the model with the non-linear relationship for YEAR we can again confirm that this component
# does not contribute to the model.  --> relacion no linear


# Luego, ajustamos una REGRESION LOCAL como bloques de construcción en un GAM usando la función lo ().
gam.lo <- gam(wage ~ s(year, df=4) + lo(age, span=0.7) + education,
              data=Wage)
plot.gam(gam.lo, se=TRUE, col='green')

# tambien podemos usar REGRESION LOCAL para crear interaccion antes del GAM
gam.lo.i <- gam(wage ~ lo(year, age, span=0.5) + education,
                data=Wage)
library(akima)
plot(gam.lo.i)  # como se distribuye en el surface

################### #########################  ################
                                                              #
# MODELOS DE REGULARIZACION : LASSO Y RIDGE                   #
  # http://uc-r.github.io/regularized_regression              #
  # https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html #
# MODELOS GAM                                                 #
# http://rpubs.com/ryankelly/GAMs                             #
################### #########################  ################




