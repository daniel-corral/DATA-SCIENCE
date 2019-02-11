#################################################
################### MACHINE LEARING #############
#################################################

#1. Introduccion
#Carga de datos
data <- read.csv("4_1_data.csv", sep = ",", dec = ".")

#Carga de librerías
library(gradDescent) #if (!require("gradDescent")) install.packages("gradDescent")
library(testthat) #if (!require("testthat")) install.packages("testthat")


#2. Analisis Exploratorio
#3. Limpieza de datos
#4. Metodología
#5. Conclusion

#Load data
data <- read.csv("4_1_data.csv", sep = ",", dec = ".")

#Create plot
plot(data$score.1, data$score.2, col = as.factor(data$label), xlab = "Score-1", ylab = "Score-2")
#Predictor variables
X <- as.matrix(data[, c(1,2)])

#Add ones to X in the first column (matrix multiplication x b)
X <- cbind(rep(1, nrow(X)), X)

########
Sigmoid <- function(x) { 
  1 / (1 + exp(-x))
}

# feed with data
x <- seq(-5, 5, 0.01)

# and plot
plot(x, Sigmoid(x), col = 'blue', ylim = c(-.2, 1))
abline(h = 0, v = 0, col = "gray60")

CostFunction <- function(parameters, X, Y) {
  n <- nrow(X)
  # function to apply (%*% Matrix multiplication)
  g <- Sigmoid(X %*% parameters)
  J <- (1/n) * sum((-Y * log(g)) - ((1 - Y) * log(1 - g)))
  return(J)
}


#Response variable
Y <- as.matrix(data$label)
#Intial parameters
initial_parameters <- rep(0, ncol(X))

#Cost at inital parameters
CostFunction(initial_parameters, X, Y)

# We want to minimize the cost function. Then derivate this funcion
TestGradientDescent <- function(iterations = 1200, X, Y) {
  
  # Initialize (b, W)
  parameters <- rep(0, ncol(X))
  # Check evolution
  print(paste("Initial Cost Function value: ", 
              convergence <- c(CostFunction(parameters, X, Y)), sep = ""))
  
  # updating (b, W) using gradient update
  
  # Derive theta using gradient descent using optim function
  # Look for information about the "optim" function (there are other options)
  parameters_optimization <- optim(par = parameters, fn = CostFunction, X = X, Y = Y, 
                                   control = list(maxit = iterations))
  #set parameters
  parameters <- parameters_optimization$par
  
  # Check evolution
  print(paste("Final Cost Function value: ", 
              convergence <- c(CostFunction(parameters, X, Y)), sep = ""))
  
  return(parameters) 
}

# How to use
parameters <- TestGradientDescent(X = X, Y = Y)

# probability of admission for student (1 = b, for the calculos)
new_student <- c(1,25,78)
print("Probability of admission for student:")

print(prob_new_student <- Sigmoid(t(new_student) %*% parameters))



# load
library("gradDescent")

# We want to minimize the cost function. Then derivate this funcion
TestGradientDescent2 <- function(iterations = 1200, learning_rate = 0.25, the_data) {
  
  # label in the last column in dataSet
  model <- gradDescentR.learn(dataSet = the_data, featureScaling = TRUE, scalingMethod = "VARIANCE", 
                              learningMethod = "GD", control = list(alpha = learning_rate, maxIter = iterations), 
                              seed = 1234)
  
  model
}


#######TESTEO DEL CÓDIGO
# How to use
TestGradientDescent2(the_data = data)


test_that("Test TestGradientDescent",{
  parameters <- TestGradientDescent(X = X, Y = Y)
  # probability of admission for student (1 = b, for the calculos)
  new_student <- c(1,25,78)
  prob_new_student <- Sigmoid(t(new_student) %*% parameters)
  print(prob_new_student)
  expect_equal(as.numeric(round(prob_new_student, digits = 4)), 0.0135)
  # Fail, test
  # expect_equal(as.numeric(round(prob_new_student, digits = 4)), 0.0130)
})


###ASIGNMENT: 
  #1. Testing: sacar la matriz de confusion
library(caret)

table()



#2.  si reflejamos iteraciones frente al coste
#modificar el codigo para que me de el grafico de como a descendiendo el coste en funcion de como se va incrementando el num de iteraciones
#en vez de devolver solo la matriz de parametros que devuelva un df que se vea como evoluciona para cada iteracion el coste




