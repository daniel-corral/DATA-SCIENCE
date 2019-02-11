###SHINY EJERCICIO MODELOS POLINOMIALES###
library(shiny)
library(ggplot2)
library(plotly)
library(shinydashboard)


shinyUI(
  fluidPage(
    titlePanel("Modelos Polinomiales"),
    sidebarLayout(
      sidebarPanel(
        ##Campos Numericos
        numericInput(inputId = "muestra", label = "Tamaño muestral",
                     value = 50, min = 1),
        numericInput(inputId = "corr", label = "Correlación",
                     value = 0.75, min = -1, max = 1, step = 0.1),
        numericInput(inputId = "ordenpol", label = "Orden Polinómico",
                     value = 4, min = 1, step = 1),
        
        ##Botones 
        actionButton(inputId = "genmuestra", label = "Generar muestra"),
        actionButton(inputId = "anmodelo", label = "Añadir modelo"), 
        #Modelo
        sliderInput("intercept",
                    "Y Intercept",
                    min = -5,
                    max = 5,
                    value = 2),
        sliderInput("slope",
                    "Slope (ratio de cambio)",
                    min = -2,
                    max = 2,
                    value = 1,
                    step = 0.1),
        sliderInput("grado",
                    "grado del polinomial:",
                    min = 1,
                    max = 7,
                    value = 3)
      ),
      mainPanel(
        plotlyOutput('grafica')
        
      )
    )
  )
) 
