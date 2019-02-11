###################        UI          ######################

library(shiny)
library(plotly)


shinyUI(
  fluidPage(
    
    titlePanel("Práctica 3."),
    sidebarLayout(
      sidebarPanel(
        
        numericInput("tamano", label = "Introduzca el tamaño de la muestra:",
                     value = 1, min = 1),
        numericInput("correl", label = "Introduzca el valor de la correlación (entre -1 y 1):",
                     value = 0, min = -1, max = 1, step = 0.1),
        actionButton("generar", label = "Generar muestra"),
        numericInput("orden", label = "Introduza el orden del polinomio del modelo:",
                     value = 1, min = 1, step = 1),
        actionButton("construir", label = "Añadir modelo")
      ),
      mainPanel(
        plotlyOutput('grafica')
      )
    )
  )
)


###################        SERVER          ######################
library(shiny)
library(mvtnorm)
library(plotly)
library(ggplot2)

shinyServer(function(input,output){
  
  grafica <- reactiveValues(muestra = ggplot(), salida = ggplot()) 
  
  observeEvent(input$generar, {
    nbiv <- as.data.frame(rmvnorm(input$tamano, mean = c(0 ,0), 
                                  sigma = matrix(c(1,input$correl,input$correl,1), ncol = 2)))
    grafica$muestra <- (ggplot(data = nbiv, aes(nbiv[,1], nbiv[,2])) + geom_point())
    grafica$salida <- (ggplot(data = nbiv, aes(nbiv[,1], nbiv[,2])) + geom_point()) #Duplico esto porque así cuando añado modelos no se van solapando y el usuario puede probar distintos modelos sobre una misma muestra sin solapamiento.
  })
  
  observeEvent(input$construir, {
    grado <- isolate(input$orden)
    grafica$salida <- grafica$muestra + stat_smooth(method = "lm", formula = y ~ poly(x,grado))
  })
  
  output$grafica <- renderPlotly({
    
    ggplotly(grafica$salida)
    
  })
  
})

