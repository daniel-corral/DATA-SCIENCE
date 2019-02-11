####MODELOS POLINOMIALES SERVER####
library(shiny)
library(ggplot2)
library(plotly)
library(mvtnorm)


shinyServer(function(input, output){
  
  #definimos variable para botones
  grafica <- 
    reactiveValues(plotear = ggplotly(), mostrar = ggplotly())
  
  #observeEvent1:En la grafica queremos responder de esta manera
  observeEvent(input$genmuestra, {
    nbiv <- as.data.frame(rmvnorm(input$muestra, mean = c(0,0),
                                  sigma = matrix(c(1,input$corr, input$corr, 1), ncol = 2)))
    grafica$plotear <- (ggplot(data = nbiv, aes(nbiv[,1], nbiv[,2])) + geom_point())
    grafica$mostrar <- (ggplot(data = nbiv, aes(nbiv[,1], nbiv[,2])) + geom_point())
  })
  #observeEvent2: Respond to "event-like" reactive inputs, values, and expressions
  observeEvent(input$anmodelo, {
    sts <- isolate(input$ordenpol) #grado
    grafica$plotear <- grafica$mostrar + stat_smooth(method = "lm", 
                                                     formula = y~poly(x,sts))
  })
  output$grafica <- renderPlotly({
    ggplotly(grafica$plotear)
    
  })
})





