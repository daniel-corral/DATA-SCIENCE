library(shiny)
library(dplyr)
library(ggplot2)
library(shinydashboard)


##MODELO

shinyServer(function(input,output) {
  # Creo un reactiveValues para guardar la gráfica (también se podría hacer con
  # reactive normal)
  output$graph <- renderPlot({
    resultado$graph
  })
  miModelo <- reactive({
    input$run
    iris %>%
      select_(isolate(input$varx), isolate(input$vary)) %>%
      kmeans(isolate(input$nclusters))
  })
  
  output$scatterplot <- renderPlot({
    ggplot(iris, aes_string(x = isolate(input$varx),
                            y = isolate(input$vary),
                            color = as.factor(miModelo()$cluster))) +
      geom_point()
  })
  
  resultado <- reactiveValues()
  
  observeEvent(input$createSample, {
    # Si se pulsa createSample añado la gráfica que corresponda
    datos <- as.data.frame(mvrnorm(input$size, c(0,0),
                                   Sigma = matrix(c(1, input$r, input$r, 1), nrow = 2)))
    colnames(datos) <- c("x", "y")
    resultado$graph <- ggplot(datos, aes(x = x, y = y)) + geom_point()
  })
  
  observeEvent(input$addModel, {# Si se pulsa addModel, añado el modelo que corresponda
    
    # req hace que si no existe la gráfica porque el usuario no ha pulsado createSample
    # antes, no se ejecuta el resto del observador
    req(resultado$graph)
    
    orden <- input$order
    # Añado a la gráfica el geom_smooth
    resultado$graph <- resultado$graph + geom_smooth(method = "lm",
                                                         formula = y ~ poly(x, orden))
  })

##GIF

output$htmlgif <- renderUI({
  # de manera que cada vez que el usuario lo pulse, se actualice el video
    tags$iframe(src = "https://giphy.com/embed/bbshzgyFQDqPHXBo4c", width = "272", height = "480")
  })

})



