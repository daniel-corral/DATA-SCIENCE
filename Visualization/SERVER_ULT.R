library(shiny)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(magrittr)


##MODELO

shinyServer(function(input,output) {
  # Creo un reactiveValues para guardar la gráfica (también se podría hacer con
  # reactive normal)
  resultado <- reactiveValues()
    output$gifmar <- renderUI({
      # de manera que cada vez que el usuario lo pulse, se actualice el video
      tags$img(src = "https://giphy.com/embed/bbshzgyFQDqPHXBo4c", width = "272", height = "480")
    })
    
  ouput$modelos <- renderUI({
    if (input$seleccion == "poli") {
      box("input poli", solidHeader = TRUE, status = "primary",
          numericInput("size", "Tamaño muestral", min = 1, value = 30),
          numericInput("r", "Correlación", min = -1, max = +1, value = 0, step = 0.05),
          actionButton("createSample", "Nueva muestra"),
          hr(),
          numericInput("order", "Orden polinomial", min = 1, value = 1, step = 1),
          actionButton("addModel", "Añadir modelo"))
    }
    else if (input$seleccion == "kmns") {
      box("Inputs Kms", solidHeader = TRUE, status = "primary",
          selectInput("varx", "Variable X", choices = c("Sepal.Length",
                                                        "Sepal.Width",
                                                        "Petal.Length",
                                                        "Petal.Width")),
          selectInput("vary", "Variable Y", choices = c("Sepal.Length",
                                                        "Sepal.Width",
                                                        "Petal.Length",
                                                        "Petal.Width")),
          numericInput("nclusters", label = "# Clusters", min = 1, step = 1, value = 2),
          actionButton("run", "Run"))
    }
  })
  observeEvent(input$createSample, {
    # Si se pulsa createSample añado la gráfica que corresponda
    datos <- as.data.frame(mvrnorm(input$size, c(0,0),
                                   Sigma = matrix(c(1, input$r, input$r, 1), nrow = 2)))
    colnames(datos) <- c("x", "y")
    resultado$grafico <- ggplot(datos, aes(x = x, y = y)) + geom_point()
  })
  
  observeEvent(input$addModel, {# Si se pulsa addModel, añado el modelo que corresponda
    
    # req hace que si no existe la gráfica porque el usuario no ha pulsado createSample
    # antes, no se ejecuta el resto del observador
    req(resultado$grafico)
    
    orden <- input$order
    # Añado a la gráfica el geom_smooth
    resultado$grafico <- resultado$grafico + geom_smooth(method = "lm",
                                                     formula = y ~ poly(x, orden))
  })
  
  miModelo <- reactive({
    input$run
    iris %>%
      select_(isolate(input$varx), isolate(input$vary)) %>%
      kmeans(isolate(input$nclusters))
  })
  
  observeEvent(input$run,{
    resultado$grafico <- ggplot(iris, aes_string(x = isolate(input$varx),
                            y = isolate(input$vary),
                            color = as.factor(miModelo()$cluster))) + geom_point()
  })
  
output$graph <- renderPlot({
  resultadp$graph
  
  })
})
  
  
 
  

  
  
