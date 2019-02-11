

library(shiny)

#Defines ui
shinyUI(fluidPage(
  # Application title
  titlePanel("Kmeans"),
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput("varx", "Variable X", choices = c("Sepal.Length",
                                                  "Sepal.Width",
                                                  "Petal.Length",
                                                  "Petal.Width")),
      
      selectInput("vary", "Variable Y", choices = c("Sepal.Length",
                                                  "Sepal.Width",
                                                  "Petal.Length",
                                                  "Petal.Width")),
      numericInput("nclusters", label = "# Clusters", min = 1, step = 1, value = 1),  ##darle un valor a value
      ##tiene que haber al menos un grupo para calc distancias
      actionButton("run", "Run")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
       plotOutput("scatterplot"), #dispersion
       verbatimTextOutput("centro")
    )
  )
))
