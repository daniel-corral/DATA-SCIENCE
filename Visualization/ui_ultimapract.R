library(shiny)
library(ggplot2)
library(dplyr)
library(shinydashboard)
data("iris")



#DASHBOARD
shinyUI(
  dashboardPage(
    dashboardHeader(title = "Práctica Molona de Marta Divassón"),
    #sidebar
    dashboardSidebar(
      sidebarMenu(
        menuItem("GIF", tabName = "gif", icon = icon("file-code-o")),
        menuItem("Modelos", tabName = "modelos", icon = icon("dashboard"), 
                 badgeColor = "fuchsia")
      )
    ),
    #body
    dashboardBody(
      tabItems(
        tabItem(tabName = "gif", uiOutput('htmlgif')),
        
        tabItem(tabName = "modelos",
                fluidRow(
                  box(width = NULL, plotOutput("scatterplot"),
                      box(solidHeader = TRUE, status = "warning", 
                          selectInput("varx", "Variable X", choices = c("Sepal.Length",
                                                                        "Sepal.Width",
                                                                        "Petal.Length",
                                                                        "Petal.Width")),
                          selectInput("vary", "Variable Y", choices = c("Sepal.Length",
                                                                        "Sepal.Width",
                                                                        "Petal.Length",
                                                                        "Petal.Width")),
                          numericInput("nclusters", label = "# Clusters", min = 1, step = 1, value = 2),
                          actionButton("run", "Run"))),
                  
                  box(width = NULL, plotOutput("graph"),
                      box(width = NULL , solidHeader = TRUE, status = "warning",
                          numericInput("size", "Tamaño muestral", min = 1, value = 30),
                          numericInput("r", "Correlación", min = -1, max = +1, value = 0, step = 0.05),
                          # Botón que genera la muestra
                          actionButton("createSample", "Nueva muestra"),
                          hr(),
                          numericInput("order", "Orden polinomial", min = 1, value = 1, step = 1),
                          actionButton("addModel", "Añadir modelo"),
                          selectInput("seleccion", "Seleccionar", choices = c("Polinomial", "KMeans"),
                                      selected = "Polinomial")))
                )
        )
      )
    )
  )
)


