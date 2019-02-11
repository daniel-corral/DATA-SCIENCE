# SHINY

library(shiny)
library(ggplot2)
library(tidyverse)
data(mpg)
                                  ########### UI ############# 
# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Práctica Molona Histograma March"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Número de barras:",
                  min = 1,
                  max = 50,
                  value = 30),
      checkboxInput("checkbox",
                    label="Ver faithfull/Ver cyl", value = TRUE)
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)


                          ########### SERVER ############# 
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    if (input$checkbox){
      x    <- faithful[, 2] # en la x el dataset faithful la segunda variable
    }else{
      x   <-mpg$cyl
    }
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white',main = "Histograma", ylab = "Frecuencia")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

