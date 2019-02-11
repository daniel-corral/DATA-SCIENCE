#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

install.packages("shiny")
library(shiny)
library(ggplot2)



# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Old Faithful Geyser Data"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      checkboxInput("dataset", 
                    label = "Ver outliers", TRUE )
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot")
    )
  )
)
#anadir un input que haga un checkbox
#un checkbox que ponga lo que sea y con cualquier nombre mientras funcione
#nada de tildes ni enyes en esta parte del codigo:

#bins <- seq(min(x), max(x), length.out = input$bins + 1)
#con el data set mpg y columna cylindros cyl 
#no hay que cambiar el codigo si no cambiar las x 
#5 o cuatro lineas de codigo se cambian y se resuelve


#como hacer un checkbox (dentro de la ui)
#shiny widget gallery
#single checkbox
#el valor del input sera true o false
#checkboxinput se llamara, donde pone sliderinput


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- faithful[, 2] 
    bins <- seq(min(x), max(x), length.out = input$bins + 1)
    z <- mpg$displ
    checkboxgeiser <- seq(min(x), max(x), length.out = input$dataset + 1)
    # draw the histogram with the specified number of bins
    hist(x, breaks = bins, col = 'darkgray', border = 'white')
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

