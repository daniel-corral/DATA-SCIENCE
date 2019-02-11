##cuando ya tenemos los dos ficheros hay que cargar shiny en los dos
library(shiny)

shinyUI(
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        radioButtons("distribucion",label = h3("Radio buttons"),
                   choices = list(Uniforme="unif",
                                  Normal="norm"
                                  )), ##o una lista o un vector
                
        textInput("tamanio",label = ("Tamaño de la muestra"),value = "10"),
        actionButton("generar",label = "Generar")
      ),
      mainPanel( ##como funciona por dentro cada una de las cosas
        verbatimTextOutput("salida"), ##esto se utiliza para enseñar texto
        plotOutput("grafica")
        
      )
    )
  )
)

    

##una de las cosas que hace: relacion entre input y grafica y se va actualizando- se puede manipular
  #este comportamiento 

##histograma: con funcion hist no ggplot
    ##quiero una uniforme o una normal

##shiny quiere crear relaciones entre todos los elementos