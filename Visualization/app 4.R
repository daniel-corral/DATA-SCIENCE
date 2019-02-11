##CLASE DOS 

library(shiny)
library(plotly)
library(ggplot2)
#ggplot(mtcars) ##datset que vamos a leer 
#hago diagrama de puntos
#ggplot(mtcars, aes(x=mpg, y=hp)+geom_point()
##install.packages("plotly")
#ggplotly(ggplot(mtcars, aes(x=mpg, y=hp))+ geom_point()) <- conviertes

##si alguna vez las tildes y las ñ empiezan a fallar: google UTF8 R option <- options(encoding, utf8) o algo asi

##crear 2 objetos:ui y server
##para que funciona el autocompletado con shiny hay que cargar librerias sino no se autocompleta

ui <- shinyUI( ##el primer padre de cualquier interfaz
  ##son parametros de funciones en shiny; importante el estilo- voy a hacer el hijo de cualquier elemento
  fluidPage( ##que tipo de layout en la pag quieres- se encancha tanto como el tamaño del navegador
    titlePanel("Primer Shiny"), ##si tiene un igual significa que no es obligatorio; entre cadenas de texto ##no tiene hijos pero tiene hermanos or tanto se pone la coma y en el mimso nivel de sangria
    sidebarLayout(
      sidebarPanel(
        ##podemos utilizar cualquier input; width: puedes cambiar los tamaños, se puede abreviar a una T pero si hago esto
        checkboxInput(inputId = "hola", label="Hola",value= TRUE),
        selectInput(inputId = "seleccion",
                    label = "Selección",
                    choices = c("mpg","disp")),
        ##hacer una lista o un vector- en todo caso existiria mtcars$.. 
        ##choices=list("Miles per Gallon"="mpg", "Otra cosa"="displ")) o choices = c("mpg","displ"))
        ##vamos a dejar que puedan elegir de una mpg y disp y selected para ver por donde empieza y por defecto es null
        sliderInput("minimo",
                    label="Minimo de la X",
                    min=0,
                    max=50,
                    value=25,
                    animate=T) ##slider hace saltos y step pones eso; locale=idioma; animate= se va moviendo el slider
      ),##no hace falta poner 2 veces el mismo nombre 
      mainPanel(
        verbatimTextOutput(outputId = "salida"),
        plotlyOutput("SegundaGrafica"), ##metemos aqui
        ##hueco dinde el servidor va a introductir cosas y con un identificador q llamamos salida
        plotlyOutput("grafica")##puedo guardar esto que será visualizada y ahora en el server poner otro
        
      )
    )
  )
)

##source on save: hacerlo cuando hay funciones

##puedes llamar a esto como quieras
server <- function(input, output){ ##recibe un input y un output; lo dejamos en blanco pq por ahora no va a hacer nada
  
  output$salida <- renderText({ ##aqui metemos lo que queramos
    input$minimo ##miramos las inputs que hemos definido mirar inputID
  })  ##vamos poniendo lo que queramos generar para sacar a la interfaz(es parte del servidor que tiene que tener un hueco que hacemos en main panel)
  ##aqui se llama renderText pq es un texto; render en el lado del servidor siempre es en panel ..output y en este render..
  output$grafica <- renderPlotly({
    ggplotly(ggplot(mtcars, aes_string(x=input$seleccion, y="hp"))+ geom_point())
    #ggplot(mtcars, aes(x=disp, y=input$variable))+geom_point() ##cambiamos la variable x por input$variable se supne que va bien pero FALLA
    #hist(faithful[,2]) ##este bloque de codigo es independiente de lo de arriba y accedes a la segunda col
    ##input$variable es una cadena de texto es c("mpg") que tiene el nombre de la variable a la que quiero acceder- gray no es ponme el color gris y aqui estás poniendo un texto que se llama mpg
  })
  output$SegundaGrafica <- renderPlotly({
    plot_ly(z=~volcano) %>% add_surface()
  })
}

#plotly(dataset(), pch = 20, cex = 3, col = "pink",
      # main = "CARACTERISTICAS DE VEHÍCULOS")})

shinyApp(ui, server)
##layout: forma de espaciar los elementos