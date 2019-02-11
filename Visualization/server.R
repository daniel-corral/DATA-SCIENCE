###funcion que recibe un input y un output -- al guardar ya detecta que es un shiny
library(shiny)

shinyServer(function(input,output){
  output$salida <- renderText({##hay que poner lo que pongas en verbaint... en main panel
    input$generar
   #input$tamanio para ver lo que hay dentro
    #vamos a ir viendo lo que hay dentro de cada input
  })
  
  output$grafica <- renderPlot({ 
    ##es equivalente a escribir lo que sale en la ult linea pero con todo lo de arriba como condicion
    #tamanio <- isolate(input$tamanio) ##esto lo haria para hacerlo mas visual y sencillo
    #distribucion <- isolate(input$distribucion)
    ##isolate: se rompan las conexiones
    input$generar ##al dejarlo suelto por ahi shiny lo utiliza o if(input$generar != 0) o mayor que -1
    vector <- if(input$distribucion == "unif")
      runif(isolate(input$tamanio))
    else
      rnorm(isolate(input$tamanio))
    hist(vector)
  })
})

#shinyServer(server) ##para que esto funcione hay que definir ui 

##tambien se haria asi 
#vector <- if(input$distribucion == "Uniforme)
  #runif(100) o input$tamanio
#else
#rnorm(100) o input$tamanio
#hist(vector)
##meter isolate en runif(isolate(input$tamanio))


###mi forma de hacerlo
#output$grafica <- renderPlot({ ##es equivalente a escribir lo que sale en la ult linea pero con todo lo de arriba como condicion
  #uniforme<- runif(100,0,1)
  #normal <- rnorm(100,0,1)
 # if (input$distribucion == "Uniforme"){
   # hist(uniforme)
 # } else {
   # hist(normal)}
