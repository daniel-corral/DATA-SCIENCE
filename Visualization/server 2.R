#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  miModelo <- reactive({
    input$run
    iris %>% 
      select_(isolate(input$varx), isolate(input$vary)) %>%
      kmeans(isolate(input$nclusters))
    ###aqui haré el modelo
    
  })
  
  output$scatterplot <- renderPlot({ ##primero hacer un kmeans
    #modelo <- quitar esta asignacion pq asi no se queda solo con el ultimo valor
    #miModelo() ##AQUI SE PONE PARENTESIS PQ ES UN REACTIVE AUNQ NO SEA UNA FUNCION
    ggplot(iris, aes_string(x=isolate(input$varx), y=isolate(input$vary), color=as.factor(miModelo()$cluster)))+
      geom_point() ##aes_string pq no son cadenas de texto
  })
  output$centro <- renderText({ ##en centro solo hay un centroide ahora
    #miModelo()
    miModelo()$centers ##el problema esq el modelo esta arriba creado no dentro entonces volver a creear
    })

  #isolate(rnorm(input$varx))
})


##input$numero de clusters 
  ##kmeans: hay que entrenarlo mas veces para emitir resultados distintos del kmeans --> se hacecon REACTIVE
##crear algo intermedio para no tener que calcular el modelo 3 veces, no hare 3 modelos


##kmeans: tecnica no supervisada(en agrupacion) porque no tiene un output: no le dices la CATEGORIA a la que pertenecen: no le das el resultado que quieres obtener
##prediccion es supervisada: pq al modelo le dices lo que queires obtener: si quiers un clasificador para iris

##voy a buscar modelo que tenga un centroide: centros de gravedad
  ##las distancias eucledianas las hace el modelo solo 

##puedes hacer la distancia entre palabras



###library(dplyr)
  ###%<% sirve para hacer operaciones

#select(iris, Sepal.Lenght)

#iris %>% kmeans(3) ##aqui pongo el numero de clusters hay que sekeccinar col
#iris %>% select(Sepal.Width, Sepal.Length) %>% kmeans(3) ###esto saca el modelo: ejecutamos el kmeans

#kmeans es input$nclusters

#el _ lee el contenido de la columna y te coje lo que le pidas y no lo que has renombrado
  ##select normal: el texto que le pones
  ##select_: lee el nombre que busca dentro del dataframe

#modelo <- iris %>% select(Sepal.Width, Sepal.Length) %>% kmeans(3)
  ##la media del cluster es el centro de gravedad de cada cluster
  #un vector que me da las categorias a las que pertenece cada flor

#library(ggplot2)
#modelo$cluster
#ggplot(iris, aes(x=Sepal.Width, y=Sepal.Length, color=as.factor(modelo$cluster)))+
  #geom_point() ##esta mal pq tiene distintos tonos y no hay continuidad- ahora no es un factor y puede creer que es un numero y son varibles cualitativas
##por eso le pongo el as factor

##esta lodigica se mete en render plot y se cambian los inputs y a tomar por culo


####ahora los centroides: means con un verbaintinTextOutput

#modelo <- iris %>% 
#select_(input$varx, input$vary) %>% ##el _ pq no son cadenas de texto
  #kmeans(input$nclusters)


###en el grafico con miModelo() salen para 2 clusters cuatro centros: pq los centroides tienen 2, variable x y la variable y


