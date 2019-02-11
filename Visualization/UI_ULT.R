# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(shinydashboard)
library(MASS)
library(magrittr) ##operadores en pipe

#DASHBOARD
shinyUI(
  dashboardPage(
    dashboardHeader(title = "Práctica Molona de Marta Divassón"),
    #sidebar
    dashboardSidebar(
      sidebarMenu(
        menuItem("GIF", tabName = "gif", icon = icon("image")),
        menuItem("Modelos", tabName = "modelos", icon = icon("dashboard"), badgeColor = "fuchsia"))
      ),
  
    #body
    dashboardBody(
      tabItems(
        tabItem(tabName = "gif",
                fluidRow(
                  box(title = "Lunes", uiOutput("gifmar")))),
        
        tabItem(tabName = "modelos",
                fluidRow(
                  box(status = "primary", width = NULL,solidHeader = TRUE,plotOutput("graph")),
                  box(status = "primary", width = NULL, solidHeader = TRUE, 
                          selectInput("seleccion", "Modelo 1 o 2", choices = c("Polinomial" = "poli", "KMeans" = "kmns"),
                                      selected = "Polinomial"))),
                uiOutput("modelos")
                
        )
      )
    )
  )
)

