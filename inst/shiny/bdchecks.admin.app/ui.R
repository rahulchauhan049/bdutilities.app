library(shiny)
library(shinydashboard)
library(yaml)

shinyUI(fluidPage(

    dashboardPage(
        dashboardHeader(title = "bdchecks Admin"),
        uiOutput("menu"),
        uiOutput("tab")
    )
))
