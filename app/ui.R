library(shiny)

shinyUI(fluidPage(

    # Application title
    titlePanel("Top 5 games of all times"),

    mainPanel(
        plotOutput("salesPlot")
    )
))
