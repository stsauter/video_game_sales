library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Top 5 games of all times"),

    mainPanel(
        plotOutput("salesPlot")
    )
))
