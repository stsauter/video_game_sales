library(shiny)
library(ggplot2)
library(randomcoloR)
library(tidyverse)

source("helpers/raw_data.R")
source("helpers/game_analysis.R")
source("helpers/game_publisher.R")
source("helpers/genre_analysis.R")
source("helpers/vgsales.R")

shinyUI(function () {
    navbarPage("Videospiel-Marktanalyse",
               tabPanel("Spiele", tab_games_layout()),
               tabPanel("Plattformen", render_platforms_tab()),
               tabPanel("Genres", tab_genre_layout()),
               tabPanel("Herausgeber", tab_publisher_layout()),
               tabPanel("Rohdaten", tab_rawdata_layout())
    )

})



render_platforms_tab <- function(){
    fluidPage(
      
        titlePanel("Hier werden die Plattformen analysiert")
    )
}


