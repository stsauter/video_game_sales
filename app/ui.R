library(shiny)
library(ggplot2)
library(randomcoloR)
library(tidyverse)

source("helpers/raw_data.R")
source("helpers/game_analysis.R")
source("helpers/vgsales.R")

shinyUI(function () {
    navbarPage("Videospiel-Marktanalyse",
               tabPanel("Spiele", tab_games_layout()),
               tabPanel("Plattformen", render_platforms_tab()),
               tabPanel("Genres", render_genres_tab()),
               tabPanel("Publisher", render_publisher_tab()),
               tabPanel("Rohdaten", tab_rawdata_layout())
    )

})



render_platforms_tab <- function(){
    fluidPage(
      
        titlePanel("Hier werden die Plattformen analysiert")
    )
}

render_genres_tab <- function(){
    fluidPage(
      
        titlePanel("Hier werden die Genres analysiert")
    )
}

render_publisher_tab <- function(){
    fluidPage(
       
        titlePanel("Hier werden die Publisher analysiert")
    )
}

