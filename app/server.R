library(shiny)

source("helpers/raw_data.R")
source("helpers/game_analysis.R")


shinyServer(function(input, output) {
    tab_games_rendering(input, output)
    tab_rawdata_rendering(input, output)

})




