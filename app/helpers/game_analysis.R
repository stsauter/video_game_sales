library(shiny)
library(ggplot2)
source("helpers/vgsales.R")


game_plot_names <- c("top_games_plot")

tab_games_layout <- function(){
  fluidPage(
    titlePanel("Meistverkaufte Spiele"),
    
    fluidRow(
      column(3,
             wellPanel(
               sliderInput("obs", "Zeitraum:",  
                           min = 1980, max = 2016, sep ="", value = c(1980, 2016))
             )       
      ),
      column(9,
             plotOutput(game_plot_names[1])
      )
    )
  
  )
}


tab_games_rendering <- function(input, output){
    
  render_top_games(input, output)
  
}


render_top_games <- function(input, output){
  
  output[[game_plot_names[1]]] <- renderPlot({
    
    vgsales <- read_game_sales_csv()
    
    top5_sales <- vgsales[1:5, ]
    df <- data.frame(Name = top5_sales$Name, Global_Sales = top5_sales$Global_Sales)
    df$Name <- factor(df$Name, levels = df$Name[order(df$Global_Sales)])
    ggplot(df) + geom_bar(stat = "identity", aes(Name, Global_Sales), fill="steelblue") + coord_flip()
  })
  
}