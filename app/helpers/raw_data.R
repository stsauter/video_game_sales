library(DT)
source("helpers/vgsales.R")

rawdata_plot_names <- c("rawdata_table")

tab_rawdata_layout <- function(){
  fluidPage(
    
    titlePanel("Rohdatenansicht"),
    br(),
    br(),
    mainPanel(
      DT::dataTableOutput(rawdata_plot_names[1])
    )
  )
}

tab_rawdata_rendering <- function(input, output){
  output[[rawdata_plot_names[1]]] <- DT::renderDataTable(DT::datatable({
    data <- read_game_sales_csv()[1:1000, ]
   
  }))

}