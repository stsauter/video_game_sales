
rawdata_outs <- c("rawdata_table")

tab_rawdata_layout <- function(){
  fluidPage(
    
    titlePanel("Rohdatenansicht"),
    br(),
    br(),
    mainPanel(
      dataTableOutput(rawdata_outs[1])
    )
  )
}

tab_rawdata_rendering <- function(input, output){
  output[[rawdata_outs[1]]] <- renderDataTable(read_game_sales_csv()[1:1000, ])
  
}

