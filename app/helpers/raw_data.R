
rawdata_outs <- c("rawdata_table", "download_data")

tab_rawdata_layout <- function(){
  fluidPage(
    
    titlePanel("Rohdatenansicht"),
    br(),
    downloadButton(rawdata_outs[1], "Download"),
    br(),
    br(),
    br(),
    mainPanel(
      dataTableOutput(rawdata_outs[2])
    )
  )
}

tab_rawdata_rendering <- function(input, output){
  
  output[[rawdata_outs[1]]] <- downloadHandler(
    filename = "vgsales.csv",
    content = function(file) {
      data <- read_game_sales_csv()
      write.csv(data, file)
    }
  )
  
  output[[rawdata_outs[2]]] <- renderDataTable(read_game_sales_csv()[1:1000, ])
  
}

