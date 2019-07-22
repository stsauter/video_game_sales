library(shiny)

shinyServer(function(input, output) {
    output$salesPlot <- renderPlot({
        
        vgsales <- read_game_sales_csv()
        
        top5_sales <- vgsales[1:5, 10]
        top5_names <- vgsales[1:5, 1]
        barplot(top5_sales, names.arg=top5_names, col = 'darkgray', border = 'white')

    })

})


read_game_sales_csv <- function(){
    vgsales <- read.csv2("./data/vgsales.csv", header = TRUE, sep = ",", dec=".")
    
}