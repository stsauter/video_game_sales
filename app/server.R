library(shiny)
library(ggplot2)

shinyServer(function(input, output) {
    output$salesPlot <- renderPlot({
        
        vgsales <- read_game_sales_csv()
        
        top5_sales <- vgsales[1:5, ]
        df <- data.frame(Name = top5_sales$Name, Global_Sales = top5_sales$Global_Sales)
        df$Name <- factor(df$Name, levels = df$Name[order(df$Global_Sales)])
        ggplot(df) + geom_bar(stat = "identity", aes(Name, Global_Sales)) + coord_flip()
    })

})


read_game_sales_csv <- function(){
    vgsales <- read.csv2("./data/vgsales.csv", header = TRUE, sep = ",", dec=".")
    
}
