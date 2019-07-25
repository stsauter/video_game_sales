

read_game_sales_csv <- function(){
  vgsales <- read.csv2("./data/vgsales.csv", header = TRUE, sep = ",", dec=".", stringsAsFactors = FALSE)
  vgsales <- subset(vgsales, Year_of_Release != "N/A")
  
}

