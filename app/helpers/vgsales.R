

read_game_sales_csv <- function(){
  vgsales <- read.csv2("./data/vgsales.csv", header = TRUE, sep = ",", dec=".", stringsAsFactors = FALSE)
  
}

