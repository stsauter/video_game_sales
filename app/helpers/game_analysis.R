
game_ins <- c("game_num_bars", "game_timespan")
game_outs <- c("games_plot")

tab_games_layout <- function(){
  tabsetPanel(type = "tabs",
              tabPanel("Bestseller", subtab_game_bestsellers()),
              tabPanel("Zeitverlauf", subtab_game_charts())
  )
}

subtab_game_bestsellers <- function(){
  fluidPage(
    titlePanel("Meistverkaufte Spiele"),
    br(),
    br(),
    fluidRow(
      column(3,
             wellPanel(
               selectizeInput(
                 game_ins[1], 'Anzahl Ergebnisse:', choices = c(5, 10, 15)
                 
               ),
               
               sliderInput(game_ins[2], "Release-Zeitraum:",  
                           min = 1980, max = 2016, sep = "", value = c(1980, 2016))
             )       
      ),
      column(9,
             plotOutput(game_outs[1])
      )
    )
  )
}

subtab_game_charts <- function(){
  fluidPage(
    titlePanel("Zeitlicher Verlauf der Verkaufszahlen aller Spiele"),
    br(),
    br()
   
  )
}


tab_games_rendering <- function(input, output){
    
  render_top_games(input, output)
  
}


render_top_games <- function(input, output){
  
  data_input <- reactive({
    vgsales <- read_game_sales_csv()
  })
  platform_colors <- reactive({
    colors <- create_platform_colors(unique(data_input()$Platform))
  })
  
  output[[game_outs[1]]] <- renderPlot({
    vgsales <- data_input()
    timespan <- input[[game_ins[2]]]
 
    filtered_df <- subset(vgsales, as.numeric(Year_of_Release) >= timespan[1] & as.numeric(Year_of_Release) <= timespan[2])
    top_sales <- filtered_df[1:input[[game_ins[1]]], ]
  
    df <- data.frame(Name = top_sales$Name, Global_Sales = top_sales$Global_Sales, Platform = top_sales$Platform)
    df <- df[order(df$Global_Sales),]
    df <- ensure_unique_game_name(df)
  
    #  df$Name <- factor(df$Name, levels = df$Name[order(df$Global_Sales)])
    ggplot(df) + geom_bar(stat = "identity", aes(x=reorder(Name, Global_Sales), Global_Sales, fill = Platform))  + coord_flip() +  
      scale_x_discrete(name ="Spiel") + scale_y_continuous(name ="Weltweit verkauft Exemplare (Angabe in Mio.)") + 
      labs(fill = "Plattform") + scale_fill_manual(values = platform_colors()) + theme(text = element_text(size=20))
  }, height = 700)
}

ensure_unique_game_name <- function(game_df){
  dups <- duplicated(game_df$Name)
  if (any(dups == TRUE)){
    game_df$Name <- as.character(game_df$Name)
    game_df[dups, "Name"] <-  paste(game_df[dups, "Name"] , "for", game_df[dups, "Platform"])
    game_df$Name <- as.factor(game_df$Name)
  }
  return(game_df)
}

create_platform_colors <- function(platforms){
  set.seed(1) 
  colors <-distinctColorPalette(length(platforms))
  names(colors) <- platforms
  return(colors)
}