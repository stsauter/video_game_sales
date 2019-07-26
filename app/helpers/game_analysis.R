
game_ins <- c("game_num_bars", "game_timespan", "game_salesplot_type")
game_outs <- c("games_plot", "sales_plot")

tab_games_layout <- function(){
  tabsetPanel(type = "tabs",
              tabPanel("Bestseller", subtab_game_bestsellers()),
              tabPanel("Verkaufszahlen", subtab_game_sales())
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

subtab_game_sales <- function(){
  fluidPage(
    titlePanel("Zeitlicher Verlauf der Verkaufszahlen aller Spiele"),
    br(),
    br(),
    fluidRow(
      column(3,
             wellPanel(
               selectizeInput(
                 game_ins[3], 'Darstellung:', choices = c("Liniendiagramm", "Balkendiagramm")
                 
               )
             )       
      ),
      column(9,
             plotOutput(game_outs[2])
      )
    )
   
  )
}


tab_games_rendering <- function(input, output){
    
  render_top_games(input, output)
  render_game_sales(input, output)
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
      scale_x_discrete(name ="Spiel") + scale_y_continuous(name ="Weltweit verkaufte Exemplare (Angabe in Mio.)") + 
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
  set.seed(2) 
  colors <-distinctColorPalette(length(platforms))
  names(colors) <- platforms
  return(colors)
}


render_game_sales <- function(input, output){
  
  data_input <- reactive({
    vgsales <- read_game_sales_csv()
    sales_by_year <- aggregate(list(vgsales$Global_Sales, vgsales$NA_Sales, vgsales$EU_Sales, vgsales$JP_Sales, vgsales$Other_Sales), 
                                    by=list(vgsales$Year_of_Release), FUN=sum)
    sales_by_year <- setNames(sales_by_year, c("Year_of_Release", "Global_Sales", "NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales"))
    sales_by_year <- subset(sales_by_year, as.numeric(Year_of_Release) <= 2016)
   
  })
  
  output[[game_outs[2]]] <- renderPlot({
    
    vgsales <- data_input()
    colors <- rainbow(4)
    line_width <- 1.0
    ggplot(vgsales) + geom_line(aes(Year_of_Release, NA_Sales, group = 1, color = "USA"), size = line_width) +
      geom_line(aes(Year_of_Release, EU_Sales, group = 1, color = "Europa"), size = line_width) +
      geom_line(aes(Year_of_Release, JP_Sales, group = 1, color = "Japan"), size = line_width) +
      geom_line(aes(Year_of_Release, Other_Sales, group = 1, color = "Rest der Welt"), size = line_width) +
      scale_x_discrete(name ="Jahr") + scale_y_continuous(name ="Verkaufte Spiele (Angabe in Mio.)") +
      scale_colour_manual("Region", values = colors) +
      theme(text = element_text(size=20)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
   
  })
}
