
game_ins <- c("game_num_bars", "game_timespan", "game_salesplot_type")
game_outs <- c("games_plot", "game_sales_plot", "game_sales_rel_plot")

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
    fluidRow(
      column(3,
             selectizeInput(
               game_ins[3], 'Darstellung als', choices = c("Liniendiagramm", "Balkendiagramm")
               
             )  
      )
    ),
    fluidRow(
      column(12,
             plotOutput(game_outs[2])
      )
    ),
    br(),
    titlePanel("Marktanteile nach Region"),
    br(),
    fluidRow(
      column(12,
             plotOutput(game_outs[3])
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

  # colors <-distinctColorPalette(length(platforms))
  # names(colors) <- platforms
  # color_file <- tempfile("color_palette", fileext = ".rds")
  # saveRDS(colors, color_file)
  colors <- readRDS("persistence/random_colors.rds")
  return(colors)
}


render_game_sales <- function(input, output){
  
  data_input <- reactive({
    vgsales <- read_game_sales_csv()
    sales_by_year <- aggregate(list(vgsales$NA_Sales, vgsales$EU_Sales, vgsales$JP_Sales, vgsales$Other_Sales), 
                                    by=list(vgsales$Year_of_Release), FUN=sum)
    sales_by_year <- setNames(sales_by_year, c("Year_of_Release", "NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales"))
    sales_by_year <- subset(sales_by_year, as.numeric(Year_of_Release) >= 1980 & as.numeric(Year_of_Release) <= 2016)
    vgsales_pivot <- gather(sales_by_year, "NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", key = "Region", value= "Sales")
   
  })
  
  output[[game_outs[2]]] <- renderPlot({

    vgsales <- data_input()
    chart_type <- input[[game_ins[3]]]
    
    if (chart_type == "Liniendiagramm"){
      line_width <- 1.0
      ggplot(vgsales) + geom_line(aes(Year_of_Release, Sales, group = Region, color = Region), size = line_width) +
        scale_x_discrete(name ="Jahr") + scale_y_continuous(name ="Verkaufte Spiele (Angabe in Mio.)") +
        scale_color_manual(name = "Region",
                           labels = c("Europa", "Japan", "USA", "Rest der Welt"), 
                           values = c("EU_Sales"="#F8766D","JP_Sales"="#7CAE00","NA_Sales"="#00BFC4","Other_Sales"="#C77CFF")) +
        theme(text = element_text(size=20)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    }
    else{
      ggplot(vgsales) + geom_bar(stat = "identity", aes(Year_of_Release, Sales, fill = Region)) +
        scale_x_discrete(name ="Jahr") + scale_y_continuous(name ="Verkaufte Spiele (Angabe in Mio.)") +
        scale_fill_discrete(name = "Region", labels = c("Europa", "Japan", "USA", "Rest der Welt")) +
        theme(text = element_text(size=20)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
      
    }
   
  })
  
  output[[game_outs[3]]] <- renderPlot({
    
    vgsales <- data_input()
    ggplot(vgsales) + geom_bar(stat = "identity", position = "fill", aes(Year_of_Release, Sales, fill = Region)) + 
      scale_x_discrete(name ="Jahr") + scale_y_continuous(name ="Relativer Anteil in %", labels = scales::percent) +
      scale_fill_discrete(name = "Region", labels = c("Europa", "Japan", "USA", "Rest der Welt")) +
      theme(text = element_text(size=20)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
  })
}

