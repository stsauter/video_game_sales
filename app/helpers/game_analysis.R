
game_ins <- c("game_num_bars", "game_region", "game_timespan", "game_differ_platforms", "game_salesplot_type")
game_outs <- c("games_plot", "games_top_annual_plot", "games_top_region_plot", "game_sales_plot", "game_sales_rel_plot")

tab_games_layout <- function(){
  tabsetPanel(type = "tabs",
              tabPanel("Bestseller", subtab_game_bestsellers()),
              tabPanel("Bestseller je Jahr", subtab_game_annual_bestsellers()),
              tabPanel("Bestseller je Region", subtab_game_region_bestsellers()),
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
               
               selectizeInput(
                 game_ins[2], 'Region:', choices = c("Global", "USA", "Europa", "Japan")
                 
               ),
               
               
               sliderInput(game_ins[3], "Release-Zeitraum:",  
                           min = 1980, max = 2016, sep = "", value = c(1980, 2016), animate = TRUE
               ),
               
               checkboxInput(game_ins[4], "Nach Plattform unterscheiden", value = TRUE)
               
             )       
      ),
      column(9,
             plotOutput(game_outs[1])
      )
    )
  )
}

subtab_game_annual_bestsellers <- function(){
  fluidPage(
    titlePanel("Bestseller der letzten 10 Jahre"),
    br(),
    br(),
    fluidRow(
      column(12,
             plotOutput(game_outs[2])
      )
    )
  )
}

subtab_game_region_bestsellers <- function(){
  fluidPage(
    titlePanel("Top 3 Spiele je Region"),
    br(),
    br(),
    fluidRow(
      column(12,
             plotOutput(game_outs[3])
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
               game_ins[5], 'Darstellung als', choices = c("Liniendiagramm", "Balkendiagramm")
               
             )  
      )
    ),
    fluidRow(
      column(12,
             plotOutput(game_outs[4])
      )
    ),
    br(),
    titlePanel("Marktanteile nach Region"),
    br(),
    fluidRow(
      column(12,
             plotOutput(game_outs[5])
      )
    )
  )
}


tab_games_rendering <- function(input, output){
    
  render_top_games(input, output)
  render_top_annual_games(input, output)
  render_top_region_games(input, output)
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
    timespan <- input[[game_ins[3]]]
    filtered_df <- subset(vgsales, as.numeric(Year_of_Release) >= timespan[1] & as.numeric(Year_of_Release) <= timespan[2])
    switch(input[[game_ins[2]]], "Global" = sales_data <- filtered_df$Global_Sales, 
           "USA" = sales_data <- filtered_df$NA_Sales, 
           "Europa" = sales_data <- filtered_df$EU_Sales,
           "Japan" = sales_data <- filtered_df$JP_Sales)
 
    switch(input[[game_ins[2]]], "Global" = x_label_prefix <- "Weltweit", 
           "USA" = x_label_prefix <- "In den USA", 
           "Europa" = x_label_prefix <- "In Europa", 
           "Japan" = x_label_prefix <- "In Japan")
    
    df <- data.frame(Name = filtered_df$Name, Sales = sales_data, Platform = filtered_df$Platform)
    
    if(isTRUE(input[[game_ins[4]]])){
      df <- df[order(df$Sales,decreasing = TRUE),]
      top_sales <- df[1:input[[game_ins[1]]], ]
      df <- data.frame(Name = top_sales$Name, Sales = top_sales$Sales, Platform = top_sales$Platform)
      df <- ensure_unique_game_name(df)
      #  df$Name <- factor(df$Name, levels = df$Name[order(df$Global_Sales)])
      ggplot(df) + geom_bar(stat = "identity", aes(x=reorder(Name, Sales), Sales, fill = Platform))  + coord_flip() +  
        scale_x_discrete(name ="Spiel") + scale_y_continuous(name = paste(x_label_prefix, "verkaufte Exemplare (Angabe in Mio.)")) + 
        labs(fill = "Plattform") + scale_fill_manual(values = platform_colors()) + theme(text = element_text(size=20))
    }
    else{
      df <- setNames(aggregate(list(df$Sales), by=list(df$Name), FUN=sum), c("Name", "Sales"))
      df <- df[order(df$Sales,decreasing = TRUE),]
      top_sales <- df[1:input[[game_ins[1]]], ]
      df <- data.frame(Name = top_sales$Name, Sales = top_sales$Sales)
      ggplot(df) + geom_bar(stat = "identity", aes(x=reorder(Name, Sales), Sales), fill = "steelblue")  + coord_flip() +  
        scale_x_discrete(name ="Spiel") + scale_y_continuous(name = paste(x_label_prefix, "verkaufte Exemplare (Angabe in Mio.)")) + 
        theme(text = element_text(size=20))
    }

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
  colors <- readRDS("persistence/random_colors.rds")
  return(colors)
}

render_top_annual_games <- function(input, output){
  
  data_input <- reactive({
    vgsales <- read_game_sales_csv()
  })
  
  output[[game_outs[2]]] <- renderPlot({
    vgsales <- data_input()
    
    df_games <- vgsales %>%
      group_by(Name, Year_of_Release) %>% 
      summarise(Global_Sales = sum(Global_Sales))
    
    df_top_games <- df_games %>% 
      group_by(Year_of_Release)  %>%  
      filter(Global_Sales== max(Global_Sales) & Year_of_Release >= 2006 & Year_of_Release <= 2016) %>% 
      arrange(Year_of_Release)

    ggplot(df_top_games) + geom_bar(stat = "identity", aes(Year_of_Release, Global_Sales, fill = Name))  + 
      scale_x_discrete(name ="Jahr") + scale_y_continuous(name = "Weltweit verkaufte Exemplare (Angabe in Mio.)") + 
      scale_fill_discrete(breaks = df_top_games$Name) + labs(fill = "Spiel") +
      theme(text = element_text(size=20)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
  }, height = 700)
}

render_top_region_games <- function(input, output){
  
  data_input <- reactive({
    vgsales <- read_game_sales_csv()
  })
  
  output[[game_outs[3]]] <- renderPlot({
    vgsales <- data_input()
    
    df_games <- vgsales %>%
      group_by(Name) %>% 
      summarise(NA_Sales = sum(NA_Sales), EU_Sales = sum(EU_Sales), JP_Sales = sum(JP_Sales), Other_Sales = sum(Other_Sales))
    
    df_games_pivot <- gather(df_games, "NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", key = "Region", value= "Sales")
    df_top_games <- df_games_pivot %>%
      group_by(Region) %>%
      top_n(n = 3, wt = Sales) %>%
      arrange(Region, desc(Sales))
    
    facet_labels <- c("Europa", "Japan", "USA", "Rest der Welt")
    names(facet_labels) <- c("EU_Sales", "JP_Sales",  "NA_Sales", "Other_Sales")
    ggplot(df_top_games) + geom_bar(stat = "identity", aes(Name, Sales, fill = Name))  + 
      scale_y_continuous(name = "Verkaufte Exemplare (Angabe in Mio.)") + 
      theme(text = element_text(size=20), axis.title.x=element_blank(), axis.text.x=element_blank()) +
      facet_grid(. ~ Region, scales = "free",  space = "free", labeller = labeller(Region = facet_labels)) + 
      labs(fill = "Spiel") 
   
    
  }, height = 700)
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
  
  output[[game_outs[4]]] <- renderPlot({

    vgsales <- data_input()
    chart_type <- input[[game_ins[5]]]
    
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
  
  output[[game_outs[5]]] <- renderPlot({
    
    vgsales <- data_input()
    ggplot(vgsales) + geom_bar(stat = "identity", position = "fill", aes(Year_of_Release, Sales, fill = Region)) + 
      scale_x_discrete(name ="Jahr") + scale_y_continuous(name ="Relativer Anteil in %", labels = scales::percent) +
      scale_fill_discrete(name = "Region", labels = c("Europa", "Japan", "USA", "Rest der Welt")) +
      theme(text = element_text(size=20)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
  })
}
