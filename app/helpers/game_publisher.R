
publisher_ins <- c("publisher_num_bars", "publisher_region", "publisher_timespan")
publisher_outs <- c("publisher_plot", "publisher_sells_plot", "publisher_annual", "publisher_regional")

tab_publisher_layout <- function(){
  tabsetPanel(type = "tabs",
              tabPanel("Übersicht", subtab_top_selling_publisher()),
              tabPanel("Jährliche Verkaufszahlen", subtab_publisher_sales()),
              tabPanel("Top Herausgeber je Jahr", subtab_publisher_annual()),
              tabPanel("Top Herausgeber je Region", subtab_publisher_regional())
  )
}

subtab_top_selling_publisher <- function(){
  fluidPage(
    titlePanel("Welche Herausgeber haben die meisten Spiele verkauft?"),
    br(),
    br(),
    fluidRow(
      column(3,
             wellPanel(
               selectizeInput(
                 publisher_ins[1], 'Anzahl Ergebnisse:', choices = c(5, 10, 15)
                 
               ),
               
               selectizeInput(
                 publisher_ins[2], 'Region:', choices = c("Global", "USA", "Europa", "Japan")
                 
               ),
               
               
               sliderInput(publisher_ins[3], "Release-Zeitraum:",  
                           min = 1980, max = 2016, sep = "", value = c(1980, 2016), animate = TRUE
               )
             )       
      ),
      column(9,
             plotOutput(publisher_outs[1])
      )
    )
  )
}


subtab_publisher_sales <- function(){
  fluidPage(
    titlePanel("Jährliche Verkaufszahlen der Top 5 Herausgeber"),
    br(),
    br(),
    fluidRow(
      column(12,
             plotOutput(publisher_outs[2])
      )
    )
  )
}

subtab_publisher_annual <- function(){
  fluidPage(
    titlePanel("Top Herausgeber je Jahr"),
    br(),
    br(),
    fluidRow(
      column(12,
             plotOutput(publisher_outs[3])
      )
    )
  )
}

subtab_publisher_regional <- function(){
  fluidPage(
    titlePanel("Top 3 Herausgeber je Region"),
    br(),
    br(),
    fluidRow(
      column(12,
             plotOutput(publisher_outs[4])
      )
    )
  )
}

tab_publisher_rendering <- function(input, output){
    
  render_top_selling_publisher(input, output)
  render_publisher_sales(input, output)
  render_publisher_annual(input, output)
  render_publisher_regional(input, output)
}


render_top_selling_publisher <- function(input, output){
  
  data_input <- reactive({
    vgsales <- read_game_sales_csv()
  })
  
  output[[publisher_outs[1]]] <- renderPlot({
    vgsales <- data_input()
    timespan <- input[[publisher_ins[3]]]
    filtered_df <- subset(vgsales, as.numeric(Year_of_Release) >= timespan[1] & as.numeric(Year_of_Release) <= timespan[2])
    switch(input[[publisher_ins[2]]], "Global" = sales_data <- filtered_df$Global_Sales, 
           "USA" = sales_data <- filtered_df$NA_Sales, 
           "Europa" = sales_data <- filtered_df$EU_Sales,
           "Japan" = sales_data <- filtered_df$JP_Sales)
 
    switch(input[[publisher_ins[2]]], "Global" = x_label_prefix <- "Weltweit", 
           "USA" = x_label_prefix <- "In den USA", 
           "Europa" = x_label_prefix <- "In Europa", 
           "Japan" = x_label_prefix <- "In Japan")
    
    df <- data.frame(Publisher = filtered_df$Publisher, Sales = sales_data)
    df <- setNames(aggregate(list(df$Sales), by=list(df$Publisher), FUN=sum), c("Publisher", "Sales"))
    df <- df[order(df$Sales,decreasing = TRUE),]
  
    max_results <- as.numeric(input[[publisher_ins[1]]])
    if(count(df) >= max_results){
      num_results <- max_results
    }
    else{
      num_results <- as.numeric(count(df))
    }
    top_publishers <- df[1:num_results, ]
    df <- data.frame(Name = top_publishers$Publisher, Sales = top_publishers$Sales)
    ggplot(df) + geom_bar(stat = "identity", aes(x=reorder(Name, Sales), Sales), fill = "steelblue")  + coord_flip() +  
      scale_x_discrete(name ="Herausgeber") + scale_y_continuous(name = paste(x_label_prefix, "verkaufte Spiele (Angabe in Mio.)")) + 
      theme(text = element_text(size=20))

  }, height = 700)
}


render_publisher_sales <- function(input, output){
  
  data_input <- reactive({
    vgsales <- read_game_sales_csv()
  })
  
  output[[publisher_outs[2]]] <- renderPlot({
    vgsales <- data_input()
    
    df_top_publisher <- data.frame(Publisher = vgsales$Publisher, Sales = vgsales$Global_Sales)
    df_top_publisher <- setNames(aggregate(list(df_top_publisher$Sales), by=list(df_top_publisher$Publisher), FUN=sum), c("Publisher", "Sales"))
    df_top_publisher <- df_top_publisher[order(df_top_publisher$Sales,decreasing = TRUE),]
    df_top_publisher <- df_top_publisher[1:5, ]
    
    df_annual_sales <- aggregate(list(vgsales$Global_Sales), by=list(vgsales$Publisher, vgsales$Year_of_Release), FUN=sum)
    df_annual_sales <- setNames(df_annual_sales, c("Publisher", "Year", "Sales"))
    df_annual_sales <- subset(df_annual_sales, Publisher %in% df_top_publisher$Publisher)
    df <- data.frame(Publisher = df_annual_sales$Publisher, Year = df_annual_sales$Year, Sales = df_annual_sales$Sales)
    df <- setNames(df, c("Publisher", "Year", "Sales"))
    
    ggplot(df) + geom_line(aes(Year, Sales, group = Publisher, color = Publisher), size = 1.0) +
      scale_x_discrete(name ="Jahr") + scale_y_continuous(name ="Verkaufte Spiele (Angabe in Mio.)") +
      theme(text = element_text(size=20)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(color = "Herausgeber")
    
  }, height = 700)
}


render_publisher_annual <- function(input, output){
  
  data_input <- reactive({
    vgsales <- read_game_sales_csv()
  })
  
  output[[publisher_outs[3]]] <- renderPlot({
    vgsales <- data_input()
    
    df_annual_sales <- vgsales %>% 
      group_by(Year_of_Release, Publisher) %>% 
      summarise(Sales = sum(Global_Sales))
    
    df_top_annual_sales <- df_annual_sales %>% 
      group_by(Year_of_Release)  %>%  
      filter(Sales == max(Sales) & Year_of_Release >= 1980 & Year_of_Release <= 2016)
    
    ggplot(df_top_annual_sales) + geom_bar(stat = "identity", aes(Year_of_Release, Sales, fill = Publisher))  + 
      scale_x_discrete(name ="Jahr") + scale_y_continuous(name = "Weltweit verkaufte Spiele (Angabe in Mio.)") + 
      theme(text = element_text(size=20)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      labs(fill = "Herausgeber") 
    
    
  }, height = 700)
}

render_publisher_regional <- function(input, output){
  
  data_input <- reactive({
    vgsales <- read_game_sales_csv()
  })
  
  output[[publisher_outs[4]]] <- renderPlot({
    vgsales <- data_input()
    
    df_publisher <- vgsales %>%
      group_by(Publisher) %>% 
      summarise(NA_Sales = sum(NA_Sales), EU_Sales = sum(EU_Sales), JP_Sales = sum(JP_Sales), Other_Sales = sum(Other_Sales))
    
    df_publisher_pivot <- gather(df_publisher, "NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", key = "Region", value= "Sales")
    df_top_publisher <- df_publisher_pivot %>%
      group_by(Region) %>%
      top_n(n = 3, wt = Sales) %>%
      arrange(Region, desc(Sales))

    facet_labels <- c("Europa", "Japan", "USA", "Rest der Welt")
    names(facet_labels) <- c("EU_Sales", "JP_Sales",  "NA_Sales", "Other_Sales")
    ggplot(df_top_publisher) + geom_bar(stat = "identity", aes(Publisher, Sales, fill = Publisher))  + 
      scale_x_discrete(name ="Herausgeber") + scale_y_continuous(name = "Verkaufte Spiele (Angabe in Mio.)") + 
      theme(text = element_text(size=20)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      facet_grid(. ~ Region, scales = "free",  space = "free", labeller = labeller(Region = facet_labels)) + 
      labs(fill = "Herausgeber") 
    
  }, height = 700)
}
