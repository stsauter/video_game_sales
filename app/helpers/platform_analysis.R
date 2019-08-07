platform_ins  <- c("platform_num_bars", "platform_region", "platform_timespan", "platform_num_bars2","platform_timespan2"
                   ,"platform_num_bars3","platform_timespan3")
platform_outs <- c("platform_plot","platform_plot2","platform_plot3","platform_plot4")

tab_platform_layout <- function(){
  tabsetPanel(type = "tabs",
              tabPanel("Verkaufszahlen", subtab_top_selling_platform()),
              tabPanel("Releases", subtab_top_releasing_platform()),
              tabPanel("Durchschnitt Verkäufe/Releases", subtab_top_sellrelease_platform()),
              tabPanel("Jährliche Verkaufszahlen", subtab_sales_platform())
  )
}
subtab_top_selling_platform <- function(){
    fluidPage(
      titlePanel("Auf welcher Plattform wurden die meisten Spiele verkauft"),
      br(),
      br(),
      fluidRow(
        column(3,
               wellPanel(
                 selectizeInput(
                   platform_ins[1], 'Anzahl Ergebnisse:', choices = c(5, 10, 15, 20, 25, 30, 35)
                   
                 ),
                 
                 selectizeInput(
                   platform_ins[2], 'Region:', choices = c("Global", "USA", "Europa", "Japan")
                   
                 ),
                 
                 
                 sliderInput(platform_ins[3], "Release-Zeitraum:",  
                             min = 1980, max = 2016, sep = "", value = c(1980, 2016), animate = TRUE
                 )
               )       
        ),
        column(9,
               plotOutput(platform_outs[1])
        )
      )
    )
  }

subtab_top_releasing_platform <- function(){
  fluidPage(
    titlePanel("Auf welcher Plattform sind die meisten Spiele erschienen"),
    br(),
    br(),
    fluidRow(
      column(3,
             wellPanel(
               selectizeInput(
                 platform_ins[4], 'Anzahl Ergebnisse:', choices = c(5, 10, 15, 20, 25, 30, 35)
                 
               ),
               
               sliderInput(platform_ins[5], "Release-Zeitraum:",  
                           min = 1980, max = 2016, sep = "", value = c(1980, 2016), animate = TRUE
               )
             )       
      ),        
      column(9,
             plotOutput(platform_outs[2])
      )
    )
  )
}

subtab_top_sellrelease_platform <- function(){
  fluidPage(
    titlePanel("Sales/Anzahl_Spiele je Platform"),
    br(),
    br(),
    fluidRow(
      column(3,
             wellPanel(
               selectizeInput(
                 platform_ins[6], 'Anzahl Ergebnisse:', choices = c(5, 10, 15, 20, 25, 30, 35)
                 
               ),
            
               sliderInput(platform_ins[7], "Release-Zeitraum:",  
                           min = 1980, max = 2016, sep = "", value = c(1980, 2016), animate = TRUE
               )
             )       
      ),
      column(9,
             plotOutput(platform_outs[4])
      )
    )
  )
}


subtab_sales_platform <- function(){
  fluidPage(
    titlePanel("Jährliche Verkaufszahlen"),
    br(),
    br(),
    fluidRow(
      column(12,
             plotOutput(platform_outs[3])
      )
    )
  )
}

tab_platform_rendering <- function(input, output){
  
  render_top_selling_platform(input, output)
  render_top_releasing_platform(input, output)
  render_top_sellrelease_platform(input, output)
  render_sales_platform(input, output)
}

render_top_selling_platform <- function(input, output){
  
  data_input <- reactive({
    vgsales <- read_game_sales_csv()
  })
  
  output[[platform_outs[1]]] <- renderPlot({
    vgsales <- data_input()
    timespan <- input[[platform_ins[3]]]
    filtered_df <- subset(vgsales, as.numeric(Year_of_Release) >= timespan[1] & as.numeric(Year_of_Release) <= timespan[2])
    switch(input[[platform_ins[2]]], "Global" = sales_data <- filtered_df$Global_Sales, 
           "USA" = sales_data <- filtered_df$NA_Sales, 
           "Europa" = sales_data <- filtered_df$EU_Sales,
           "Japan" = sales_data <- filtered_df$JP_Sales)
    
    switch(input[[platform_ins[2]]], "Global" = x_label_prefix <- "Weltweit", 
           "USA" = x_label_prefix <- "In den USA", 
           "Europa" = x_label_prefix <- "In Europa", 
           "Japan" = x_label_prefix <- "In Japan")
    
    df <- data.frame(Platform = filtered_df$Platform, Sales = sales_data)
    df <- setNames(aggregate(list(df$Sales), by=list(df$Platform), FUN=sum), c("Platform", "Sales"))
    df <- df[order(df$Sales,decreasing = TRUE),]
    
    max_results <- as.numeric(input[[platform_ins[1]]])
    if(count(df) >= max_results){
      num_results <- max_results
    }
    else{
      num_results <- as.numeric(count(df))
    }
    top_platforms <- df[1:num_results, ]
    df <- data.frame(Name = top_platforms$Platform, Sales = top_platforms$Sales)
    ggplot(df) + geom_bar(stat = "identity", aes(x=reorder(Name, Sales), Sales), fill = "steelblue")  + coord_flip() +  
      scale_x_discrete(name ="Platform") + scale_y_continuous(name = paste(x_label_prefix, "verkaufte Spiele (Angabe in Mio.)")) + 
      theme(text = element_text(size=20))
    
  }, height = 700)
}

render_top_releasing_platform <- function(input, output){
  
  data_input <- reactive({
    vgsales <- read_game_sales_csv()
  })
  
  output[[platform_outs[2]]] <- renderPlot({
    vgsales <- data_input()
    vgsales <- cbind(vgsales, "Counter" = 1)
    timespan <- input[[platform_ins[5]]]
    filtered_df <- subset(vgsales, as.numeric(Year_of_Release) >= timespan[1] & as.numeric(Year_of_Release) <= timespan[2])
    
    df <- data.frame(Platform = filtered_df$Platform, Counter = filtered_df$Counter)
    df <- setNames(aggregate(df$Counter, by=list(df$Platform), FUN=sum), c("Platform", "Counter"))
    df <- df[order(df$Counter,decreasing = TRUE),]
    
    max_results <- as.numeric(input[[platform_ins[4]]])
    if(count(df) >= max_results){
      num_results <- max_results
    }
    else{
      num_results <- as.numeric(count(df))
    }
    top_platforms <- df[1:num_results, ]
    df <- data.frame(Name = top_platforms$Platform, Counter = top_platforms$Counter)
    ggplot(df) + geom_bar(stat = "identity", aes(x=reorder(Name, Counter), Counter), fill = "steelblue")  + coord_flip() +  
      scale_x_discrete(name ="Platform") + scale_y_continuous(name ="Anzahl Spiele") + 
      theme(text = element_text(size=20))
    
  }, height = 700)
}

render_top_sellrelease_platform <- function(input, output){
  
  data_input <- reactive({
    vgsales <- read_game_sales_csv()
  })
  
  output[[platform_outs[4]]] <- renderPlot({
    vgsales <- data_input()
    vgsales <- cbind(vgsales, "Counter" = 1)
    timespan <- input[[platform_ins[7]]]
    filtered_df <- subset(vgsales, as.numeric(Year_of_Release) >= timespan[1] & as.numeric(Year_of_Release) <= timespan[2])
    
    df <- data.frame(Platform = filtered_df$Platform, Counter = filtered_df$Counter, Sales = filtered_df$Global_Sales)
    df <- setNames(aggregate(list(df$Counter,df$Sales), by=list(df$Platform), FUN=sum), c("Platform", "Counter", "Sales"))
    df$SalesCounter <- df$Sales/df$Counter
    df <- df[order(df$SalesCounter,decreasing = TRUE),]
    
    max_results <- as.numeric(input[[platform_ins[6]]])
    if(count(df) >= max_results){
      num_results <- max_results
    }
    else{
      num_results <- as.numeric(count(df))
    }
    top_platforms <- df[1:num_results, ]
    df <- data.frame(Name = top_platforms$Platform, SalesCounter = top_platforms$SalesCounter)
    ggplot(df) + geom_bar(stat = "identity", aes(x=reorder(Name, SalesCounter), SalesCounter), fill = "steelblue")  + coord_flip() +  
      scale_x_discrete(name ="Platform") + scale_y_continuous(name ="SalesCounter in Mio.") + 
      theme(text = element_text(size=20))
    
  }, height = 700)
}

render_sales_platform <- function(input, output){
  
  data_input <-reactive({
    vgsales <- read_game_sales_csv()
    
    sales_by_year_plat  <- aggregate(vgsales$Global_Sales,by=list(vgsales$Year_of_Release,vgsales$Platform), FUN=sum)
    sales_by_year_plat  <- setNames(sales_by_year_plat, c("Year_of_Release","Platform", "Sales"))
    sales_by_year_plat  <- subset(sales_by_year_plat, as.numeric(Year_of_Release) >= 1980 & as.numeric(Year_of_Release) <= 2016)
  })

  output[[platform_outs[3]]] <- renderPlot({
    
    vgsales <- data_input()
    line_width <- 1.0
    
    ggplot(vgsales) + geom_line(aes(x = Year_of_Release, y = Sales, group = Platform, color = Platform), size = line_width) +
      scale_x_discrete(name ="Jahr") + scale_y_continuous(name ="Verkaufte Spiele (Angabe in Mio.)") +
      theme(text = element_text(size=20)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }, height = 700)
}
