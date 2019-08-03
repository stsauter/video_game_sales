genre_ins  <- c("Release-Zeitraum")
genre_outs <- c("genre_plot","genre_plot2", "genre_plot3", "genre_plot4")

tab_genre_layout <- function(){
  tabsetPanel(type = "tabs",
              tabPanel("Allgemeines", subtab_genre_allg()),
              tabPanel("Abh�ngigkeiten", subtab_genre_korr()),
              tabPanel("Regionen", subtab_genre_region())
  )
}

subtab_genre_allg <- function(){
  fluidPage(
    br(),
    titlePanel("Anzahl Spiele je Genre"),
    br(),
    fluidRow(
      column(3,
             wellPanel(
               sliderInput(genre_ins[1], "Release-Zeitraum:",  
                           min = 1980, max = 2016, sep = "", value = c(1980, 2016))
             )       
      ),         
      column(12,
             plotOutput(genre_outs[1])
      )
    ),
    br(),
    titlePanel("Verkaufszahlenentwicklung"),
    br(),
    fluidRow(
      column(12,
             plotOutput(genre_outs[2])
      )
    )
  )
}

subtab_genre_korr <- function(){
  fluidPage(
    br(),
    titlePanel("Korrelation"),
    br(),
    fluidRow(
      column(12,
             plotOutput(genre_outs[3])
      )
    )
  )
}

subtab_genre_region <- function(){
  fluidPage(
    br(),
    titlePanel("Regionen"),
    br(),
    fluidRow(
      column(12,
             plotOutput(genre_outs[4])
      )
    )
  )
}


tab_genre_rendering <- function(input, output){
  
  render_genre_allg(input, output)
  render_genre_korr(input, output)
  render_genre_region(input, output)
}


render_genre_allg <- function(input, output){
  
  data_input <- reactive({
    vgsales <- read_game_sales_csv()
  })
  
  data_input2 <-reactive({
    vgsales <- read_game_sales_csv()
    
    sales_by_year_genre  <- aggregate(vgsales$Global_Sales,by=list(vgsales$Year_of_Release,vgsales$Genre), FUN=sum)
    sales_by_year_genre  <- setNames(sales_by_year_genre, c("Year_of_Release","Genre", "Sales"))
    sales_by_year_genre  <- subset(sales_by_year_genre, as.numeric(Year_of_Release) >= 1980 & as.numeric(Year_of_Release) <= 2016)
#    vgsales_pivot <- gather(sales_by_year_genre, "Global_Sales", key = "Genre", value= "Sales"), wird nicht ben�tigt
  })
  
  output[[genre_outs[1]]] <- renderPlot({
    
    vgsales <- data_input()
    timespan <- input[[genre_ins[1]]]
    line_width <- 1.0
    
    filtered_df <- subset(vgsales, as.numeric(Year_of_Release) >= timespan[1] & as.numeric(Year_of_Release) <= timespan[2])
    
    df <- data.frame(Genre = filtered_df$Genre)
    
      ggplot(df, aes(x=Genre, fill=Genre)) +geom_bar(stat="count") +
        scale_x_discrete(name ="Genre") + scale_y_continuous(name ="Anzahl Spiele") +
        theme(text = element_text(size=20)) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + theme(legend.position = "none")
    }    
  )
  output[[genre_outs[2]]] <- renderPlot({
    
    vgsales <- data_input2()
    line_width <- 1.0

    ggplot(vgsales) + geom_line(aes(x = Year_of_Release, y = Sales, group = Genre, color = Genre), size = line_width) +
      scale_x_discrete(name ="Jahr") + scale_y_continuous(name ="Verkaufte Spiele (Angabe in Mio.)") +
      theme(text = element_text(size=20)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }    
  )
}

render_genre_korr <- function(input, output){
  
  data_input <- reactive({
    vgsales <- read_game_sales_csv()
    vgsales <- cbind(vgsales, "Counter" = 1)
    
    sales_count_by_year_genre  <- aggregate(list(vgsales$Global_Sales, vgsales$Counter),by=list(vgsales$Year_of_Release,vgsales$Genre), FUN=sum)
    sales_count_by_year_genre  <- setNames(sales_count_by_year_genre, c("Year_of_Release","Genre", "Sales", "Count"))
    sales_count_by_year_genre  <- subset(sales_count_by_year_genre, as.numeric(Year_of_Release) >= 1980 & as.numeric(Year_of_Release) <= 2016)
  })  
  
  output[[genre_outs[3]]] <- renderPlot({
    
    vgsales <- data_input()
    line_width <- 1.0
    ggplot(vgsales,aes(x=Count, y=Sales, group = Genre, color = Genre)) + geom_point() +
      scale_x_continuous(name ="Anzahl Spiele") + scale_y_continuous(name ="Verkaufte Spiele (Angabe in Mio.)") 
  }    
  )
}

render_genre_region <- function(input, output){
  
  data_input <- reactive({
    vgsales <- read_game_sales_csv()
    sales_by_genre <- aggregate(list(vgsales$NA_Sales, vgsales$EU_Sales, vgsales$JP_Sales, vgsales$Other_Sales), 
                                by=list(vgsales$Genre), FUN=sum)
    sales_by_genre <- setNames(sales_by_genre, c("Genre", "NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales"))
    vgsales_pivot <- gather(sales_by_genre, "NA_Sales", "EU_Sales", "JP_Sales", "Other_Sales", key = "Region", value= "Sales")
    
  })  
  
  output[[genre_outs[4]]] <- renderPlot({
    
    vgsales <- data_input()
    line_width <- 1.0
    ggplot(vgsales) + geom_bar(stat = "identity", aes(Genre, Sales, fill = Region)) +
      scale_x_discrete(name ="Genre") + scale_y_continuous(name ="Verkaufte Spiele (Angabe in Mio.)") +
      scale_fill_discrete(name = "Region", labels = c("Europa", "Japan", "USA", "Rest der Welt")) +
      theme(text = element_text(size=20)) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
  }    
  )
}