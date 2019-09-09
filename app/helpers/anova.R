tab_anova_layout <- function(){
  fluidPage(
    br(),
    titlePanel("Varianzanalyse"),
    br(),
    fluidRow(
      column(8, 
             h4(tagList("Mithilfe einer Varianzanalyse soll untersucht werden, 
                        ob die nominalen Merkmale \'Plattform\', \'Genre\' und \'Herausgeber\' einen Einfluss 
                        auf die globalen Verkaufszahlen von Videospielen haben. 
                        Dabei werden nur Spiele betrachtet, die sich weltweit mindestens eine Million mal verkauft haben.",
                        br(),
                        br(),
                        "Die Varianzanalyse soll mit der ANOVA-Methode durchgeführt werden. Voraussetzung hierfür ist jedoch, 
                        dass die Zielgröße, in diesem Fall also das weltweite Absatzvolumen, normalverteilt ist. 
                        Dies wird mithilfe des Shapiro-Wilk-Tests überprüft:"
                    )
             )
      )
    ),
    fluidRow(
      column(4, verbatimTextOutput("shapiro")
            
      )
    ),
    fluidRow(
      column(8, 
             h4(tagList("Da der p-Wert < 0.05 ist, müssen wir davon ausgehen, dass die Zielgröße nicht normalverteilt ist. 
                        ANOVA kann also in diesem Fall nicht angewandt werden. Daher werden wir im Folgenden den Kruskal-Wallis-Test für die Varianzanalyse einsetzen.",
                        br(),
                        br()
                  )
          )
      )
    ),
    br(),
    
    h3("Plattform"),
    fluidRow(
      column(8, 
             h4("Wir prüfen, ob es bei den Spieleplattformen einen signifikanten Einfluss auf die Verkaufszahlen gibt:"
          )
      )
    ),
    fluidRow(
      column(4, verbatimTextOutput("kruskal_platform")
             
      )
    ),
    fluidRow(
      column(8, 
             h4("Da der p-Wert < 0.05 ist, können wir annehmen, dass das Merkmal \'Plattform\' einen Einfluss auf die Verkaufszahlen hat."
           )
      )
    ),
    br(),
    
    h3("Genre"),
    fluidRow(
      column(8, 
             h4("Wir prüfen, ob es bei den Genres einen signifikanten Einfluss auf die Verkaufszahlen gibt:"
             )
      )
    ),
    fluidRow(
      column(4, verbatimTextOutput("kruskal_genre")
             
      )
    ),
    fluidRow(
      column(8, 
             h4("Da der p-Wert < 0.05 ist, können wir annehmen, dass das Merkmal \'Genre\' einen Einfluss auf die Verkaufszahlen hat."
             )
      )
    ),
    br(),
    
    h3("Herausgeber"),
    fluidRow(
      column(8, 
             h4("Wir prüfen, ob es bei den Herausgebern einen signifikanten Einfluss auf die Verkaufszahlen gibt:"
             )
      )
    ),
    fluidRow(
      column(4, verbatimTextOutput("kruskal_publisher")
             
      )
    ),
    fluidRow(
      column(8, 
             h4("Da der p-Wert < 0.05 ist, können wir annehmen, dass das Merkmal \'Herausgeber\' einen Einfluss auf die Verkaufszahlen hat."
             )
      )
    )
  )
}


tab_anova_rendering <- function(input, output) {
  render_shapiro(input, output)
  render_kruskal_platform(input, output)
  render_kruskal_genre(input, output)
  render_kruskal_publisher(input, output)
  
}

render_shapiro <- function(input, output){
  vgsales <- read_game_sales_csv()
  filtered_df <- subset(vgsales, Global_Sales >= 1.0)
  filtered_df$Log_Sales <- log(filtered_df$Global_Sales)
  res <- shapiro.test(filtered_df$Log_Sales)
  output$shapiro  <- renderText({
    paste("Shapiro-Wilk normality test:\n", "p-value =", res[2][1])
    
  })
}

render_kruskal_platform <- function(input, output){
  vgsales <- read_game_sales_csv()
  filtered_df <- subset(vgsales, Global_Sales >= 1.0)
  res <- kruskal.test(filtered_df$Global_Sales ~ filtered_df$Platform)
  output$kruskal_platform <- renderText({
    paste("Kruskal-Wallis rank sum test:\n", "p-value =", res[3][1])
    
  })
}

render_kruskal_genre<- function(input, output){
  vgsales <- read_game_sales_csv()
  filtered_df <- subset(vgsales, Global_Sales >= 1.0)
  res <- kruskal.test(filtered_df$Global_Sales ~ filtered_df$Genre)
  output$kruskal_genre<- renderText({
    paste("Kruskal-Wallis rank sum test:\n", "p-value =", res[3][1])
    
  })
}

render_kruskal_publisher<- function(input, output){
  vgsales <- read_game_sales_csv()
  filtered_df <- subset(vgsales, Global_Sales >= 1.0)
  res <- kruskal.test(filtered_df$Global_Sales ~ filtered_df$Publisher)
  output$kruskal_publisher<- renderText({
    paste("Kruskal-Wallis rank sum test:\n", "p-value =", res[3][1])
    
  })
}

# leveneTest(filtered_df$Global_Sales ~ filtered_df$Genre)
# summary(aov(filtered_df$Global_Sales ~ filtered_df$Genre)) 
# filtered_df$Plaatform<-as.factor(filtered_df$Plaatform) 
# pairwise.wilcox.test(filtered_df$Global_Sales, filtered_df$Genre, p.adjust.method = "BH")