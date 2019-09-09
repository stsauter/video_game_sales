tab_anova_layout <- function(){
  fluidPage(
    br(),
    titlePanel("ANOVA-Varianzanalyse"),
    br(),
    fluidRow(
      column(8, 
             h4(tagList("Mithilfe einer ANOVA-Analyse soll untersucht werden, 
                        ob die nominalen Merkmale \'Plattform\', \'Genre\' und \'Herausgeber\' einen Einfluss 
                        auf die globalen Verkaufszahlen von Videospielen haben. 
                        Dabei werden nur Spiele betrachtet, die sich weltweit mindestens eine Million mal verkauft haben.",
                        br(),
                        br(),
                        "Zunächst wird allerdings mithilfe des Shapiro-Wilk-Tests überprüft, ob die Zielgröße normalverteilt ist:"
                    )
             )
      )
    ),
    fluidRow(
      column(4, verbatimTextOutput("shapiro")
            
      )
    ),
    br(),
    fluidRow(
      column(8, 
             h4(tagList("Mithilfe einer ANOVA-Analyse soll untersucht werden, 
                        ob die nominalen Merkmale \'Plattform\', \'Genre\' und \'Herausgeber\' einen Einfluss 
                        auf die globalen Verkaufszahlen von Videospielen haben. 
                        Dabei werden nur Spiele betrachtet, die sich weltweit mindestens eine Million mal verkauft haben.",
                        br(),
                        br(),
                        "Zunächst wird allerdings mithilfe des Shapiro-Wilk-Tests überprüft, ob die Zielgröße normalverteilt ist:"
             )
             )
      )
    )
  )
}


tab_anova_rendering <- function(input, output) {
  render_shapiro(input, output)
  
}

render_shapiro <- function(input, output){
  vgsales <- read_game_sales_csv()
  filtered_df <- subset(vgsales, Global_Sales >=1.0)
  res <- shapiro.test(filtered_df$Global_Sales)
  output$shapiro  <- renderText({
    paste("Shapiro-Wilk normality test:\n", "p-value =", res[2][1])
    
  })
}


# leveneTest(filtered_df$Global_Sales ~ filtered_df$Genre)
# summary(aov(filtered_df$Global_Sales ~ filtered_df$Genre)) 
# filtered_df$Plaatform<-as.factor(filtered_df$Plaatform) 