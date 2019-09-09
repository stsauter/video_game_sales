tab_start_layout <- function(){
  fluidPage(
    br(),
    titlePanel("Einleitung"),
    br(),
    fluidRow(
      column(6,
            h4(tagList("Ziel dieser App ist es, die Verkaufszahlen von Videospielen zu visualisieren und zu analysieren.",
                       br(),
                       "Als statistische Einheit sind diejenigen Videospiele definiert, welche im Zeitraum von 1980 bis 2016 verkauft wurden.",
                       br(),
                       br(),
                       "Folgende Merkmale der statistischen Einheit werden in den Analysen betrachtet:",
                       br(),
                       br(),
                       tags$li("Name des Spiels"),
                       tags$li("Zugehörige Spieleplattform (z.B. XBox, PS4, ...)"),
                       tags$li("Erscheinungsjahr"),
                       tags$li("Genre"),
                       tags$li("Herausgeber"),
                       tags$li("Absatzvolumen in den USA"),
                       tags$li("Absatzvolumen in Europa"),
                       tags$li("Absatzvolumen in Japan"),
                       tags$li("Absatzvolumen in den restlichen Märkten"),
                       tags$li("Gesamtes Absatzvolumen weltweit"),
                       br(),
                       br(),
                       "Der zugrundeliegende Datensatz stammt von Kaggle: ",
                       a("Video Game Sales and Ratings", href="https://www.kaggle.com/kendallgillies/video-game-sales-and-ratings"))
               )
             
      ),
      column(6,
             imageOutput("mario")
      )
      
    )
  )
}

tab_start_rendering <- function(input, output) {
  output$mario <- renderImage({

    list(src = "images/supermario.png",
         contentType = "image/png")
       #  width = 400,
        # height = 300)
  }, deleteFile = FALSE)
}
