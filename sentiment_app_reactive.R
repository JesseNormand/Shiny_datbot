library(shiny)
library(dplyr)
library(ggplot2)
library(sentimentr)

dat_reviews <- read.csv("dat_reviews.csv")

attach(dat_reviews)

# Define UI for app -----------------------------------------------------------

ui <- fluidPage(

  titlePanel("Sentiment Analyzer"),
  # tags$style("#text {font-size:20px;
  #              color:black;
  #              display:block;
  #            font-style:bold;}"),
  sidebarLayout(
    sidebarPanel(
      
      
      # sliderInput(inputId = "trials",
      #             label = "Select the number of trials to run",
      #             min = 500,
      #             max = 1000,
      #             value = 500),
      # 
      # sliderInput(inputId = "min",
      #             label = "Select mininmal expected revenue",
      #             min = 100000,
      #             max = 300000,
      #             value = 100000),
      # 
      # sliderInput(inputId = "max",
      #             label = "Select maximum expected revenue",
      #             min = 100000,
      #             max = 300000,
      #             value = 185000),
      # 
      # textOutput(outputId = "text")
  
),


mainPanel(
  
  tabsetPanel(type = "tabs",
              tabPanel("Density", plotOutput("plot")),
              tabPanel("Bar Plot", plotOutput("barplot"))
             
              
  )
)
)
)

              
#Define server -------------------------------------------------------------

server <- function(input, output, session) {
  
  
  #Begin sentiment analysis
  
  dfInput <- reactive({dat_reviews %>% 
    get_sentences() %>% 
    sentiment() -> dat_reviews_senti
    return(dat_reviews_senti)
   })
  
  output$plot <- renderPlot({
  ggplot(dfInput()) + geom_density(aes(sentiment))
    
   })
  output$barplot <- renderPlot({
  dfInput()%>% 
    mutate(sentiment_scores = ifelse(sentiment > 0, "Positive", "Negative")) %>% 
    count(cohort, sentiment_scores) %>% 
    ggplot() + geom_col(aes(y = cohort, x = n, fill = sentiment_scores))

})
  
}



#Create a shiny appp object -------------------------------------------------

shinyApp(ui = ui, server = server)

