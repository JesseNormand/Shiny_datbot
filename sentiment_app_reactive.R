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
      tableOutput("text")
      
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
  
  output$text <- renderTable({
  dfInput() %>% 
    count(ifelse(sentiment > 0, "Positive", "Negative")) %>% 
      as.data.frame()
  
})

}



#Create a shiny appp object -------------------------------------------------

shinyApp(ui = ui, server = server)

