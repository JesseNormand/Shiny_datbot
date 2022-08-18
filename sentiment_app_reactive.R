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
      tableOutput("text"),
      tableOutput("textplot"),
      actionButton("button", "Analyze Text")
      
),


mainPanel(
  
  tabsetPanel(type = "tabs",
              tabPanel("Bar Plot", plotOutput("barplot")),
              tabPanel("Density", plotOutput("plot"))
              
              # tabPanel("Text Plot", plotOutput("textplot"))
              
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
  
  text_an <- eventReactive(input$button, {
    sentiment_by(dfInput()) %>% 
      get_sentences() %>% 
      sentiment_by() %>% 
      highlight()

})
  
  output$textplot <- renderDataTable({
    table(text_an())
  
})

}

#Create a shiny appp object -------------------------------------------------

shinyApp(ui = ui, server = server)

