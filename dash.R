## app.R ##
library(shinydashboard)
library(plotly)
library(sentimentr)

dat_reviews <- read.csv("dat_reviews.csv")

attach(dat_reviews)

ui <- dashboardPage(
  dashboardHeader(title = "Text Sentiment Analysis"),
  dashboardSidebar(),
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      box(plotOutput("barplot", height = 250)),
      box(plotlyOutput("plot", height = 250)),
      
      # box(
        # title = "Controls",
        # box(plotOutput("plot", height = 250))
      )
    )
  )


server <- function(input, output) {
  #Begin sentiment analysis
  
  dfInput <- reactive({dat_reviews %>% 
      get_sentences() %>% 
      sentiment() -> dat_reviews_senti
    return(dat_reviews_senti)
  })
  
 
  # output$plot <- renderPlot({
  p <- reactive({dat_reviews %>% 
  ggplot(dfInput()) + geom_density(aes(sentiment),color="blue", fill="lightblue") +
        geom_vline(aes(xintercept=mean(sentiment)),
                   color="blue", linetype="dashed", size=.5)
    fig <- ggplotly(p)
  })
  
  output$plot <- renderPlotly({
  
    fig
    
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

shinyApp(ui, server)
