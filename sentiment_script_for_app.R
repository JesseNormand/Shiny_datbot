#Library
library(sentimentr)

#Load data

dat_neg <- read.csv("reviews.csv") %>% as.tibble(dat_neg) %>% 
  clean_names()

attach(dat_neg)

head(dat_neg, 2)

#process data for analysis

  #keep the columns we want to analyze
dat_neg <- data.frame(negative_review,positive_review)

  #create a new column combining two col
dat_neg$reviews <- paste(dat_neg$negative_review, dat_neg$positive_review)

  #drop columns the columns left over
dat_neg <- subset(dat_neg, select = -c(negative_review, positive_review))

  #shuffle the data and turn back to df

dat_neg <- dat_neg[sample(1:nrow(dat_neg)), ]

dat_neg <- as.data.frame(dat_neg)

#Take sample of data to work with

dat_neg <- dat_neg[1:10000,]
  
dat_reviews <- as.data.frame(dat_neg)

#Create random values to add to our demonstration

dat_reviews$cohort <- paste(sample(x = c("Cohort North", "Cohort South", "Cohort East"), size = 10000, replace = TRUE))

#Begin sentiment analysis

dat_reviews %>% 
  get_sentences() %>% 
  sentiment() -> dat_reviews_senti

head(dat_reviews_senti)

#Plot our sentiment scores to view overall sentiment bias

dat_reviews_senti %>% 
  ggplot() + geom_density(aes(sentiment))


dat_reviews_senti %>% 
  ggplot() + geom_boxplot(aes(x = corhort, y = sentiment))
  

#Compute negative and positive

dat_reviews_senti %>% 
count(ifelse(sentiment > 0, "Positive", "Negative"))

  #vis
dat_reviews_senti %>% 
  mutate(sentiment_scores = ifelse(sentiment > 0, "Positive", "Negative")) %>% 
  count(cohort, sentiment_scores) %>% 
  ggplot() + geom_col(aes(x = cohort, y = n, fill = sentiment_scores)) 

#highlight

sentiment_by(dat_reviews_senti) %>% 
  get_sentences() %>% 
  sentiment_by() %>% 
  highlight()




sentiment(text)

sentiment_by(text)

View(emotion(text))
