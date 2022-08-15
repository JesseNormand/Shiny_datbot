# data cleaning
library(janitor)
# data prep
library(dplyr)
# tidymodels
library(rsample)
library(recipes)
library(parsnip)
library(tune)
library(dials)
library(workflows)
library(yardstick)

dat1 <- read.csv("workorder_data.csv")

attach(dat1)

as_tibble(dat1)

#Vis barchart

ggplot(data = dat1, aes(x = work_type)) +
  geom_bar()

ggplot(data = dat1, aes(x = factor(work_type))) +
  geom_bar() + geom_bar(fill = "coral") +
  theme_classic() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  coord_flip() 

#Compute frequency and data frame

freq_table <- table(dat1$work_type)

freq_table <- as.data.frame(freq_table) %>% 
  arrange(desc(Freq))

freq_table

#Compute freq percentage

freq_percent <- table(dat1$work_type)/nrow(dat1)

freq_percent <- as.data.frame(freq_percent) %>% 
  arrange(desc(Freq))

freq_percent

#Plot freq

ggplot(data = freq_percent, aes(x = Var1, y = Freq)) +
  geom_point(shape = 24, fill = "blue" ) + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

#Explore cities

dat1_top <- dat1 %>% 
  arrange(desc(work_type)) %>% 
  count(city)
  
  group_by(city) %>% 
  mutate(count = n())

