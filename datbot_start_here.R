library(tidymodels)
library(tidyverse)
library(dplyr)
library(tidyr)
library(lubridate)
tidymodels_prefer()


dat1 <- read.csv("workorder_data.csv")

attach(dat1)

as_tibble(dat1)


#Create factor table to conduct joint and conditional distribution for two categorical variables

Client <- factor(client, levels = c("Client 1", "Client 2", "Client 3", "Client 4"))

distinct <- distinct(dat1, work_type)

#to grab variables in format

Work_type <- factor(work_type, levels = c(distinct))

#Create factor

Work_type <- factor(work_type, levels = c("QC Service", "Maintenance Occupied", "Winterization", "Snow Removal", "After-Hours Service", "Unit Turn", "Inspection", "Rekey", "Unit Turn Level 2", "Maintenance Vacant", "Maintenance REO", "HVAC Service Call", "Cleaning", "Plumbing", "Pest Control", "Debris Removal", "Eviction", "Utility Activation", "Move Out Inspection", "Eviction Lockout", "Move In Inspection", "Rekey Level 2", "Grass Cut", "Dewinterization", "Eviction Setout", "Initial Service Package", "Storage Unit Trashout", "Appliances",
                                          "Digital Lock Install", "Foundation Assessment"))

#Compute distribution of work_type                    
freqWork_type = table(Work_type)
relfreqWork_type = table(Work_type)/6759
cbind(freqWork_type, relfreqWork_type)

#Compute distribution of client
freqClient = table(Client)
relfreqClient = table(Client)/6759
cbind(freqClient, relfreqClient)

#Compute distribution between categorical variables using joint table library(gmodels)
joint <- CrossTable(Client, Work_type, prop.chisq = FALSE)

#cross table

joint_counts <- joint$t

barplot(joint_counts, beside = FALSE, col = rainbow(4), ylab = 'Frequency', xlab = 'Work Order Type')
legend('topright', c('Client 1', 'Client 2', 'Client 3', 'Client 4'), pch = 15,
                     col = rainbow(4))


##
fx<-matrix(as.numeric(Work_type)) 
fx_1<-matrix(as.numeric(Client))

ggplot(data = dat1) + geom_point(aes(x = work_type, y = as.factor(state), colour = as.factor(date),
                                     shape = as.factor(client)),
                                 size = 3)
#Convert data to quarter

dat1$date <- paste0(year(dat1$date),         # Convert dates to quarterly
                             "/0",
                             quarter(dat1$date))



#-----Model---------------------------------------------------------------------

#convert to factor

dat1$work_type <- as.factor(dat1$work_type)
dat1$client<- as.factor(dat1$client)
dat1$state<- as.factor(dat1$state)

dat1 <- dat1 %>% 
  mutate(work_type = as_factor(work_type))

#set response as numeric

dat1$work_type <- as.numeric(dat1$work_type)

#Split the data

set.seed(99)
dat_split <- initial_split(dat1, prop = .80)
dat_train <- training(dat_split)
dat_test <- testing(dat_split)


lm_model <- 
  logistic_reg() %>% 
  set_engine("glm") %>% 
  set_mode("classification")

lm_form_fit <- lm_model %>% 
  fit(work_type ~ state + client + date, data = dat_train, family = "binomial") 

rec_patterns <- recipe(work_type ~ state + client + date, data = dat_train) %>%
  step_dummy(work_type, one_hot = TRUE) %>%
  prep()

dat2 <- bake(rec_patterns, new_data = dat_train)


#Xgboost

xgb_spec <- 
  boost_tree() %>% 
  set_engine("xgboost") %>% 
  set_mode("classification") 

xg_model <- xgb_spec  %>% 
  fit(work_type ~ state + client + date, data = dat_train) 


#Create Model

tree_model <- 
  decision_tree(min_n = 2) %>% 
  set_engine("rpart") %>% 
  set_mode("classification")

tree_fit <- 
  tree_model %>% 
  fit(work_type ~ date, data = dat_train)


#Make predictions LM

dat_test_small <- dat_test %>% slice(1:5)
predict(lm_form_fit, new_data = dat_test_small)

#Make predictions Tree

dat_test_small <- dat_test %>% slice(1:5)
predict(tree_fit, new_data = dat_test_small)

#Merge predictions with the original data LM

dat_test_small %>% 
  select(work_type) %>% 
  bind_cols(predict(lm_form_fit, dat_test_small))

#Merge predictions with the original data Tree


dat_test_small %>% 
  select(work_type) %>% 
  bind_cols(predict(tree_fit, dat_test_small))

#Make predictions LM

dat_test_small <- dat_test %>% slice(1:5)
predict(xg_model , new_data = dat_test_small)

#Merge predictions with the original data LM

dat_test_small %>% 
  select(work_type) %>% 
  bind_cols(predict(xg_model, dat_test_small))