library(tidymodels)
library(tidyverse)
library(janitor)
library(caret)
library(dplyr)
library(tidyr)
library(gmodels)
tidymodels_prefer()


dat1 <- read.csv("workorder_data.csv")

attach(dat1)

#Create factor table

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

#

joint_counts <- joint$t

barplot(joint_counts, beside = FALSE, col = rainbow(4), ylab = 'Frequency', xlab = 'Work Order Type')
legend('topright', c('Client 1', 'Client 2', 'Client 3', 'Client 4'), pch = 15,
                     col = rainbow(4))
