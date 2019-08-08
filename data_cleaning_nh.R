### set wd ###
setwd("~/Box Sync/Lab/Words/data_exp_8700-v20-9")

### load v important packages ###
library(plyr)
library(readxl)
library(tidyverse)
library(ggplot2)

### import task data ###
## Main task, A = positive, L = negative
data1 <- read_csv("data_exp_8700-v20_task-l2xg.csv")
## Main task, A = negative, L = positive
data2 <- read_csv("data_exp_8700-v20_task-bx4b.csv")
## Screener task, A = positive, L = negative
data3 <- read_csv("data_exp_8700-v20_task-jdgj.csv")
## Screener task, A = negative, L = positive
data4 <- read_csv("data_exp_8700-v20_task-3raa.csv")
## Demographic Questionnaire and Screener Questions
demog <- read_csv("data_exp_8700-v20_questionnaire-rok5.csv")

### pick the cool colomns ###
data1 <- data1[, c("Participant Public ID", "Trial Number", "Reaction Time", "Response", "Correct", "Incorrect", "randomise_trials", "display", 
                   "ANSWER", "# exwords", "# wordlist", "Metadata")]
data2 <- data2[, c("Participant Public ID", "Trial Number", "Reaction Time", "Response", "Correct", "Incorrect", "randomise_trials", "display", 
                   "ANSWER", "# exwords", "# wordlist", "Metadata")]
data3 <- data3[, c("Participant Public ID", "Trial Number", "Reaction Time", "Response", "Correct", "Incorrect", "randomise_trials", "display", 
                   "ANSWER", "# exwords", "# wordlist", "Metadata")]
data4 <- data4[, c("Participant Public ID", "Trial Number", "Reaction Time", "Response", "Correct", "Incorrect", "randomise_trials", "display", 
                   "ANSWER", "# exwords", "# wordlist", "Metadata")]

### merge counter-balanced responses ###
data <- rbind(data1, data2, data3, data4)

### rename (need to get rid of `#`'s)
names(data) <- c("Participant Public ID", "Trial Number", "Reaction Time", "Response", "Correct", "Incorrect", "randomise_trials", "display", 
                 "ANSWER", "exwords", "wordlist", "Metadata")

### remove the words "positive" and "negative" from the screener blocks
data<-data[(data$wordlist != "POSITIVE"),]
data<-data[(data$wordlist != "NEGATIVE"),]

### clean workspace ###
rm(data1, data2, data3, data4)
data<-data[!is.na(data$wordlist),]

### check list of participants ###
participants <- unique(data$`Participant Public ID`)
print(participants)

### count # of trials per participant ###
pay <- count(data$`Participant Public ID`)

count(pay$freq>627) 

pay$x <- as.character(pay$x)
final.participant <- ifelse((pay$freq>627), pay$x, NA)

final.participant <- na.omit(final.participant)

data <- data[ data$`Participant Public ID` %in% final.participant, ]

### create 1 = neg and 0 = pos score for each trial ###
data$rating <- ifelse(data$Response == "negative", 1, 
                      ifelse(data$Response == "positive", 0, NA))

### grab mean and standard deviation of postiive/negative judgments ###
words.summary <- (ddply(data2, "wordlist", summarise, 
                        neg.avg = mean(rating, na.rm = FALSE),
                        neg.sd = sd(rating, na.rm = FALSE),
                        RT = mean(`Reaction Time`, na.rm = FALSE),
                        RT.sd = sd(`Reaction Time`, na.rm = FALSE),
                        avg.cor = mean(Correct, na.rm = FALSE),
                        avg.inc = mean(Incorrect, na.rm = FALSE)))

data2 <- subset(data, (`Reaction Time` >= 250 & `Reaction Time` <= 3000))

words.summary <- words.summary[order(words.summary$neg.avg),]

row.names(words.summary) <- NULL

list <- count(data2$`Participant Public ID`)
count(list$freq < (627/2))


# Write that dataset out to a csv, if ye want. Filename includes date and time.
write.csv(words.summary,paste("~/Box Sync/Lab/Words/","words.summary",
                          format(Sys.time(),'_%Y-%m-%d_%H-%M-%S'),
                          '.csv',sep = ''))


