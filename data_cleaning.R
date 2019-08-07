### set wd ###
setwd("~/Documents/Nick-Grad/Neta_Lab/words/")

### load v important packages ###
library(readxl)
library(tidyverse)
library(plyr)
library(ggplot2)

### import task data ###
data1 <- read_xlsx("data/pilot_data_20190719/data_exp_8700-v20_task-l2xg.xlsx")

data2 <- read_xlsx("data/pilot_data_20190719/data_exp_8700-v20_task-bx4b.xlsx")

### pick the cool colomns ###
data1 <- data1[, c("Participant Public ID", "Trial Number", "Reaction Time", "Response", "Correct", "Incorrect", "randomise_trials", "display", 
                   "ANSWER", "# exwords", "# wordlist", "Metadata")]
data2 <- data2[, c("Participant Public ID", "Trial Number", "Reaction Time", "Response", "Correct", "Incorrect", "randomise_trials", "display", 
                   "ANSWER", "# exwords", "# wordlist", "Metadata")]

### merge counter-balanced responses ###
data <- rbind(data1, data2)

### rename (need to get rid of `#`'s)
names(data) <- c("Participant Public ID", "Trial Number", "Reaction Time", "Response", "Correct", "Incorrect", "randomise_trials", "display", 
                 "ANSWER", "exwords", "wordlist", "Metadata")

### clean workspace ###
rm(data1, data2)

### check list of participants ###
participants <- unique(data$`Participant Public ID`)
print(participants)

### count # of trials per participant ###
count(data$`Participant Public ID`)

### create 1 = neg and 0 = pos score for each trial ###
data$rating <- ifelse(data$Response == "negative", 1, 
                      ifelse(data$Response == "positive", 0, NA))

### subject ratings ###
data <- spread(data, key = wordlist, value = Response)

### grab mean and standard deviation of postiive/negative judgments ###
words.summary <- (ddply(data, "wordlist", summarise, 
                        neg.avg = mean(rating, na.rm = FALSE),
                        neg.sd = sd(rating, na.rm = FALSE),
                        RT = mean(`Reaction Time`, na.rm = FALSE),
                        RT.sd = sd(`Reaction Time`, na.rm = FALSE),
                        avg.cor = mean(Correct, na.rm = FALSE),
                        avg.inc = mean(Incorrect, na.rm = FALSE)))

words.summary <- na.omit(words.summary)


