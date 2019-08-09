### set wd ###
cbpath <- "~/Documents/GitHub/Words/data_exp_8700-v20-9/"
path <- cbpath
setwd(path)

### load v important packages, but quietly ###
suppressPackageStartupMessages(library(plyr)) 
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggplot2))

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

### remove the words "positive" and "negative" from the screener blocks
data3<-data3[(data3$`# wordlist` != "POSITIVE"),]
data3<-data3[(data3$`# wordlist` != "NEGATIVE"),]
data4<-data4[(data4$`# wordlist` != "POSITIVE"),]
data4<-data4[(data4$`# wordlist` != "NEGATIVE"),]

### merge counter-balanced responses ###
data <- rbind(data1, data2, data3, data4)

### rename (need to get rid of `#`'s)
names(data) <- c("Participant Public ID", "Trial Number", "Reaction Time", "Response", "Correct", "Incorrect", "randomise_trials", "display", 
                 "ANSWER", "exwords", "wordlist", "Metadata")

### clean workspace ###
rm(data1, data2, data3, data4)
data<-data[!is.na(data$wordlist),]

### check list of participants ###
participants <- unique(data$`Participant Public ID`)
print(participants)

### count # of trials per participant ###
pay <- plyr::count(data$`Participant Public ID`)

plyr::count(pay$freq>629) 

pay$x <- as.character(pay$x)
final.participant <- ifelse((pay$freq>629), pay$x, NA)

final.participant <- na.omit(final.participant)

data <- data[ data$`Participant Public ID` %in% final.participant, ]

### create 1 = neg and 0 = pos score for each trial ###
data$rating <- ifelse(data$Response == "negative", 1, 
                      ifelse(data$Response == "positive", 0, NA))

### grab mean and standard deviation of postiive/negative judgments ###
words.summary <- (ddply(data, "wordlist", plyr::summarise, 
                        neg.avg = mean(rating, na.rm = FALSE),
                        neg.sd = sd(rating, na.rm = FALSE),
                        RT = mean(`Reaction Time`, na.rm = FALSE),
                        RT.sd = sd(`Reaction Time`, na.rm = FALSE),
                        avg.cor = mean(Correct, na.rm = FALSE),
                        avg.inc = mean(Incorrect, na.rm = FALSE)))
# write.csv(words.summary, "~/Documents/Nick-Grad/Neta_Lab/Words/words.summary.csv")
write.csv(words.summary,paste(path,"words.summary",'.csv',sep = ''))

### plot all RTs per subj ###
data$`Participant Public ID` <- as.character(data$`Participant Public ID`)
ggplot(data = data, aes(x = `Participant Public ID`, y = `Reaction Time`)) +
  geom_point() +
  ylim(0, 25000)

### calculate percent trials retained after RT cutoff ###
orig <- data %>% count(`Participant Public ID`)
sub <- subset(data, (`Reaction Time` >= 250 & `Reaction Time` <= 4000)) %>% count(`Participant Public ID`)
comb <- merge(orig, sub, by = "Participant Public ID")
comb$PercentRemaining <- (comb$n.y / comb$n.x)
# write.csv(comb, "~/Desktop/250to4000ms.csv")


data2 <- subset(data, (`Reaction Time` >= 250 & `Reaction Time` <= 3000))

words.summary <- words.summary[order(words.summary$neg.avg),]

row.names(words.summary) <- NULL

list <- count(data2$`Participant Public ID`)
count(list$freq < (627/2))


# Write that dataset out to a csv, if ye want. Filename includes date and time.
# write.csv(words.summary,paste(path,"words.summary",
#                           format(Sys.time(),'_%Y-%m-%d_%H-%M-%S'),
#                           '.csv',sep = ''))


