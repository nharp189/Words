### set wd ###
cbpath <- '~/Documents/GitHub/Words/'
nhpath <- '~/Documents/Nick-Grad/Neta_Lab/Words/'
path <- cbpath
setwd(path)

### load v important packages, but quietly ###
suppressPackageStartupMessages(library(plyr)) 
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggplot2))

### import task data ###
## Main task, A = positive, L = negative
data1 <- read_csv('data_exp_8700-v20-9/data_exp_8700-v20_task-l2xg.csv')
## Main task, A = negative, L = positive
data2 <- read_csv('data_exp_8700-v20-9/data_exp_8700-v20_task-bx4b.csv')
## Screener task, A = positive, L = negative
data3 <- read_csv('data_exp_8700-v20-9/data_exp_8700-v20_task-jdgj.csv')
## Screener task, A = negative, L = positive
data4 <- read_csv('data_exp_8700-v20-9/data_exp_8700-v20_task-3raa.csv')

### pick the cool colomns ###
data1 <- data1[, c("Participant Public ID", "Trial Number", "Reaction Time", "Response", "Correct", "Incorrect", "randomise_trials", "display", 
                   "ANSWER", "# exwords", "# wordlist", "Metadata")]
data2 <- data2[, c("Participant Public ID", "Trial Number", "Reaction Time", "Response", "Correct", "Incorrect", "randomise_trials", "display", 
                   "ANSWER", "# exwords", "# wordlist", "Metadata")]
data3 <- data3[, c("Participant Public ID", "Trial Number", "Reaction Time", "Response", "Correct", "Incorrect", "randomise_trials", "display", 
                   "ANSWER", "# exwords", "# wordlist", "Metadata")]
data4 <- data4[, c("Participant Public ID", "Trial Number", "Reaction Time", "Response", "Correct", "Incorrect", "randomise_trials", "display", 
                   "ANSWER", "# exwords", "# wordlist", "Metadata")]

### remove the words "positive" and "negative" from the main blocks ###
data2<-data2[(data2$`# wordlist` != "POSITIVE"),]
data2<-data2[(data2$`# wordlist` != "NEGATIVE"),]
data1<-data1[(data1$`# wordlist` != "POSITIVE"),]
data1<-data1[(data1$`# wordlist` != "NEGATIVE"),]

### merge counter-balanced screening blocks ###
data <- rbind(data3, data4)

### rename (need to get rid of `#`'s)
names(data) <- c("subjID", "Trial Number", "Reaction Time", "Response", "Correct", "Incorrect", "randomise_trials", "display", 
                 "ANSWER", "exwords", "wordlist", "Metadata")

### positive/negative were screeners and actual words of interest ###
### select only first instance of each ###
### first split data into dataframe for each subject ###
split.data <- split(data, data$subjID)
### then write over each subjects data frame with only first instance of each word ###
split.data <- lapply(split.data, function(data) {
  data <- data[match(unique(data$wordlist), data$wordlist),]  
})
data <- bind_rows(split.data, .id = "column_label")
data <- select(data,-c("column_label"))

### merge counter-balanced testing blocks ###
### this renaming could be made more elegant... ###
### rename (need to get rid of `#`'s)
names(data1) <- c("subjID", "Trial Number", "Reaction Time", "Response", "Correct", "Incorrect", "randomise_trials", "display", 
                 "ANSWER", "exwords", "wordlist", "Metadata")
### rename (need to get rid of `#`'s)
names(data2) <- c("subjID", "Trial Number", "Reaction Time", "Response", "Correct", "Incorrect", "randomise_trials", "display", 
                 "ANSWER", "exwords", "wordlist", "Metadata")
data <- rbind(data, data1, data2)

### clean workspace ###
rm(data1, data2, data3, data4)
data<-data[!is.na(data$wordlist),]

### check list of participants ###
participants <- unique(data$subjID)
# print(participants)

### count # of trials per participant ###
pay <- plyr::count(data$subjID)

# plyr::count(pay$freq>629) 

pay$x <- as.character(pay$x)
final.participant <- ifelse((pay$freq>629), pay$x, NA)

final.participant <- na.omit(final.participant)

data <- data[ data$subjID %in% final.participant, ]

### create 1 = neg and 0 = pos score for each trial ###
data$rating <- ifelse(data$Response == "negative", 1, 
                      ifelse(data$Response == "positive", 0, NA))


### use to swtich b/w different RT cutoffs ###
###                                        ###
#### data <- subset(data, (`Reaction Time` >= 250 & `Reaction Time` <= 2932))

### insane was doubled... oops, select only first instance ###
### first split data into dataframe for each subject ###
split.data <- split(data, data$subjID)
### then write over each subjects data frame with only first instance of each word ###
split.data <- lapply(split.data, function(data) {
  data <- data[match(unique(data$wordlist), data$wordlist),]  
})
### marry the data again ###
data <- bind_rows(split.data, .id = "column_label")

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
  ylim(0, 10000) +
  geom_hline(mapping = NULL, yintercept = 2932.776)

### calculate percent trials retained after RT cutoff ###
orig <- data %>% count(`Participant Public ID`)
sub <- subset(data, (`Reaction Time` >= 250 & `Reaction Time` <= 2214.349)) %>% count(`Participant Public ID`)
comb <- merge(orig, sub, by = "Participant Public ID")
comb$PercentRemaining <- (comb$n.y / comb$n.x)
 write.csv(comb, "~/Desktop/250to2214ms.csv")


data2 <- subset(data, (`Reaction Time` >= 250 & `Reaction Time` <= 3000))

words.summary <- words.summary[order(words.summary$neg.avg),]

row.names(words.summary) <- NULL

# list <- count(data2$`Participant Public ID`)
# count(list$freq < (627/2))

# Write that dataset out to a csv, if ye want. Filename includes date and time.
# write.csv(words.summary,paste(path,"words.summary",
#                           format(Sys.time(),'_%Y-%m-%d_%H-%M-%S'),
#                           '.csv',sep = ''))



############################ Demographics ###########################
## Demographic Questionnaire and Screener Questions
demog <- read_csv('data_exp_8700-v20-9/data_exp_8700-v20_questionnaire-rok5.csv')

### pick the cool colomns ###
demog <- demog[, c("Participant Public ID", "Question Key","Response")]

### separate into 2 data frames
demog_age<-demog[(demog$`Question Key` == "age"),]
demog_race<-demog[(demog$`Question Key`== "race"),]
demog_sex<-demog[(demog$`Question Key`== "sex"),]

### remove Question Type columns
demog_age <- demog_age[, c("Participant Public ID", "Response")]
demog_race <- demog_race[, c("Participant Public ID", "Response")]
demog_sex <- demog_sex[, c("Participant Public ID", "Response")]

### combine dfs back together
demog <- merge(demog_age,demog_race,by="Participant Public ID")
demog <- merge(demog,demog_sex,by="Participant Public ID")

### rename race and age, make age numeric
names(demog) <- c("Participant Public ID", "age", "race","sex")
demog$age <- as.numeric(demog$age)

### clean workspace ###
rm(demog_age,demog_race,demog_sex)
demog<-demog[!is.na(demog$`Participant Public ID`),]

### subset demog for only the participants who completed the whole thing
demog <- demog[demog$`Participant Public ID` %in% final.participant, ]

### calculations for mean age, race, and sex distributions
mean(demog$age)

sum(str_count(demog$race, "White - not of Hispanic Origin"))/length(final.participant) *100
sum(str_count(demog$race, "American Indian or Alaskan Native"))/length(final.participant) *100
sum(str_count(demog$race, "Asian"))/length(final.participant) *100
sum(str_count(demog$race, "Black - not of Hispanic Origin"))/length(final.participant) *100
sum(str_count(demog$race, "Hispanic or Latino"))/length(final.participant) *100
sum(str_count(demog$race, "Native Hawaiian or Other Pacific Islander"))/length(final.participant) *100
sum(str_count(demog$race, "Other"))/length(final.participant) *100

sum(str_count(demog$sex, "Female"))/length(final.participant) *100
sum(str_count(demog$sex, "Male"))/length(final.participant) *100
sum(str_count(demog$sex, "Other"))/length(final.participant) *100

### displays race in a table
prop.table(table(demog$race))
tbl <- table(demog$race)
cbind(tbl,prop.table(tbl))
