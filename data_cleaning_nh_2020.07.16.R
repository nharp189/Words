### set wd ###
#cbpath <- '~/Documents/GitHub/Words/'
nhpath <- '~/Documents/Nick-Grad/Neta_Lab/Words/'
path = nhpath
setwd(path)

### load v important packages, but quietly ###
suppressPackageStartupMessages(library(plyr)) 
suppressPackageStartupMessages(library(readxl))
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(ggplot2))

### import task data ###
## Main task, A = positive, L = negative
data1 <- read_csv('data/pilot/data_exp_8700-v20_task-l2xg.csv')
## Main task, A = negative, L = positive
data2 <- read_csv('data/pilot/data_exp_8700-v20_task-bx4b.csv')
## Screener task, A = positive, L = negative
data3 <- read_csv('data/pilot/data_exp_8700-v20_task-jdgj.csv')
## Screener task, A = negative, L = positive
data4 <- read_csv('data/pilot/data_exp_8700-v20_task-3raa.csv')
list.IDs <- list(unique(data1$`Participant Private ID`), unique(data2$`Participant Private ID`),
                 unique(data3$`Participant Private ID`), unique(data4$`Participant Private ID`))
unique(list.IDs)
### pick the cool colomns ###
data1 <- data1[, c("Participant Public ID", "Participant Private ID", "Trial Number", "Reaction Time", "Response", "Correct", "Incorrect", "randomise_trials", "display", 
                   "ANSWER", "# exwords", "# wordlist", "Metadata")]
data2 <- data2[, c("Participant Public ID", "Participant Private ID", "Trial Number", "Reaction Time", "Response", "Correct", "Incorrect", "randomise_trials", "display", 
                   "ANSWER", "# exwords", "# wordlist", "Metadata")]
data3 <- data3[, c("Participant Public ID", "Participant Private ID", "Trial Number", "Reaction Time", "Response", "Correct", "Incorrect", "randomise_trials", "display", 
                   "ANSWER", "# exwords", "# wordlist", "Metadata")]
data4 <- data4[, c("Participant Public ID", "Participant Private ID", "Trial Number", "Reaction Time", "Response", "Correct", "Incorrect", "randomise_trials", "display", 
                   "ANSWER", "# exwords", "# wordlist", "Metadata")]

### remove the words "positive" and "negative" from the main blocks ###
data2<-data2[(data2$`# wordlist` != "POSITIVE"),]
data2<-data2[(data2$`# wordlist` != "NEGATIVE"),]
data1<-data1[(data1$`# wordlist` != "POSITIVE"),]
data1<-data1[(data1$`# wordlist` != "NEGATIVE"),]

### merge counter-balanced screening blocks ###
data <- rbind(data3, data4)
library(readxl)
library(tidyverse)
participants <- read_xlsx("~/Desktop/participants (1).xlsx")
participants <- participants %>% subset(Status == "Complete")
lista <- unique(data$`Participant Public ID`)
unique(lista)
unique(participants$PublicID)
unique(data$`Participant Private ID`)
unique(data$`Participant Public ID`)
setdiff(participants$PublicID, lista)
setdiff(lista, participants$PublicID)
### rename (need to get rid of `#`'s)
names(data) <- c("subjID", "Participant Private ID", "Trial Number", "Reaction Time", "Response", "Correct", "Incorrect", "randomise_trials", "display", 
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
names(data1) <- c("subjID", "Participant Private ID", "Trial Number", "Reaction Time", "Response", "Correct", "Incorrect", "randomise_trials", "display", 
                 "ANSWER", "exwords", "wordlist", "Metadata")
### rename (need to get rid of `#`'s)
names(data2) <- c("subjID", "Participant Private ID", "Trial Number", "Reaction Time", "Response", "Correct", "Incorrect", "randomise_trials", "display", 
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

### insane was doubled... oops, select only first instance ###
### first split data into dataframe for each subject ###
split.data <- split(data, data$subjID)
### then write over each subjects data frame with only first instance of each word ###
split.data <- lapply(split.data, function(data) {
  data <- data[match(unique(data$wordlist), data$wordlist),]  
})
### marry the data again ###
data <- bind_rows(split.data, .id = "column_label")

### use to swtich b/w different RT cutoffs ###
###                                        ###
data <- subset(data, (`Reaction Time` >= 250 & `Reaction Time` <= 
                        (mean(data$`Reaction Time`) + 3 *sd(data$`Reaction Time`))))
write.csv(list(unique(data$`Participant Private ID`)), "pilot_subjects.csv")


### remove bad subjects (i.e., A1DCKRRPA4AWVD) ###
### this subject has only 472/629 responses ###
data <- subset(data, !subjID == "A1DCKRRPA4AWVD")
# plyr::count(data$subjID)
### grab mean and standard deviation of postiive/negative judgments ###
words.summary <- (ddply(data, "wordlist", plyr::summarise, 
                        neg.avg = mean(rating, na.rm = FALSE),
                        neg.sd = sd(rating, na.rm = FALSE),
                        RT = mean(`Reaction Time`, na.rm = FALSE),
                        RT.sd = sd(`Reaction Time`, na.rm = FALSE),
                        avg.cor = mean(Correct, na.rm = FALSE),
                        avg.inc = mean(Incorrect, na.rm = FALSE)))

### pair labels with words ###
{### import elexicon generate list (this list used min / max for lexical characteristics
### from ~30 proposed ambiguous words) ###
lex <- read.csv("I166930.csv")
### import valence and arousal data from BRM paper ###
rate <- read.csv("BRM-emot-submit.csv")
amb <- read.csv("59amb.csv")
### merge by word... ###
full.data <- merge(lex, rate, by = "Word")
amb.data <- merge(amb, rate, by ="Word")
amb.data$Val <- "AMB"
### do scrum 4/4 suggestions ###
new.data <- subset(full.data, (A.Mean.Sum > (mean(amb.data$A.Mean.Sum)-sd(amb.data$A.Mean.Sum))) 
                   & (A.Mean.Sum < (mean(amb.data$A.Mean.Sum)+sd(amb.data$A.Mean.Sum))))
### if valence is b/w 3 and 7 AND sd is < sd of new.data arousal, remove ###
neg <- subset(new.data, V.Mean.Sum < 3)
neg$Val <- "NEG"
pos <- subset(new.data, V.Mean.Sum > 7)
pos$Val <- "POS"
final <- rbind(neg, pos, amb.data)
final <- final[match(unique(final$Word), final$Word),]
colnames(final)[colnames(final)=="Word"] <- "wordlist"
rm(lex, rate, amb, full.data, amb.data, new.data, neg, pos)
final <- mutate_all(final, .funs=toupper)
words.summary <- merge(words.summary, final, by = "wordlist")
}

# write.csv(words.summary, "~/Documents/Nick-Grad/Neta_Lab/Words/words.summary.csv")
write.csv(words.summary,paste(path,"words.summary",'.csv',sep = ''))

### pick words b/w 30% and 70% negative and above 875ms ###
temp <- subset(words.summary, words.summary$RT > 875 & (words.summary$neg.avg <= .70 & words.summary$neg.avg >= .30))


write.csv(temp, "words.summary.above875ms.csv")


# list <- count(data2$`Participant Public ID`)
# count(list$freq < (627/2))

# Write that dataset out to a csv, if ye want. Filename includes date and time.
# write.csv(words.summary,paste(path,"words.summary",
#                           format(Sys.time(),'_%Y-%m-%d_%H-%M-%S'),
#                           '.csv',sep = ''))


unique(data$subjID)
############################ Demographics ###########################
## Demographic Questionnaire and Screener Questions
demog <- read_csv('data/pilot/data_exp_8700-v20_questionnaire-rok5.csv')
plyr::count(unique(demog$`Participant Public ID`) %in% unique(data$subjID))
demog <- subset(demog, !is.na(demog$`Participant Public ID`))
### pick the cool colomns ###
demog <- demog[, c("Participant Public ID", "Question Key","Response")]

### separate into 3 data frames
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
demog <- demog[demog$`Participant Public ID` %in% data$subjID, ]

### calculations for mean age, race, and sex distributions
mean(demog$age)
mean(demog$age)
max(demog$age)
plyr::count(demog$race)
plyr::count(demog$sex)
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

# ### create an interactive scatterplot
# p <- ggplot(words.summary, aes(x = wordlist, y = neg.avg,
#                                              text = paste(
#                                                "Word: ", wordlist, 
#                                                "\n", "Valence Mean: ", (neg.avg*100), 
#                                                "\n", "Valence SD: ", (neg.sd*100),
#                                                sep = ""), fill = Val)) + 
#   scale_fill_manual(values = c("blue","red","green")) +
#   labs(x = "Stimulus Valence", 
#        y = "% Negative Rating Across Participants",
#        title = "Average Valence Ratings Across Participants",
#        fill = "Valence") +
#   geom_jitter() 
# p <- ggplotly(p, tooltip = "text")
# print(p)
# 
# ### save interactive scatterplot as an html file
# wordpath <- "~/Documents/Github/words/"
# setwd(wordpath)
# htmlwidgets::saveWidget(as_widget(p), "scatterplot_pilot_val.html")
# 
# ### create an interactive scatterplot
# p <- ggplot(words.summary, aes(x = wordlist, y = RT,
#                                text = paste(
#                                  "Word: ", wordlist, 
#                                  "\n", "RT: ", (RT), 
#                                  "\n", "RT SD: ", (RT.sd),
#                                  sep = ""), fill = Val)) + 
#   scale_fill_manual(values = c("blue","red","green")) +
#   labs(x = "Stimulus Valence", 
#        y = "Reaction Time (ms)",
#        fill = "Valence") +
#   geom_jitter() +
#   geom_hline(yintercept=875, linetype="dashed", color = "#2C528C", size=0.5) 
# p <- ggplotly(p, tooltip = "text")
# print(p)
# 
# ### save interactive scatterplot as an html file
# wordpath <- "~/Documents/Github/words/"
# setwd(wordpath)
# htmlwidgets::saveWidget(as_widget(p), "scatterplot_pilot_rt.html")

