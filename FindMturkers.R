nhpath <- '~/Documents/Nick-Grad/Neta_Lab/Words/'
path = nhpath
setwd(path)

{library(readr)
  library(tidyverse)
  library(utils)
  library(lsr)}

### read in the final words subjects n = 227 ###
full <- read_csv("data/study1_redo_data/words_study1_data_2020.08.10.csv")

### read in the Mturk & CloudResearch logs ###
Mturk <- readbulk::read_bulk("Mturk/Words_Faces_IAPS_IPANAT_Round2/Mturk/")
Cloud <- readbulk::read_bulk("Mturk/Words_Faces_IAPS_IPANAT_Round2/CloudResearch/")

### make them characters ###
for(i in 1:ncol(Mturk)) {
  Mturk[, i] <- as.character(Mturk[, i])
}

for(i in 1:ncol(Cloud)) {
  Cloud[, i] <- as.character(Cloud[, i])
}

Cloud_Sub <- subset(Cloud, Cloud$Actual.Completion.Code %in% full$Participant.Completion.Code)
Mturk_Sub <- subset(Mturk, Mturk$Answer.surveycode %in% full$Participant.Completion.Code)

Cloud_Sub <- Cloud_Sub[, c("AmazonIdentifier", "Actual.Completion.Code")]
names(Cloud_Sub) <- c("Mturk_ID", "Participant.Completion.Code")

Mturk_Sub <- Mturk_Sub[, c("WorkerId", "Answer.surveycode")]
names(Mturk_Sub) <-c("Mturk_ID", "Participant.Completion.Code")                        

combined <- rbind(Cloud_Sub, Mturk_Sub)

setdiff(full$Participant.Completion.Code, combined$Participant.Completion.Code)
# no Mturk ID for szplnl; had shitty gorilla code ###
# A3F51C49T9A34D pairs with tpqovn ###
# AJ4B5W7LPJEQO pairs with subpzo ###
# no Mturk ID for kgssah; had shitty gorilla code ###
unique(combined$Participant.Completion.Code)
combined[duplicated(combined$Participant.Completion.Code), ]
temp <- Mturk[which(Mturk$Answer.surveycode %in% c("kgwptl",
                                                           "pflrlx")), ]

combined <- unique(combined)

covid_data <- read.csv("~/Documents/Nick-Grad/Neta_Lab/covid19/Git/Words_Covid_Combined_2020.06.22.csv")
combined <- combined %>% subset(!(Mturk_ID %in% covid_data$mturk))

write.csv(combined, "WordsMturkers_DidNotDoCOVIDTime1.csv")
