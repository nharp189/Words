### set directory ###
setwd("~/Documents/Nick-Grad/Neta_Lab/words/")

### load packages ###
library(readbulk)
library(lme4)
library(lavaan)

### read in ipanat data ###
data <- read_bulk("data/ipanat/", fun = read_csv, 
                           extension = ".csv")

### label which non-word ###
{
data[grepl("2nua", data$File), "NonWord"] <- "BELNI"
data[grepl("mie5", data$File), "NonWord"] <- "SAMFE"
data[grepl("k9e2", data$File), "NonWord"] <- "SUKOV"
data[grepl("l69s", data$File), "NonWord"] <- "TALEP"
data[grepl("za7g", data$File), "NonWord"] <- "TUNBA"
data[grepl("zm13", data$File), "NonWord"] <- "VIKES"
}

### grab useful cols ###
data <- data[, c("Participant.Public.ID", "Question.Key",
                 "Response", "NonWord")]

### drop bad rows ###
data <- data[!grepl("QUESTIONNAIRE", data$Question.Key),]

data <- data[grepl("quantised", data$Question.Key), ]

### make wide ###
data <- spread(data, key = Question.Key, value = Response)

### make numeric ###
for(i in 3:ncol(data)) {
  data[, i] <- as.numeric(data[, i])
}

### score positive and negative affect measures ###
data$POS <- rowMeans(data[, c("cheerful-quantised",
                              "energetic-quantised",
                              "happy-quantised")])

data$NEG <- rowMeans(data[, c("helpless-quantised",
                              "inhibited-quantised",
                              "tense-quantised")])

### drop subjects that were not kept in analysis ###
### SOURCE 1:539 study1_redo_data_cleaning.R ###
data <- subset(data, data$Participant.Public.ID %in% full$Participant.Public.ID)

### now average positive and negative affect ###
data <- ddply(data, "Participant.Public.ID", summarise,
              POS = mean(POS, na.rm = T),
              NEG = mean(NEG, na.rm = T))


full.new <- merge(full, data[, c("Participant.Public.ID",
                     "POS", "NEG")], by = "Participant.Public.ID")

summary(lm(POS ~ sur_rate + amb_rate + amw_rate, full.new))
summary(lm(NEG ~ sur_rate + amb_rate + amw_rate, full.new))
summary(lm(IPANAT.comb ~ sur_rate + amb_rate + amw_rate, full.new))

full.new$IPANAT.comb <- full.new$NEG - full.new$POS

cor.test(full.new$IPANAT.comb, full.new$sur_rate, method = "spearman")

plot(full.new$IPANAT.comb, full.new$amw_rate)
shapiro.test(full.new$IPANAT.comb)

cor(full.new[, c("sur_rate", "amb_rate", "amw_rate", "IPANAT.comb")])

myModel <- ' # regressions
             IPANAT.comb ~ f1

             # latent variable definitions 
               f1 =~ amb_rate + sur_rate + amw_rate
           '
fit <- sem(myModel, data = full.new)
summary(fit, standardized = T, fit.measures =T)

cfa <- '
### latent variable definitions ###
f1 =~ amb_rate + sur_rate + amw_rate'
fit <- cfa(cfa, data = full.new,
           std.lv=TRUE,  
           missing="fiml")
summary(fit, standardized = T, fit.measures = T)

