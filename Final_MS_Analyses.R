nhpath <- '~/Documents/Nick-Grad/Neta_Lab/Words/'
path = nhpath
setwd(path)

### Pilot Study ###
{library(emmeans)
library(papaja)
library(readr)
library(tidyverse)
library(utils)
library(lme4)
library(lmerTest)
library(lsr)}

############################ Demographics ###########################
## Demographic Questionnaire and Screener Questions
demog <- read_csv('data/pilot_data_20190719/data_exp_8700-v20_questionnaire-rok5.csv')
unique(demog$`Participant Private ID`)
### pick the cool colomns ###
demog <- demog[, c("Participant Private ID", "Question Key","Response")]

### separate into 3 data frames
demog_age<-demog[(demog$`Question Key` == "age"),]
demog_race<-demog[(demog$`Question Key`== "race"),]
demog_sex<-demog[(demog$`Question Key`== "sex"),]

### remove Question Type columns
demog_age <- demog_age[, c("Participant Private ID", "Response")]
demog_race <- demog_race[, c("Participant Private ID", "Response")]
demog_sex <- demog_sex[, c("Participant Private ID", "Response")]

### combine dfs back together
demog <- merge(demog_age,demog_race,by="Participant Private ID")
demog <- merge(demog,demog_sex,by="Participant Private ID")

### rename race and age, make age numeric
names(demog) <- c("Participant Private ID", "age", "race","sex")
demog$age <- as.numeric(demog$age)

### clean workspace ###
rm(demog_age,demog_race,demog_sex)
demog<-demog[!is.na(demog$`Participant Private ID`),]

### subset demog for only the participants who completed the whole thing
pilot_subj <- read_csv("pilot_subjects.csv")
demog <- demog[demog$`Participant Private ID` %in% pilot_subj$c.784848..798597..798586..792099..798585..801388..802507..798607.., ]

### calculations for mean age, race, and sex distributions
plyr::count(demog$sex)

min(demog$age)
max(demog$age)

mean(demog$age)
sd(demog$age)

plyr::count(demog$race)


### read in data ###
words.summary <- read.csv("words.summary.csv")

### make Val a factor ###
words.summary$Val <- as.factor(words.summary$Val)
### multiply % negative rating x 100 ###
words.summary$neg.avg <- words.summary$neg.avg * 100

### recode to pretty names ###
words.summary$Val <- dplyr::recode(words.summary$Val,
                                   "AMB" = "Expected to be Ambiguous",
                                   "NEG" = "Negative (Warriner et al., 2013)",
                                   "POS" = "Positive (Warriner et al., 2013)")
### rather than plotting the words, just do a # to make the x axis prettier ###
words.summary$arbitrary <- c(1:nrow(words.summary))

### plot the ratings ###
ggplot(words.summary, aes(x = arbitrary, y = neg.avg, color = Val,
                          shape = Val, fill = Val)) +
  scale_fill_manual(values=c("#00D120", "#FF2D00", "#006BE3"),
                    labels=c("Expected to\nbe Ambiguous\n", "Coded 'Negative'\nby Warriner et al.\n(2013)\n",
                             "Coded 'Positive'\nby Warriner et al.\n(2013)\n")) +
  scale_color_manual(values=c('#00D120','#FF2D00', '#006BE3'),
                     labels=c("Expected to\nbe Ambiguous\n", "Coded 'Negative'\nby Warriner et al.\n(2013)\n",
                              "Coded 'Positive'\nby Warriner et al.\n(2013)\n")) +
  scale_shape_manual(values = c(1, 2, 0),
                     labels=c("Expected to\nbe Ambiguous\n", "Coded 'Negative'\nby Warriner et al.\n(2013)\n",
                              "Coded 'Positive'\nby Warriner et al.\n(2013)\n")) +
  ylab("Percent Negative Ratings") +
  xlab("Words") +
  geom_point(size = 3) +
  labs(color = "Valence", shape = "Valence", fill = "Valence") +
  scale_x_discrete(name = "Words",
                   limits = c(1, 50, 100, 150, 200,
                              250, 300, 350, 400, 450,
                              500, 550, 600, 629)) +
  theme(axis.title.x = element_text(size = 25),
        axis.text.x.bottom = element_text(size = 15,
                                          angle = 45, hjust = 1),
        panel.background = element_blank(),
        axis.text.y = element_text(size = 25),
        axis.title.y = element_text(size = 20),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 25)) #,
        #legend.position = "none")

### plot the RT ###
ggplot(words.summary, aes(x = arbitrary, y = RT, color = Val,
                          shape = Val)) +
  scale_color_manual(values=c('#00D120','#FF2D00', '#006BE3')) +
  ylab("Reaction Time (ms)") +
  geom_point(size = 3) +
  labs(color = "Valence", shape = "Valence") +
  geom_hline(yintercept = 875) +
  scale_x_discrete(name = "Words",
                   limits = c(1, 50, 100, 150, 200,
                              250, 300, 350, 400, 450,
                              500, 550, 600, 629)) +
  theme(axis.title.x = element_text(size = 25),
        axis.text.x.bottom = element_text(size = 15,
                                          angle = 45, hjust = 1),
        panel.background = element_blank(),
        axis.text.y = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        legend.text = element_text(size = 15),
        legend.title = element_text(size = 25),
        legend.position = "none")

### select words ###
pos <- subset(words.summary, words.summary$neg.avg <= 25)
pos <- subset(pos, !(pos$wordlist == "POSITIVE"))
pos <- pos[order(pos$RT), ]
### count the lowest 16 RTs ###
pos$count <- 1:nrow(pos)
pos <- subset(pos, pos$count %in% c(1:16))

neg <- subset(words.summary, words.summary$neg.avg >= 75)
neg <- subset(neg, !(neg$wordlist == "NEGATIVE"))
neg <- neg[order(neg$RT), ]
### count the lowest 16 RTs ###
neg$count <- 1:nrow(neg)
neg <- subset(neg, neg$count %in% c(1:16))

amb <- subset(words.summary, ((words.summary$neg.avg > 25 & words.summary$neg.avg < 75) &
                                words.summary$RT >= 875))
#setdiff(amb$wordlist, amb.2.5sd$wordlist)
amb <- subset(amb, !(amb$wordlist %in% c("ABUNDANT", "INHERIT", "FACELESS", "HEADSTONE",
              "COURTROOM", "COSMIC", "RECEIVE", "RECESSION")))
amb$count <- 1:nrow(amb)
final.words <- rbind(neg, pos, amb)

### variances equal ###
var.test(subset(final.words, final.words$wordlist %in% pos$wordlist | final.words$wordlist %in% neg$wordlist)$Length,
         subset(final.words, final.words$wordlist %in% amb$wordlist)$Length)
t.test(subset(final.words, final.words$wordlist %in% pos$wordlist | final.words$wordlist %in% neg$wordlist)$Length,
       subset(final.words, final.words$wordlist %in% amb$wordlist)$Length, paired = F, var.equal = T)
cohensD(subset(final.words, final.words$wordlist %in% pos$wordlist | final.words$wordlist %in% neg$wordlist)$Length,
        subset(final.words, final.words$wordlist %in% amb$wordlist)$Length)

mean(subset(final.words, final.words$wordlist %in% amb$wordlist)$Length)
sd(subset(final.words, final.words$wordlist %in% amb$wordlist)$Length)

mean(subset(final.words, final.words$wordlist %in% pos$wordlist | final.words$wordlist %in% neg$wordlist)$Length)
sd(subset(final.words, final.words$wordlist %in% pos$wordlist | final.words$wordlist %in% neg$wordlist)$Length)

### variances unequal ###
var.test(subset(final.words, final.words$wordlist %in% pos$wordlist | final.words$wordlist %in% neg$wordlist)$RT,
         subset(final.words, final.words$wordlist %in% amb$wordlist)$RT)
t.test(subset(final.words, final.words$wordlist %in% pos$wordlist | final.words$wordlist %in% neg$wordlist)$RT,
       subset(final.words, final.words$wordlist %in% amb$wordlist)$RT, paired = F, var.equal = F)
cohensD(subset(final.words, final.words$wordlist %in% pos$wordlist | final.words$wordlist %in% neg$wordlist)$RT,
        subset(final.words, final.words$wordlist %in% amb$wordlist)$RT)

mean(subset(final.words, final.words$wordlist %in% amb$wordlist)$RT)
sd(subset(final.words, final.words$wordlist %in% amb$wordlist)$RT)

mean(subset(final.words, final.words$wordlist %in% pos$wordlist | final.words$wordlist %in% neg$wordlist)$RT)
sd(subset(final.words, final.words$wordlist %in% pos$wordlist | final.words$wordlist %in% neg$wordlist)$RT)


### variances equal ###
var.test(subset(final.words, final.words$wordlist %in% pos$wordlist | final.words$wordlist %in% neg$wordlist)$Freq_HAL,
         subset(final.words, final.words$wordlist %in% amb$wordlist)$Freq_HAL)
t.test(subset(final.words, final.words$wordlist %in% pos$wordlist | final.words$wordlist %in% neg$wordlist)$Freq_HAL,
       subset(final.words, final.words$wordlist %in% amb$wordlist)$Freq_HAL, paired = F, var.equal = T)
cohensD(subset(final.words, final.words$wordlist %in% pos$wordlist | final.words$wordlist %in% neg$wordlist)$Freq_HAL,
        subset(final.words, final.words$wordlist %in% amb$wordlist)$Freq_HAL)

mean(subset(final.words, final.words$wordlist %in% amb$wordlist)$Freq_HAL)
sd(subset(final.words, final.words$wordlist %in% amb$wordlist)$Freq_HAL)

mean(subset(final.words, final.words$wordlist %in% pos$wordlist | final.words$wordlist %in% neg$wordlist)$Freq_HAL)
sd(subset(final.words, final.words$wordlist %in% pos$wordlist | final.words$wordlist %in% neg$wordlist)$Freq_HAL)


### Study 1 ###

############################ Methods ###########################
## Demographic Questionnaire and Screener Questions
#run study1_redo_data_cleaning.R to line 384 and then make v2_data.summary skipping some lines.. 
full <- read_csv("data/study1_redo_data/words_study1_data_persubj2Trim.csv")
full$age <- as.numeric(full$age)

### count sex ###
plyr::count(full$sex)

### age descriptives ###
min(full$age)
max(full$age)
mean(full$age)
sd(full$age)

plyr::count(full$race)

########################   Results #############################
### Manipulation Check ###
### Response Matrix ###
### drop unnecessary columns ###
temp <- read.csv("data/study1_redo_data/Resp_Matrix.csv")
temp <- temp[, c(1, 3, 41:200)]
Resp.matrix <- temp
Resp.matrix <- Resp.matrix[, c(3:162)]
Resp.matrix <- as.data.frame(t(Resp.matrix))
Resp.matrix$stimMeans <- rowMeans(Resp.matrix, na.rm = T)
#Resp.matrix$stimSDs <- apply(Resp.matrix[,1:6],1,sd)
Resp.matrix <- transform(Resp.matrix, SD=apply(Resp.matrix,1, sd, na.rm = TRUE))

### RT matrix ###
RT_Matrix <- read.csv("data/study1_redo_data/RT_Matrix.csv")
RT_Matrix <- RT_Matrix[, c(1, 3, 41:200)]
RT_Matrix <- RT_Matrix[, c(3:162)]
RT_Matrix <- as.data.frame(t(RT_Matrix))
RT_Matrix$stimMeans <- rowMeans(RT_Matrix, na.rm = T)
RT_Matrix$stimSDs <- apply(RT_Matrix[,1:229],1,sd)
RT_Matrix <- transform(RT_Matrix, SD=apply(RT_Matrix,1, sd, na.rm = TRUE))

### Make matrices for each category ###
AmbWordResponseMatrix <- Resp.matrix[grep("ambiguous_WORD", rownames(Resp.matrix)), ]
ClearWordResponseMatrix <- Resp.matrix[grep("positive_WORD", rownames(Resp.matrix)), ]
ClearWordResponseMatrix <- rbind(ClearWordResponseMatrix,
                                 Resp.matrix[grep("negative_WORD", rownames(Resp.matrix)), ])

mean(AmbWordResponseMatrix$SD)
sd(AmbWordResponseMatrix$SD)
mean(ClearWordResponseMatrix$SD)
sd(ClearWordResponseMatrix$SD)

### compare ###
t.test(AmbWordResponseMatrix$SD, ClearWordResponseMatrix$SD, paired = T)
cohensD(AmbWordResponseMatrix$SD, ClearWordResponseMatrix$SD)
### RT Matrix ###
AmbWordRT_Matrix <- RT_Matrix[grep("ambiguous_WORD", rownames(RT_Matrix)), ]
ClearWordRT_Matrix <- RT_Matrix[grep("positive_WORD", rownames(RT_Matrix)), ]
ClearWordRT_Matrix <- rbind(ClearWordRT_Matrix,
                            RT_Matrix[grep("negative_WORD", rownames(RT_Matrix)), ])

#ClearWordRT_Matrix$stimSDs <- replace_na(ClearWordRT_Matrix$stimSDs, 0)
mean(AmbWordRT_Matrix$stimMeans, na.rm = F)
sd(AmbWordRT_Matrix$stimMeans)
mean(ClearWordRT_Matrix$stimMeans, na.rm = F)
sd(ClearWordRT_Matrix$stimMeans)

t.test(AmbWordRT_Matrix$stimMeans, ClearWordRT_Matrix$stimMeans, paired = T)
cohensD(AmbWordRT_Matrix$stimMeans, ClearWordRT_Matrix$stimMeans)



### Valence Ratings ###

### set contrasts ###
options(contrasts = c("contr.sum","contr.poly"))

### make data long ###
aov.bias.data <- gather(full, Condition, PerNeg,
                        ang_rate, hap_rate, sur_rate,
                        neg_rate, pos_rate, amb_rate,
                        new_rate, pow_rate, amw_rate)

aov.bias.data$Val <- dplyr::recode(aov.bias.data$Condition,
                                   "ang_rate" = "Negative",
                                   "neg_rate" = "Negative",
                                   "new_rate" = "Negative",
                                   "hap_rate" = "Positive",
                                   "pos_rate" = "Positive",
                                   "pow_rate" = "Positive",
                                   "sur_rate" = "Ambiguous",
                                   "amb_rate" = "Ambiguous",
                                   "amw_rate" = "Ambiguous")
aov.bias.data$Stim <- dplyr::recode(aov.bias.data$Condition,
                                    "ang_rate" = "Faces",
                                    "neg_rate" = "Scenes",
                                    "new_rate" = "Words",
                                    "hap_rate" = "Faces",
                                    "pos_rate" = "Scenes",
                                    "pow_rate" = "Words",
                                    "sur_rate" = "Faces",
                                    "amb_rate" = "Scenes",
                                    "amw_rate" = "Words")

aov.bias.data$PerNeg <- aov.bias.data$PerNeg * 100

# summary(aov.model <- aov(PerNeg ~ Val * Stim + Error(Participant.Public.ID/(Val * Stim)), 
#             data=aov.bias.data))

lmer.model <- lmer(PerNeg ~ Val * Stim + (1 | `Participant.Public.ID`) +
                     (1 | `Participant.Public.ID`:Val) + (1 | `Participant.Public.ID`:Stim),
                   aov.bias.data,
                   REML = F)

### model summary ###
anova(lmer.model)
summary(lmer.model)

### estimated marginal means for Valence ###
emmeans(lmer.model, pairwise ~ Val, adjust = "none")

### descriptives ###
mean(aov.bias.data[which(aov.bias.data$Val == "Negative"),]$PerNeg, na.rm = T)
sd(aov.bias.data[which(aov.bias.data$Val == "Negative"),]$PerNeg, na.rm = T)

mean(aov.bias.data[which(aov.bias.data$Val == "Ambiguous"),]$PerNeg, na.rm = T)
sd(aov.bias.data[which(aov.bias.data$Val == "Ambiguous"),]$PerNeg, na.rm = T)

mean(aov.bias.data[which(aov.bias.data$Val == "Positive"),]$PerNeg, na.rm = T)
sd(aov.bias.data[which(aov.bias.data$Val == "Positive"),]$PerNeg, na.rm = T)

### estimated marginal means for Stimulus ###
emmeans(lmer.model, pairwise ~ Stim, adjust = "none")

### estimated marginal means for interaction ###
emmeans(lmer.model, pairwise ~ Val*Stim, adjust = "none")


### REACTION TIME ###
### make data long ###
aov.RT.data <- gather(full, Condition, RT,
                      ang_rt, hap_rt, sur_rt,
                      neg_rt, pos_rt, amb_rt,
                      new_rt, pow_rt, amw_rt)

aov.RT.data$Val <- dplyr::recode(aov.RT.data$Condition,
                                 "ang_rt" = "Negative",
                                 "neg_rt" = "Negative",
                                 "new_rt" = "Negative",
                                 "hap_rt" = "Positive",
                                 "pos_rt" = "Positive",
                                 "pow_rt" = "Positive",
                                 "sur_rt" = "Ambiguous",
                                 "amb_rt" = "Ambiguous",
                                 "amw_rt" = "Ambiguous")
aov.RT.data$Stim <- dplyr::recode(aov.RT.data$Condition,
                                  "ang_rt" = "Faces",
                                  "neg_rt" = "Scenes",
                                  "new_rt" = "Words",
                                  "hap_rt" = "Faces",
                                  "pos_rt" = "Scenes",
                                  "pow_rt" = "Words",
                                  "sur_rt" = "Faces",
                                  "amb_rt" = "Scenes",
                                  "amw_rt" = "Words")

# summary(aov.RT.model <- aov(RT ~ Val * Stim + Error(Participant.Public.ID/(Val * Stim)), 
#                          data=aov.RT.data))

lmer.RT.model <- lmer(RT ~ Val * Stim + (1 | `Participant.Public.ID`) +
                        (1 | `Participant.Public.ID`:Val) + (1 | `Participant.Public.ID`:Stim),
                      aov.RT.data,
                      REML = F)

### model summary ###
anova(lmer.RT.model)
summary(lmer.RT.model)
 
### estimated marginal means for Valence ###
emmeans(lmer.RT.model, pairwise ~ Val, adjust = "none")

### estimated marginal means for Stimulus ###
emmeans(lmer.RT.model, pairwise ~ Stim, adjust = "none")

### estimated marginal means for Interaction ###
emmeans(lmer.RT.model, pairwise ~ Val:Stim, adjust = "none")

### Comparing Valence Bias Across Stimulus Categories ###
shapiro.test(full$sur_rate) # non-normal
shapiro.test(full$amb_rate) # normal
shapiro.test(full$amw_rate) # normal 

### recode sex ###
full$mal0fem1 <- recode(full$sex,
                        "Female" = 1,
                        "Male" = 0)
# ### correlations controlling for age and sex ###1
face_scene <- full
face_scene <- subset(face_scene, !(is.na(face_scene$sur_rate)))
face_scene <- subset(face_scene, !(is.na(face_scene$amb_rate)))

pcor.test(face_scene$sur_rate, face_scene$amb_rate, c(face_scene$age, face_scene$mal0fem1), method = "spearman")

face_word <- full
face_word <- subset(face_word, !(is.na(face_word$sur_rate)))
face_word <- subset(face_word, !(is.na(face_word$amw_rate)))
pcor.test(face_word$sur_rate, face_word$amw_rate, c(face_word$age, face_word$mal0fem1), method = "spearman")

word_scene <- full
word_scene <- subset(word_scene, !(is.na(word_scene$amb_rate)))
word_scene <- subset(word_scene, !(is.na(word_scene$amw_rate)))
pcor.test(word_scene$amb_rate, word_scene$amw_rate, c(word_scene$age, word_scene$mal0fem1))

### get residuals ###
face_scene$sur_rate_Resid <- lm(sur_rate ~ age + sex, face_scene)$residuals
face_scene$amb_rate_Resid <- lm(amb_rate ~ age + sex, face_scene)$residuals

face_word$sur_rate_Resid <- lm(sur_rate ~ age + sex, face_word)$residuals
face_word$amw_rate_Resid <- lm(amw_rate ~ age + sex, face_word)$residuals

word_scene$amb_rate_Resid <- lm(amb_rate ~ age + sex, word_scene)$residuals
word_scene$amw_rate_Resid <- lm(amw_rate ~ age + sex, word_scene)$residuals

### plot ###
ggplot(face_scene, aes(x=sur_rate_Resid, y = amb_rate_Resid))+
  geom_point()+
  xlab("Ambiguous Faces (Residuals)")+
  ylab("Ambiguous Scenes (Residuals)")+
  geom_smooth(method="lm", color = "green") +
  theme(axis.title.x = element_text(size = 60),
        axis.text.x.bottom = element_text(size = 60),
        panel.background = element_blank(),
        axis.text.y = element_text(size = 60),
        axis.title.y = element_text(size = 60),
        legend.text = element_text(size = 60)) +
  theme_apa(base_size = 20)

ggplot(face_word, aes(x=sur_rate_Resid, y = amw_rate_Resid))+
  geom_point()+
  xlab("Ambiguous Faces (Residuals)")+
  ylab("Ambiguous Words (Residuals)")+
  geom_smooth(method="lm", color = "green") +
  theme(axis.title.x = element_text(size = 12),
        axis.text.x.bottom = element_text(size = 15),
        panel.background = element_blank(),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 12),
        legend.text = element_text(size = 15)) +
  theme_apa(base_size = 20)

ggplot(word_scene, aes(x=amb_rate_Resid, y = amw_rate_Resid))+
  geom_point()+
  xlab("Ambiguous Scenes (Residuals)")+
  ylab("Ambiguous Words (Residuals)")+
  geom_smooth(method="lm", color = "green") +
  theme(axis.title.x = element_text(size = 12),
        axis.text.x.bottom = element_text(size = 15),
        panel.background = element_blank(),
        axis.text.y = element_text(size = 15),
        axis.title.y = element_text(size = 12),
        legend.text = element_text(size = 15)) +
  theme_apa(base_size = 20)
