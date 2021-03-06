######### A few chunks are commented out and replaced with edited versions. 
######### Some folders don't have data in them bc I am checking preliminary 
######### results, so it gives me errors if I don't edit. 



### set wd ###
setwd("~/Downloads/data_exp_10365-v4/")

### load packages ###
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(MASS))
suppressPackageStartupMessages(library(ppcor))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(reshape2))

### set stim lists ###
{
  face1 <- c("AM28ANS.jpg", "AM34SUS.JPG", "08F_SP_O.jpg",
             "36M_HA_O.jpg", "AM06SUS.JPG", "27M_SP_O.jpg",
             "03F_HA_O.jpg", "AM12SUS.JPG", "37M_AN_C.jpg",
             "06F_SP_O.jpg", "08F_AN_C.jpg", "07F_SP_O.jpg", 
             "AF34SUS.JPG", "01F_AN_C.jpg", "AF06HAS.jpg",
             "AM01HAS.jpg", "20M_AN_C.jpg", "AF13SUS.JPG",
             "20M_SP_O.jpg", "24M_HA_O.jpg", "09F_AN_C.jpg",
             "AF03SUS.JPG", "01F_SP_O.jpg", "AF08HAS.jpg")
  face2 <- c("AM10ANS.jpg", "01F_HA_O.jpg", "AM14HAS.jpg",
             "AF02SUS.JPG", "03F_AN_C.jpg", "28M_HA_O.jpg",
             "AF14ANS.jpg", "AF30SUS.JPG", "09F_SP_O.jpg",
             "AF01SUS.JPG", "02F_SP_O.jpg", "36M_AN_C.jpg",
             "AM18SUS.JPG", "AM35SUS.JPG", "28M_AN_C.jpg",
             "08F_HA_O.jpg", "24M_SP_O.jpg","07F_HA_O.jpg",
             "37M_HA_O.jpg", "36M_SP_O.jpg", "28M_SP_O.jpg",
             "AM13SUS.JPG", "23M_SP_O.jpg", "AF07ANS.jpg")
  words1 <- c("Slide34.jpeg", "Slide8.jpeg", "Slide15.jpeg",
              "Slide28.jpeg", "Slide18.jpeg", "Slide25.jpeg",
              "Slide19.jpeg", "Slide10.jpeg", "Slide29.jpeg",
              "Slide5.jpeg", "Slide30.jpeg", "Slide31.jpeg",
              "Slide23.jpeg", "Slide20.jpeg", "Slide9.jpeg",
              "Slide24.jpeg", "Slide6.jpeg", "Slide13.jpeg",
              "Slide35.jpeg", "Slide32.jpeg", "Slide11.jpeg",
              "Slide16.jpeg", "Slide17.jpeg", "Slide26.jpeg",
              "Slide22.jpeg", "Slide12.jpeg", "Slide33.jpeg",
              "Slide7.jpeg", "Slide21.jpeg", "Slide36.jpeg", 
              "Slide27.jpeg", "Slide14.jpeg")
  words2 <- c("Slide53.jpeg", "Slide37.jpeg", "Slide49.jpeg",
              "Slide67.jpeg", "Slide65.jpeg", "Slide57.jpeg",
              "Slide46.jpeg", "Slide47.jpeg", "Slide50.jpeg",
              "Slide39.jpeg", "Slide68.jpeg", "Slide48.jpeg",
              "Slide61.jpeg", "Slide44.jpeg", "Slide51.jpeg",
              "Slide41.jpeg", "Slide52.jpeg", "Slide54.jpeg",
              "Slide56.jpeg", "Slide55.jpeg", "Slide38.jpeg",
              "Slide45.jpeg", "Slide59.jpeg", "Slide66.jpeg",
              "Slide60.jpeg", "Slide62.jpeg", "Slide58.jpeg",
              "Slide40.jpeg", "Slide63.jpeg", "Slide42.jpeg",
              "Slide64.jpeg", "Slide43.jpeg")
  iaps1 <- c("8380.jpg", "7460.jpg", "2340.jpg",
             "5825.jpg", "4598.jpg", "2550.jpg",
             "9830.jpg", "3211.jpg", "8060.jpg",
             "2057.jpg", "2704.jpg", "8466.jpg",
             "7590.jpg", "9910.jpg", "2480.jpg",
             "9185.jpg", "9561.jpg", "3360.jpg",
             "1460.jpg", "7380.jpg", "6837.jpg",
             "9424.jpg", "7570.jpg", "3310.jpg")
  iaps2 <- c("2717.jpg", "9050.jpg", "9220.jpg",
             "9295.jpg", "9421.jpg", "9432.jpg",
             "1440.jpg", "1441.jpg", "1710.jpg",
             "2091.jpg", "2347.jpg", "7502.jpg",
             "1030.jpg", "1303.jpg", "1560.jpg",
             "2339.jpg", "2485.jpg", "2688.jpg",
             "4233.jpg", "6410.jpg", "7430.jpg",
             "7620.jpg", "8010.jpg", "8501.jpg")
}

###################################################

#### read in the demographics data ###
v4_demo <- read.csv("data_exp_10365-v4_questionnaire-rok5.csv")

### pull relevant columns ###
v4_demo <- v4_demo[, c("Participant.Public.ID", "sex", "age")]

### read in the task data ###
v4_dhn3 <- read.csv("data_exp_10365-v4_task-dhn3.csv")
v4_uyls <- read.csv("data_exp_10365-v4_task-uyls.csv")
v4_7sqx <- read.csv("data_exp_10365-v4_task-7sqx.csv")
v4_6nyj <- read.csv("data_exp_10365-v4_task-6nyj.csv")
v4_5unm <- read.csv("data_exp_10365-v4_task-5unm.csv")
v4_b5b7 <- read.csv("data_exp_10365-v4_task-b5b7.csv")
v4_xsf9 <- read.csv("data_exp_10365-v4_task-xsf9.csv")
v4_tahj <- read.csv("data_exp_10365-v4_task-tahj.csv")
v4_jpit <- read.csv("data_exp_10365-v4_task-jpit.csv")
v4_u5dg <- read.csv("data_exp_10365-v4_task-u5dg.csv")
v4_rugu <- read.csv("data_exp_10365-v4_task-rugu.csv")
v4_myk4 <- read.csv("data_exp_10365-v4_task-myk4.csv")
v4_pdjg <- read.csv("data_exp_10365-v4_task-pdjg.csv")
v4_nq9b <- read.csv("data_exp_10365-v4_task-nq9b.csv")
v4_tazw <- read.csv("data_exp_10365-v4_task-tazw.csv")
v4_y89e <- read.csv("data_exp_10365-v4_task-y89e.csv")
v4_f3ii <- read.csv("data_exp_10365-v4_task-f3ii.csv")
v4_71u8 <- read.csv("data_exp_10365-v4_task-71u8.csv")
v4_7f4u <- read.csv("data_exp_10365-v4_task-7f4u.csv")
v4_b86o <- read.csv("data_exp_10365-v4_task-b86o.csv")
v4_udom <- read.csv("data_exp_10365-v4_task-udom.csv")
v4_x6ib <- read.csv("data_exp_10365-v4_task-x6ib.csv")
v4_shzk <- read.csv("data_exp_10365-v4_task-shzk.csv")
v4_dko9 <- read.csv("data_exp_10365-v4_task-dko9.csv")

v4_data <- rbind(v4_dhn3, v4_uyls, v4_7sqx,
                 v4_6nyj, v4_5unm, v4_b5b7,
                 v4_xsf9, v4_tahj, v4_jpit,
                 v4_u5dg,
                 v4_pdjg, v4_nq9b, v4_tazw,
                 v4_y89e, v4_f3ii, v4_71u8,
                 v4_7f4u, v4_b86o, v4_udom,
                 v4_x6ib, v4_shzk, v4_dko9)

# v4_data <- rbind(v4_dhn3, v4_uyls, v4_7sqx,
#                  v4_6nyj, v4_5unm, v4_b5b7,
#                  v4_xsf9, v4_tahj, v4_jpit,
#                  v4_u5dg, v4_rugu, v4_myk4,
#                  v4_pdjg, v4_nq9b, v4_tazw,
#                  v4_y89e, v4_f3ii, v4_71u8,
#                  v4_7f4u, v4_b86o, v4_udom,
#                  v4_x6ib, v4_shzk, v4_dko9)

### clean workspace ###

rm(v4_dhn3, v4_uyls, v4_7sqx,
   v4_6nyj, v4_5unm, v4_b5b7,
   v4_xsf9, v4_tahj, v4_jpit,
   v4_u5dg,
   v4_pdjg, v4_nq9b, v4_tazw,
   v4_y89e, v4_f3ii, v4_71u8,
   v4_7f4u, v4_b86o, v4_udom,
   v4_x6ib, v4_shzk, v4_dko9)

# rm(v4_dhn3, v4_uyls, v4_7sqx,
#    v4_6nyj, v4_5unm, v4_b5b7,
#    v4_xsf9, v4_tahj, v4_jpit,
#    v4_u5dg, v4_rugu, v4_myk4,
#    v4_pdjg, v4_nq9b, v4_tazw,
#    v4_y89e, v4_f3ii, v4_71u8,
#    v4_7f4u, v4_b86o, v4_udom,
#    v4_x6ib, v4_shzk, v4_dko9)

v4_data <- v4_data[, c("Event.Index", "Participant.Public.ID",
                       "randomiser.jf65", "randomiser.2tba",
                       "counterbalance.93ib", "counterbalance.m678",
                       "counterbalance.9xcr", "counterbalance.wbyz",
                       "counterbalance.vply", "counterbalance.hleo",
                       "counterbalance.3v4j", "counterbalance.ulkf",
                       "counterbalance.bi4u", "counterbalance.dozd",
                       "counterbalance.g69h", "counterbalance.8ump",
                       "counterbalance.wch9", "counterbalance.2v12",
                       "counterbalance.mcio", "counterbalance.eu1d",
                       "counterbalance.s1b5", "counterbalance.u2fx",
                       "counterbalance.sg3w", "counterbalance.t9rn",
                       "counterbalance.7mrq", "counterbalance.8zsd",
                       "counterbalance.e4l7", "counterbalance.o3nf",
                       "order.j7mk", "Screen.Name", "Reaction.Time", "Response",
                       "clearval", "Attempt", "Metadata", "order1", "order2",
                       "order3", "order4", "order5", "order6", "order7",
                       "order8", "order9", "order10", "order11", "order12")]

v4_data <- v4_data[!(v4_data$Event.Index == "END OF FILE"), ]

### set up the stim list based on order assignment ###
v4_data_ord1 <- subset(v4_data, counterbalance.m678 == "order1" | 
                         counterbalance.bi4u == "order1" |
                         counterbalance.dozd == "order1" |
                         counterbalance.g69h == "order1")
v4_data_ord1$stim.pres <- v4_data_ord1$order1

v4_data_ord2 <- subset(v4_data, counterbalance.93ib == "order2" |
                         counterbalance.8ump == "order2" |
                         counterbalance.wch9 == "order2" |
                         counterbalance.2v12 == "order2")
v4_data_ord2$stim.pres <- v4_data_ord2$order2

v4_data_ord3 <- subset(v4_data, counterbalance.m678 == "order3" |
                         counterbalance.bi4u == "order3" |
                         counterbalance.dozd == "order3" |
                         counterbalance.g69h == "order3")
v4_data_ord3$stim.pres <- v4_data_ord3$order3

v4_data_ord4 <- subset(v4_data, counterbalance.93ib == "order4" |
                         counterbalance.8ump == "order4" |
                         counterbalance.wch9 == "order4" |
                         counterbalance.2v12 == "order4")
v4_data_ord4$stim.pres <- v4_data_ord4$order4

v4_data_ord5 <- subset(v4_data, counterbalance.9xcr == "order5" |
                         counterbalance.mcio == "order5" |
                         counterbalance.eu1d == "order5" |
                         counterbalance.s1b5 == "order5")
v4_data_ord5$stim.pres <- v4_data_ord5$order5

v4_data_ord6 <- subset(v4_data, counterbalance.9xcr == "order6" |
                         counterbalance.mcio == "order6" |
                         counterbalance.eu1d == "order6" |
                         counterbalance.s1b5 == "order6")
v4_data_ord6$stim.pres <- v4_data_ord6$order6

v4_data_ord7 <- subset(v4_data, counterbalance.wbyz == "order7" |
                         counterbalance.ulkf == "order7" |
                         counterbalance.u2fx == "order7" |
                         counterbalance.sg3w == "order7")
v4_data_ord7$stim.pres <- v4_data_ord7$order7

v4_data_ord8 <- subset(v4_data, counterbalance.vply == "order8" |
                         counterbalance.t9rn == "order8" |
                         counterbalance.7mrq == "order8" |
                         counterbalance.8zsd == "order8")
v4_data_ord8$stim.pres <- v4_data_ord8$order8

v4_data_ord9 <- subset(v4_data, counterbalance.wbyz == "order9" |
                         counterbalance.ulkf == "order9" |
                         counterbalance.u2fx == "order9" |
                         counterbalance.sg3w == "order9")
v4_data_ord9$stim.pres <- v4_data_ord9$order9

v4_data_ord10 <- subset(v4_data, counterbalance.vply == "order10" |
                          counterbalance.t9rn == "order10" |
                          counterbalance.7mrq == "order10" |
                          counterbalance.8zsd == "order10")
v4_data_ord10$stim.pres <- v4_data_ord10$order10

v4_data_ord11 <- subset(v4_data, counterbalance.hleo == "order11" |
                          counterbalance.e4l7 == "order11" |
                          counterbalance.o3nf == "order11")
v4_data_ord11$stim.pres <- v4_data_ord11$order11

v4_data_ord12 <- subset(v4_data, counterbalance.hleo == "order12" |
                          counterbalance.e4l7 == "order12" |
                          counterbalance.o3nf == "order12")
v4_data_ord12$stim.pres <- v4_data_ord12$order12

# v4_data_ord11 <- subset(v4_data, counterbalance.hleo == "order11" |
#                           counterbalance.3v4j == "order11" |
#                           counterbalance.e4l7 == "order11" |
#                           counterbalance.o3nf == "order11")
# v4_data_ord11$stim.pres <- v4_data_ord11$order11
# 
# v4_data_ord12 <- subset(v4_data, counterbalance.hleo == "order12" |
#                           counterbalance.3v4j == "order12" |
#                           counterbalance.e4l7 == "order12" |
#                           counterbalance.o3nf == "order12")
# v4_data_ord12$stim.pres <- v4_data_ord12$order12

### merge back together ###
v4_data <- rbind(v4_data_ord1, v4_data_ord2,
                 v4_data_ord3, v4_data_ord4,
                 v4_data_ord5, v4_data_ord6,
                 v4_data_ord7, v4_data_ord8,
                 v4_data_ord9, v4_data_ord10,
                 v4_data_ord11, v4_data_ord12)

remove(v4_data_ord1,v4_data_ord2,v4_data_ord3,v4_data_ord4,v4_data_ord5,
       v4_data_ord6,v4_data_ord7,v4_data_ord8,v4_data_ord9,v4_data_ord10,
       v4_data_ord11,v4_data_ord12)

### drop order# columns for only one stim column ###
drop <- c("order1", "order2","order3", "order4", "order5", "order6", "order7",
         "order8", "order9", "order10", "order11", "order12")

### drop unnecessary columns and the duplicate rows ###
### these contain NA's in stim pres ###
v4_data <- v4_data[ , !(names(v4_data) %in% drop)]
v4_data <- v4_data[!is.na(v4_data$stim.pres), ]

### add stim type factor label ###
v4_data$stimtype <- ifelse(v4_data$stim.pres %in% c(face1, face2), "FACE",
                                  ifelse(v4_data$stim.pres %in% c(words1, words2), "WORD",
                                         ifelse(v4_data$stim.pres %in% c(iaps1, iaps2), "IAPS", "")))

### drop trials w/ responses in less than 200ms ###
v4_data$Reaction.Time <- as.numeric(v4_data$Reaction.Time)
v4_data$RT.Outl <- ifelse((v4_data$Reaction.Time <= 200 & v4_data$Screen.Name == "stim"), 1, 0)
outliers <- subset(v4_data, RT.Outl == 1)
v4_data <- subset(v4_data, RT.Outl == 0)
table(outliers$Participant.Public.ID)

### keep first response only ###
v4_data$flag <- ifelse(v4_data$Attempt >= 2, 1, 0)

v4_data <- subset(v4_data, flag == 0)

### count number of trials for each participant ###
table(v4_data$Participant.Public.ID)

### make "positive" 0 and "negative" 1 ###
v4_data$rate <- recode(v4_data$Response,
                       "positive" = 0,
                       "negative" = 1)

v4_data.summary <- (ddply(v4_data, "Participant.Public.ID", summarise, 
                          sur_rate = mean(rate[which(stimtype == "FACE" & clearval == "ambiguous")], na.rm = TRUE),
                          hap_rate = mean(rate[which(stimtype == "FACE" & clearval == "positive")], na.rm = TRUE),
                          ang_rate = mean(rate[which(stimtype == "FACE" & clearval == "negative")], na.rm = TRUE),
                          amb_rate = mean(rate[which(stimtype == "IAPS" & clearval == "ambiguous")], na.rm = TRUE),
                          pos_rate = mean(rate[which(stimtype == "IAPS" & clearval == "positive")], na.rm = TRUE),
                          neg_rate = mean(rate[which(stimtype == "IAPS" & clearval == "negative")], na.rm = TRUE),
                          amw_rate = mean(rate[which(stimtype == "WORD" & clearval == "ambiguous")], na.rm = TRUE),
                          pow_rate = mean(rate[which(stimtype == "WORD" & clearval == "positive")], na.rm = TRUE),
                          new_rate = mean(rate[which(stimtype == "WORD" & clearval == "negative")], na.rm = TRUE)))

 
# ### make blocks column if you want to check for bad specific blocks ###
# v4_data$block <- ifelse(v4_data$stim.pres %in% face1, "face1",
#                         ifelse(v4_data$stim.pres %in% face2, "face2",
#                                ifelse(v4_data$stim.pres %in% iaps1, "iaps1",
#                                       ifelse(v4_data$stim.pres %in% iaps2, "iaps2",
#                                              ifelse(v4_data$stim.pres %in% words1, "words1",
#                                                     ifelse(v4_data$stim.pres %in% words2, "words2",""))))))
# 
# v4_data.blocksummary <- ddply(v4_data, c("Participant.Public.ID", "clearval", "block"), summarise,
#                           N    = sum(!is.na(rate)),
#                           mean = mean(rate, na.rm = TRUE),
#                           sd   = sd(rate, na.rm = TRUE),
#                           se   = sd / sqrt(N))
# 
# ### count number of bad blocks per participant ###
# v4_data$block <- ifelse(v4_data$stim.pres %in% face1, "face1",
#                         ifelse(v4_data$stim.pres %in% face2, "face2",
#                                ifelse(v4_data$stim.pres %in% iaps1, "iaps1",
#                                       ifelse(v4_data$stim.pres %in% iaps2, "iaps2",
#                                              ifelse(v4_data$stim.pres %in% words1, "words1",
#                                                     ifelse(v4_data$stim.pres %in% words2, "words2",""))))))
# v4_data.blocksummary$badblocks <- ifelse((v4_data.blocksummary$clearval == "negative" & v4_data.blocksummary$mean <= .6),1,
#                                          ifelse((v4_data.blocksummary$clearval == "positive" & v4_data.blocksummary$mean >= .4),1,0))

### count the bad responders ###
{v4_data.summary$bad <- ifelse(v4_data.summary$hap_rate > .4, 1,
                        ifelse(v4_data.summary$ang_rate < .6, 1,
                               ifelse(v4_data.summary$pos_rate > .4, 1,
                                      ifelse(v4_data.summary$neg_rate < .6, 1,
                                             ifelse(v4_data.summary$pow_rate > .4, 1,
                                                    ifelse(v4_data.summary$new_rate < .6, 1,0))))))
sum(v4_data.summary$bad)}


###################################################
full <- merge(v4_data.summary, v4_demo, by = "Participant.Public.ID")
###################
### assess normality ###
shapiro.test(full$sur_rate) ## non-normal
shapiro.test(full$amb_rate) # normal
shapiro.test(full$amw_rate) # normal

### quick correlations ###
cor.test(full$sur_rate, full$amw_rate, use = "complete.obs")
cor.test(full$amb_rate, full$amw_rate, use = "complete.obs")
cor.test(full$sur_rate, full$amb_rate, use = "complete.obs")

### correlations controlling for age and sex ###
pcor.test(full$sur_rate, full$amw_rate, c(full$age, full$mal0fem1))
pcor.test(full$amb_rate, full$amw_rate, c(full$age, full$mal0fem1))
pcor.test(full$sur_rate, full$amb_rate, c(full$age, full$mal0fem1))

