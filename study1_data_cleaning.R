### set wd ###
setwd("~/Documents/Nick-Grad/Neta_Lab/Words/data/study1_data/data_exp_9792-v16/")

### load packages ###
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(plyr))

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
}

###################################################
### change directory ###
setwd("~/Documents/Nick-Grad/Neta_Lab/Words/data/study1_data/data_exp_9792-v16/")

### read in the task data ###
v16_7sqx <- read.csv("data_exp_9792-v16_task-7sqx.csv")
v16_6nyj <- read.csv("data_exp_9792-v16_task-6nyj.csv")
v16_5unm <- read.csv("data_exp_9792-v16_task-5unm.csv")
v16_b5b7 <- read.csv("data_exp_9792-v16_task-b5b7.csv")
v16_ulkf <- read.csv("data_exp_9792-v16_task-ulkf.csv")

v16_data <- rbind( v16_7sqx,
                  v16_6nyj, v16_5unm, v16_b5b7)

rm(v16_7sqx, v16_6nyj, v16_5unm, v16_b5b7)

v16_data <- v16_data[, c("Event.Index", "Participant.Public.ID",
                         "randomiser.jf65", "randomiser.2tba",
                         "counterbalance.93ib", "counterbalance.m678",
                         "counterbalance.9xcr", "counterbalance.wbyz",
                         "counterbalance.vply", "counterbalance.hleo",
                         "counterbalance.3v2j",
                         "order.j7mk", "Reaction.Time", "Response",
                         "clearval", "Metadata", "order1", "order2",
                         "order3", "order4", "order5", "order6", "order7",
                         "order8", "order9", "order10", "order11", "order12")]

v16_data_ord1 <- subset(v16_data, counterbalance.m678 == "order1")
v16_data_ord2 <- subset(v16_data, counterbalance.93ib == "order2")
v16_data_ord3 <- subset(v16_data, counterbalance.m678 == "order3")
v16_data_ord4 <- subset(v16_data, counterbalance.93ib == "order4")
v16_data_ord5 <- subset(v16_data, counterbalance.9xcr == "order5")
v16_data_ord6 <- subset(v16_data, counterbalance.9xcr == "order6")
v16_data_ord7 <- subset(v16_data, counterbalance.wbyz == "order7")
v16_data_ord8 <- subset(v16_data, counterbalance.vply == "order8")
v16_data_ord9 <- subset(v16_data, counterbalance.wbyz == "order9")
v16_data_ord10 <- subset(v16_data, counterbalance.vply == "order10")
v16_data_ord11 <- subset(v16_data, counterbalance.hleo == "order11")
v16_data_ord12 <- subset(v16_data, counterbalance.hleo == "order12")

### set stim lists ##

### add stim type factor label ###
{
  v16_data_ord1$stimtype <- ifelse(v16_data_ord1$order1 %in% face1, "FACE", 
                                   ifelse(v16_data_ord1$order1 %in% words1, "WORD", 
                                          ifelse(v16_data_ord1$order1 %in% iaps1, "IAPS", "")))
  v16_data_ord2$stimtype <- ifelse(v16_data_ord2$order2 %in% face1, "FACE", 
                                   ifelse(v16_data_ord2$order2 %in% words1, "WORD", 
                                          ifelse(v16_data_ord2$order2 %in% iaps1, "IAPS", "")))
  v16_data_ord3$stimtype <- ifelse(v16_data_ord3$order3 %in% face1, "FACE", 
                                   ifelse(v16_data_ord3$order3 %in% words1, "WORD", 
                                          ifelse(v16_data_ord3$order3 %in% iaps1, "IAPS", "")))
  v16_data_ord4$stimtype <- ifelse(v16_data_ord4$order4 %in% face1, "FACE", 
                                   ifelse(v16_data_ord4$order4 %in% words1, "WORD", 
                                          ifelse(v16_data_ord4$order4 %in% iaps1, "IAPS", "")))
  v16_data_ord5$stimtype <- ifelse(v16_data_ord5$order5 %in% face1, "FACE", 
                                   ifelse(v16_data_ord5$order5 %in% words1, "WORD", 
                                          ifelse(v16_data_ord5$order5 %in% iaps1, "IAPS", "")))
  v16_data_ord6$stimtype <- ifelse(v16_data_ord6$order6 %in% face1, "FACE", 
                                   ifelse(v16_data_ord6$order6 %in% words1, "WORD", 
                                          ifelse(v16_data_ord6$order6 %in% iaps1, "IAPS", "")))
  
  v16_data_ord7$stimtype <- ifelse(v16_data_ord7$order6 %in% face2, "FACE", 
                                   ifelse(v16_data_ord7$order6 %in% words2, "WORD", 
                                          ifelse(v16_data_ord7$order6 %in% iaps1, "IAPS", "")))
  v16_data_ord8$stimtype <- ifelse(v16_data_ord8$order8 %in% face2, "FACE", 
                                   ifelse(v16_data_ord8$order8 %in% words2, "WORD", 
                                          ifelse(v16_data_ord8$order8 %in% iaps1, "IAPS", "")))
  v16_data_ord9$stimtype <- ifelse(v16_data_ord9$order9 %in% face2, "FACE", 
                                   ifelse(v16_data_ord9$order9 %in% words2, "WORD", 
                                          ifelse(v16_data_ord9$order9 %in% iaps1, "IAPS", "")))
  v16_data_ord10$stimtype <- ifelse(v16_data_ord10$order10 %in% face2, "FACE", 
                                    ifelse(v16_data_ord10$order10 %in% words2, "WORD", 
                                           ifelse(v16_data_ord10$order10 %in% iaps1, "IAPS", "")))
  v16_data_ord11$stimtype <- ifelse(v16_data_ord11$order11 %in% face2, "FACE", 
                                    ifelse(v16_data_ord11$order11 %in% words2, "WORD", 
                                           ifelse(v16_data_ord11$order11 %in% iaps1, "IAPS", "")))
  v16_data_ord12$stimtype <- ifelse(v16_data_ord12$order12 %in% face2, "FACE", 
                                    ifelse(v16_data_ord12$order12 %in% words2, "WORD", 
                                           ifelse(v16_data_ord12$order12 %in% iaps1, "IAPS", "")))
}

### merge back together ###
### would be nice to have a loop to do the above work... ###
v16_data <- rbind(v16_data_ord1, v16_data_ord2,
                  v16_data_ord3, v16_data_ord4,
                  v16_data_ord5, v16_data_ord6,
                  v16_data_ord7, v16_data_ord8,
                  v16_data_ord9, v16_data_ord10,
                  v16_data_ord11, v16_data_ord12)

### make "positive" 0 and "negative" 1 ###
v16_data$rate <- recode(v16_data$Response,
                        "positive" = 0,
                        "negative" = 1)


v16_data.summary <- (ddply(v16_data, "Participant.Public.ID", summarise, 
                           sur_rate = mean(rate[which(stimtype == "FACE" & clearval == "ambiguous")], na.rm = TRUE),
                           hap_rate = mean(rate[which(stimtype == "FACE" & clearval == "positive")], na.rm = TRUE),
                           ang_rate = mean(rate[which(stimtype == "FACE" & clearval == "negative")], na.rm = TRUE),
                           amb_rate = mean(rate[which(stimtype == "IAPS" & clearval == "ambiguous")], na.rm = TRUE),
                           pos_rate = mean(rate[which(stimtype == "IAPS" & clearval == "positive")], na.rm = TRUE),
                           neg_rate = mean(rate[which(stimtype == "IAPS" & clearval == "negative")], na.rm = TRUE),
                           amw_rate = mean(rate[which(stimtype == "WORD" & clearval == "ambiguous")], na.rm = TRUE),
                           pow_rate = mean(rate[which(stimtype == "WORD" & clearval == "positive")], na.rm = TRUE),
                           new_rate = mean(rate[which(stimtype == "WORD" & clearval == "negative")], na.rm = TRUE)))

###################################################
### change directory ###
setwd("~/Documents/Nick-Grad/Neta_Lab/Words/data/study1_data/data_exp_9792-v17/")

### read in the task data ###
v17_dhn3 <- read.csv("data_exp_9792-v17_task-dhn3.csv")
v17_uyls <- read.csv("data_exp_9792-v17_task-uyls.csv")
v17_7sqx <- read.csv("data_exp_9792-v17_task-7sqx.csv")
v17_6nyj <- read.csv("data_exp_9792-v17_task-6nyj.csv")
v17_5unm <- read.csv("data_exp_9792-v17_task-5unm.csv")
v17_b5b7 <- read.csv("data_exp_9792-v17_task-b5b7.csv")
v17_xsf9 <- read.csv("data_exp_9792-v17_task-xsf9.csv")
v17_tahj <- read.csv("data_exp_9792-v17_task-tahj.csv")
v17_jpit <- read.csv("data_exp_9792-v17_task-jpit.csv")
# v17_u5dg <- read.csv("data_exp_9792-v17_task-u5dg.csv")
v17_rugu <- read.csv("data_exp_9792-v17_task-rugu.csv")
v17_myk4 <- read.csv("data_exp_9792-v17_task-myk4.csv")

v17_data <- rbind(v17_dhn3, v17_uyls, v17_7sqx,
                  v17_6nyj, v17_5unm, v17_b5b7,
                  v17_xsf9, v17_tahj, v17_jpit,
                  v17_rugu, v17_myk4)

rm(v17_dhn3, v17_uyls, v17_7sqx, v17_6nyj, v17_5unm, v17_b5b7)

v17_data <- v17_data[, c("Event.Index", "Participant.Public.ID",
                         "randomiser.jf65", "randomiser.2tba",
                         "counterbalance.93ib", "counterbalance.m678",
                         "counterbalance.9xcr", "counterbalance.wbyz",
                         "counterbalance.vply", "counterbalance.hleo",
                         "counterbalance.3v2j",
                         "order.j7mk", "Reaction.Time", "Response",
                         "clearval", "Metadata", "order1", "order2",
                         "order3", "order4", "order5", "order6", "order7",
                         "order8", "order9", "order10", "order11", "order12")]

v17_data_ord1 <- subset(v17_data, counterbalance.m678 == "order1")
v17_data_ord2 <- subset(v17_data, counterbalance.93ib == "order2")
v17_data_ord3 <- subset(v17_data, counterbalance.m678 == "order3")
v17_data_ord4 <- subset(v17_data, counterbalance.93ib == "order4")
v17_data_ord5 <- subset(v17_data, counterbalance.9xcr == "order5")
v17_data_ord6 <- subset(v17_data, counterbalance.9xcr == "order6")
v17_data_ord7 <- subset(v17_data, counterbalance.wbyz == "order7")
v17_data_ord8 <- subset(v17_data, counterbalance.vply == "order8")
v17_data_ord9 <- subset(v17_data, counterbalance.wbyz == "order9")
v17_data_ord10 <- subset(v17_data, counterbalance.vply == "order10")
v17_data_ord11 <- subset(v17_data, counterbalance.hleo == "order11")
v17_data_ord12 <- subset(v17_data, counterbalance.hleo == "order12")

### set stim lists ##

### add stim type factor label ###
{
  v17_data_ord1$stimtype <- ifelse(v17_data_ord1$order1 %in% face1, "FACE", 
                                   ifelse(v17_data_ord1$order1 %in% words1, "WORD", 
                                          ifelse(v17_data_ord1$order1 %in% iaps1, "IAPS", "")))
  v17_data_ord2$stimtype <- ifelse(v17_data_ord2$order2 %in% face1, "FACE", 
                                   ifelse(v17_data_ord2$order2 %in% words1, "WORD", 
                                          ifelse(v17_data_ord2$order2 %in% iaps1, "IAPS", "")))
  v17_data_ord3$stimtype <- ifelse(v17_data_ord3$order3 %in% face1, "FACE", 
                                   ifelse(v17_data_ord3$order3 %in% words1, "WORD", 
                                          ifelse(v17_data_ord3$order3 %in% iaps1, "IAPS", "")))
  v17_data_ord4$stimtype <- ifelse(v17_data_ord4$order4 %in% face1, "FACE", 
                                   ifelse(v17_data_ord4$order4 %in% words1, "WORD", 
                                          ifelse(v17_data_ord4$order4 %in% iaps1, "IAPS", "")))
  v17_data_ord5$stimtype <- ifelse(v17_data_ord5$order5 %in% face1, "FACE", 
                                   ifelse(v17_data_ord5$order5 %in% words1, "WORD", 
                                          ifelse(v17_data_ord5$order5 %in% iaps1, "IAPS", "")))
  v17_data_ord6$stimtype <- ifelse(v17_data_ord6$order6 %in% face1, "FACE", 
                                   ifelse(v17_data_ord6$order6 %in% words1, "WORD", 
                                          ifelse(v17_data_ord6$order6 %in% iaps1, "IAPS", "")))
  v17_data_ord7$stimtype <- ifelse(v17_data_ord7$order6 %in% face2, "FACE", 
                                   ifelse(v17_data_ord7$order6 %in% words2, "WORD", 
                                          ifelse(v17_data_ord7$order6 %in% iaps1, "IAPS", "")))
  v17_data_ord8$stimtype <- ifelse(v17_data_ord8$order8 %in% face2, "FACE", 
                                   ifelse(v17_data_ord8$order8 %in% words2, "WORD", 
                                          ifelse(v17_data_ord8$order8 %in% iaps1, "IAPS", "")))
  v17_data_ord9$stimtype <- ifelse(v17_data_ord9$order9 %in% face2, "FACE", 
                                   ifelse(v17_data_ord9$order9 %in% words2, "WORD", 
                                          ifelse(v17_data_ord9$order9 %in% iaps1, "IAPS", "")))
  v17_data_ord10$stimtype <- ifelse(v17_data_ord10$order10 %in% face2, "FACE", 
                                    ifelse(v17_data_ord10$order10 %in% words2, "WORD", 
                                           ifelse(v17_data_ord10$order10 %in% iaps1, "IAPS", "")))
  v17_data_ord11$stimtype <- ifelse(v17_data_ord11$order11 %in% face2, "FACE", 
                                    ifelse(v17_data_ord11$order11 %in% words2, "WORD", 
                                           ifelse(v17_data_ord11$order11 %in% iaps1, "IAPS", "")))
  v17_data_ord12$stimtype <- ifelse(v17_data_ord12$order12 %in% face2, "FACE", 
                                    ifelse(v17_data_ord12$order12 %in% words2, "WORD", 
                                           ifelse(v17_data_ord12$order12 %in% iaps1, "IAPS", "")))
}

### merge back together ###
### would be nice to have a loop to do the above work... ###
v17_data <- rbind(v17_data_ord1, v17_data_ord2,
                  v17_data_ord3, v17_data_ord4,
                  v17_data_ord5, v17_data_ord6,
                  v17_data_ord7, v17_data_ord8,
                  v17_data_ord9, v17_data_ord10,
                  v17_data_ord11, v17_data_ord12)

### make "positive" 0 and "negative" 1 ###
v17_data$rate <- recode(v17_data$Response,
                        "positive" = 0,
                        "negative" = 1)


v17_data.summary <- (ddply(v17_data, "Participant.Public.ID", summarise, 
                           sur_rate = mean(rate[which(stimtype == "FACE" & clearval == "ambiguous")], na.rm = TRUE),
                           hap_rate = mean(rate[which(stimtype == "FACE" & clearval == "positive")], na.rm = TRUE),
                           ang_rate = mean(rate[which(stimtype == "FACE" & clearval == "negative")], na.rm = TRUE),
                           amb_rate = mean(rate[which(stimtype == "IAPS" & clearval == "ambiguous")], na.rm = TRUE),
                           pos_rate = mean(rate[which(stimtype == "IAPS" & clearval == "positive")], na.rm = TRUE),
                           neg_rate = mean(rate[which(stimtype == "IAPS" & clearval == "negative")], na.rm = TRUE),
                           amw_rate = mean(rate[which(stimtype == "WORD" & clearval == "ambiguous")], na.rm = TRUE),
                           pow_rate = mean(rate[which(stimtype == "WORD" & clearval == "positive")], na.rm = TRUE),
                           new_rate = mean(rate[which(stimtype == "WORD" & clearval == "negative")], na.rm = TRUE)))

#####################################################
### change directory ###
setwd("~/Documents/Nick-Grad/Neta_Lab/Words/data/study1_data/data_exp_9792-v18/")

### read in the task data ###
v18_dhn3 <- read.csv("data_exp_9792-v18_task-dhn3.csv")
v18_uyls <- read.csv("data_exp_9792-v18_task-uyls.csv")
v18_7sqx <- read.csv("data_exp_9792-v18_task-7sqx.csv")
v18_6nyj <- read.csv("data_exp_9792-v18_task-6nyj.csv")
v18_5unm <- read.csv("data_exp_9792-v18_task-5unm.csv")
v18_b5b7 <- read.csv("data_exp_9792-v18_task-b5b7.csv")
v18_xsf9 <- read.csv("data_exp_9792-v18_task-xsf9.csv")
v18_tahj <- read.csv("data_exp_9792-v18_task-tahj.csv")
v18_jpit <- read.csv("data_exp_9792-v18_task-jpit.csv")
v18_u5dg <- read.csv("data_exp_9792-v18_task-u5dg.csv")
v18_rugu <- read.csv("data_exp_9792-v18_task-rugu.csv")
v18_myk4 <- read.csv("data_exp_9792-v18_task-myk4.csv")
v18_pdjg <- read.csv("data_exp_9792-v18_task-pdjg.csv")
v18_nq9b <- read.csv("data_exp_9792-v18_task-nq9b.csv")
v18_tazw <- read.csv("data_exp_9792-v18_task-tazw.csv")
v18_y89e <- read.csv("data_exp_9792-v18_task-y89e.csv")
v18_f3ii <- read.csv("data_exp_9792-v18_task-f3ii.csv")
v18_71u8 <- read.csv("data_exp_9792-v18_task-71u8.csv")
v18_7f4u <- read.csv("data_exp_9792-v18_task-7f4u.csv")
v18_b86o <- read.csv("data_exp_9792-v18_task-b86o.csv")
v18_udom <- read.csv("data_exp_9792-v18_task-udom.csv")
v18_x6ib <- read.csv("data_exp_9792-v18_task-x6ib.csv")
v18_shzk <- read.csv("data_exp_9792-v18_task-shzk.csv")
# v18_dko9 <- read.csv("data_exp_9792-v18_task-dk09.csv")

v18_data <- rbind(v18_dhn3, v18_uyls, v18_7sqx,
                  v18_6nyj, v18_5unm, v18_b5b7,
                  v18_xsf9, v18_tahj, v18_jpit,
                  v18_u5dg, v18_rugu, v18_myk4,
                  v18_pdjg, v18_nq9b, v18_tazw,
                  v18_y89e, v18_f3ii, v18_71u8,
                  v18_7f4u, v18_b86o, v18_udom,
                  v18_x6ib, v18_shzk)

### clean workspace ###
rm(v18_dhn3, v18_uyls, v18_7sqx,
   v18_6nyj, v18_5unm, v18_b5b7,
   v18_xsf9, v18_tahj, v18_jpit,
   v18_u5dg, v18_rugu, v18_myk4,
   v18_pdjg, v18_nq9b, v18_tazw,
   v18_y89e, v18_f3ii, v18_71u8,
   v18_7f4u, v18_b86o, v18_udom,
   v18_x6ib, v18_shzk)

v18_data <- v18_data[, c("Event.Index", "Participant.Public.ID",
                         "randomiser.jf65", "randomiser.2tba",
                         "counterbalance.93ib", "counterbalance.m678",
                         "counterbalance.9xcr", "counterbalance.wbyz",
                         "counterbalance.vply", "counterbalance.hleo",
                         "counterbalance.3v2j", "counterbalance.ulkf",
                         "counterbalance.bi4u", "counterbalance.dozd",
                         "counterbalance.g69h", "counterbalance.8ump",
                         "counterbalance.wch9", "counterbalance.2v12",
                         "counterbalance.mcio", "counterbalance.eu1d",
                         "counterbalance.s1b5", "counterbalance.u2fx",
                         "counterbalance.sg3w", "counterbalance.t9rn",
                         "counterbalance.7mrq", "counterbalance.8zsd",
                         "counterbalance.e4l7", "counterbalance.o3nf",
                         "order.j7mk", "Reaction.Time", "Response",
                         "clearval", "Metadata", "order1", "order2",
                         "order3", "order4", "order5", "order6", "order7",
                         "order8", "order9", "order10", "order11", "order12")]

v18_data_ord1 <- subset(v18_data, counterbalance.m678 == "order1" | 
                          counterbalance.bi4u == "order1" |
                          counterbalance.dozd == "order1" |
                          counterbalance.g69h == "order1")
v18_data_ord2 <- subset(v18_data, counterbalance.93ib == "order2" |
                          counterbalance.8ump == "order2" |
                          counterbalance.wch9 == "order2" |
                          counterbalance.2v12 == "order2")
v18_data_ord3 <- subset(v18_data, counterbalance.m678 == "order3" |
                          counterbalance.bi4u == "order3" |
                          counterbalance.dozd == "order3" |
                          counterbalance.g69h == "order3")
v18_data_ord4 <- subset(v18_data, counterbalance.93ib == "order4" |
                          counterbalance.8ump == "order4" |
                          counterbalance.wch9 == "order4" |
                          counterbalance.2v12 == "order4")
v18_data_ord5 <- subset(v18_data, counterbalance.9xcr == "order5" |
                          counterbalance.mcio == "order5" |
                          counterbalance.eu1d == "order5" |
                          counterbalance.s1b5 == "order5")
v18_data_ord6 <- subset(v18_data, counterbalance.9xcr == "order6" |
                          counterbalance.mcio == "order6" |
                          counterbalance.eu1d == "order6" |
                          counterbalance.s1b5 == "order6")
v18_data_ord7 <- subset(v18_data, counterbalance.wbyz == "order7" |
                          counterbalance.ulkf == "order7" |
                          counterbalance.u2fx == "order7" |
                          counterbalance.sg3w == "order7")
v18_data_ord8 <- subset(v18_data, counterbalance.vply == "order8" |
                          counterbalance.t9rn == "order8" |
                          counterbalance.7mrq == "order8" |
                          counterbalance.8zsd == "order8")
v18_data_ord9 <- subset(v18_data, counterbalance.wbyz == "order9" |
                          counterbalance.ulkf == "order9" |
                          counterbalance.u2fx == "order9" |
                          counterbalance.sg3w == "order9")
v18_data_ord10 <- subset(v18_data, counterbalance.vply == "order10" |
                           counterbalance.t9rn == "order10" |
                           counterbalance.7mrq == "order10" |
                           counterbalance.8zsd == "order10")
v18_data_ord11 <- subset(v18_data, counterbalance.hleo == "order11" |
                           counterbalance.3v2j == "order11" |
                           counterbalance.e4l7 == "order11" |
                           counterbalance.o3nf == "order11")
v18_data_ord12 <- subset(v18_data, counterbalance.hleo == "order12" |
                           counterbalance.3v2j == "order12" |
                           counterbalance.e4l7 == "order12" |
                           counterbalance.o3nf == "order12")

### add stim type factor label ###
{
v18_data_ord1$stimtype <- ifelse(v18_data_ord1$order1 %in% face1, "FACE", 
                                 ifelse(v18_data_ord1$order1 %in% words1, "WORD", 
                                        ifelse(v18_data_ord1$order1 %in% iaps1, "IAPS", "")))
v18_data_ord2$stimtype <- ifelse(v18_data_ord2$order2 %in% face1, "FACE", 
                                 ifelse(v18_data_ord2$order2 %in% words1, "WORD", 
                                        ifelse(v18_data_ord2$order2 %in% iaps1, "IAPS", "")))
v18_data_ord3$stimtype <- ifelse(v18_data_ord3$order3 %in% face1, "FACE", 
                                 ifelse(v18_data_ord3$order3 %in% words1, "WORD", 
                                        ifelse(v18_data_ord3$order3 %in% iaps1, "IAPS", "")))
v18_data_ord4$stimtype <- ifelse(v18_data_ord4$order4 %in% face1, "FACE", 
                                 ifelse(v18_data_ord4$order4 %in% words1, "WORD", 
                                        ifelse(v18_data_ord4$order4 %in% iaps1, "IAPS", "")))
v18_data_ord5$stimtype <- ifelse(v18_data_ord5$order5 %in% face1, "FACE", 
                                 ifelse(v18_data_ord5$order5 %in% words1, "WORD", 
                                        ifelse(v18_data_ord5$order5 %in% iaps1, "IAPS", "")))
v18_data_ord6$stimtype <- ifelse(v18_data_ord6$order6 %in% face1, "FACE", 
                                 ifelse(v18_data_ord6$order6 %in% words1, "WORD", 
                                        ifelse(v18_data_ord6$order6 %in% iaps1, "IAPS", "")))
v18_data_ord7$stimtype <- ifelse(v18_data_ord7$order6 %in% face2, "FACE", 
                                 ifelse(v18_data_ord7$order6 %in% words2, "WORD", 
                                        ifelse(v18_data_ord7$order6 %in% iaps1, "IAPS", "")))
v18_data_ord8$stimtype <- ifelse(v18_data_ord8$order8 %in% face2, "FACE", 
                                 ifelse(v18_data_ord8$order8 %in% words2, "WORD", 
                                        ifelse(v18_data_ord8$order8 %in% iaps1, "IAPS", "")))
v18_data_ord9$stimtype <- ifelse(v18_data_ord9$order9 %in% face2, "FACE", 
                                 ifelse(v18_data_ord9$order9 %in% words2, "WORD", 
                                        ifelse(v18_data_ord9$order9 %in% iaps1, "IAPS", "")))
v18_data_ord10$stimtype <- ifelse(v18_data_ord10$order10 %in% face2, "FACE", 
                                 ifelse(v18_data_ord10$order10 %in% words2, "WORD", 
                                        ifelse(v18_data_ord10$order10 %in% iaps1, "IAPS", "")))
v18_data_ord11$stimtype <- ifelse(v18_data_ord11$order11 %in% face2, "FACE", 
                                 ifelse(v18_data_ord11$order11 %in% words2, "WORD", 
                                        ifelse(v18_data_ord11$order11 %in% iaps1, "IAPS", "")))
v18_data_ord12$stimtype <- ifelse(v18_data_ord12$order12 %in% face2, "FACE", 
                                 ifelse(v18_data_ord12$order12 %in% words2, "WORD", 
                                        ifelse(v18_data_ord12$order12 %in% iaps1, "IAPS", "")))
}

### merge back together ###
### would be nice to have a loop to do the above work... ###
v18_data <- rbind(v18_data_ord1, v18_data_ord2,
                  v18_data_ord3, v18_data_ord4,
                  v18_data_ord5, v18_data_ord6,
                  v18_data_ord7, v18_data_ord8,
                  v18_data_ord9, v18_data_ord10,
                  v18_data_ord11, v18_data_ord12)

### make "positive" 0 and "negative" 1 ###
v18_data$rate <- recode(v18_data$Response,
                        "positive" = 0,
                        "negative" = 1)

v18_data.summary <- (ddply(v18_data, "Participant.Public.ID", summarise, 
                           sur_rate = mean(rate[which(stimtype == "FACE" & clearval == "ambiguous")], na.rm = TRUE),
                           hap_rate = mean(rate[which(stimtype == "FACE" & clearval == "positive")], na.rm = TRUE),
                           ang_rate = mean(rate[which(stimtype == "FACE" & clearval == "negative")], na.rm = TRUE),
                           amb_rate = mean(rate[which(stimtype == "IAPS" & clearval == "ambiguous")], na.rm = TRUE),
                           pos_rate = mean(rate[which(stimtype == "IAPS" & clearval == "positive")], na.rm = TRUE),
                           neg_rate = mean(rate[which(stimtype == "IAPS" & clearval == "negative")], na.rm = TRUE),
                           amw_rate = mean(rate[which(stimtype == "WORD" & clearval == "ambiguous")], na.rm = TRUE),
                           pow_rate = mean(rate[which(stimtype == "WORD" & clearval == "positive")], na.rm = TRUE),
                           new_rate = mean(rate[which(stimtype == "WORD" & clearval == "negative")], na.rm = TRUE)))


cor.test(v18_data.summary$sur_rate, v18_data.summary$amw_rate, use = "complete.obs")
cor.test(v18_data.summary$amb_rate, v18_data.summary$amw_rate, use = "complete.obs")
cor.test(v18_data.summary$sur_rate, v18_data.summary$amb_rate, use = "complete.obs")

#####################
### merge the data together ###
full <- rbind(v16_data.summary, v17_data.summary, v18_data.summary)

###################
cor.test(full$sur_rate, full$amw_rate, use = "complete.obs")
cor.test(full$amb_rate, full$amw_rate, use = "complete.obs")
cor.test(full$sur_rate, full$amb_rate, use = "complete.obs")
