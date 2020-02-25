### set wd ###
nhpath <- "~/Documents/Nick-Grad/Neta_Lab/words/data/study1_redo_data/"
cbpath <- "~/Documents/Github/words/data/study1_redo_data/"
setwd(cbpath)

### load packages ###
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(plyr))
suppressPackageStartupMessages(library(MASS))
suppressPackageStartupMessages(library(ppcor))
suppressPackageStartupMessages(library(dplyr))
suppressPackageStartupMessages(library(reshape2))
suppressPackageStartupMessages(library(BayesMed)) 
suppressPackageStartupMessages(library(ggplot2)) 
suppressPackageStartupMessages(library(plotly)) 
suppressPackageStartupMessages(library(htmlwidgets)) 
   # to get BayesMed to install properly, I had to use the following websites
   # to download/install specific packages
   # https://sourceforge.net/projects/mcmc-jags/
   # http://macappstore.org/gsl/

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
v2_demo <- read.csv("data_exp_10365-v2_questionnaire-rok5.csv")
v4_demo <- read.csv("data_exp_10365-v4_questionnaire-rok5.csv")
v5_demo <- read.csv("data_exp_10365-v5_questionnaire-rok5.csv")

### pull relevant columns, then combine ###
v2_demo <- v2_demo[, c("Participant.Public.ID", "sex", "age","race")]
v4_demo <- v4_demo[, c("Participant.Public.ID", "sex", "age","race")]
v5_demo <- v5_demo[, c("Participant.Public.ID", "sex", "age","race")]
v2_demo <- rbind(v2_demo,v4_demo,v5_demo)
remove(v4_demo,v5_demo)

### read in the task data ###
v2_dhn3 <- read.csv("data_exp_10365-v2_task-dhn3.csv")
v2_uyls <- read.csv("data_exp_10365-v2_task-uyls.csv")
v2_7sqx <- read.csv("data_exp_10365-v2_task-7sqx.csv")
v2_6nyj <- read.csv("data_exp_10365-v2_task-6nyj.csv")
v2_5unm <- read.csv("data_exp_10365-v2_task-5unm.csv")
v2_b5b7 <- read.csv("data_exp_10365-v2_task-b5b7.csv")
v2_xsf9 <- read.csv("data_exp_10365-v2_task-xsf9.csv")
v2_tahj <- read.csv("data_exp_10365-v2_task-tahj.csv")
v2_jpit <- read.csv("data_exp_10365-v2_task-jpit.csv")
v2_u5dg <- read.csv("data_exp_10365-v2_task-u5dg.csv")
v2_rugu <- read.csv("data_exp_10365-v2_task-rugu.csv")
v2_myk4 <- read.csv("data_exp_10365-v2_task-myk4.csv")
v2_pdjg <- read.csv("data_exp_10365-v2_task-pdjg.csv")
v2_nq9b <- read.csv("data_exp_10365-v2_task-nq9b.csv")
v2_tazw <- read.csv("data_exp_10365-v2_task-tazw.csv")
v2_y89e <- read.csv("data_exp_10365-v2_task-y89e.csv")
v2_f3ii <- read.csv("data_exp_10365-v2_task-f3ii.csv")
v2_71u8 <- read.csv("data_exp_10365-v2_task-71u8.csv")
v2_7f4u <- read.csv("data_exp_10365-v2_task-7f4u.csv")
v2_b86o <- read.csv("data_exp_10365-v2_task-b86o.csv")
v2_udom <- read.csv("data_exp_10365-v2_task-udom.csv")
v2_x6ib <- read.csv("data_exp_10365-v2_task-x6ib.csv")
v2_shzk <- read.csv("data_exp_10365-v2_task-shzk.csv")
v2_dko9 <- read.csv("data_exp_10365-v2_task-dko9.csv")
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
v5_dhn3 <- read.csv("data_exp_10365-v5_task-dhn3.csv")
v5_uyls <- read.csv("data_exp_10365-v5_task-uyls.csv")
v5_7sqx <- read.csv("data_exp_10365-v5_task-7sqx.csv")
v5_6nyj <- read.csv("data_exp_10365-v5_task-6nyj.csv")
v5_5unm <- read.csv("data_exp_10365-v5_task-5unm.csv")
v5_b5b7 <- read.csv("data_exp_10365-v5_task-b5b7.csv")
v5_xsf9 <- read.csv("data_exp_10365-v5_task-xsf9.csv")
v5_tahj <- read.csv("data_exp_10365-v5_task-tahj.csv")
v5_jpit <- read.csv("data_exp_10365-v5_task-jpit.csv")
v5_u5dg <- read.csv("data_exp_10365-v5_task-u5dg.csv")
v5_rugu <- read.csv("data_exp_10365-v5_task-rugu.csv")
v5_myk4 <- read.csv("data_exp_10365-v5_task-myk4.csv")
v5_pdjg <- read.csv("data_exp_10365-v5_task-pdjg.csv")
v5_nq9b <- read.csv("data_exp_10365-v5_task-nq9b.csv")
v5_tazw <- read.csv("data_exp_10365-v5_task-tazw.csv")
v5_y89e <- read.csv("data_exp_10365-v5_task-y89e.csv")
v5_f3ii <- read.csv("data_exp_10365-v5_task-f3ii.csv")
v5_71u8 <- read.csv("data_exp_10365-v5_task-71u8.csv")
v5_7f4u <- read.csv("data_exp_10365-v5_task-7f4u.csv")
v5_b86o <- read.csv("data_exp_10365-v5_task-b86o.csv")
v5_udom <- read.csv("data_exp_10365-v5_task-udom.csv")
v5_x6ib <- read.csv("data_exp_10365-v5_task-x6ib.csv")
v5_shzk <- read.csv("data_exp_10365-v5_task-shzk.csv")
v5_dko9 <- read.csv("data_exp_10365-v5_task-dko9.csv")

v2_data <- rbind(v2_dhn3, v2_uyls, v2_7sqx,
                 v2_6nyj, v2_5unm, v2_b5b7,
                 v2_xsf9, v2_tahj, v2_jpit,
                 v2_u5dg, v2_rugu, v2_myk4,
                 v2_pdjg, v2_nq9b, v2_tazw,
                 v2_y89e, v2_f3ii, v2_71u8,
                 v2_7f4u, v2_b86o, v2_udom,
                 v2_x6ib, v2_shzk, v2_dko9)

v2_data <- v2_data[, c("Event.Index", "Participant.Public.ID",
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
                       "order.j7mk", "Screen.Name", "Reaction.Time", "Response",
                       "clearval", "Attempt", "Metadata", "order1", "order2",
                       "order3", "order4", "order5", "order6", "order7",
                       "order8", "order9", "order10", "order11", "order12")]

v4_data <- rbind(v4_dhn3, v4_uyls, v4_7sqx,
                 v4_6nyj, v4_5unm, v4_b5b7,
                 v4_xsf9, v4_tahj, v4_jpit,
                 v4_u5dg, v4_rugu, v4_myk4,
                 v4_pdjg, v4_nq9b, v4_tazw,
                 v4_y89e, v4_f3ii, v4_71u8,
                 v4_7f4u, v4_b86o, v4_udom,
                 v4_x6ib, v4_shzk, v4_dko9)

v4_data <- v4_data[, c("Event.Index", "Participant.Public.ID",
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
                       "order.j7mk", "Screen.Name", "Reaction.Time", "Response",
                       "clearval", "Attempt", "Metadata", "order1", "order2",
                       "order3", "order4", "order5", "order6", "order7",
                       "order8", "order9", "order10", "order11", "order12")]

v5_data <- rbind(v5_dhn3, v5_uyls, v5_7sqx,
                 v5_6nyj, v5_5unm, v5_b5b7,
                 v5_xsf9, v5_tahj, v5_jpit,
                 v5_u5dg, v5_rugu, v5_myk4,
                 v5_pdjg, v5_nq9b, v5_tazw,
                 v5_y89e, v5_f3ii, v5_71u8,
                 v5_7f4u, v5_b86o, v5_udom,
                 v5_x6ib, v5_shzk, 
                 v5_dko9)

v5_data <- v5_data[, c("Event.Index", "Participant.Public.ID",
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
                       "order.j7mk", "Screen.Name", "Reaction.Time", "Response",
                       "clearval", "Attempt", "Metadata", "order1", "order2",
                       "order3", "order4", "order5", "order6", "order7",
                       "order8", "order9", "order10", "order11", "order12")]

v2_data <- rbind(v2_data,v4_data,v5_data)

### clean workspace ###
rm(v2_dhn3, v2_uyls, v2_7sqx,
   v2_6nyj, v2_5unm, v2_b5b7,
   v2_xsf9, v2_tahj, v2_jpit,
   v2_u5dg, v2_rugu, v2_myk4,
   v2_pdjg, v2_nq9b, v2_tazw,
   v2_y89e, v2_f3ii, v2_71u8,
   v2_7f4u, v2_b86o, v2_udom,
   v2_x6ib, v2_shzk, v2_dko9,
   v4_dhn3, v4_uyls, v4_7sqx,
   v4_6nyj, v4_5unm, v4_b5b7,
   v4_xsf9, v4_tahj, v4_jpit,
   v4_u5dg, v4_rugu, v4_myk4,
   v4_pdjg, v4_nq9b, v4_tazw,
   v4_y89e, v4_f3ii, v4_71u8,
   v4_7f4u, v4_b86o, v4_udom,
   v4_x6ib, v4_shzk, v4_dko9, v4_data,
   v5_dhn3, v5_uyls, v5_7sqx,
   v5_6nyj, v5_5unm, v5_b5b7,
   v5_xsf9, v5_tahj, v5_jpit,
   v5_u5dg, v5_rugu, v5_myk4,
   v5_pdjg, v5_nq9b, v5_tazw,
   v5_y89e, v5_f3ii, v5_71u8,
   v5_7f4u, v5_b86o, v5_udom,
   v5_x6ib, v5_shzk, 
   v5_dko9, v5_data)

v2_data <- v2_data[!(v2_data$Event.Index == "END OF FILE"), ]

### set up the stim list based on order assignment ###
v2_data_ord1 <- subset(v2_data, counterbalance.m678 == "order1" | 
                         counterbalance.bi4u == "order1" |
                         counterbalance.dozd == "order1" |
                         counterbalance.g69h == "order1")
v2_data_ord1$stim.pres <- v2_data_ord1$order1

v2_data_ord2 <- subset(v2_data, counterbalance.93ib == "order2" |
                         counterbalance.8ump == "order2" |
                         counterbalance.wch9 == "order2" |
                         counterbalance.2v12 == "order2")
v2_data_ord2$stim.pres <- v2_data_ord2$order2

v2_data_ord3 <- subset(v2_data, counterbalance.m678 == "order3" |
                         counterbalance.bi4u == "order3" |
                         counterbalance.dozd == "order3" |
                         counterbalance.g69h == "order3")
v2_data_ord3$stim.pres <- v2_data_ord3$order3

v2_data_ord4 <- subset(v2_data, counterbalance.93ib == "order4" |
                         counterbalance.8ump == "order4" |
                         counterbalance.wch9 == "order4" |
                         counterbalance.2v12 == "order4")
v2_data_ord4$stim.pres <- v2_data_ord4$order4

v2_data_ord5 <- subset(v2_data, counterbalance.9xcr == "order5" |
                         counterbalance.mcio == "order5" |
                         counterbalance.eu1d == "order5" |
                         counterbalance.s1b5 == "order5")
v2_data_ord5$stim.pres <- v2_data_ord5$order5

v2_data_ord6 <- subset(v2_data, counterbalance.9xcr == "order6" |
                         counterbalance.mcio == "order6" |
                         counterbalance.eu1d == "order6" |
                         counterbalance.s1b5 == "order6")
v2_data_ord6$stim.pres <- v2_data_ord6$order6

v2_data_ord7 <- subset(v2_data, counterbalance.wbyz == "order7" |
                         counterbalance.ulkf == "order7" |
                         counterbalance.u2fx == "order7" |
                         counterbalance.sg3w == "order7")
v2_data_ord7$stim.pres <- v2_data_ord7$order7

v2_data_ord8 <- subset(v2_data, counterbalance.vply == "order8" |
                         counterbalance.t9rn == "order8" |
                         counterbalance.7mrq == "order8" |
                         counterbalance.8zsd == "order8")
v2_data_ord8$stim.pres <- v2_data_ord8$order8

v2_data_ord9 <- subset(v2_data, counterbalance.wbyz == "order9" |
                         counterbalance.ulkf == "order9" |
                         counterbalance.u2fx == "order9" |
                         counterbalance.sg3w == "order9")
v2_data_ord9$stim.pres <- v2_data_ord9$order9

v2_data_ord10 <- subset(v2_data, counterbalance.vply == "order10" |
                          counterbalance.t9rn == "order10" |
                          counterbalance.7mrq == "order10" |
                          counterbalance.8zsd == "order10")
v2_data_ord10$stim.pres <- v2_data_ord10$order10

v2_data_ord11 <- subset(v2_data, counterbalance.hleo == "order11" |
                          counterbalance.3v2j == "order11" |
                          counterbalance.e4l7 == "order11" |
                          counterbalance.o3nf == "order11")
v2_data_ord11$stim.pres <- v2_data_ord11$order11

v2_data_ord12 <- subset(v2_data, counterbalance.hleo == "order12" |
                          counterbalance.3v2j == "order12" |
                          counterbalance.e4l7 == "order12" |
                          counterbalance.o3nf == "order12")
v2_data_ord12$stim.pres <- v2_data_ord12$order12

### merge back together ###
v2_data <- rbind(v2_data_ord1, v2_data_ord2,
                 v2_data_ord3, v2_data_ord4,
                 v2_data_ord5, v2_data_ord6,
                 v2_data_ord7, v2_data_ord8,
                 v2_data_ord9, v2_data_ord10,
                 v2_data_ord11, v2_data_ord12)

remove(v2_data_ord1,v2_data_ord2,v2_data_ord3,v2_data_ord4,v2_data_ord5,
       v2_data_ord6,v2_data_ord7,v2_data_ord8,v2_data_ord9,v2_data_ord10,
       v2_data_ord11,v2_data_ord12)

### drop order# columns for only one stim column ###
drop <- c("order1", "order2","order3", "order4", "order5", "order6", "order7",
         "order8", "order9", "order10", "order11", "order12")

### drop unnecessary columns and the duplicate rows ###
### these contain NA's in stim pres ###
v2_data <- v2_data[ , !(names(v2_data) %in% drop)]
v2_data <- v2_data[!is.na(v2_data$stim.pres), ]

### add stim type factor label ###
v2_data$stimtype <- ifelse(v2_data$stim.pres %in% c(face1, face2), "FACE",
                                  ifelse(v2_data$stim.pres %in% c(words1, words2), "WORD",
                                         ifelse(v2_data$stim.pres %in% c(iaps1, iaps2), "IAPS", "")))

### drop trials w/ responses in less than 200ms ###
v2_data$Reaction.Time <- as.numeric(v2_data$Reaction.Time)
v2_data$RT.Outl <- ifelse((v2_data$Reaction.Time <= 200 & v2_data$Screen.Name == "stim"), 1, 0)
outliers <- subset(v2_data, RT.Outl == 1)
v2_data <- subset(v2_data, RT.Outl == 0)
table(outliers$Participant.Public.ID)

### keep first response only ###
v2_data$flag <- ifelse(v2_data$Attempt >= 2, 1, 0)

v2_data <- subset(v2_data, flag == 0)

### display number of trials for each participant ###
table(v2_data$Participant.Public.ID)

### remove subject if fewer than 75% of trials have a response ###
### v2_data.rm = starting a list of removed sjs to replace w/ correct age category ###
v2_data.rm <- as.data.frame(table(v2_data$Participant.Public.ID))
names(v2_data.rm) <- c("Participant.Public.ID", "trials")
v2_data <- merge(v2_data, v2_data.rm, by = "Participant.Public.ID")
v2_data <- v2_data[(v2_data$trials > 120),]

### make "positive" 0 and "negative" 1 ###
v2_data$rate <- recode(v2_data$Response,
                       "positive" = 0,
                       "negative" = 1)

### pull full response matrix ###
temp <- v2_data %>% 
  group_by(Participant.Public.ID) %>% 
  mutate(key = paste("stim", paste(paste(clearval, stimtype, sep = "_"), stim.pres, sep = "_"), sep = "_")) %>%  # need this to retain image/stim name
  tidyr::spread(., 
                key = key,
                value = rate) %>% 
  tidyr::fill(dplyr::starts_with("stim"), .direction = "up") %>% 
  dplyr::distinct(., Participant.Public.ID, .keep_all = TRUE)

### analysis of response matrix suggests that
### slide14, slide18, and slide43 words 
### are being rated clearly positive, and should be dropped
v2_data <- v2_data %>% subset(!(stim.pres %in% c("Slide14.jpeg", 
                                           "Slide18.jpeg",
                                           "Slide43.jpeg")))



### also of note is IAPS9432 and IAPS9561 are being rated
### as ambiguous rather than negative... strange

v2_data.summary <- (ddply(v2_data, "Participant.Public.ID", summarise, 
                          sur_rate = mean(rate[which(stimtype == "FACE" & clearval == "ambiguous")], na.rm = TRUE),
                          hap_rate = mean(rate[which(stimtype == "FACE" & clearval == "positive")], na.rm = TRUE),
                          ang_rate = mean(rate[which(stimtype == "FACE" & clearval == "negative")], na.rm = TRUE),
                          amb_rate = mean(rate[which(stimtype == "IAPS" & clearval == "ambiguous")], na.rm = TRUE),
                          pos_rate = mean(rate[which(stimtype == "IAPS" & clearval == "positive")], na.rm = TRUE),
                          neg_rate = mean(rate[which(stimtype == "IAPS" & clearval == "negative")], na.rm = TRUE),
                          amw_rate = mean(rate[which(stimtype == "WORD" & clearval == "ambiguous")], na.rm = TRUE),
                          pow_rate = mean(rate[which(stimtype == "WORD" & clearval == "positive")], na.rm = TRUE),
                          new_rate = mean(rate[which(stimtype == "WORD" & clearval == "negative")], na.rm = TRUE),
                          all_rate = mean(rate[which(clearval == "ambiguous")], na.rm = TRUE), 
                          sur_rt = mean(Reaction.Time[which(stimtype == "FACE" & clearval == "ambiguous")], na.rm = TRUE),
                          hap_rt = mean(Reaction.Time[which(stimtype == "FACE" & clearval == "positive")], na.rm = TRUE),
                          ang_rt = mean(Reaction.Time[which(stimtype == "FACE" & clearval == "negative")], na.rm = TRUE),
                          amb_rt = mean(Reaction.Time[which(stimtype == "IAPS" & clearval == "ambiguous")], na.rm = TRUE),
                          pos_rt = mean(Reaction.Time[which(stimtype == "IAPS" & clearval == "positive")], na.rm = TRUE),
                          neg_rt = mean(Reaction.Time[which(stimtype == "IAPS" & clearval == "negative")], na.rm = TRUE),
                          amw_rt = mean(Reaction.Time[which(stimtype == "WORD" & clearval == "ambiguous")], na.rm = TRUE),
                          pow_rt = mean(Reaction.Time[which(stimtype == "WORD" & clearval == "positive")], na.rm = TRUE),
                          new_rt = mean(Reaction.Time[which(stimtype == "WORD" & clearval == "negative")], na.rm = TRUE),
                          sur_neg_rt = mean(Reaction.Time[which(stimtype == "FACE" & clearval == "ambiguous" & rate ==1)], na.rm = TRUE),
                          sur_pos_rt = mean(Reaction.Time[which(stimtype == "FACE" & clearval == "ambiguous" & rate ==1)], na.rm = TRUE),
                          amb_neg_rt = mean(Reaction.Time[which(stimtype == "FACE" & clearval == "ambiguous" & rate ==1)], na.rm = TRUE),
                          amb_pos_rt = mean(Reaction.Time[which(stimtype == "FACE" & clearval == "ambiguous" & rate ==1)], na.rm = TRUE),
                          amw_neg_rt = mean(Reaction.Time[which(stimtype == "FACE" & clearval == "ambiguous" & rate ==1)], na.rm = TRUE),
                          amw_pos_rt = mean(Reaction.Time[which(stimtype == "FACE" & clearval == "ambiguous" & rate ==1)], na.rm = TRUE),))

### double check that data wrangling worked and the values are correct ###
test <- dplyr::select(temp, Participant.Public.ID, stim_ambiguous_FACE_01F_SP_O.jpg:stim_ambiguous_FACE_AM35SUS.JPG)
test$sur_rate <- rowMeans(test[, c(2:25)], na.rm = T)

### make character for easier sorting when viewing the dataframes ###
v2_data.summary$Participant.Public.ID <- as.character(v2_data.summary$Participant.Public.ID)
test$Participant.Public.ID <- as.character(test$Participant.Public.ID)

### compare test and v2_data.summary visually ###



# ### make blocks column if you want to check for bad specific blocks ###
# v2_data$block <- ifelse(v2_data$stim.pres %in% face1, "face1",
#                         ifelse(v2_data$stim.pres %in% face2, "face2",
#                                ifelse(v2_data$stim.pres %in% iaps1, "iaps1",
#                                       ifelse(v2_data$stim.pres %in% iaps2, "iaps2",
#                                              ifelse(v2_data$stim.pres %in% words1, "words1",
#                                                     ifelse(v2_data$stim.pres %in% words2, "words2",""))))))
# 
# v2_data.blocksummary <- ddply(v2_data, c("Participant.Public.ID", "clearval", "block"), summarise,
#                           N    = sum(!is.na(rate)),
#                           mean = mean(rate, na.rm = TRUE),
#                           sd   = sd(rate, na.rm = TRUE),
#                           se   = sd / sqrt(N))
# 
# ### count number of bad blocks per participant ###
# v2_data$block <- ifelse(v2_data$stim.pres %in% face1, "face1",
#                         ifelse(v2_data$stim.pres %in% face2, "face2",
#                                ifelse(v2_data$stim.pres %in% iaps1, "iaps1",
#                                       ifelse(v2_data$stim.pres %in% iaps2, "iaps2",
#                                              ifelse(v2_data$stim.pres %in% words1, "words1",
#                                                     ifelse(v2_data$stim.pres %in% words2, "words2",""))))))
# v2_data.blocksummary$badblocks <- ifelse((v2_data.blocksummary$clearval == "negative" & v2_data.blocksummary$mean <= .6),1,
#                                          ifelse((v2_data.blocksummary$clearval == "positive" & v2_data.blocksummary$mean >= .4),1,0))

### count the bad responders ###
{v2_data.summary$bad <- ifelse(v2_data.summary$hap_rate > .4, 1,
                        ifelse(v2_data.summary$ang_rate < .6, 1,
                               ifelse(v2_data.summary$pos_rate > .4, 1,
                                      ifelse(v2_data.summary$neg_rate < .6, 1,
                                             ifelse(v2_data.summary$pow_rate > .4, 1,
                                                    ifelse(v2_data.summary$new_rate < .6, 1,0))))))
sum(v2_data.summary$bad)}

### add bad responders to list of removed sjs, get demographics ###
v2_data.rm <- v2_data.rm[(v2_data.rm$trials >119),]
v2_data.rm$rm_rate <- v2_data.summary$bad
v2_data.rm <- v2_data.rm[which(v2_data.rm$trials < 120 | v2_data.rm$rm_rate == 1),]
v2_data.rm <- merge(v2_data.rm, v2_demo, by = "Participant.Public.ID")


### remove subject if bad responser ###
v2_data.summary <- v2_data.summary[(v2_data.summary$bad != 1),]

### drop bad subjs from response matrix ###
temp <- temp %>% subset(Participant.Public.ID %in% v2_data.summary$Participant.Public.ID)

### add demographic variables ###
temp <- merge(temp, v2_demo, by = "Participant.Public.ID")
temp$age <- as.numeric(temp$age)

<<<<<<< HEAD
### drop unnecessary columns ###
temp <- temp[, c(1, 3, 40:202)]
=======
###remove colums 2-28 (staggered order info) ###
temp <- subset(temp, select = -c(2:28))
>>>>>>> 9a0258e2d5444e15325793cad968291ec5ab99e0

### write out response matrix ###
# write.csv(temp, "~/Desktop/subj_response_matrix.csv", row.names = F)

###################################################
full <- merge(v2_data.summary, v2_demo, by = "Participant.Public.ID")
full$age <- as.numeric(full$age)
### histogram of ages ###
hist(full$age,breaks = length(unique(full$age)),xlim = c(min(full$age),max(full$age)))

### new sex column
full$mal0fem1 <- ifelse(full$sex == "Female", 1,
                               ifelse(full$sex == "Male", 0,""))
full$mal0fem1 <- as.numeric(full$mal0fem1)

### display table of races
prop.table(table(full$race))
tbl <- table(full$race)
cbind(tbl,prop.table(tbl))

###################
### assess normality ###
shapiro.test(full$sur_rate) ## non-normal
shapiro.test(full$amb_rate) # normal
shapiro.test(full$amw_rate) # normal
shapiro.test(full$age)

### quick correlations ###
cor.test(full$sur_rate, full$amw_rate, use = "complete.obs", method = "spearman")
cor.test(full$amb_rate, full$amw_rate, use = "complete.obs")
cor.test(full$sur_rate, full$amb_rate, use = "complete.obs", method = "spearman")

### correlations controlling for age and sex ###
pcor.test(full$sur_rate, full$amw_rate, c(full$age, full$mal0fem1))
pcor.test(full$amb_rate, full$amw_rate, c(full$age, full$mal0fem1))
pcor.test(full$sur_rate, full$amb_rate, c(full$age, full$mal0fem1))

### correlations for those under 60 years old only (for SAS 2020) ###
full_60 <- full[full$age < 60,]
pcor.test(full_60$sur_rate, full_60$amw_rate, c(full_60$age, full_60$mal0fem1))
pcor.test(full_60$amb_rate, full_60$amw_rate, c(full_60$age, full_60$mal0fem1))
pcor.test(full_60$sur_rate, full_60$amb_rate, c(full_60$age, full_60$mal0fem1))

###################
### Bayes partial correlation controlling for age and sex ###
# jzs_partcor(full$sur_rate, full$amw_rate, c(full$age, full$mal0fem1)) #can't do 2 controls!
# jzs_partcor(full$amb_rate, full$amw_rate, c(full$age, full$mal0fem1)) #can't do 2 controls!
# jzs_partcor(full$sur_rate, full$amb_rate, c(full$age, full$mal0fem1)) #can't do 2 controls!


ggplot(full, aes(x=sur_rate, y = amw_rate))+
   geom_point()+
   geom_smooth(method="lm")

ggplot(full, aes(x=amb_rate, y = amw_rate))+
   geom_point()+
   geom_smooth(method="lm")

ggplot(full, aes(x=amb_rate, y = sur_rate))+
   geom_point()+
   geom_smooth(method="lm")

### Bayes partial correlations with age as variable, controlling for sex ###
jzs_partcor(full$age, full$sur_rate, c(full$mal0fem1))
jzs_partcor(full$age, full$amb_rate, c(full$mal0fem1))
jzs_partcor(full$age, full$amw_rate, c(full$mal0fem1))
jzs_partcor(full$age, full$all_rate, c(full$mal0fem1))
?jzs_partcor
jzs_cor(full$age, full$sur_rate)
jzs_cor(full$age, full$amb_rate)
jzs_cor(full$age, full$amw_rate)
jzs_cor(full$age, full$all_rate)

ggplot(full, aes(x=full$age, y = sur_rate))+
   geom_point()+
   geom_smooth(method="lm")

ggplot(full, aes(x=full$age, y = amb_rate))+
   geom_point()+
   geom_smooth(method="lm")

ggplot(full, aes(x=full$age, y = amw_rate))+
   geom_point()+
   geom_smooth(method="lm")

ggplot(full, aes(x=full$age, y = all_rate))+
   geom_point()+
   geom_smooth(method="lm")

cor.test(full$sur_rate, full$age, use = "complete.obs", method = "spearman")
cor.test(full$amb_rate, full$age, use = "complete.obs", method = "spearman")
cor.test(full$amw_rate, full$age, use = "complete.obs", method = "spearman")

pcor.test(full$sur_rate, full$age, full$mal0fem1, method = "Spearman")
pcor.test(full$amb_rate, full$age, full$mal0fem1, method = "Spearman")
pcor.test(full$amw_rate, full$age, full$mal0fem1, method = "Spearman")

################### Stim analysis

### make a df of the sitmulus properties to merge later
stim.props <- subset(v2_data, select = c("stimtype","clearval","stim.pres"))
### remove duplicate rows
stim.props <- distinct(stim.props)

### create df that summarizes mean and sd for valence and rt for each stimulus
v2_data.stim <- (ddply(v2_data, "stim.pres", summarise, 
                       rate_mean = mean(rate), 
                       rate_sd = sd(rate),
                       rt_mean = mean(Reaction.Time),
                       rt_sd = sd(Reaction.Time)))

### add on properties of each stim
v2_data.stim <- merge(stim.props, v2_data.stim, by = "stim.pres")

### rename words from their slide names 
### forgive me, efficient syntax gods ###
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide5.jpeg"] <- "STUN"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide6.jpeg"] <- "SNAPPY"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide7.jpeg"] <- "CLUB"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide8.jpeg"] <- "RETREAT"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide9.jpeg"] <- "DISCIPLINE"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide10.jpeg"] <- "TERMINAL"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide11.jpeg"] <- "CRUSH"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide12.jpeg"] <- "BATTER"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide13.jpeg"] <- "PICK"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide14.jpeg"] <- "PLAYER"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide15.jpeg"] <- "POP"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide16.jpeg"] <- "SURVIVAL"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide17.jpeg"] <- "TRICK"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide18.jpeg"] <- "CHECK"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide19.jpeg"] <- "RUSH"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide20.jpeg"] <- "COURT"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide21.jpeg"] <- "EVIL"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide22.jpeg"] <- "SLAVE"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide23.jpeg"] <- "ROTTEN"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide24.jpeg"] <- "CRAPPY"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide25.jpeg"] <- "DECAY"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide26.jpeg"] <- "DEADLY"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide27.jpeg"] <- "CROOK"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide28.jpeg"] <- "VANDAL"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide29.jpeg"] <- "LOVEABLE"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide30.jpeg"] <- "MUSIC"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide31.jpeg"] <- "MOTHER"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide32.jpeg"] <- "FOOD"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide33.jpeg"] <- "TREASURED"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide34.jpeg"] <- "FRUITFUL"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide35.jpeg"] <- "COMIC"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide36.jpeg"] <- "LEMONADE"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide37.jpeg"] <- "CLOSE"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide38.jpeg"] <- "REVOLUTION"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide39.jpeg"] <- "FIGHTER"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide40.jpeg"] <- "HAIL"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide41.jpeg"] <- "HANG"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide42.jpeg"] <- "OPERATION"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide43.jpeg"] <- "DIRECT"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide44.jpeg"] <- "SHAKE"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide45.jpeg"] <- "BEAT"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide46.jpeg"] <- "RADICAL"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide47.jpeg"] <- "THICK"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide48.jpeg"] <- "SHRED"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide49.jpeg"] <- "TRAFFIC"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide50.jpeg"] <- "BREAK"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide51.jpeg"] <- "OVERCOME"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide52.jpeg"] <- "CATCH"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide53.jpeg"] <- "FUNERAL"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide54.jpeg"] <- "PISS"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide55.jpeg"] <- "URINE"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide56.jpeg"] <- "WRECK"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide57.jpeg"] <- "USELESS"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide58.jpeg"] <- "LAWSUIT"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide59.jpeg"] <- "MISFIRE"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide60.jpeg"] <- "SPINELESS"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide61.jpeg"] <- "BRAVE"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide62.jpeg"] <- "COMEDIAN"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide63.jpeg"] <- "SUNSET"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide64.jpeg"] <- "AMUSE"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide65.jpeg"] <- "FEMININE"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide66.jpeg"] <- "THINKER"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide67.jpeg"] <- "RABBIT"
v2_data.stim$stim.pres[v2_data.stim$stim.pres == "Slide68.jpeg"] <- "GENTLEMAN"

### create an interactive scatterplot
scatterplot_stim <- ggplot(v2_data.stim, aes(x = clearval, y = rate_mean,
                                             text = paste(
                                                "Stimulus: ", stim.pres, 
                                                "\n", "Valence Mean: ", (rate_mean*100), 
                                                "\n", "Valence SD: ", (rate_sd*100), 
                                                "\n", "RT Mean: ", rt_mean, 
                                                "\n", "RT SD: ", rt_sd, 
                                                sep = ""))) + 
   labs(x = "Stimulus Valence", 
        y = "% Negative Rating Across Participants",
        title = "Average Valence Ratings Across Participants",
        color = "Stimulus Type") +
   geom_jitter(aes(color = stimtype)) 
p <- ggplotly(scatterplot_stim, tooltip = "text")

### save interactive scatterplot as an html file
wordpath <- "~/Documents/Github/words/"
setwd(wordpath)
htmlwidgets::saveWidget(as_widget(p), "scatterplot_stim.html")

### interactive plot for rt
scatterplot_rt <- ggplot(v2_data.stim, aes(x = stim.pres, y = rate_mean,
                                             text = paste(
                                                "Stimulus: ", stim.pres, 
                                                "\n", "Valence Mean: ", (rate_mean*100), 
                                                "\n", "Valence SD: ", (rate_sd*100), 
                                                "\n", "RT Mean: ", rt_mean, 
                                                "\n", "RT SD: ", rt_sd, 
                                                sep = ""))) + 
   scale_fill_manual(values = c("blue","red","green")) +
   labs(x = "Stimulus Valence", 
        y = "Reaction Time (ms)",
        fill = "Valence") + 
   geom_jitter(aes(fill = clearval))
p <- ggplotly(scatterplot_rt, tooltip = "text")
print(p)
### save interactive scatterplot as an html file
wordpath <- "~/Documents/Github/words/"
setwd(wordpath)
htmlwidgets::saveWidget(as_widget(p), "scatterplot_rt.html")

### pirate plots
### Stimuli
stim.data <- gather(full, key = condition, value = Negativity,
                    "hap_rate", "ang_rate", "sur_rate", "pos_rate", "neg_rate", "amb_rate",
                    "pow_rate","new_rate","amw_rate")

stim.data$Valence <- ifelse(stim.data$condition %in% 
                               c("ang_rate", "neg_rate","new_rate"), "Negative",
                            ifelse(stim.data$condition %in% 
                                      c("hap_rate", "pos_rate","pow_rate"), "Positive",
                                   ifelse(stim.data$condition %in% 
                                             c("sur_rate", "amb_rate","amw_rate"), "Ambiguous", "")))
stim.data$Stimuli <- ifelse(stim.data$condition %in% 
                               c("sur_rate", "hap_rate", "ang_rate"), "Faces",
                            ifelse(stim.data$condition %in% 
                                      c("pos_rate", "neg_rate", "amb_rate"), "IAPS",
                                   ifelse(stim.data$condition %in% 
                                             c("pow_rate", "new_rate", "amw_rate"), "Words", "")))

stim.data$Valence = factor(stim.data$Valence, levels = c("Negative","Ambiguous","Positive"))
pirateplot(Negativity ~ Stimuli * Valence, data = stim.data,
           pal = c("Red", "Blue", "Green"), inf.method = "se", 
           bar.f.o = 0,
           inf.f.o = .5, 
           bean.b.o = 0,
           bean.f.o = .4,
           point.o = 1,
           bean.f.col = c("Red","Red","red", "Blue","Blue","Blue","Green","Green","Green"),
           bean.b.col = "White",
           point.col = c("Red","Red","red", "Blue","Blue","Blue","Green","Green","Green"),
           avg.line.col = c("Red","Red","red", "Blue","Blue","Blue","Green","Green","Green"),
           back.col = "White",
           gl.col = "White", #turn off grid lines
           main = "Valence")


pirateplot(Negativity ~ Stimuli , data = stim.data,
           pal = c("Red", "Blue", "Green"), inf.method = "se", 
           bar.f.o = 0,
           inf.f.o = .5, 
           bean.b.o = 0,
           bean.f.o = .4,
           point.o = 1,
           bean.f.col = c("Red", "Blue", "Green"),
           bean.b.col = "White",
           point.col = c("Red", "Blue", "Green"),
           avg.line.col = c("Red", "Blue", "Green"),
           back.col = "White",
           gl.col = "White", #turn off grid lines
           main = "Valence")


stim.data$Valence = factor(stim.data$Valence, levels = c("Negative","Ambiguous","Positive"))
pirateplot(Negativity ~ Valence , data = stim.data,
           pal = c("Red", "Blue", "Green"), inf.method = "se", 
           bar.f.o = 0,
           inf.f.o = .5, 
           bean.b.o = 0,
           bean.f.o = .4,
           point.o = 1,
           bean.f.col = c("Red", "Blue", "Green"),
           bean.b.col = "White",
           point.col = c("Red", "Blue", "Green"),
           avg.line.col = c("Red", "Blue", "Green"),
           back.col = "White",
           gl.col = "White", #turn off grid lines
           main = "Valence")

stim.data$Valence = factor(stim.data$Valence, levels = c("Negative","Ambiguous","Positive"))
pirateplot(Negativity ~ Valence * Stimuli, data = stim.data,
           pal = c("Red", "Blue", "Green"), inf.method = "se", 
           bar.f.o = 0,
           inf.f.o = .5, 
           bean.b.o = 0,
           bean.f.o = .4,
           point.o = 1,
           bean.f.col = c("Red","Blue","Green", "Red","Blue","Green","Red","Blue","Green"),
           bean.b.col = "White",
           point.col = c("Red","Blue","Green", "Red","Blue","Green","Red","Blue","Green"),
           avg.line.col = c("Red","Blue","Green", "Red","Blue","Green","Red","Blue","Green"),
           back.col = "White",
           gl.col = "White", #turn off grid lines
           main = "Valence")

### Reaction Time
rt.data <- gather(full, key = condition, value = Negativity,
                    "hap_rt", "ang_rt", "sur_rt", "pos_rt", "neg_rt", "amb_rt",
                    "pow_rt","new_rt","amw_rt")

rt.data$Valence <- ifelse(rt.data$condition %in% 
                               c("ang_rt", "neg_rt","new_rt"), "Negative",
                            ifelse(rt.data$condition %in% 
                                      c("hap_rt", "pos_rt","pow_rt"), "Positive",
                                   ifelse(rt.data$condition %in% 
                                             c("sur_rt", "amb_rt","amw_rt"), "Ambiguous", "")))
rt.data$Stimuli <- ifelse(rt.data$condition %in% 
                               c("sur_rt", "hap_rt", "ang_rt"), "Faces",
                            ifelse(rt.data$condition %in% 
                                      c("pos_rt", "neg_rt", "amb_rt"), "IAPS",
                                   ifelse(rt.data$condition %in% 
                                             c("pow_rt", "new_rt", "amw_rt"), "Words", "")))

rt.data$Valence = factor(rt.data$Valence, levels = c("Negative","Ambiguous","Positive"))
pirateplot(Negativity ~ Stimuli * Valence, data = rt.data,
           pal = c("Red", "Blue", "Green"), inf.method = "se", 
           bar.f.o = 0,
           inf.f.o = .5, 
           bean.b.o = 0,
           bean.f.o = .4,
           point.o = 1,
           bean.f.col = c("Red","Red","red", "Blue","Blue","Blue","Green","Green","Green"),
           bean.b.col = "White",
           point.col = c("Red","Red","red", "Blue","Blue","Blue","Green","Green","Green"),
           avg.line.col = c("Red","Red","red", "Blue","Blue","Blue","Green","Green","Green"),
           back.col = "White",
           gl.col = "White", #turn off grid lines
           main = "Valence")


pirateplot(Negativity ~ Stimuli , data = rt.data,
           pal = c("Red", "Blue", "Green"), inf.method = "se", 
           bar.f.o = 0,
           inf.f.o = .5, 
           bean.b.o = 0,
           bean.f.o = .4,
           point.o = 1,
           bean.f.col = c("Red", "Blue", "Green"),
           bean.b.col = "White",
           point.col = c("Red", "Blue", "Green"),
           avg.line.col = c("Red", "Blue", "Green"),
           back.col = "White",
           gl.col = "White", #turn off grid lines
           main = "Valence")

rt.data$Valence = factor(rt.data$Valence, levels = c("Negative","Ambiguous","Positive"))
pirateplot(Negativity ~ Valence , data = rt.data,
           pal = c("Red", "Blue", "Green"), inf.method = "se", 
           bar.f.o = 0,
           inf.f.o = .5, 
           bean.b.o = 0,
           bean.f.o = .4,
           point.o = 1,
           bean.f.col = c("Red", "Blue", "Green"),
           bean.b.col = "White",
           point.col = c("Red", "Blue", "Green"),
           avg.line.col = c("Red", "Blue", "Green"),
           back.col = "White",
           gl.col = "White", #turn off grid lines
           main = "Valence")

rt.data$Valence = factor(rt.data$Valence, levels = c("Negative","Ambiguous","Positive"))
pirateplot(Negativity ~ Valence * Stimuli, data = rt.data,
           pal = c("Red", "Blue", "Green"), inf.method = "se", 
           bar.f.o = 0,
           inf.f.o = .5, 
           bean.b.o = 0,
           bean.f.o = .4,
           point.o = 1,
           bean.f.col = c("Red","Blue","Green", "Red","Blue","Green","Red","Blue","Green"),
           bean.b.col = "White",
           point.col = c("Red","Blue","Green", "Red","Blue","Green","Red","Blue","Green"),
           avg.line.col = c("Red","Blue","Green", "Red","Blue","Green","Red","Blue","Green"),
           back.col = "White",
           gl.col = "White", #turn off grid lines
           main = "Valence")

### Reaction Time for ambiguous sitm as pos or neg
rt.data2 <- gather(full, key = condition, value = Negativity,
                  "sur_pos_rt", "sur_neg_rt","amb_pos_rt", "amb_neg_rt", "amw_pos_rt","amw_neg_rt")

rt.data2$Valence <- ifelse(rt.data2$condition %in% 
                             c("ang_rt", "neg_rt","new_rt"), "Negative",
                          ifelse(rt.data2$condition %in% 
                                    c("hap_rt", "pos_rt","pow_rt"), "Positive",
                                 ifelse(rt.data2$condition %in% 
                                           c("sur_rt", "amb_rt","amw_rt"), "Ambiguous", "")))
rt.data2$Stimuli <- ifelse(rt.data2$condition %in% 
                             c("sur_rt", "hap_rt", "ang_rt"), "Faces",
                          ifelse(rt.data2$condition %in% 
                                    c("pos_rt", "neg_rt", "amb_rt"), "IAPS",
                                 ifelse(rt.data2$condition %in% 
                                           c("pow_rt", "new_rt", "amw_rt"), "Words", "")))

rt.data2$ValenceChoice = factor(rt.data2$Valence, levels = c("Negative","Positive"))
pirateplot(Negativity ~ Stimuli * Valence, data = rt.data,
           pal = c("Red", "Green"), inf.method = "se", 
           bar.f.o = 0,
           inf.f.o = .5, 
           bean.b.o = 0,
           bean.f.o = .4,
           point.o = 1,
           bean.f.col = c("Red","Red","Red", "Green","Green","Green"),
           bean.b.col = "White",
           point.col = c("Red","Red","Red","Green","Green","Green"),
           avg.line.col = c("Red","Red","Red", "Green","Green","Green"),
           back.col = "White",
           gl.col = "White", #turn off grid lines
           main = "Valence")