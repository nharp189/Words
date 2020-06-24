### set wd ###
nhpath <- "~/Documents/Nick-Grad/Neta_Lab/Words/"
path = nhpath
setwd(path)

library(utils)
library(dplyr)

### import elexicon generate list (this list used min / max for lexical characteristics
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

### pick random pos and neg words for first block###
set.seed(23)
firstblockpos <- pos[sample(nrow(pos), 20), ]
firstblockneg <- neg[sample(nrow(neg), 20), ]

final <- rbind(neg, pos, amb.data)

### create full list without words in first block ###
firstblock <- rbind(firstblockpos, firstblockneg)
task2 <- final[!(final$Word %in% firstblock$Word),]

tra <- subset(final, final$Word == "insane")
### make words uppercase, but can get rid of these lines later ###
## messes up other stuff... ###
task2 <- mutate_all(task2, .funs=toupper)
# write.csv(task2, "task2.csv")
firstblockpos <- mutate_all(firstblockpos, .funs=toupper)
# write.csv(firstblockpos, "firstblockpos.csv")
firstblockneg <- mutate_all(firstblockneg, .funs=toupper)
# write.csv(firstblockneg, "firstblockneg.csv")
#save dataframes as csv with date and time in file name 
# write.csv(final,paste("words.csv",
  #                     format(Sys.time(),'_%Y-%m-%d_%H-%M-%S'),
  #                     '.csv',sep = ''))

# write.csv(firstblockneg,paste("firstblockneg.csv",
  #                    format(Sys.time(),'_%Y-%m-%d_%H-%M-%S'),
  #                    '.csv',sep = ''))

#write.csv(firstblockpos,paste("firstblockpos.csv",
  #                            format(Sys.time(),'_%Y-%m-%d_%H-%M-%S'),
  #                            '.csv',sep = ''))
#write.csv(final, "~/Desktop/posnegwords.csv")
#bad <- read.csv("~/Desktop/swearWords.csv")
#bad <- as.list(bad)
#baddata$words <- bad
#bad.long <- gather(bad)


### checking for duplicate word -- insane is both AMB and NEG ###
# list <- c(as.character(firstblock$Word), as.character(task2$Word))
# list <- mutate_all(as.data.frame(list), .funs=toupper)
# list <- list[order(list$list),]
# setdiff(list, list.2)
# list <- as.data.frame(list)
# list.2 <- as.data.frame(list.2)
# lista <- c("a", "b", "c")
# listb <- c("a")













# pirateplot(A.Mean.Sum ~ VAL, data = select)
# 
# ambwords <- subset(full.data, A.Mean.Sum > 5)
# write.csv(ambwords, "~/Desktop/ambwords.csv")
# 
# 
# 
# 
# ### top / bottom quartiles for valence and middle quartiles for arousal ###
# quantile(full.data$V.Mean.Sum)
# 
# 
# pop_sd <- sd(full.data$V.Mean.Sum)*sqrt((length(full.data$V.Mean.Sum)-1)/length(full.data$V.Mean.Sum))
# pop_mean <- mean(full.data$V.Mean.Sum)
# 
# full.data$Val.z <- ((full.data$V.Mean.Sum - pop_mean)/pop_sd)
# sum(full.data$Val.z >= 2)
# 
# 
# full.data <- full.data[full.data$Length <= 9,]
# View(full.data)
# 
# ### val 4.5 and below (neg) and 6.7 and up (pos) ###
# #full.data$vquartile <- with(full.data, cut(V.Mean.Sum, 
#  #                               breaks=quantile(V.Mean.Sum, probs=seq(0,1, by=0.25), na.rm=TRUE), 
#   #                              include.lowest=TRUE))
# #full.data$vquartile <- as.numeric(full.data$vquartile)
# #full.data$aquantile <-  with(full.data, cut(A.Mean.Sum, 
# #                                breaks=quantile(A.Mean.Sum, probs=seq(0,1, by=0.25), na.rm=TRUE), 
# #                                include.lowest=TRUE))
# #full.data$aquantile <- as.numeric(full.data$aquantile)
# 
# # full.data$mid <- ifelse(full.data$aquantile == 2, 1,
# #                         ifelse(full.data$aquantile == 3, 1, 0))
# # std(comb$Length)
# # mean(comb$Length)
# # 
# # 
# # 
# # View(full.data)
# # neg <- subset(full.data, vquartile == 1)
# # neg <- subset(neg, aquantile == c(2, 3))
# # pos <- subset(full.data, vquartile == 4)
# # pos <- subset(pos, aquantile == c(2, 3))
# # View(pos)
# # comb <- rbind(neg, pos)
# # 
# # shapiro.test(comb.new$Length)
# # comb.new <- subset(comb.new, Length >= 5)
# # hist(comb.new$Length)
# # 
# # write_csv(pos, "~/Desktop/pos.csv")
# # View(full.data)
# # squantile(full.data$A.Mean.Sum)
# 
# 
# ### order by val mean ###
# sort.data <- full.data[order(full.data$V.Mean.Sum),]
# 
# ### add new id column ###
# sort.data$id <- seq.int(nrow(sort.data))
# rownames(sort.data) <- sort.data$id
# 
# ### grab min val through 3.5 for neg, and 6.5 through max val for pos###
# neg.words <- sort.data[1:490,]
# pos.words <- sort.data[3419:4145,]
# 
# ### pick appropriately arousing words... looking for ~ 4.5 ###
# ### sort each list by arousal ###
# aro.neg <-neg.words[order(neg.words$A.Mean.Sum),] 
# aro.pos <-pos.words[order(pos.words$A.Mean.Sum),] 
# 
# ### re-number ###
# aro.neg$id <- seq.int(nrow(aro.neg))
# aro.pos$id <- seq.int(nrow(aro.pos))
# row.names(aro.neg) <- aro.neg$id
# row.names(aro.pos) <- aro.pos$id
# 
# ### get rid of anything arousal < 3.5 ... , 
# ### like AMB ratings on Words Summary excel sheet ###
# aro.neg <- aro.neg[-c(1:18),]
# aro.pos <- aro.pos[-c(1:116),]
# 
# aro.neg$id <- seq.int(nrow(aro.neg))
# aro.pos$id <- seq.int(nrow(aro.pos))
# row.names(aro.neg) <- aro.neg$id
# row.names(aro.pos) <- aro.pos$id
# 
# ### ... or arousal > 5.5
# aro.neg <- aro.neg[-c(330:472),]
# aro.pos <- aro.pos[-c(502:611),]
# 
# ### add val labels ###
# aro.neg$val <- "neg"
# aro.pos$val <- "pos"
# 
# ### merge ###
# words <- rbind(aro.neg, aro.pos)
# 
# ### remove all words with length 8 or higher ###
# words <- words[order(words$Length),]
# words$id <- seq.int(nrow(words))
# row.names(words) <- words$id
# words <- words[!(words$Length >= 8),]
# 
# words$id <- seq.int(nrow(words))
# row.names(words) <- words$id
# 
# ### down to 233 neg, 360 pos ###
# ### get rid of low Log_Freq below 8 ###
# words <-words[order(words$Log_Freq_HAL),]
# words$id <- seq.int(nrow(words))
# row.names(words) <- words$id
# words <- words[words$Log_Freq_HAL >= 8,]
# 
# pos.words <- subset(words, val == "pos")
# colMeans(pos.words[,2:21])
# 
# 
# View(words)
# words <-words[order(words$val),]
# colMeans(words[,2:21])
# 
# 
# 
#  ### could order 13,000 words by val sd and pick new ambiguous ###
# 
# 
# ### check for high SD words to add to AMB word list ###
# rate <- rate[order(rate$V.SD.Sum),]
