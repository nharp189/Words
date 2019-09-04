### set wd ###
cbpath <- '~/Documents/GitHub/Words/'
nhpath <- '~/Documents/Nick-Grad/Neta_Lab/Words/'
path <- nhpath
setwd(path)

source("data_cleaning_nh.R")


# ### make item-level data ###
# ### each word is a row and each subject is a column ###
# split.data <- split(data, data$subjID)
# split.data <- lapply(split.data, function(data) {
#   #data <- data[match(unique(data$wordlist), data$wordlist),]  
#   data <- spread(data, key = subjID, value = rating)
# })
# # final.participant <- final.participant[!final.participant %in% grep(paste0("A1DCKRRPA4AWVD", collapse = "|"), final.participant, value = T)]
# item.level.data <- bind_cols(split.data)
# item.level.data <- item.level.data[,c("wordlist", final.participant)]

### plot the words ###
ggplot(aes(x = wordlist, y = neg.avg), data = words.summary) +
  geom_point(stat = "identity")

### looks like 21 positive (e.g., below .25) and 20 negative (e.g., above .75) ###
pos.words <- subset(words.summary, words.summary$neg.avg <= .25)
neg.words <- subset(words.summary, words.summary$neg.avg >= .75)

### plot the words between .25 and .75 neg.avg ###
ggplot(aes(x = wordlist, y = RT, color = Val), data = subset(words.summary, (words.summary$neg.avg <= .75 & words.summary$neg.avg >= .25))) +
  geom_point(stat = "identity")

### plot the words between .25 and .75 neg.avg ###
ggplot(aes(x = wordlist, y = RT, color = Val), data = subset(words.summary, (words.summary$neg.avg <= .25))) +
  geom_point(stat = "identity")

### plot the words between .25 and .75 neg.avg ###
ggplot(aes(x = wordlist, y = RT, color = Val), data = subset(words.summary, (words.summary$neg.avg >= .75))) +
  geom_point(stat = "identity")

### pull words and list for pos, neg, and amb ###
### want quick pos and neg words ###
### want slow amb words ###
pos.words <- subset(pos.words, RT <= 800)
neg.words <- subset(neg.words, RT <= 800)
amb.words <- subset(words.summary, (words.summary$neg.avg <= .75 & words.summary$neg.avg >= .25))
amb.words <- subset(amb.words, RT >= 875)

### now clean up data frames to include a few columns and output ###
pos.words <- pos.words[, c("wordlist", "neg.avg", "RT")]
neg.words <- neg.words[, c("wordlist", "neg.avg", "RT")]
amb.words <- amb.words[, c("wordlist", "neg.avg", "RT", "Val")]

write.csv(pos.words, "pos.words.csv")
write.csv(neg.words, "neg.words.csv")
write.csv(amb.words, "amb.words.csv")
