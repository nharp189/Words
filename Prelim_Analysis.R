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

## this is relatively uninformative ##
mean(words.summary$neg.sd)
sd(words.summary$neg.sd)
## most words have high SD ##

words <- words.summary[order(-words.summary$neg.sd),]
row.names(words) = NULL

top100sd <- words[1:100, ]

top100sd.ambonly <- subset(top100sd, top100sd$Val == "AMB")
mean(top100sd.ambonly$neg.avg)
sd(top100sd.ambonly$neg.avg)
mean(top100sd.ambonly$neg.sd)
sd(top100sd.ambonly$neg.sd)