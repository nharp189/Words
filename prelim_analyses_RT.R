source("data_cleaning_nh.R")

### plot all RTs per subj ###
data$`Participant Public ID` <- as.character(data$`Participant Public ID`)
ggplot(data = data, aes(x = `Participant Public ID`, y = `Reaction Time`)) +
  geom_point() +
  ylim(0, 10000) +
  geom_hline(mapping = NULL, yintercept = 2932.776)

### calculate percent trials retained after RT cutoff ###
orig <- data %>% count(`Participant Public ID`)
sub <- subset(data, (`Reaction Time` >= 250 & `Reaction Time` <= 2214.349)) %>% count(`Participant Public ID`)
comb <- merge(orig, sub, by = "Participant Public ID")
comb$PercentRemaining <- (comb$n.y / comb$n.x)
write.csv(comb, "~/Desktop/250to2214ms.csv")


data2 <- subset(data, (`Reaction Time` >= 250 & `Reaction Time` <= 3000))

words.summary <- words.summary[order(words.summary$neg.avg),]

row.names(words.summary) <- NULL