v2_data$PrevTrial <- ifelse(v2_data$clearval == "ambiguous",
                            as.character(v2_data$clearval[-1]), "")

### count overall ###
count(v2_data$PrevTrial)

### count for each subject ###
table <- as.data.frame.matrix(table(v2_data$Participant.Public.ID, v2_data$PrevTrial))

table <- table[-c(1), ]
min(table$negative)

table$Ratio <- table$negative/table$positive

table$Participant.Public.ID <- rownames(table)
temp <- merge(v2_data.summary, table, by = "Participant.Public.ID")

shapiro.test(temp$all_amb_rate)
shapiro.test(temp$Ratio)

plot(temp$Ratio)

cor.test(temp$all_amb_rate, temp$Ratio,
         method = "spearman")
