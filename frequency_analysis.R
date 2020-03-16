### set wd ###
setwd("~/Documents/Nick-Grad/Neta_Lab/words/")
library(data.table)

### source data cleaning 1:535 ###
### to get temp dataframe ###

### read in Elex data ###
charac <- read_csv("Items_Elex.csv")

### read in temp from study1_redo_data_cleaning.R ###
temp2 <- as.data.frame(t(temp))

resp.matrix <- setDT(temp2, keep.rownames = TRUE)[]

### drop extraneous rows ###
resp.matrix <- resp.matrix[-c(1:3), ]
### make data frame again, not DT ###
resp.matrix <- as.data.frame(resp.matrix)
### make numeric, not factor ###
for(i in 2:ncol(resp.matrix)) {
  resp.matrix[, i] <- as.numeric(as.character(resp.matrix[, i]))
}
### calculate word RT avg ###
resp.matrix$words_avg <- rowMeans(resp.matrix[, c(2:198)], na.rm = T)

### subset to only words... ###
resp.matrix <- resp.matrix[grep("WORD", resp.matrix$rn), ]

### pair w/ the freq data ###
stim <- as.data.frame(str_split_fixed(resp.matrix$rn, "_", 4))
resp.matrix$val <- stim$V2
resp.matrix$rn <- stim$V4

### make character, not factor ###
resp.matrix$rn <- as.character(resp.matrix$rn)

### catie's renaming code ###
### redo this at some point... ###
### forgive me, efficient syntax gods ###
{
  resp.matrix$rn[resp.matrix$rn == "Slide5.jpeg"] <- "STUN"
  resp.matrix$rn[resp.matrix$rn == "Slide6.jpeg"] <- "SNAPPY"
  resp.matrix$rn[resp.matrix$rn == "Slide7.jpeg"] <- "CLUB"
  resp.matrix$rn[resp.matrix$rn == "Slide8.jpeg"] <- "RETREAT"
  resp.matrix$rn[resp.matrix$rn == "Slide9.jpeg"] <- "DISCIPLINE"
  resp.matrix$rn[resp.matrix$rn == "Slide10.jpeg"] <- "TERMINAL"
  resp.matrix$rn[resp.matrix$rn == "Slide11.jpeg"] <- "CRUSH"
  resp.matrix$rn[resp.matrix$rn == "Slide12.jpeg"] <- "BATTER"
  resp.matrix$rn[resp.matrix$rn == "Slide13.jpeg"] <- "PICK"
  resp.matrix$rn[resp.matrix$rn == "Slide14.jpeg"] <- "PLAYER"
  resp.matrix$rn[resp.matrix$rn == "Slide15.jpeg"] <- "POP"
  resp.matrix$rn[resp.matrix$rn == "Slide16.jpeg"] <- "SURVIVAL"
  resp.matrix$rn[resp.matrix$rn == "Slide17.jpeg"] <- "TRICK"
  resp.matrix$rn[resp.matrix$rn == "Slide18.jpeg"] <- "CHECK"
  resp.matrix$rn[resp.matrix$rn == "Slide19.jpeg"] <- "RUSH"
  resp.matrix$rn[resp.matrix$rn == "Slide20.jpeg"] <- "COURT"
  resp.matrix$rn[resp.matrix$rn == "Slide21.jpeg"] <- "EVIL"
  resp.matrix$rn[resp.matrix$rn == "Slide22.jpeg"] <- "SLAVE"
  resp.matrix$rn[resp.matrix$rn == "Slide23.jpeg"] <- "ROTTEN"
  resp.matrix$rn[resp.matrix$rn == "Slide24.jpeg"] <- "CRAPPY"
  resp.matrix$rn[resp.matrix$rn == "Slide25.jpeg"] <- "DECAY"
  resp.matrix$rn[resp.matrix$rn == "Slide26.jpeg"] <- "DEADLY"
  resp.matrix$rn[resp.matrix$rn == "Slide27.jpeg"] <- "CROOK"
  resp.matrix$rn[resp.matrix$rn == "Slide28.jpeg"] <- "VANDAL"
  resp.matrix$rn[resp.matrix$rn == "Slide29.jpeg"] <- "LOVABLE"
  resp.matrix$rn[resp.matrix$rn == "Slide30.jpeg"] <- "MUSIC"
  resp.matrix$rn[resp.matrix$rn == "Slide31.jpeg"] <- "MOTHER"
  resp.matrix$rn[resp.matrix$rn == "Slide32.jpeg"] <- "FOOD"
  resp.matrix$rn[resp.matrix$rn == "Slide33.jpeg"] <- "TREASURED"
  resp.matrix$rn[resp.matrix$rn == "Slide34.jpeg"] <- "FRUITFUL"
  resp.matrix$rn[resp.matrix$rn == "Slide35.jpeg"] <- "COMIC"
  resp.matrix$rn[resp.matrix$rn == "Slide36.jpeg"] <- "LEMONADE"
  resp.matrix$rn[resp.matrix$rn == "Slide37.jpeg"] <- "CLOSE"
  resp.matrix$rn[resp.matrix$rn == "Slide38.jpeg"] <- "REVOLUTION"
  resp.matrix$rn[resp.matrix$rn == "Slide39.jpeg"] <- "FIGHTER"
  resp.matrix$rn[resp.matrix$rn == "Slide40.jpeg"] <- "HAIL"
  resp.matrix$rn[resp.matrix$rn == "Slide41.jpeg"] <- "HANG"
  resp.matrix$rn[resp.matrix$rn == "Slide42.jpeg"] <- "OPERATION"
  resp.matrix$rn[resp.matrix$rn == "Slide43.jpeg"] <- "DIRECT"
  resp.matrix$rn[resp.matrix$rn == "Slide44.jpeg"] <- "SHAKE"
  resp.matrix$rn[resp.matrix$rn == "Slide45.jpeg"] <- "BEAT"
  resp.matrix$rn[resp.matrix$rn == "Slide46.jpeg"] <- "RADICAL"
  resp.matrix$rn[resp.matrix$rn == "Slide47.jpeg"] <- "THICK"
  resp.matrix$rn[resp.matrix$rn == "Slide48.jpeg"] <- "SHRED"
  resp.matrix$rn[resp.matrix$rn == "Slide49.jpeg"] <- "TRAFFIC"
  resp.matrix$rn[resp.matrix$rn == "Slide50.jpeg"] <- "BREAK"
  resp.matrix$rn[resp.matrix$rn == "Slide51.jpeg"] <- "OVERCOME"
  resp.matrix$rn[resp.matrix$rn == "Slide52.jpeg"] <- "CATCH"
  resp.matrix$rn[resp.matrix$rn == "Slide53.jpeg"] <- "FUNERAL"
  resp.matrix$rn[resp.matrix$rn == "Slide54.jpeg"] <- "PISS"
  resp.matrix$rn[resp.matrix$rn == "Slide55.jpeg"] <- "URINE"
  resp.matrix$rn[resp.matrix$rn == "Slide56.jpeg"] <- "WRECK"
  resp.matrix$rn[resp.matrix$rn == "Slide57.jpeg"] <- "USELESS"
  resp.matrix$rn[resp.matrix$rn == "Slide58.jpeg"] <- "LAWSUIT"
  resp.matrix$rn[resp.matrix$rn == "Slide59.jpeg"] <- "MISFIRE"
  resp.matrix$rn[resp.matrix$rn == "Slide60.jpeg"] <- "SPINELESS"
  resp.matrix$rn[resp.matrix$rn == "Slide61.jpeg"] <- "BRAVE"
  resp.matrix$rn[resp.matrix$rn == "Slide62.jpeg"] <- "COMEDIAN"
  resp.matrix$rn[resp.matrix$rn == "Slide63.jpeg"] <- "SUNSET"
  resp.matrix$rn[resp.matrix$rn == "Slide64.jpeg"] <- "AMUSE"
  resp.matrix$rn[resp.matrix$rn == "Slide65.jpeg"] <- "FEMININE"
  resp.matrix$rn[resp.matrix$rn == "Slide66.jpeg"] <- "THINKER"
  resp.matrix$rn[resp.matrix$rn == "Slide67.jpeg"] <- "RABBIT"
  resp.matrix$rn[resp.matrix$rn == "Slide68.jpeg"] <- "GENTLEMAN"
}

### make lower case ###
resp.matrix$rn <- tolower(resp.matrix$rn)

### rename the column ###
names(resp.matrix)[names(resp.matrix) == "rn"] <- "Word"

### MERGE MISSING 2 WORDS ###
final <- merge(resp.matrix, charac, by = "Word")

### pick only ambiguous words ###
amb <- subset(final, final$val == "ambiguous")
write.csv(amb, "ambiguous_words_itemresp+lex.csv")
setdiff(subset(resp.matrix, resp.matrix$val == "ambiguous")$Word, amb$Word)
### check is an outlier (MAYBE?), and driving the correlation significane... ###
amb <- subset(amb, amb$Word != "check")
shapiro.test(amb$Freq_HAL) # blech non-normal, should use spearman

### make numeric ###
amb$Freq_HAL <- as.numeric(as.character(amb$Freq_HAL))
amb$Log_Freq_HAL <- as.numeric(as.character(amb$Log_Freq_HAL))

cor.test(amb$words_avg, amb$Freq_HAL, method = "spearman")
plot(amb$words_avg, amb$Freq_HAL)

t.test((subset(final, final$val == "ambiguous")$Freq_HAL),
       (subset(final, final$val %in% c("positive", "negative"))$Freq_HAL),
       var.equal = F)
class(final$Freq_HAL)

subset(final, final$val == "ambiguous")$Freq_HAL


subset(final, final$val %in% c("positive", "negative")$Freq_HAL)

final$Freq_HAL
### RT ITEM RESPONSE MATRIX ###
### transpose ###
RT.matrix.t <- as.data.frame(t(RT.matrix))
### reset row name ###
RT.matrix.t <- setDT(RT.matrix.t, keep.rownames = TRUE)[]
### drop extraneous rows ###
RT.matrix.t <- RT.matrix.t[-c(1:3), ]
### make data frame again, not DT ###
RT.matrix.t <- as.data.frame(RT.matrix.t)
### make numeric, not factor ###
for(i in 2:ncol(RT.matrix.t)) {
  RT.matrix.t[, i] <- as.numeric(as.character(RT.matrix.t[, i]), digits = 7)
}
### calculate word RT avg ###
RT.matrix.t$RT <- rowMeans(RT.matrix.t[, c(2:198)], na.rm = T)

### subset to only words... ###
RT.matrix.t <- RT.matrix.t[grep("WORD", RT.matrix.t$rn), ]

### pair w/ the freq data ###
stim <- as.data.frame(str_split_fixed(RT.matrix.t$rn, "_", 4))
RT.matrix.t$val <- stim$V2
RT.matrix.t$rn <- stim$V4

### make character, not factor ###
RT.matrix.t$rn <- as.character(RT.matrix.t$rn)

### catie's renaming code ###
### redo this at some point... ###
### forgive me, efficient syntax gods ###
{
RT.matrix.t$rn[RT.matrix.t$rn == "Slide5.jpeg"] <- "STUN"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide6.jpeg"] <- "SNAPPY"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide7.jpeg"] <- "CLUB"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide8.jpeg"] <- "RETREAT"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide9.jpeg"] <- "DISCIPLINE"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide10.jpeg"] <- "TERMINAL"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide11.jpeg"] <- "CRUSH"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide12.jpeg"] <- "BATTER"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide13.jpeg"] <- "PICK"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide14.jpeg"] <- "PLAYER"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide15.jpeg"] <- "POP"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide16.jpeg"] <- "SURVIVAL"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide17.jpeg"] <- "TRICK"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide18.jpeg"] <- "CHECK"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide19.jpeg"] <- "RUSH"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide20.jpeg"] <- "COURT"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide21.jpeg"] <- "EVIL"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide22.jpeg"] <- "SLAVE"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide23.jpeg"] <- "ROTTEN"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide24.jpeg"] <- "CRAPPY"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide25.jpeg"] <- "DECAY"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide26.jpeg"] <- "DEADLY"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide27.jpeg"] <- "CROOK"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide28.jpeg"] <- "VANDAL"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide29.jpeg"] <- "LOVABLE"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide30.jpeg"] <- "MUSIC"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide31.jpeg"] <- "MOTHER"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide32.jpeg"] <- "FOOD"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide33.jpeg"] <- "TREASURED"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide34.jpeg"] <- "FRUITFUL"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide35.jpeg"] <- "COMIC"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide36.jpeg"] <- "LEMONADE"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide37.jpeg"] <- "CLOSE"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide38.jpeg"] <- "REVOLUTION"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide39.jpeg"] <- "FIGHTER"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide40.jpeg"] <- "HAIL"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide41.jpeg"] <- "HANG"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide42.jpeg"] <- "OPERATION"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide43.jpeg"] <- "DIRECT"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide44.jpeg"] <- "SHAKE"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide45.jpeg"] <- "BEAT"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide46.jpeg"] <- "RADICAL"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide47.jpeg"] <- "THICK"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide48.jpeg"] <- "SHRED"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide49.jpeg"] <- "TRAFFIC"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide50.jpeg"] <- "BREAK"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide51.jpeg"] <- "OVERCOME"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide52.jpeg"] <- "CATCH"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide53.jpeg"] <- "FUNERAL"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide54.jpeg"] <- "PISS"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide55.jpeg"] <- "URINE"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide56.jpeg"] <- "WRECK"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide57.jpeg"] <- "USELESS"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide58.jpeg"] <- "LAWSUIT"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide59.jpeg"] <- "MISFIRE"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide60.jpeg"] <- "SPINELESS"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide61.jpeg"] <- "BRAVE"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide62.jpeg"] <- "COMEDIAN"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide63.jpeg"] <- "SUNSET"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide64.jpeg"] <- "AMUSE"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide65.jpeg"] <- "FEMININE"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide66.jpeg"] <- "THINKER"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide67.jpeg"] <- "RABBIT"
RT.matrix.t$rn[RT.matrix.t$rn == "Slide68.jpeg"] <- "GENTLEMAN"
}

### make lower case ###
RT.matrix.t$rn <- tolower(RT.matrix.t$rn)

### rename the column ###
names(RT.matrix.t)[names(RT.matrix.t) == "rn"] <- "Word"

### MERGE MISSING 2 WORDS ###
final <- merge(RT.matrix.t, charac, by = "Word")

### make numeric ###
final$Freq_HAL <- as.numeric(as.character(final$Freq_HAL))
final$Log_Freq_HAL <- as.numeric(as.character(final$Log_Freq_HAL))
final <- subset(final, final$Word != "check")
t.test(subset(final, final$val == "ambiguous")$Freq_HAL, subset(final, final$val %in% c("positive", "negative"))$Freq_HAL)

### pick only ambiguous words ###
amb <- subset(final, final$val == "ambiguous")

### check might be outlier? ###
amb <- subset(amb, amb$Word != "check")
clear <- subset(final, final$val %in% c("positive", "negative"))



cor.test(amb$RT, amb$Freq_HAL, method = "spearman")
### not much here... ###
cor.test(amb$RT, amb$Freq_HAL)
plot(amb$RT, amb$Freq_HAL)

cor.test(v2_data.summary$amw_rate, v2_data.summary$amw_rt)

tempz <- subset(amb, amb$RT <= 780) ### so it looks like player and check have probs w/ frequence and RT too.. 
tempz <- resp.matrix[, c("Word", "words_avg")]
