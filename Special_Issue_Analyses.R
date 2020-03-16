### set directory ###
setwd("~/Documents/Nick-Grad/Neta_Lab/words/")

library(data.table)


### Neeed to clean before sourcing... ###
### Just run lines 1:535 for now... ###
# source(study1_redo_data_cleaning.R)

### assess normality ###
shapiro.test(full$sur_rate) ## non-normal
shapiro.test(full$amb_rate) # normal
shapiro.test(full$amw_rate) # normal
shapiro.test(full$age) # non-normal

### quick correlations/regressions ###
cor.test(full$sur_rate, full$amw_rate, use = "complete.obs", method = "spearman")
cor.test(full$amb_rate, full$amw_rate, use = "complete.obs", method = "pearson")
summary(lm(amb_rate ~ amw_rate, full))

cor.test(full$sur_rate, full$amb_rate, use = "complete.obs", method = "spearman")


ggplot(data = full, mapping = aes(x = amb_rate, y = amw_rate)) +
  geom_point() +
  geom_abline(intercept = .26110, slope = .47033)


### read in Elex data ###
charac <- read_csv("Items_Elex.csv")






### correlations controlling for age and sex ###
pcor.test(full$sur_rate, full$amw_rate, c(full$age, full$mal0fem1))
pcor.test(full$amb_rate, full$amw_rate, c(full$age, full$mal0fem1))
pcor.test(full$sur_rate, full$amb_rate, c(full$age, full$mal0fem1))