#install.packages("mirt")
library(mirt)

output <- mirt::mirt(temp[, c(4:163)], 3, itemtype = "ideal")

extra.temp <- temp 
temp[, c(52:83)]
#install.packages("polycor")
library(polycor)
output <- hetcor(temp[, c(52:83)])

items.resp <- temp[, c(52:83)]

items.resp <- sapply(items.resp, as.factor)

output <- hetcor(items.resp)

het.mat <- output$correlations

factanal(covmat = het.mat, factors = 3, rotation = "promax")
?factanal
