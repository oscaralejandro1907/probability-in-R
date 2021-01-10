# Title     : TODO
# Objective : TODO
# Created by: oscarhernandezlopez
# Created on: 03/01/21

require(chisq.posthoc.test)

data<-read.csv("data.csv",header = TRUE)

t <- table(data$Optimal, data$nCust)
chi2 <- chisq.test(t)

ph <- chisq.posthoc.test(t, method = "bonferroni")
ph

