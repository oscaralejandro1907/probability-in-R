# Title     : TODO
# Objective : TODO
# Created by: oscarhernandezlopez
# Created on: 07/01/21

require(lmtest)

df<-read.csv("data.csv",header = TRUE)
df$Dataset <- as.factor(df$Dataset)

opmod1<-lm(Bool_Opt ~ nCust, data = df)
opmod1

#plot the data
png('pred.png',width = 2100, height = 1800,res = 380)
plot(x=df$nCust, y = df$Bool_Opt,
     xlab = "Number of Customers",
     ylab = "Probability of reaching an Optimal Value",
     pch = 20,
     ylim = c(-0.4,1.4))

#add horizontal dashed lines and text
abline(h=1,lty=2,col="darkred")
abline(h=0,lty=2,col="darkred")
text(43, 0.9, cex = 0.8, "Optimal reached")
text(10, -0.1, cex = 0.8,"Optimal reached")

#add the estimated regression line
abline(opmod1, lwd=1.8, col = "steelblue")
dev.off()

coeftest(opmod1, vcov. = NULL, type = "HC1")

opmod2 <- lm(Bool_Opt ~ nCust + CPU, data = df)


