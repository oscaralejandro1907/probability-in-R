# Title     : TODO
# Objective : TODO
# Created by: oscarhernandezlopez
# Created on: 30/11/20

require(ggplot2)

returns <- 0.1/100
sharpe <- 2
std <- returns*sqrt(252)/sharpe
days <- 40

ret <- numeric()
sharpes <- numeric()

for (i in 1:2500){
  returns_d <- rnorm(days,returns,std)
  ret <- c(ret,returns_d)
  sharpes <- c(sharpes,sqrt(252)*mean(returns_d)/sd(returns_d))
}

png('hist2500.png',width = 2000, height = 1500,res = 250)

df <- as.data.frame(sharpes)
ggplot(data = df, aes(df$sharpes)) + geom_histogram(fill="#56B4E9") +
  xlab("Sharpe Ratio") +
  theme_bw()

dev.off()