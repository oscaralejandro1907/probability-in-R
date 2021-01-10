# Title     : TODO
# Objective : TODO
# Created by: oscarhernandezlopez
# Created on: 04/11/20

require(ggplot2)

#18 p.249 Keys
tryKeySim <- function (n=1){
  tryK <- sample(c(0:5), n, replace = FALSE,
                prob = c(1/6,1/6,1/6,1/6,1/6,1/6))
}

res<-numeric()
for (i in 1:120) {
  res <- c(res,tryKeySim(1))
}

mean(res)

png('hist_18_249.png',width = 2000, height = 1600,res = 300)
hist(res, seq(0,5,by=0.25), main = NULL, xlab ="number of tries")
dev.off()

#How many Monte Carlo experiments are enough?
B <- 10^seq(1,5, len = 100)

compute_prob <- function (B,n=1){
  r <- replicate(B, tryKeySim(n))
  mean(r)
}

prob <- sapply(B,compute_prob)
png('plot_MC.png',width = 2000, height = 1600,res = 300)
qplot(log10(B), prob, geom = "line") + labs(y = "Expected Value", x = "Repetitions (log10)") + theme_bw()
dev.off()