# Title     : TODO
# Objective : TODO
# Created by: oscarhernandezlopez
# Created on: 09/11/20

require(ggplot2)

f<-function (){
  X <-runif(1000,0,1)
  Y<- runif(1000,0,1)
  mean(X^Y)
}

samp<-replicate(10000,f())
png('hist12_280.png',width = 2000, height = 1600,res = 200)
hist(samp,main = NULL, xlab = 'Expected Values')
dev.off()

B <- 10^seq(1,5, len = 100)

compute_prob <- function (B){
  r <- replicate(B, f())
  mean(r)
}

prob <- sapply(B,compute_prob)
png('plot_MClog2.png',width = 2000, height = 1600,res = 300)
qplot(log10(B), prob, geom = "point") + labs(y = "Expected Value", x = "Repetitions (log10)") + theme_bw()
dev.off()