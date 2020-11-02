# Title     : TODO
# Objective : TODO
# Created by: oscarhernandezlopez
# Created on: 01/11/20

f<-function (){
  X <-runif(1000,0,1)
  Y<- runif(1000,0,1)
  return(mean(X**Y))
}

samp<-replicate(10000,f())
png('hist.png',width = 2000, height = 1600,res = 200)
hist(samp,main = NULL, xlab = 'Expected Values')
dev.off()