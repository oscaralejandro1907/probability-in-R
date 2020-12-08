# Title     : TODO
# Objective : TODO
# Created by: oscarhernandezlopez
# Created on: 04/12/20

data<-read.csv("wall_thickness.csv",header = TRUE)

pop_mean <- mean(data$Wall.Thickness)

#Plot all the observations in the data
png('hist_data.png',width = 2000, height = 1600,res = 250)
hist(data$Wall.Thickness, col = "pink", xlab = "wall thickness of pipes", main = NULL)
abline(v=pop_mean, col="red",lty=1)
dev.off()

#Take sample size=10 for 9000 times
#and calculate the arithmetic mean and plot
n<-9000
s10<-numeric()

for (i in 1:n){
  s10[i] <- mean(sample(data$Wall.Thickness, 10, replace = TRUE))
}

png('hist_sample10.png',width = 2000, height = 1600,res = 250)
hist(s10, col="lightgreen", xlab = "wall thickness of pipes", main = NULL)
abline(v=mean(s10), col = "red")
dev.off()

#Do the same with samples of size 30, 50 and 500

s30 <- numeric()
s50 <- numeric()
s500 <- numeric()

for (i in 1:n){
  s30[i] <- mean(sample(data$Wall.Thickness,30,replace = TRUE))
  s50[i] <- mean(sample(data$Wall.Thickness,30,replace = TRUE))
  s500[i] <- mean(sample(data$Wall.Thickness,30,replace = TRUE))
}

png('hist_sample30.png',width = 2000, height = 1600,res = 325)
hist(s30, col="lightblue", xlab = "wall thickness of pipes", main = NULL)
abline(v=mean(s30), col = "red")
dev.off()

png('hist_sample50.png',width = 2000, height = 1600,res = 325)
hist(s50, col="gray", xlab = "wall thickness of pipes", main = NULL)
abline(v=mean(s50), col = "red")
dev.off()

png('hist_sample500.png',width = 2000, height = 1600,res = 325)
hist(s500, col="orange", xlab = "wall thickness of pipes", main = NULL)
abline(v=mean(s500), col = "red")
dev.off()
