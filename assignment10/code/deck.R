# Title     : TODO
# Objective : TODO
# Created by: oscarhernandezlopez
# Created on: 05/11/20

library(ggplot2)
library(ggrepel)

#1 p.247 (Cards)

list_of_means <- numeric()

pickCardSim <- function (reps){
  profit <- c(-1,1)
  prob <- c(5/9,4/9)
  winnings <- sample(profit, size = reps,replace = TRUE, prob = prob)
  mean(winnings)
}

n<-10000
for (i in 1:n) {
  list_of_means <- c(list_of_means,pickCardSim(n))
}

dat <- data.frame(test = 1, observed = c(-1/9),
                  exp_min = c(min(list_of_means)), exp_max = c(max(list_of_means)))

png('line_1_247.png',width = 2000, height = 1000,res = 300)
# change the seed number below for slightly different configuration of "observed":
set.seed(126)
ggplot(dat, aes(y = test)) +
  geom_segment(aes(x = exp_min, xend = exp_max, yend = test), colour = "steelblue", size = 2.5, alpha = 0.5) +
  geom_point(aes(x = observed), size = 1) +
  geom_text_repel(aes(x = observed), label = "Theoretical Value") +
  scale_y_continuous("Tests", breaks = 1, minor_breaks = NULL) +
  labs(x = "Values\n(blue line show observed expectation rank)") + theme_bw()
dev.off()
####################################
#deck <- c(1:10)
#
#rules <- function (number){
#  profit <- 0
#  if (number %% 2 == 0) {
#    profit <- profit - 1
#  } else {
#    profit <- profit + 1
#    }
#  return (profit)
#}
#
#exp <- numeric()
#for (i in 1:10000){
#  s <- sample(deck,size = 1,replace = TRUE,prob = rep(1/9,10))
#  exp <- c(exp,rules(s))
#}
#
#print("ReSult")
#sum(exp)
#mean(exp)

#https://homerhanumat.github.io/r-notes/monte-carlo-simulation.html
#https://stats.stackexchange.com/questions/325478/exciting-visualisation-for-observed-value-vs-expected-range