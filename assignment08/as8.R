# Title     : TODO
# Objective : TODO
# Created by: oscarhernandezlopez
# Created on: 26/10/20

require(caret)

data<-read.csv("covid19_confirmed_mx.csv",header = TRUE)

population <- 126200000
cases <- sum(data[,ncol(data)])

pA <- cases / population
pnotA <- 1 - pA
pB_A <- 0.99
pB_notA <- 0.01

p_bayes <- (pA * pB_A)/((pA * pB_A) + (pnotA*pB_notA))


#Sample Size
p <- seq(from = 0.007, to = 0.012, by = 0.0005)

xval <- function (p){
  acc <- 0.99
  spec <- 0.98
  x <- seq(from =40, to=10000, by = 10)
  for (i in x){
    pc <- 1 - (1-p)^i
    prob <- (acc*pc)/((acc*pc) + (1-pc)*(1-spec))
    if(prob>=0.99){
      break
    }
  }
  i
}
sapply(p,xval)

png('sample_plot.png',width = 2000, height = 1400, res = 275)
plot(sapply(p,xval), x = p, ylab = 'sample size (x)')
dev.off()

#Prevalence
lvs <- c("no_covid", "covid")
truth <- factor(rep(lvs, times = c(1501864, 1069392)),
                levels = rev(lvs))

pred <- factor(
               c(
                 rep(lvs, times = c(429104, 1072760)),
                 rep(lvs, times = c(178232, 891160))),
               levels = rev(lvs))

xtab <- table(pred, truth)

sensitivity(pred, truth)
sensitivity(xtab)
posPredValue(pred, truth)
posPredValue(pred, truth, prevalence = 0.55)

specificity(pred, truth)
negPredValue(pred, truth)
negPredValue(xtab)
negPredValue(pred, truth, prevalence = 0.55)


prev <- seq(0.001, .99, length = 20)
npvVals <- ppvVals <- prev  * NA
for(i in seq(along = prev))
  {
    ppvVals[i] <- posPredValue(pred, truth, prevalence = prev[i])
    npvVals[i] <- negPredValue(pred, truth, prevalence = prev[i])
  }

png('plot.png',width = 2000, height = 1400, res = 275)
plot(prev, ppvVals,
     ylim = c(0, 1),
     type = "l",
     ylab = "Predicted Values",
     xlab = "Prevalence (i.e. prior)")
points(prev, npvVals, type = "l", col = "red")
abline(h=sensitivity(pred, truth), lty = 2)
abline(h=specificity(pred, truth), lty = 2, col = "red")
legend(.00, .7,
       c("ppv", "npv", "sens", "spec"),
       col = c("black", "red", "black", "red"),
       lty = c(1, 1, 2, 2))
dev.off()