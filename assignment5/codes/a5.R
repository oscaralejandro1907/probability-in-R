# Title     : TODO
# Objective : TODO
# Created by: oscarhernandezlopez
# Created on: 01/10/20

require(swfscMisc)
require(ggplot2)

#Parameters
n <- 1000
mu <- 10
std_dev <- 2
seed <- 27

data_Norm <- rnorm(n,mu,std_dev)

gaussianNoise <- function (mu, sigma) { #Box-Muller Transform
  u <- runif(2)
  z0 <- sqrt(-2*log(u[1])) * cos(2*pi*u[2])
  z1 <- sqrt(-2*log(u[1])) * sin(2*pi*u[2])
  pair <- c(z0,z1)
  return (sigma * pair + mu) #Return a pair (zo,z1)
}

linearCongruentialGen <- function (n, seed) {
  a <- 11551
  c <- 27077
  m <- 39709
  x <- seed
  gen_data <- numeric()
  while (length(gen_data)<n){
    x <- (a * x + c) %% m
    gen_data <- c(gen_data,x)
  }
  return (gen_data/(m-1)) #Return a seed generation pseudo-random numbers
}

lcr <- linearCongruentialGen(n,seed)

gaussian_lcr <- function (mu, sigma) {
  z0_lcr <- sqrt(-2*log(lcr[1])) * cos(2*pi*lcr[2])
  z1_lcr <- sqrt(-2*log(lcr[1])) * sin(2*pi*lcr[2])
  pair <- c(z0_lcr,z1_lcr)

  return (sigma * pair + mu)
}

#Experiments Section
data_Z0 <- numeric()
data_Z1 <- numeric()

for (i in 1:n){
  data_Z0 <- c(data_Z0,gaussianNoise(mu,std_dev)[1])
  data_Z1 <- c(data_Z1,gaussianNoise(mu,std_dev)[2])
}

dat <- data.frame(dens = c(data_Z0, data_Z1,data_Norm)
                  , lines = rep(c("Z0", "Z1","Norm"), each = 100))
#Plot.
png('densplot1.png',width = 1366, height = 768,res = 200)
myplot <- ggplot(dat, aes(x = dens, fill = lines)) + geom_density(alpha = 0.5)
myplot + labs(y= "Density", x = "Values") + theme_bw()
dev.off()

L <- list(Z0 = data_Z0,
          Z1 = data_Z1,
          ZNorm = data_Norm)
result <- stack(L)
names(result) <- c("Value", "Source")

png('boxplot1.png',width = 1366, height = 768,res = 200)
box_plot <- ggplot(result, aes(x = Source, y = Value, fill=Source))
box_plot + geom_boxplot() + theme_bw()
dev.off()

# Compute the analysis of variance
res.aov <- aov(Value ~ Source, data = result)
# Summary of the analysis
sink('aov.txt',append = TRUE)
summary(res.aov)
sink(file = NULL)