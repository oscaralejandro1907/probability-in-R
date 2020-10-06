# Title     : TODO
# Objective : TODO
# Created by: oscarhernandezlopez
# Created on: 05/10/20

require(swfscMisc)
require(ggplot2)

n <- 1000
mu <- 10
std_dev <- 2
seed <- 27

linearCongruentialGen <- function (n, seed) {
  a <- 2505
  c <- 5005
  m <- 15145
  x <- seed
  gen_data <- numeric()
  while (length(gen_data)<n){
    x <- (a * x + c) %% m
    gen_data <- c(gen_data,x)
  }
  return (gen_data/(m-1)) #Return a seed generation pseudo-random numbers
}

lcr <- linearCongruentialGen(n,seed)
sink('unif_test.txt',append = TRUE)
uniform.test(hist(lcr))
sink(file = NULL)

gaussian_lcr <- function (mu, sigma, list) {
  z0_list <- sqrt(-2*log(list[i])) * cos(2*pi*list[i+1])
  z1_list <- sqrt(-2*log(list[i])) * sin(2*pi*list[i+1])
  pair <- c(z0_list,z1_list)

  return (sigma * pair + mu)
}

dat_z0_lcr <- numeric()
#dat_z1_lcr <- numeric()

for (i in 1:n-1){
  dat_z0_lcr <- c(dat_z0_lcr,gaussian_lcr(mu,std_dev,lcr))
  #dat_z1_lcr <- c(dat_z1_lcr,gaussian_lcr(mu,std_dev,lcr))
}

df_lcr <- as.data.frame(dat_z0_lcr)

# Histogram with density plot
png('hist2.png',width = 1366, height = 768,res = 200)
breaks <- pretty(range(dat_z0_lcr), n = nclass.FD(dat_z0_lcr), min.n = 1)
ggplot(df_lcr, aes(x=dat_z0_lcr)) +
 geom_histogram(aes(y=..density..), colour="black", fill="white",binwidth = breaks[2]-breaks[1])+
 geom_density(alpha=.2, fill="#FF6666")+ theme_bw()
dev.off()

shapiro.test(dat_z0_lcr)