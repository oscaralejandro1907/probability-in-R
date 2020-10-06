# Title     : TODO
# Objective : TODO
# Created by: oscarhernandezlopez
# Created on: 05/10/20

require(swfscMisc)

n <- 1000
seed <- 27

nonlinearGen <- function (n, seed) {
  a <- 20  #an even number
  b <- 41
  c <- 15 #an odd number
  m <- 2^exp(1)
  x <- seed
  gen_data <- numeric()
  while (length(gen_data)<n){
    x <- (a * x^2 + b * x + c) %% m
    gen_data <- c(gen_data,x)
  }
  return (gen_data/(m-1))
}

nonlgen<-nonlinearGen(n,seed)
uniform.test(hist(nonlgen))