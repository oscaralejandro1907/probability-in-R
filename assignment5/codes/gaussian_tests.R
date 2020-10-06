# Title     : TODO
# Objective : TODO
# Created by: oscarhernandezlopez
# Created on: 04/10/20

n <- 1000
mu <- 400
dev <- 20

#Testing Independency
gaussianNoise1 <- function (mu, sigma) { #Box-Muller Transform
  u1 <- runif(1)
  u2 <- runif(1)
  while (u1<u2){
    u2 <- runif(1)
  }
  z0 <- sqrt(-2*log(u1)) * cos(2*pi*u2)
  z1 <- sqrt(-2*log(u1)) * sin(2*pi*u2)
  pair1 <- c(z0,z1)
  return (sigma * pair1 + mu) #Return a pair (zo,z1)
}
pair1 <- gaussianNoise1(mu,dev)
for (i in 1:1000) {
  pair1 <- c(pair1,gaussianNoise1(mu,dev))
}

gaussianNoise2 <- function (mu, sigma) { #Box-Muller Transform
  u1 <- runif(1)
  u2 <- u1-runif(1)

  z0 <- sqrt(-2*log(u1)) * cos(2*pi*u2)
  z1 <- sqrt(-2*log(u1)) * sin(2*pi*u2)
  pair2 <- c(z0,z1)
  return (sigma * pair2 + mu) #Return a pair (zo,z1)
}
pair2 <- gaussianNoise2(mu,dev)
for (i in 1:1000){
  pair2 <- c(pair2,gaussianNoise2(mu,dev))
}

gaussianNoise3 <- function (mu, sigma) { #Box-Muller Transform
  u1 <- runif(1)
  u2 <- runif(1,1,10)-2*u1

  z0 <- sqrt(-2*log(u1)) * cos(2*pi*u2)
  z1 <- sqrt(-2*log(u1)) * sin(2*pi*u2)
  pair3 <- c(z0,z1)
  return (sigma * pair3 + mu) #Return a pair (zo,z1)
}
pair3 <- gaussianNoise3(mu,dev)
for (i in 1:1000){
  pair3 <- c(pair3,gaussianNoise3(mu,dev))
}

gaussianNoise4 <- function (mu, sigma) { #Box-Muller Transform
  u1 <- runif(1)
  u2 <- runif(1)
  u2<- (u2-u1)/u1

  z0 <- sqrt(-2*log(u1)) * cos(2*pi*u2)
  z1 <- sqrt(-2*log(u1)) * sin(2*pi*u2)
  pair4 <- c(z0,z1)
  return (sigma * pair4 + mu) #Return a pair (zo,z1)
}
pair4 <- gaussianNoise3(mu,dev)
for (i in 1:1000){
  pair4 <- c(pair4,gaussianNoise4(mu,dev))
}

gaussianNoise5 <- function (mu, sigma) { #Box-Muller Transform
  u1 <- runif(1)
  u2 <- 2*u1

  z0 <- sqrt(-2*log(u1)) * cos(2*pi*u2)
  z1 <- sqrt(-2*log(u1)) * sin(2*pi*u2)
  pair5 <- c(z0,z1)
  return (sigma * pair5 + mu) #Return a pair (zo,z1)
}
pair5 <- gaussianNoise5(mu,dev)
for (i in 1:1000){
  pair5 <- c(pair5,gaussianNoise5(mu,dev))
}

gaussianNoise6 <- function (mu, sigma) { #Box-Muller Transform
  u1 <- runif(1)
  u2 <- runif(1)
  u2 <- u1*u2

  z0 <- sqrt(-2*log(u1)) * cos(2*pi*u2)
  z1 <- sqrt(-2*log(u1)) * sin(2*pi*u2)
  pair6 <- c(z0,z1)
  return (sigma * pair6 + mu) #Return a pair (zo,z1)
}
pair6 <- gaussianNoise6(mu,dev)
for (i in 1:1000){
  pair6 <- c(pair6,gaussianNoise6(mu,dev))
}

gaussianNoise <- function (mu, sigma) { #Box-Muller Transform
  u <- runif(2)
  z0 <- sqrt(-2*log(u[1])) * cos(2*pi*u[2])
  z1 <- sqrt(-2*log(u[1])) * sin(2*pi*u[2])
  pair <- c(z0,z1)
  return (sigma * pair + mu) #Return a pair (zo,z1)
}
pair <- gaussianNoise(mu,dev)
for (i in 1:1000){
  pair <- c(pair,gaussianNoise(mu,dev))
}
shapiro.test(pair)