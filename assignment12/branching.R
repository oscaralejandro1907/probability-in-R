# Title     : TODO
# Objective : TODO
# Created by: oscarhernandezlopez
# Created on: 24/11/20

pj <- function (j){
  return (exp(-2)*2^j/factorial(j)) #Probability pj of j offspring
}

d <- pj(0)

for (i in 0:100){
  d_new <- 0
  for (j in 0:100){  #Sumation of d for j offspring
  d_new <- d_new + pj(j)*(d^j)
  }
  d <- d_new
}

