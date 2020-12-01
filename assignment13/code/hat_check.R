# Title     : TODO
# Objective : TODO
# Created by: oscarhernandezlopez
# Created on: 30/11/20

order <- seq(1,20)

give_hand <- function (){
  x <- sample(20,20)
  is.element(TRUE, x==order)
}

r <- replicate(10000,give_hand())
sum(r==FALSE)/10000

1/exp(1)

