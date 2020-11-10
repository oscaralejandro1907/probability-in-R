# Title     : TODO
# Objective : TODO
# Created by: oscarhernandezlopez
# Created on: 05/11/20

#3 p.278 (Light Bulb)
f <- function (x) {
  la <- 0.05
  return(la^2*x*exp(-la*x))
}

png('dens_3_278.png',width = 2000, height = 1600,res = 300)
curve(f(x),
      from = 0,
      to = 200, ylab = "Density",
      xlab = "Hours")
dev.off()








g <- function (x) x*f(x) #Expected value
h <- function (x) x^2*f(x) #Variance

calc <- integrate(h,lower = 0,
                  upper = Inf)$value