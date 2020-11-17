# Title     : TODO
# Objective : TODO
# Created by: oscarhernandezlopez
# Created on: 15/11/20

df<-read.csv("PG_BEI.DE_2007_2017.csv",header = TRUE)

#1. Proof: Cov[aX+b, cY+d] = acCov[X,Y]
LHS <- numeric()
RHS <- numeric()

for (i in 1:30) {
  a<-runif(1,0,100)
  b<-rnorm(1,0,1)
  c<-rpois(1,5)
  d<-rbinom(1,5,0.4)

  lhs1 <- cov(a * df$PG + b, c * df$BEI.DE + d)
  LHS <- c(LHS,lhs1)
  rhs1 <- a * c * cov(df$PG, df$BEI.DE)
  RHS <- c(RHS,rhs1)
}


#2. Proof: Var[X+Y] = Var[X] + Var[Y] + 2Cov[X,Y]
lhs2 <- var(df$PG + df$BEI.DE)
rhs2 <- var(df$PG) + var(df$BEI.DE) + 2 * cov(df$PG,df$BEI.DE)