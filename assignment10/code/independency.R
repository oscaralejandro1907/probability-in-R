# Title     : TODO
# Objective : TODO
# Created by: oscarhernandezlopez
# Created on: 10/11/20

#
require(ggplot2)

E_xy <- numeric()
Ex_Ey <- numeric()

for (i in 1:10000){
  x <- sample(2:12,size = 10000,replace = TRUE, prob = table(outer(1:6,1:6,"+")) / 36)
  y <- sample(-5:5,size = 10000,replace = TRUE, prob = table(outer(1:6,1:6,"-")) / 36)

  E_xy <- c(E_xy,mean(x*y))
  Ex_Ey <- c(Ex_Ey,mean(x)*mean(y))
}

L <- list(e_xy = E_xy,
          ex_ey = Ex_Ey)
result <- stack(L)
names(result) <- c("Value", "Source")

png('boxplot1.png',width = 2000, height = 1400,res = 300)
box_plot <- ggplot(result, aes(x = Source, y = Value, fill=Source))
box_plot + geom_boxplot() + scale_fill_discrete(name = "Source", labels = c("E(XY)", "E(X)E(Y)")) + theme_bw()
dev.off()