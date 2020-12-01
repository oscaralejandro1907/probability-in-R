# Title     : TODO
# Objective : TODO
# Created by: oscarhernandezlopez
# Created on: 30/11/20

dvalues <- 1:6
n <- 10

results <- sample( x = dvalues, size = n, replace = TRUE)

ns <- seq(from = 10, to = 50000, by = 10)

means <- list()
length(means) <- length(ns)

for (i in 1:length(ns)) {
     means[[i]] <- mean(sample( x = dvalues, size = ns[i], replace = TRUE))
}

png('die_plot.png',width = 2000, height = 1500,res = 250)

plot(ns, unlist(means),
     ylab = "Sample Mean",
     xlab = "Sample Size",
     pch = 16,
     cex = .6)
abline(h = 3.5, col = "red")

dev.off()