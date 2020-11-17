# Title     : TODO
# Objective : TODO
# Created by: oscarhernandezlopez
# Created on: 16/11/20

require(ggplot2)
require(distr)
require(scales)

amz <- read.csv("AMZN.csv",header = TRUE)
tsl <- read.csv("TSLA.csv",header = TRUE)

#Boxplot
L <- list(Amazon = amz$Adj.Close,
          Tesla = tsl$Adj.Close)
result <- stack(L)
names(result) <- c("Value", "Source")

png('boxplot_stocks.png',width = 2000, height = 1300,res = 350)
box_plot <- ggplot(result, aes(x = Source, y = Value, fill=Source))
box_plot + geom_boxplot() + theme_bw()
dev.off()

#Densplot
myData <- data.frame(Amazon=amz$Adj.Close,
                     Tesla=tsl$Adj.Close)

dens <- apply(myData, 2, density)
png('dens.png',width = 2000, height = 1300,res = 350)
plot(NA, xlim=range(sapply(dens, "[", "x")),
     ylim=range(sapply(dens, "[", "y")),xlab = "Adjusted Close Price", ylab = "Density")
mapply(lines, dens, col=1:length(dens))
labs(y="saasg",x="agsag")
legend("topright", legend=names(dens), fill=1:length(dens))
dev.off()

#Convolution
X <- amz$Adj.Close
Y <- tsl$Adj.Close

Z <- as.data.frame(convolve(X, Y, type = "circular"))

png('hist_stocks.png',width = 2000, height = 1300,res = 350)
ggplot(Z, aes(x=convolve(X, Y, type = "circular")) ) +
  geom_histogram(aes(y=..density..), colour="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666") + theme_bw() +
  labs(x="Convolution of Amazon and Tesla's Adjusted Close Price") +
  scale_x_continuous(labels = comma)
dev.off()