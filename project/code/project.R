# Title     : TODO
# Objective : TODO
# Created by: oscarhernandezlopez
# Created on: 21/12/20

require(ggplot2)

data<-read.csv("data.csv",header = TRUE)

mycols <- c("#0073C2FF", "#EFC000FF")

#png('pie_dataset.png',width = 2000, height = 1300,res = 450)
ggplot(data, aes(x = "Count", y = Dataset, fill = Dataset)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) +
  theme_bw()
#dev.off()

#png('mosaicplot.png',width = 2000, height = 2000,res = 450)
mosaicplot(~Dataset + Optimal,data = data,
           main = NULL, shade = TRUE)
#dev.off()

test <- chisq.test(table(data$Optimal, data$Dataset))
test$statistic
test$p.value
