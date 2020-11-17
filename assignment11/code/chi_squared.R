# Title     : TODO
# Objective : TODO
# Created by: oscarhernandezlopez
# Created on: 13/11/20

require(ggplot2)
require(dplyr)

data<-read.csv("data.csv",header = TRUE)

mycols <- c("#0073C2FF", "#EFC000FF")

png('pie_dataset.png',width = 2000, height = 1300,res = 350)
ggplot(data, aes(x = "Count", y = Dataset, fill = Dataset)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + theme_bw()
dev.off()

png('pie_optimal.png',width = 2000, height = 1300,res = 350)
ggplot(data, aes(x = "Count", y = Optimal, fill = Optimal)) +
  geom_bar(width = 1, stat = "identity", color = "white") +
  coord_polar("y", start = 0) + theme_bw()
dev.off()

tbl <- table(data$Optimal, data$Dataset)
sink(file = "ctable1.txt")
prop.table(tbl,1) %>% round(3)  #Sum 1 by rows
sink(file = NULL)

sink(file = "chisq.txt")
chisq.test(tbl)
sink(file = NULL)

sink(file = "ctableexpected.txt")
chisq.test(tbl)$expected #https://www.youtube.com/watch?v=LnaeG0MzQVw&ab_channel=UTSSC
sink(file = NULL)