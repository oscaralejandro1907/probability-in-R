# Title     : TODO
# Objective : TODO
# Created by: oscarhernandezlopez
# Created on: 05/01/21

require(ggplot2)


df<-read.csv("data.csv",header = TRUE)

# Convert the variable nCust from a numeric to a factor variable
df$nCust <- as.factor(df$nCust)

#Boxplot of Customers vs Gap.
ggplot(df, aes(x=nCust, y=Gap,fill=nCust)) +
  geom_boxplot() + theme_bw()
#ggsave("boxplot.png")

#Barplot of Customers
ggplot(df, aes(x=nCust, y=Optimal, fill=Optimal)) +
  geom_bar(stat="identity")+theme_minimal()
 # ggsave("barplot.png")

ggplot(df, aes(x=BoundsNoCuts)) +
  geom_histogram(binwidth = 100,color="#56B4E9", fill="#56B4E9") +
  labs(x="Bounds") +
  theme_bw()
#ggsave("histogram.png")





