# Title     : TODO
# Objective : TODO
# Created by: oscarhernandezlopez
# Created on: 08/01/21

df<-read.csv("data.csv",header = TRUE)

df_i <- subset(df, Improvement != "NA" & Optimal != "Yes")

