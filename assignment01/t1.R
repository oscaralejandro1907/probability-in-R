# Title     : TODO
# Objective : TODO
# Created by: oscarhernandezlopez
# Created on: 02/09/20

dat<-read.csv('/Users/oscarhernandezlopez/Dropbox/R/T1-Probabilidades/IGAE_1.csv')

dat_trimmed <- dat[,2:(ncol(dat))]
dat_transpose <- t(dat_trimmed)

png('boxplot_activities.png',width = 2000, height = 1600, res = 300)
boxplot(dat_transpose)
dev.off()

primary<-read.csv('/Users/oscarhernandezlopez/Dropbox/R/T1-Probabilidades/primary.csv')
primarytrimmed<-primary[,2:(ncol(primary))]

primarytranspose <- t(primarytrimmed)

png('boxplot_month1.png',width = 1366, height = 768,res = 100)
boxplot(primarytrimmed)
dev.off()

png('boxplot_year1.png',width = 1366, height = 768, res = 100)
boxplot(primarytranspose)
dev.off()








