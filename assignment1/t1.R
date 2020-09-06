# Title     : TODO
# Objective : TODO
# Created by: oscarhernandezlopez
# Created on: 02/09/20

dat<-read.csv('/Users/oscarhernandezlopez/Dropbox/R/T1-Probabilidades/IGAE_1.csv')

dat_trimmed <- dat[,2:(ncol(dat))]
dat_transpose <- t(dat_trimmed)

png('boxplot_t1.png')
boxplot(dat_transpose)
dev.off()


