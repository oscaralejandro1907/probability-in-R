# Title     : Statistical Tests
# Objective : Compute 8 statistical tests in R
# Created by: oscarhernandezlopez
# Created on: 09/10/20

require(ggplot2)


data<-read.csv("IGAE_1.csv",header = TRUE)
df_data<-read.csv("df_data.csv",header = TRUE)

#Extract data
pri <- data[1,2:(ncol(data))]   #https://dzone.com/articles/learn-r-how-extract-rows
sec <- data[2,2:(ncol(data))]
ter <- data[3,2:(ncol(data))]


data_trimmed <- data[,2:13]

df<- t(data[,2:ncol(data)])
dft <- as.data.frame(df)
names(dft) <- c('primary','secondary','tertiary')

df12 <- do.call(rbind, Map(data.frame, A=pri, B=sec))

#One Sample t-Test (for Primary Activities)
sink('t_test.txt',append = TRUE)
t.test(pri, mu=90, conf.int = TRUE)
sink(file = NULL)

#Wilcoxon Signed Rank Test (for Secondary Activities)
sink('wilcoxon.txt',append = TRUE)
wilcox.test(as.numeric(sec), mu=90, conf.int = TRUE)
sink(file = NULL)

#Two Sample t-Test and Wilcoxon Rank Sum Test
sink('wilcoxon_2sample.txt',append = TRUE)
wilcox.test(as.numeric(pri),as.numeric(sec), alternative = 'g', mu=90)
sink(file = NULL)

#Shapiro Test
sink('shapiro.txt',append = TRUE)
shapiro.test(as.numeric(pri))
sink(file = NULL)

png('hist.png',width = 2000, height = 1600, res = 275)
hist(as.numeric(pri), main = NULL, xlab = "Values of Primary Activities")
dev.off()

#Kolmogorov And Smirnov Test
sink('ks.txt',append = TRUE)
ks.test(as.numeric(pri),as.numeric(sec))
sink(file = NULL)


ggplot(df_data[1:660,],aes(x=Activity,
                       y=Value, fill=Activity))+
            geom_violin()+
            geom_jitter(width=0.15, alpha=0.5)+
            theme(legend.position = "none") + theme_bw()
          ggsave("violin.png")

#Fisher’s F-Test
sink('fisher.txt',append = TRUE)
var.test(as.numeric(sec),as.numeric(ter))
sink(file = NULL)

#Chi Squared Test
critical_value <- qchisq(0.95,22)
sink('chi_squared.txt',append = TRUE)
chisq.test(data_trimmed)
cat("Critical Value: ", critical_value)
sink(file = NULL)

#Correlation
sink('correlation.txt',append = TRUE)
cor.test(as.numeric(pri),as.numeric(sec))
sink(file = NULL)

png('scatter.png',width = 2000, height = 1600, res = 450)
ggplot(dft, aes(x=primary, y=secondary)) +
  geom_point() + geom_smooth() + theme_bw()
dev.off()