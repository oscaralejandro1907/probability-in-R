require(gutenbergr) #Download books from online library
require(tidytext) #Clean text
require(dplyr)  #Data Manipulation
require(dgof) #For the KS Test

#Load the book: "The Autobiography of Benjamin Franklin"
book<-gutenberg_download(c(148))

words <- book %>% unnest_tokens(word, text, "words")

w <- as.list(words$word)

lchar<- nchar(w)
get_mean<-mean(lchar)
poi <-rpois(length(lchar), get_mean)

hgW <- hist(lchar, plot = FALSE)
hgP <- hist(poi, plot = FALSE)

c1 <- rgb(173,216,230,max = 255, alpha = 80, names = "lt.blue")
c2 <- rgb(255,192,203, max = 255, alpha = 80, names = "lt.pink")

png('hg_Poisson_Words.png',width = 2000, height = 1600,res = 250)
plot(hgW, col = c1, main = NULL, xlab = '',xlim = range(c(lchar,poi)))
plot(hgP,col = c2, add = TRUE, xlim = range(c(lchar,poi)))
legend("topright", c("Word Length", "Poisson Distribution"), fill=c(c1, c2))
dev.off()

#Plotting the results of ecdf
png('KS.png',width = 2000, height = 1600,res = 250)
plot(ecdf(lchar),
     xlim = range(c(lchar,poi)),
     col = "lightblue",
     main = NULL,
     ylab = "Cumulative Probability")
plot(ecdf(poi),
     add = TRUE,
     lty = "dashed",
     col = "red")
legend("right", c("Word Length", "Poisson Distribution"), fill=c('lightblue', 'red'))
dev.off()

#Performing the K-S Test
sink(file = "ksoutput.txt")
ks.test(lchar,poi, alternative = "less")
sink(file = NULL)

#EXPERIMENTS
rep <- 15000
la <- 3
goal <- 1
br <- 8

arr <- numeric()
for (r in 1:rep){
  t <- numeric()
  while (sum(t)<goal){ # until the goal is reached
    t <-c(t,rexp(1,la))  #time between arrivals
  }
  arr <- c(arr,length(t))  #Count of total arrivals in all rep
}
exp_poisson <- rpois(rep,la)
ha<-hist(arr,plot = FALSE)
he<-hist(exp_poisson, plot = FALSE)

png('hg_changing_reps_10000_3.png',width = 2000, height = 1800,res = 325)
plot(ha, col = c1, main = NULL, xlab = '',xlim = range(c(exp_poisson,arr)))
plot(he,col = c2, add = TRUE, xlim = range(c(exp_poisson,arr)))
legend("right", c("Exponencial Sum", "Poisson Distribution"), fill=c(c1,c2))
dev.off()