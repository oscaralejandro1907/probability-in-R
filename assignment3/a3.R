require(gutenbergr) #Download books from online library
require(tidytext) #Clean text
require(dplyr)  #Data Manipulation
require(textshape)
require(tokenizers)

library(fitdistrplus)

#Load the book: "The Autobiography of Benjamin Franklin"
book<-gutenberg_download(c(148))

#Variables used:
words <- book %>% unnest_tokens(word, text, "words")  #contains words
sentences <- book %>% unnest_tokens(sentence, text, "sentences")
paragraphs <- book %>% unnest_tokens(paragraph, text, "paragraphs")

#See how many words have been written for the next "I" (Geometric)
w <- as.list(words$word)
checker<-sapply(w, function(x) sum(unlist(tokenize_words(x)) %in% c('i')))
results <- numeric()
counter <- 0
for (elem in checker){
  counter <- counter+1
  if(elem == 1){
    results <- c(results,counter)
    counter <- 0
  }
}
png('words_forI.png',width = 1366, height = 768,res = 150)
hist(results,breaks = 60, main = NULL, xlab = "Words written")
dev.off()

#See the number of "I" present in all (n) sentences (Binomial)
df <- as.data.frame(sentences$sentence)
stol <- lapply(as.list(1:dim(df)[1]), function(x) df[x[1],]) #Dataframe rows to lists
success<-sapply(stol, function(x) sum(unlist(tokenize_words(x)) %in% c('i')))
png('number_of_I.png',width = 1366, height = 768,res = 150)
hist(success, breaks = 10,xlim = c(0,6), main = NULL, xlab = "Amount of I")
dev.off()

#Word lenghts
lchar<- nchar(w)
png('word_lengths.png',width = 1366, height = 768,res = 150)
hist(lchar,breaks = 23,xlim = c(0,10), main = NULL, xlab = "Word length")
dev.off()

#Amount of words per paragraph
p <- as.list(paragraphs$paragraph)
wpp<-lengths(gregexpr("\\W+", p)) + 1
wppFiltered <- wpp[wpp >= 30]
png('word_amount.png',width = 1366, height = 768,res = 150)
hist(wppFiltered, breaks = 20, main = NULL, xlab = "Amount of words")
dev.off()


png('_fitofwordlenght.png',width = 1366, height = 768,res = 150)
descdist(lchar, boot = 1000, method = "sample", discrete = TRUE)
dev.off()

png('_fitofwpp.png',width = 1366, height = 768,res = 150)
descdist(wppFiltered, boot = 1000, method = "sample", discrete = TRUE)
dev.off()