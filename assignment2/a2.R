require(gutenbergr) #Download books from online library
require(tidytext) #Clean text
require(dplyr)  #Data Manipulation

library(textshape)

#Load the book: "The Autobiography of Benjamin Franklin"
book<-gutenberg_download(c(148))

#Variables used:
letters <- book %>% unnest_tokens(chars, text, "characters")  #contains letters
words <- book %>% unnest_tokens(word, text, "words")  #contains words

#Work with Letters:
png('barplot_letters.png',width = 2100, height = 768,res = 180)
barplot(table(letters$chars)) #Barplot of letters
dev.off()

freq_l <- as.data.frame(table(letters$chars))
names(freq_l) <- c('Letter', 'FrequencyL')

rl <- freq_l[freq_l$FrequencyL>500,]  #Filter relevant letters
png('barplot_relevant_letters.png',width = 1366, height = 768,res = 150)
barplot(rl$FrequencyL, names.arg = rl$Letter)
dev.off()

rlo <- rl[order(rl$FrequencyL, decreasing=TRUE),]
png('barplot_relevant_letters_ordered.png',width = 1366, height = 768,res = 150)
barplot(rlo$FrequencyL, names.arg = rlo$Letter)
dev.off()

#Work with Words:
png('barplot_words.png',width = 1366, height = 768,res = 150)
barplot(sort(table(words$word), decreasing = TRUE)) #Barplot of words
dev.off()

freq_w <- as.data.frame(table(words$word))
names(freq_w) <- c('Word', 'FrequencyW')

muw <- freq_w[freq_w$FrequencyW > 100,]
png('barplot_most_used_words.png',width = 2100, height = 768,res = 180)
barplot(muw$FrequencyW, names.arg = muw$Word)
dev.off()

rw <- muw[muw$FrequencyW < 200,]
png('barplot_relevant_words.png',width = 2048, height = 768,res = 150)
barplot(rw$FrequencyW, names.arg = rw$Word)
dev.off()

rw_o <- rw[order(rw$FrequencyW, decreasing=TRUE),]
png('barplot_relevant_words_ordered.png',width = 2166, height = 768,res = 180)
barplot(rw_o$FrequencyW, names.arg = rw_o$Word)
barplot(rw_o$FrequencyW, names.arg = rw_o$Word, log = 'y')
dev.off()

places<-as.data.frame(table(grep("york|london|boston|newport|philadelphia|paris", words$word,value=TRUE)))
names(places) <- c('Place', 'FrequencyP')
rp <- places[places$FrequencyP>2,]  #Filter relevant places
png('barplot_places.png',width = 1366, height = 768,res = 150)
barplot(rp$FrequencyP, names.arg = rp$Place)
dev.off()