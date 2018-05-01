tweets <- read.csv("Data/labeled_data.csv", stringsAsFactors = FALSE)
head(tweets)

class(tweets$class)
tweets$class <- as.character(tweets$class)

tweets$class[tweets$class == 0] <- "suspicious"
tweets$class[tweets$class == 1 | tweets$class == 2] <- "safe"

summary(tweets$class)
tweets$class <- as.factor(tweets$class)
table(tweets$class)

library(tm)
library(wordcloud)
library(SnowballC)
library(RColorBrewer)


tweetsFinal <- tweets[, 6:7]
colnames(tweetsFinal) <- c("suspicious", "text")
finalDF <- rbind(spam, deceptiveOpinion, tweetsFinal)

finalCorpus <- Corpus(VectorSource(finalDF$text))
finalCorpus <- tm_map(finalCorpus, tolower)
finalCorpus <- tm_map(finalCorpus, removeNumbers)
finalCorpus <- tm_map(finalCorpus, removePunctuation)
finalCorpus <- tm_map(finalCorpus, removeWords, stopwords("english"))
finalCorpus <-
  tm_map(finalCorpus, removeWords, c(as.character(stop_words)))
finalCorpus <- tm_map(finalCorpus, stemDocument)

#wordcloud(tweetsCorpus, min.freq = 20,random.order = FALSE,random.color = FALSE,colors = brewer.pal(9,"Spectral"))

frequencies = DocumentTermMatrix(finalCorpus)
frequencies
inspect(frequencies[1000:1005, 505:515])
findFreqTerms(frequencies, lowfreq = 20)

sparse = removeSparseTerms(frequencies, 0.995)
sparse

tweetsSparse <- as.data.frame(as.matrix(sparse))
colnames(tweetsSparse) <- make.names(colnames(tweetsSparse))
tweetsSparse$suspicious <- finalDF$class

class(sparse)

set.seed(100)
wordcloud(
  finalCorpus,
  min.freq = 400,
  random.order = FALSE,
  random.color = FALSE,
  colors = brewer.pal(9, "Spectral"),
  scale = c(3, 1)
)


library(tidyverse)

finalDFTokens <-
  finalDF %>% unnest_tokens(word, text) %>% anti_join(stop_words) %>% count(suspicious, word, sort = TRUE) %>% ungroup()
tail(finalDFTokens)
head(finalDFTokens)

totalWords <-
  finalDFTokens %>% group_by(suspicious) %>% summarize(total = sum(n))
finalDFTokens <- left_join(finalDFTokens, totalWords)
finalDFTokens

finalDFTokens <- finalDFTokens %>% bind_tf_idf(word, suspicious, n)
finalDFTokens

finalDFTokens %>% select(-total) %>% arrange(desc(tf_idf))

finalDFTokens %>% arrange(desc(tf_idf)) %>% mutate(word = factor(word, levels = rev(unique(word)))) %>% group_by(suspicious) %>% top_n(40) %>% ungroup %>% ggplot(aes(word, tf_idf, fill = suspicious)) +
  geom_col(show.legend = FALSE) + labs(x = NULL, y = "tf-idf") + facet_wrap( ~
                                                                               suspicious, ncol = 2, scales = "free") + coord_flip()
library(reshape2)

par(bg="black")
set.seed(100)

finalDFTokens %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment, sort = TRUE) %>% acast(word ~
                                                                                                         sentiment, value.var = "n", fill = 0) %>% comparison.cloud(colors = brewer.pal(9, "RdBu"), max.words = 100, scale = c(5, 0.5))
title(main = "Most frequently occurring words, classified by sentiment",col.main = "white",line = 3,cex.main = 2)
