require(tm)
require(wordcloud)
require(RColorBrewer)
require(ggplot2)
require(SnowballC)
require(biclust)
require(cluster)
require(igraph)
require(fpc)
require(caret)
require(openNLP)
require(NLP)
require(ngram)
require(qdap)
require(dynamicTreeCut)

set.seed(1234)

blogs <- readLines("en_US/en_US.blogs.txt")
news <- readLines("en_US/en_US.news.txt", skipNul= TRUE)
twits <- readLines("en_US/en_US.twitter.txt", skipNul = TRUE)

blogs1 <- paste(blogs, collapse = " ")
news1 <- paste(news, collapse = " ")
twits1 <- paste(twits, collapse = " ")

#samples <- c(sample(blogs, 800000), sample(news, 70000), sample(twits, 200000))
samples <- c(sample(blogs, 80), sample(news, 70), sample(twits, 20))

samples <- iconv(samples, "UTF-8", "latin1")

write.table(samples,"text_US.txt",row.names=FALSE,col.names=FALSE,quote=FALSE,append=FALSE)

DS <- data.frame(samples)
docs <- Corpus(DataframeSource(DS))

docs <- tm_map(docs, removePunctuation)   
docs <- tm_map(docs, removeNumbers)   
docs <- tm_map(docs, content_transformer(tolower)   )
docs <- tm_map(docs, removeWords, stopwords("english")) 
docs <- tm_map(docs, stripWhitespace)   
docs <- tm_map(docs, stemDocument)   
docs <- tm_map(docs, PlainTextDocument)   

dtm <- DocumentTermMatrix(docs)   
tdm <- TermDocumentMatrix(docs)

require(RWeka)

OnegramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 1, max = 1))
tdmn1 <- TermDocumentMatrix(docs, control = list(tokenize = OnegramTokenizer))
tdmn1 <- removeSparseTerms(tdmn1, 0.75)

TwogramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 2, max = 2))
tdmn2 <- TermDocumentMatrix(docs, control = list(tokenize = TwogramTokenizer))
tdmn2 <- removeSparseTerms(tdmn2, 0.75)

ThreegramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 3, max = 3))
tdmn3 <- TermDocumentMatrix(docs, control = list(tokenize = ThreegramTokenizer))
tdmn3 <- removeSparseTerms(tdmn3, 0.75)

FourgramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 4, max = 4))
tdmn4 <- TermDocumentMatrix(docs, control = list(tokenize = FourgramTokenizer))
tdmn4 <- removeSparseTerms(tdmn4, 0.75)

FivegramTokenizer <- function(x) NGramTokenizer(x, Weka_control(min = 5, max = 5))
tdmn5 <- TermDocumentMatrix(docs, control = list(tokenize = FivegramTokenizer))
tdmn5 <- removeSparseTerms(tdmn5, 0.75)


tdmn1Matrix <- as.matrix(tdmn1)
tf1grams <- rowSums(tdmn1Matrix)
tf1sorted <- sort(tf1grams, decreasing = TRUE)
len1 <- length(tf1sorted)
indexes1Gram <- 1:len1
names(indexes1Gram) <- names(tf1sorted)

tdmn2Matrix <- as.matrix(tdmn2)
tf2grams <- rowSums(tdmn2Matrix)
tf2sorted <- sort(tf2grams, decreasing = TRUE)
len2 <- length(tf2sorted)
indexes2Gram <- 1:len2
names(indexes2Gram) <- names(tf2sorted)

tdmn3Matrix <- as.matrix(tdmn3)
tf3grams <- rowSums(tdmn3Matrix)
tf3sorted <- sort(tf3grams, decreasing = TRUE)
len3 <- length(tf3sorted)
indexes3Gram <- 1:len3
names(indexes3Gram) <- names(tf3sorted)

tdmn4Matrix <- as.matrix(tdmn4)
tf4grams <- rowSums(tdmn4Matrix)
tf4sorted <- sort(tf4grams, decreasing = TRUE)
len4 <- length(tf4sorted)
indexes4Gram <- 1:len4
names(indexes4Gram) <- names(tf4sorted)

tdmn5Matrix <- as.matrix(tdmn5)
tf5grams <- rowSums(tdmn5Matrix)
tf5sorted <- sort(tf5grams, decreasing = TRUE)
len5 <- length(tf5sorted)
indexes5Gram <- 1:len5
names(indexes5Gram) <- names(tf5sorted)

