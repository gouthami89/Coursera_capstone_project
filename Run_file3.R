cname <- getwd()
docs <- Corpus(DirSource(cname))

docs <- tm_map(docs, removePunctuation)
docs <- tm_map(docs, removeNumbers)
docs <- tm_map(docs, content_transformer(tolower))
docs <- tm_map(docs, removeWords, stopwords("english"))
docs <- tm_map(docs, stripWhitespace)
docs <- tm_map(docs, stemDocument)
docs <- tm_map(docs, PlainTextDocument)

dtm <- DocumentTermMatrix(docs)
tdm <- TermDocumentMatrix(docs)

dtmMatrix <- as.matrix(dtm)
tdmMatrix <- as.matrix(tdm)

write.csv(dtmMatrix, 'dtm.csv')
write.csv(tdmMatrix, 'tdm.csv')

onegram <- function(x) NGramTokenizer(x, Weka_control(min=1, max=1))
tdm1 <- TermDocumentMatrix(docs, control = list(tokenize = onegram))
tdm1Matrix <- as.matrix(tdm1)
td1grams <- sort(rowSums(tdm1Matrix), decreasing = TRUE)
write.csv(td1grams, 'tdm1.csv')

twogram <- function(x) NGramTokenizer(x, Weka_control(min=2, max=2))
tdm2 <- TermDocumentMatrix(docs, control = list(tokenize = twogram))
tdm2Matrix <- as.matrix(tdm2)
td2grams <- sort(rowSums(tdm2Matrix), decreasing = TRUE)
write.csv(td2grams, 'tdm2.csv')

threegram <- function(x) NGramTokenizer(x, Weka_control(min=3, max=3))
tdm3 <- TermDocumentMatrix(docs, control = list(tokenize = threegram))
tdm3Matrix <- as.matrix(tdm3)
td3grams <- sort(rowSums(tdm3Matrix), decreasing = TRUE)
write.csv(td3grams, 'tdm3.csv')

fourgram <- function(x) NGramTokenizer(x, Weka_control(min=4, max=4))
tdm4 <- TermDocumentMatrix(docs, control = list(tokenize = fourgram))
tdm4Matrix <- as.matrix(tdm4)
td4grams <- sort(rowSums(tdm4Matrix), decreasing = TRUE)
write.csv(td4grams, 'tdm4.csv')

len1 <- length(td1grams)
len2 <- length(td2grams)
len3 <- length(td3grams)
len4 <- length(td4grams)

ind1 <- 1:len1
ind2 <- 1:len2
ind3 <- 1:len3
ind4 <- 1:len4

names(ind1) <- names(td1grams)
names(ind2) <- names(td2grams)
names(ind3) <- names(td3grams)
names(ind4) <- names(td4grams)

write.csv(ind1, 'ind1.csv')
write.csv(ind2, 'ind2.csv')
write.csv(ind3, 'ind3.csv')
write.csv(ind4, 'ind4.csv')

