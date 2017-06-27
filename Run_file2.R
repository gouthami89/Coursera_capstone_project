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

onegramscoll <- paste(names(ind1), collapse = " ")
twogramscoll <- paste(names(ind2), collapse = " ")
threegramscoll <- paste(names(ind3), collapse = " ")
fourgramscoll <- paste(names(ind4), collapse = " ")

onegramsfreq <- unname(td1grams)
twogramsfreq <- unname(td2grams)
threegramsfreq <- unname(td3grams)
fourgramsfreq <- unname(td4grams)

onegramsvect <- unlist(strsplit(onegramscoll, split = " "))
twogramsvect <- unlist(strsplit(twogramscoll, split = " "))
threegramsvect <- unlist(strsplit(threegramscoll, split = " "))
fourgramsvect <- unlist(strsplit(fourgramscoll, split = " "))

prior2 <- twogramsvect[seq.int(1,len2*2, 2)]
post2 <- twogramsvect[seq.int(2,len2*2, 2)]
prior2i <- ind1[prior2]
post2i <- ind1[post2]

prior3_1 <- threegramsvect[seq.int(1,len3*3, 3)]
prior3_1 <- threegramsvect[seq.int(2,len3*3, 3)]
prior3 <- paste(prior3_1, prior3_2, sep = " ")
post3 <- threegramsvect[seq.int(3,len3*3, 3)]
prior3i <- ind2[prior3]
post2i <- ind1[post3]

prior4_1 <- fourgramsvect[seq.int(1,len4*4, 4)]
prior4_2 <- fourgramsvect[seq.int(2,len4*4, 4)]
prior4_3 <- fourgramsvect[seq.int(3,len4*4, 4)]
prior4 <- paste(prior4_1, prior4_2, prior4_3, sep = " ")
post4 <- fourgramsvect[seq.int(4,len4*4, 4)]
prior2i <- ind3[prior4]
post2i <- ind1[post4]

prob2gram <- vector(mode = "integer", length = len2)
for (i in 1:len2)
prob2gram[i] <- twogramsfreq[i]/onegramsfreq[prior2i]
names(prob2gram) <- names(ind2)

prob3gram <- vector(mode = "integer", length = len3)
for (i in 1:len3)
prob3gram[i] <- threegramsfreq[i]/twogramsfreq[prior3i]
names(prob3gram) <- names(ind3)

prob4gram <- vector(mode = "integer", length = len4)
for (i in 1:len4)
prob4gram[i] <- 4gramsfreq[i]/3gramsfreq[prior3i]
names(prob4gram) <- names(ind4)

library(data.table)

dt2Grams <- data.table(grams = names(ind2), 
index = ind2, 
frequency = 2gramsfreq, 
prior = prior2, 
posterior = post2, 
probabilities =prob2Gram)

dt3Grams <- data.table(grams = names(ind3), 
index = ind3, 
frequency = 3gramsfreq, 
prior = prior3, 
posterior = post3, 
probabilities =prob3Gram)

dt4Grams <- data.table(grams = names(ind4), 
index = ind4, 
frequency = 4gramsfreq, 
prior = prior4, 
posterior = post4, 
probabilities =prob4Gram)

setkey(dt2Grams, prior)
setkey(dt3Grams, prior)
setkey(dt4Grams, prior)
setkey(dt5Grams, prior)

dt2sorted <- dt2Grams[order(prior, -probabilities)]
dt3sorted <- dt3Grams[order(prior, -probabilities)]
dt4sorted <- dt4Grams[order(prior, -probabilities)]

run2gram <- function(data){
dt <- dt2sorted[word(grams,1) == data]$posterior}
