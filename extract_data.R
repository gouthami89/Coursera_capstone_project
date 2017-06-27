blogs <- readLines("en_US/en_US.blogs.txt")
nblogwords <- sum(sapply(strsplit(blogs, "\\s+"), length))

news <- readLines("en_US/en_US.news.txt", skipNul = TRUE)
twits <- readLines("en_US/en_US.twitter.txt", skipNul = TRUE)

samples <- c(sample(blogs, 800000), sample(news, 70000), sample(twits, 200000))

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

