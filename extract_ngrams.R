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