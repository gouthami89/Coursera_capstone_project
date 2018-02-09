require(shiny)
require(tm)
require(stylo)
require(data.table)
require(RWeka)
require(stringr)
require(dplyr)
require(xtable)
require(stringr)

stem_text<- function(text, language = "porter", mc.cores = 1) {
  # stem each word in a block of text
  stem_string <- function(str, language) {
    str <- strsplit(x = str, split = "\s")
    str <- wordStem(unlist(str), language = language)
    str <- paste(str, collapse = " ")
    return(str)
  }
  # stem each text block in turn
  x <- mclapply(X = text, FUN = stem_string, language, mc.cores = mc.cores)
  
  # return stemed text blocks
  return(unlist(x))
}
  
predicting_text <- function(data,n){
  require(RTextTools)
  data <- tolower(data)
  data_un <- wordStem(unlist(strsplit(data, split = " ")))
  
  if (length(data_un > 3)) {
    data_un <- tail(data_un,3)}
  
  len <- length(data_un)
  data_un <- as.character(data_un)
  data_un <- paste(data_un, collapse = " ")
  if (len == 3) {
    dt <- dt4sorted[dt4sorted$prior== data_un,]$post
    outp1 <- dt[1:n]
      data_tun <- tail(data_un,1)
      dt <- dt3sorted[dt3sorted$prior== data_un,]$post
      outp2 <- dt[1:n]
        data_un <- tail(data_un,1)
        dt <- dt2sorted[dt2sorted$prior== data_un,]$post
        outp3 <- dt[1:n]
        
       if  ((is.na(outp1)) & is.na(outp2) & is.na(outp3)){
          outpp <- ind1$gram[1:10]
          outpps <- paste(outpp, collapse = " ")
          outp <- paste("NO MATCH-  Common words:", outpps)
       }
        
        else if ((is.na(outp1)) & is.na(outp2)){
          outp <- paste(outp3)
        }
        
        else if ((is.na(outp1)) & is.na(outp3)){
          outp <- paste(outp2)
        }
        
        else if ((is.na(outp3)) & is.na(outp2)){
          outp <- paste(outp1)
        }
        
        else if ((is.na(outp1))){
          outp <- paste(outp2, outp3)
        }
    
        else {
          outp <- paste(outp1, outp2, outp3)
        }
  }

  else if (len == 2) {
    dt <- dt3sorted[dt3sorted$prior== data_un,]$post
    outp1 <- dt[1:n]
      data_un <- tail(data_un,1)
      dt <- dt2sorted[dt2sorted$prior== data_un,]$post
      outp2 <- dt[1:n]
      if  ((is.na(outp1)) & is.na(outp2)){
        outpp <- ind1$gram[1:10]
        outpps <- paste(outpp, collapse = " ")
        outp <- paste("NO MATCH-  Common words:", outpps)
      }
      
      else if ((is.na(outp1))){
        outp <- paste(outp2)
      }
      else if ((is.na(outp2))){
        outp <- paste(outp1)
      }
      
      else {
        outp <- paste(outp1, outp2)
      }
      
      
  }
    
    
  
  else {
    dt <- dt2sorted[dt2sorted$prior== data_un,]$post
    outp1 <- dt[1:n]
    if  ((is.na(outp1))){
      outpp <- ind1$gram[1:10]
      outpps <- paste(outpp, collapse = " ")
      outp <- paste("NO MATCH-  Common words:", outpps)
    }
    
    else {
      outp <- paste(outp1)
    } 
    
  }
  
  output <- paste(as.character(outp), collapse = " ")
  
}

shinyServer(
  function(input,output) {
    
    # Get list of predicted words
    nextWord <- eventReactive(input$button1, {predicting_text(stem_text(input$text1), input$words1)})
    output$table1 <- renderText({nextWord()})
    
  }
  
)