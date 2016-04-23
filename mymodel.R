library(tm)
library(dplyr)
predictNextWord <-function(input,profanity,unigramDF, bigramDF, trigramDF, fourgramDF, fivegramDF, maxResults = 3) {
  originalinput <- input
  input <- removePunctuation(input)
  input <- removeNumbers(input)
  input <- tolower(input)
  input <- stripWhitespace(input)
  input <- input[grepl('[[:alpha:]]',input)]
  
  if(identical(input, character(0))) {return("")}
  
  input5 <- paste(tail(unlist(strsplit(input," ")), n=4), collapse = " ")
  seekfive<-grepl(paste0("^",input5,"$"),fivegramDF$fourgram)
  subfive<-fivegramDF[seekfive,]
  
  input4 <- paste(tail(unlist(strsplit(input," ")), n=3), collapse = " ")
  seekfour<-grepl(paste0("^",input4,"$"),fourgramDF$trigram)
  subfour<-fourgramDF[seekfour,]
  
  input3 <- paste(tail(unlist(strsplit(input," ")), n=2), collapse = " ")
  seektri<-grepl(paste0("^",input3,"$"),trigramDF$bigram)
  subtri<-trigramDF[seektri,]
  
  input2 <- tail(unlist(strsplit(input," ")), n=1)
  seekbi <- grepl(paste0("^",input2,"$"),bigramDF$unigram)
  subbi <- bigramDF[seekbi,]
  
  unigramDF$s <- unigramDF$freq/nrow(unigramDF)*0.000000001#weighted Good-Turing probabability of unigram
  useuni <- unigramDF[order(unigramDF$s,decreasing = T),]#ordered weighted Good-Turing probability
  useunia <- useuni[1:maxResults,]
  
  if (sum(seekfive) == 0) {
    if (sum(seekfour) == 0) {
      if (sum(seektri) == 0) {
        if(sum(seekbi)==0) { 
          return(head(unigramDF[order(unigramDF$freq,decreasing = T),1],maxResults)) 
        }
        subbi$s <- 0.001*subbi$freq/sum(seekbi)#weighted Good-Turing probability of bigram
        names <- c(subbi$name,useunia$unigram)
        score <- c(subbi$s,useunia$s)
      }
      subbi$s <- 0.001*subbi$freq/sum(seekbi)
      subtri$s <- 0.01*subtri$freq/sum(subtri$freq)
      names <- c(subtri$name,subbi$name,useunia$unigram)
      score <- c(subtri$s,subbi$s,useunia$s)
    }
    subbi$s <- 0.001*subbi$freq/sum(seekbi)
    subtri$s <- 0.01*subtri$freq/sum(subtri$freq)
    subfour$s <- 0.1*subfour$freq/sum(subfour$freq)
    names <- c(subfour$name,subtri$name,subbi$name,useunia$unigram)
    score <- c(subfour$s,subtri$s,subbi$s,useunia$s)
  } else {
    subbi$s <- 0.001*subbi$freq/sum(seekbi)
    subtri$s <- 0.01*subtri$freq/sum(subtri$freq)
    subfour$s <- 0.1*subfour$freq/sum(subfour$freq)
    subfive$s <- 1*subfive$freq/sum(subfive$freq)
    names <- c(subfive$name,subfour$name,subtri$name,subbi$name,useunia$unigram)
    score <- c(subfive$s,subfour$s,subtri$s,subbi$s,useunia$s)
  }
  names <- unlist(names)
  predictWord <- data.frame(next_word=names,score=score,stringsAsFactors = F)
  predictWord <- predictWord[order(predictWord$score,decreasing = T),]
  final <- unique(predictWord$next_word)
  final <- setdiff(final,profanity)
  final <- final[grepl('[[:alpha:]]',final)][1:maxResults]
  final_sentence <- paste(originalinput, final, sep=" ")
  final<- data.frame(Next_Word=final, Predicted_Sentence=final_sentence)
  final<- add_rownames(final, "Rank")
  return(final)
}

