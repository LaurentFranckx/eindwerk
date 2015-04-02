#to do: add correction factor 0.4 for score in stupid backoff, and final prob when no prediction in corpus
#to do: verify that there are no duplications


# see "Interpolation" page on Stanfor NLP

SearchWrapper <- function(searchstring, corpus, corpuslist){
  result <- SearchStrCorpus(searchstring, corpus, corpuslist)
  if(length(result) == 0) result[c(1,2,3)] <- c("a ", "and ", "I ")
  if(length(result) == 1) result[c(2,3)] <- c("a ", "and ")
  if(length(result) == 2) result[3] <- c("a ")
  return(result)
}



SearchStrCorpus <- function(searchstring, corpus, corpuslist, ML_est = character()){
  splitstring <- strsplit(searchstring, " ")[[1]]
  stringlenth <- length(splitstring)
  or_stringlenth <- stringlenth
  if(stringlenth < 4){
    search_string <- paste(searchstring, " ", sep="")
  } else {
    search_string <- paste(splitstring[stringlenth -2], splitstring[stringlenth -1], splitstring[stringlenth], "", sep=" ")
    stringlenth <- 3
  }
  ML_est <- SearchClStrCorpus(search_string, stringlenth, corpus, corpuslist, ML_est)
  if(length(ML_est) < 3) {
    if(or_stringlenth > 2){
      search_string <- paste(splitstring[or_stringlenth -1], splitstring[or_stringlenth],  sep=" ")
      ML_est <- SearchStrCorpus(search_string, corpus, corpuslist, ML_est)
      #once you have only word left, you cannot strip any further
    } else if (or_stringlenth == 1){
      result <- c("a ")
      result <- ML_est
      return(result)
    } else {
      search_string <- paste(splitstring[or_stringlenth],  sep=" ")
      ML_est <- SearchStrCorpus(search_string, corpus, corpuslist, ML_est)      
    }   
    
  } else if(length(ML_est) == 3){
      result <- ML_est
      return(result)
    } else {
      stop("ML_est should before it grows larger than four. ")
    }  
}


SearchClStrCorpus <- function(search_string, strgtgth, corpus, corpuslist, ML_est = ML_est){
  name_df <- paste("US.",corpus, strgtgth +1, "Markov", sep ="" )
  Markovchains    <- corpuslist[[name_df]]
  ML1_df <- Markovchains$ML1
  ML1_df$V3 <- as.character(ML1_df$V3)
  #maybe no need to return bayes prob in real application
#  ML1_est <- ML1_df[ML1_df$first == search_string, c("V3", "Bayes_prob")]
  ML1_est <- ML1_df[ML1_df$first == search_string, "V3"]
  ML_est <- c(ML_est,ML1_est)
  if(length(ML_est) == 3| length(ML1_est) == 0){
    return(ML_est)
  } else {
    ML2_df <- Markovchains$ML2
    ML2_df$V3 <- as.character(ML2_df$V3)
    #maybe no need to return bayes prob in real application
    #ML2_est <- ML2_df[ML2_df$first == search_string, c("V3", "Bayes_prob") ]
    ML2_est <- ML2_df[ML2_df$first == search_string, "V3" ]
    ML_est <- c(ML_est,ML2_est)
    if(length(ML_est) == 3 | length(ML2_est) == 0){
      return(ML_est)
    } else {
      ML3_df <- Markovchains$ML3
      ML3_df$V3 <- as.character(ML3_df$V3)
      #maybe no need to return bayes prob in real application
      #ML3_est <- ML3_df[ML3_df$first == search_string, c("V3", "Bayes_prob")]
      ML3_est <- ML3_df[ML3_df$first == search_string, "V3"]
      ML_est <- c(ML_est,ML3_est)
    }
    
  }
 return(ML_est) 
}  

