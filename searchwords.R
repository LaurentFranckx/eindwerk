#to do: add correction factor 0.4 for score in stupid backoff, and final prob when no prediction in corpus
# see "Interpolation" page on Stanfor NLP



SearchStrCorpus <- function(searchstring, corpus){
  splitstring <- strsplit(searchstring, " ")[[1]]
  stringlenth <- length(splitstring)
  or_stringlenth <- stringlenth
  if(stringlenth < 4){
    search_string <- paste(searchstring, " ", sep="")
#    SearchClStrCorpus(search_string, stringlenth)
    } else {
      search_string <- paste(splitstring[stringlenth -2], splitstring[stringlenth -1], splitstring[stringlenth], "", sep=" ")
      stringlenth <- 3
    }
  result <- SearchClStrCorpus(search_string, stringlenth, corpus)
  if(nrow(result) == 0) {
    cat("No results found in this iteration. \n")
    if(or_stringlenth > 2){
      search_string <- paste(splitstring[or_stringlenth -1], splitstring[or_stringlenth],  sep=" ")
      cat("New search string is: ", search_string, ".\n", sep="")
#      stringlenth <- 2
      result <- SearchStrCorpus(search_string, corpus)
    } else {
        search_string <- paste(splitstring[or_stringlenth],  sep=" ")
        cat("New search string is: ", search_string, ".\n")
      #      stringlenth <- 2
        result <- SearchStrCorpus(search_string, corpus)
      
    }
    
    
    } else {
      return(result)
    }
  return(result)
}
  
  
SearchClStrCorpus <- function(search_string, strgtgth, corpus){
  load(paste(corpus, strgtgth +1, "Markov.RData" , sep= ""))
  searchcorpus <- get(paste(corpus, strgtgth +1, "Markov", sep ="" ))
  ML1_df <- searchcorpus$ML1
  names(ML1_df) <- gsub("to_predict","V3", names(ML1_df) )
  names(ML1_df) <- gsub("V5","V3", names(ML1_df) )
  ML1_est <- ML1_df[ML1_df$first == search_string, c("V3", "Bayes_prob")]
  #ML_est <- list()
  #ML_est <- matrix()
  ML_est <- ML1_est
  if(length(ML1_est) == 0){
    return(ML1_est)
  } else {
    ML2_df <- searchcorpus$ML2
    names(ML2_df) <- gsub("to_predict","V3", names(ML2_df) )
    names(ML2_df) <- gsub("V5","V3", names(ML2_df) )
    ML2_est <- ML2_df[ML2_df$first == search_string, c("V3", "Bayes_prob") ]
    ML_est <- rbind(ML_est, ML2_est)
    if(length(ML2_est) == 0){
      return(ML_est)
    } else {
      ML3_df <- searchcorpus$ML3
      names(ML3_df) <- gsub("to_predict","V3", names(ML3_df) )
      names(ML3_df) <- gsub("V5","V3", names(ML3_df) )
      ML3_est <- ML3_df[ML3_df$first == search_string, c("V3", "Bayes_prob")]
      ML_est <- rbind(ML_est, ML3_est)
      return(ML_est)
    }
    
  }
  
}  
  
