#to do: add correction factor 0.4 for score in stupid backoff, and final prob when no prediction in corpus
# see "Interpolation" page on Stanfor NLP


SearchStrCorpus <- function(searchstring, corpus, corpuslist){
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
  result <- SearchClStrCorpus(search_string, stringlenth, corpus, corpuslist)
  #  if(nrow(result) == 0) {
  if(length(result) == 0) {
    #    cat("No results found in this iteration. \n")
    if(or_stringlenth > 2){
      search_string <- paste(splitstring[or_stringlenth -1], splitstring[or_stringlenth],  sep=" ")
      #     cat("New search string is: ", search_string, ".\n", sep="")
      #      stringlenth <- 2
      result <- SearchStrCorpus(search_string, corpus, corpuslist)
    } else if (or_stringlenth == 1){
      #result <- c("a",0)
      result <- c("and ")
    } else {
      search_string <- paste(splitstring[or_stringlenth],  sep=" ")
      # cat("New search string is: ", search_string, ".\n")
      #      stringlenth <- 2
      result <- SearchStrCorpus(search_string, corpus, corpuslist)
      
    }   
    
  } else {
    return(result)
  }
  return(result)
}

  
SearchClStrCorpus <- function(search_string, strgtgth, corpus, corpuslist){
#  load(paste(corpus, strgtgth +1, "Markov.RData" , sep= ""))
   searchcorpusname <- paste(corpus, strgtgth +1, "Markov", sep ="" )
#  ML1_df <- searchcorpus$ML1
  ML1_df <- corpuslist[[searchcorpusname]]
  ML1_est <- ML1_df[ML1_df$first == search_string, c("V3", "Bayes_prob")]
  #ML_est <- list()
  #ML_est <- matrix()
  return(ML1_est)

}  
  
