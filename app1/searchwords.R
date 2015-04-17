#to do: add correction factor 0.4 for score in stupid backoff, and final prob when no prediction in corpus
#to do: verify that there are no duplications


# see "Interpolation" page on Stanfor NLP

SearchWrapper <- function(searchstring, corpus, corpuslist, decrease =0.7, maxlength = 4){
    rawresult <- SearchStrCorpus(searchstring, corpus, corpuslist, maxlength)
#   if(length(result) == 0) result[c(1,2,3)] <- c("a ", "and ", "I ")
#   if(length(result) == 1) result[c(2,3)] <- c("a ", "and ")
#   if(length(result) == 2) result[3] <- c("a ")
    result <- rawresult[complete.cases(rawresult),]

    result <- as.data.frame(result)
    most_common <- c("a","and","I")
    if(nrow(result) == 0){
      final_res <- most_common 
    #  return(character(0))
    } else {
      maxn <- max(result$n)
      result$weight_prob <- with(result, Bayes_prob * (1 - distance))
      result$weight_bo <- with(result, weight_prob * (1-decrease)^(maxn-n))
      final_res <- aggregate(result$weight_bo, by = list(result$predword), FUN = sum)
      
      final_res <- final_res[order(final_res$x, decreasing = TRUE),]
      final_res <- final_res[1:3, 1]
      final_res <- as.character(final_res)  
      count_na <- sum(is.na(final_res))
      final_res[is.na(final_res)] <- sample(most_common,count_na)
      
    }
return(final_res)
#  return(list(rawresult,result,final_res))
}



SearchStrCorpus <- function(searchstring, corpus, corpuslist, maxlength,  ML_est = character()){
  splitstring <- strsplit(searchstring, " ")[[1]]
  stringlenth <- length(splitstring)
  or_stringlenth <- stringlenth
  if(stringlenth < maxlength + 1){
    search_string <- paste(searchstring, " ", sep="")
  } else {
    search_string <- ""
    for(i in 0: (maxlength -1 )) search_string <- paste(splitstring[stringlenth -i], search_string, sep=" ")
    stringlenth <- maxlength
  }
  ML_est <- SearchClStrCorpus(search_string, stringlenth, corpus, corpuslist, ML_est)
#  if(length(ML_est) < 3) {
    if(or_stringlenth > 2){
      search_string <- paste(splitstring[or_stringlenth -1], splitstring[or_stringlenth],  sep=" ")
      ML_est <- SearchStrCorpus(search_string, corpus, corpuslist,maxlength, ML_est = ML_est)
      #once you have only word left, you cannot strip any further
    } else if (or_stringlenth == 1){
#      result <- c("a ")
      result <- ML_est
      rm(ML_est)
      return(result)
    } else {
      search_string <- paste(splitstring[or_stringlenth],  sep=" ")
      ML_est <- SearchStrCorpus(search_string, corpus, corpuslist, maxlength, ML_est = ML_est)      
    }   
    
#   } else if(length(ML_est) == 3){
#       result <- ML_est
#       rm(ML_est)
#       return(result)
#     } else {
#       stop("ML_est should before it grows larger than four. ")
#     }  
 return(ML_est)
}


SearchClStrCorpus <- function(search_string, strgtgth, corpus, corpuslist, ML_est = ML_est){
  names_ML <- c("n-gram", "predword", "Bayes_prob", "distance", "n")
  name_df <- paste("US.",corpus, strgtgth +1, "Markov", sep ="" )
  Markovchains    <- corpuslist[[name_df]]
  ML1_df <- Markovchains$ML1
  ML1_df$V3 <- as.character(ML1_df$V3)
  ML1_df$V3 <- as.character(ML1_df$V3)
  ML1_df$first <- as.character(ML1_df$first)
  
  #maybe no need to return bayes prob in real application
  #  ML1_est <- ML1_df[ML1_df$first == search_string, c("V3", "Bayes_prob")]
  # ML1_est <- ML1_df[ML1_df$first == search_string, "V3"]
  
  illus_amatch <- amatch(search_string ,ML1_df$first, method = "jw",maxDist=0.1)
  ML1_amatch  <- ML1_df[illus_amatch,  ]
  #   ML1_est <- ML1_amatch[,"V3"]
  ML1_est <- ML1_amatch[,c("first","V3", "Bayes_prob")]
  dist1 <- stringdist(search_string ,ML1_est$first, method = "jw")
  ML1_est <- as.data.frame(c(ML1_est, dist1, strgtgth))
  names(ML1_est) <- names_ML
  #  if(!is.na(ML1_est)) {
  if (length(ML_est) == 0) {
    ML_est <- ML1_est
  } else {
    ML_est <- rbind(ML_est,ML1_est)       
  }
  
#} 
#  if(length(ML_est) == 3| length(ML1_est) == 0){
#   if(length(ML_est) == 3| is.na(ML1_est)){
#     return(ML_est)
#   } else {
  ML2_df <- Markovchains$ML2
  ML2_df$V3 <- as.character(ML2_df$V3)
  ML2_df$first <- as.character(ML2_df$first)

#maybe no need to return bayes prob in real application
#ML2_est <- ML2_df[ML2_df$first == search_string, c("V3", "Bayes_prob") ]
#ML2_est <- ML2_df[ML2_df$first == search_string, "V3" ]

  illus_amatch <- amatch(search_string ,ML2_df$first, method = "jw",maxDist=0.1)
  ML2_amatch  <- ML2_df[illus_amatch,  ]
# ML2_est <- ML2_amatch[,"V3"]
  ML2_est <- ML2_amatch[,c("first","V3", "Bayes_prob")]
  dist2 <- stringdist(search_string ,ML2_est$first, method = "jw")
  ML2_est <- as.data.frame(c(ML2_est, dist2, strgtgth))
  names(ML2_est) <- names_ML

#   if(!is.na(ML2_est)){
  if (nrow(ML_est) == 0) {
    ML_est <- ML2_est     
  } else {
    ML_est <- rbind(ML_est,ML2_est)
  
  }

#} 
#    if(length(ML_est) == 3 | length(ML2_est) == 0){
#     if(length(ML_est) == 3 | is.na(ML2_est) ){
#       return(ML_est)
#     } else {
  ML3_df <- Markovchains$ML3
  ML3_df$V3 <- as.character(ML3_df$V3)
  ML3_df$first <- as.character(ML3_df$first)

#maybe no need to return bayes prob in real application
#ML3_est <- ML3_df[ML3_df$first == search_string, c("V3", "Bayes_prob")]
#ML3_est <- ML3_df[ML3_df$first == search_string, "V3"]

  illus_amatch <- amatch(search_string ,ML3_df$first, method = "jw",maxDist=0.1)
  ML3_amatch  <- ML3_df[illus_amatch,  ]
#ML3_est <- ML3_amatch[,"V3"]
  ML3_est  <- ML3_amatch[,c("first","V3", "Bayes_prob")]
  dist3 <- stringdist(search_string ,ML3_est$first, method = "jw")
  ML3_est <- as.data.frame(c(ML3_est, dist3, strgtgth))
  names(ML3_est) <- names_ML

#   if(!is.na(ML2_est)){
  if (nrow(ML_est) == 0) {
    ML_est <- ML3_est     
  } else {
    ML_est <- rbind(ML_est,ML3_est)
  
  }
# } 
# }

#}
return(ML_est) 
}  

