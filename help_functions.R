

LoadNgram <- function(txttoanalyse, ngram, req_freq){
  textname <- paste(txttoanalyse, "Tokened", ngram, "_unique.txt", sep="")
  tokendngr <- read.table(textname, stringsAsFactors = FALSE)
  names(tokendngr) <- c("freq", ngram)
  tokendngr2 <- tokendngr[tokendngr$freq > req_freq, ]
  return(list("Original n-gram" = tokendngr, "n-gram to analyse" = tokendngr2))
}

SplitAndMargeNGr <- function(tokendngr, ngram){
  #only split the 3-grams that occur at least 3 times 
  system.time(
    tokendngr_split <- stri_split_boundaries(tokendngr[, ngram], simplify = TRUE)   
  )
  
  if(ngram == "ThreeGr"){
    first <- paste(tokendngr_split[, 1], tokendngr_split[, 2], sep ="")  
  } else if(ngram == "TwoGr")  {
    first <- paste(tokendngr_split[, 1])  
  }
  
  tokendngr_split_merged <- cbind(first, tokendngr_split)
  tokendngr_split_merged <- cbind(tokendngr_split_merged, tokendngr )
  tokendngr_split_merged <- tokendngr_split_merged[ , c("first", "V4", ngram , "freq" )]
  names(tokendngr_split_merged) <- gsub("V4","to_predict",names(tokendngr_split_merged))
  return(tokendngr_split_merged)
}
  
MarkovChain <- function(tokendngr_split_merged, ngram){    
  df_to_aggr <- tokendngr_split_merged[  , c("freq")]
  
  #### sum the number of times the first two terms occur together
  #takes more than two hours with the 3-grams that occur at least 2 times 
  # takes more than 1 hour as well with the 3-grams that occur at least three times
  # alot of operations take place in cached memory, moreover
  system.time(
    tokendngr_split_merged_sum  <-  aggregate(df_to_aggr, by = list(tokendngr_split_merged$first),  FUN = sum )
    
  )
  
  names(tokendngr_split_merged_sum) <- c("first", "freq_first")
  
  Markov_mat <- join(tokendngr_split_merged, tokendngr_split_merged_sum)
  Markov_mat$Bayes_prob <- with(Markov_mat, freq/freq_first)
  Markov_mat <- Markov_mat[order(Markov_mat$freq_first,Markov_mat$freq), ]
  old_Markov_mat <- Markov_mat
  
  Markov_ML <- list()
  Markov_ML[["Markov_mat"]] <- Markov_mat
  
  for(i in 1:3){
    #find the maximal Bayesian prob for each first "two term"
    df_to_max <- old_Markov_mat[  , c("Bayes_prob")]
    df_max <- aggregate(df_to_max, by = list(old_Markov_mat$first) , FUN = max)
    names(df_max) <- c("first", "Bayes_prob_max")
    
    #Markov_mat2 compares for each 3-gram the actual Bayesian prob with the max prob for the first 2-gram
    #   old_Markov_mat$first_two <- as.character(old_Markov_mat$first_two)
    #   df_max$first_two <- as.character(df_max$first_two)
    Markov_mat2 <- join(old_Markov_mat,df_max)
    
    #select all the rows with less than the max Bayes prob for second iteration
    old_Markov_mat <- Markov_mat2[Markov_mat2$Bayes_prob < Markov_mat2$Bayes_prob_max, ]
    
    #this gives, for each first 2-gram, the 3-gram with the highest likelihood
    #in the case of draws for the likelikhood, there will be duplications
    Markov_help <- Markov_mat2[Markov_mat2$Bayes_prob == Markov_mat2$Bayes_prob_max, ]
    Mardupl <- Markov_help[duplicated(Markov_help$first),]
    Markov_help <- Markov_help[!(Markov_help[, ngram] %in% Mardupl[, ngram]),     ]
    
    markov_name <- paste("ML",i,sep="")
    Markov_ML[[markov_name]] <- Markov_help
    
    if(nrow(Markov_mat2) != nrow(Markov_help) + nrow(old_Markov_mat) + nrow(Mardupl)) stop("Problem in creation Markow table")
    old_Markov_mat <- rbind(old_Markov_mat,Mardupl) 
    old_Markov_mat  <- old_Markov_mat[  , setdiff(names(old_Markov_mat),"Bayes_prob_max" )]
  }
  
  return(Markov_ML)
}