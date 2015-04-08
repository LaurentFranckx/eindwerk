

LoadNgram <- function(txttoanalyse, ngram, req_freq){
 # US.blogsTokened3Gr_uniq.txt
  
  if(ngram == "FiveGr"){
    gram_nr <- 5  
  } else if (ngram == "FourGr"){
    gram_nr <- 4  
  } else if(ngram == "ThreeGr")  {
    gram_nr <- 3  
  }  else if (ngram == "TwoGr")  {
    gram_nr <- 2  
  } else {
    stop("Unkown n-gram",  ngram, ".\n")
  }
  
  
  textname <- paste(txttoanalyse, "Tokened", gram_nr, "Gr_uniq.txt", sep="")
  tokendngr <- read.table(textname, stringsAsFactors = FALSE)
  names(tokendngr) <- c("freq", ngram)
  tokendngr2 <- tokendngr[tokendngr$freq > req_freq, ]
  return(list("Original n-gram" = tokendngr, "n-gram to analyse" = tokendngr2))
}

SplitAndMargeNGr <- function(tokendngr, ngram){
  if(nrow(tokendngr) == 0) {
     warning("Empty tokendngr for ", ngram)
     return(tokendngr)
  }
  tokendngr_split <- stri_split_boundaries(tokendngr[, ngram], simplify = TRUE)   
  if(ngram == "FiveGr"){
    first <- paste(tokendngr_split[, 1], tokendngr_split[, 2], tokendngr_split[, 3], tokendngr_split[, 4], sep ="") 
    vcol <- "V6"
  } else if (ngram == "FourGr"){
    first <- paste(tokendngr_split[, 1], tokendngr_split[, 2], tokendngr_split[, 3], sep ="") 
    vcol <- "V5"
  } else if(ngram == "ThreeGr")  {
    first <- paste(tokendngr_split[, 1], tokendngr_split[, 2], sep ="")  
    vcol <- "V4"
  }  else if (ngram == "TwoGr")  {
    first <- paste(tokendngr_split[, 1]) 
    vcol <- "V3"
  } else {
    stop("Unkown n-gram",  ngram, ".\n")
  }
  
  tokendngr_split_merged <- cbind(first, tokendngr_split)
  tokendngr_split_merged <- cbind(tokendngr_split_merged, tokendngr )
  tokendngr_split_merged <- tokendngr_split_merged[ , c("first", vcol, ngram , "freq" )]
  names(tokendngr_split_merged) <- gsub("V4","to_predict",names(tokendngr_split_merged))
  return(tokendngr_split_merged)
}
  
MarkovChain <- function(tokendngr_split_merged, ngram){    
  if(nrow(tokendngr_split_merged) == 0){
    warning("Empty tokendngr for ", ngram)
    return(tokendngr_split_merged)
  }
  df_to_aggr <- tokendngr_split_merged[  , c("freq")]
  
  #### sum the number of times the first two terms occur together
  tokendngr_split_merged_sum  <-  aggregate(df_to_aggr, by = list(tokendngr_split_merged$first),  FUN = sum )
  
  names(tokendngr_split_merged_sum) <- c("first", "freq_first")
  
  tokendngr_split_merged <- data.table(tokendngr_split_merged)
  tokendngr_split_merged_sum <- data.table(tokendngr_split_merged_sum)
  Markov_mat <- merge(tokendngr_split_merged, tokendngr_split_merged_sum, by = "first")
  Markov_mat$Bayes_prob <- with(Markov_mat, freq/freq_first)
  Markov_mat <- Markov_mat[order(Markov_mat$freq_first,Markov_mat$freq), ]
  old_Markov_mat <- Markov_mat
  
  Markov_ML <- list()
  Markov_ML[["Markov_mat"]] <- Markov_mat
  
  for(i in 1:3){
    #find the maximal Bayesian prob for each first "two term"
    old_Markov_mat <- data.frame(old_Markov_mat)
    df_to_max <- old_Markov_mat[  , c("Bayes_prob")]
    if (length(df_to_max) == 0){
       warning("In iteration " , i , " of the construction of the Markow matrix there are no more
                             rows left to aggregate. \n Returning incomplete Markow matrix.")
       return(Markov_ML)
     }
     df_max <- aggregate(df_to_max, by = list(old_Markov_mat$first) , FUN = max)
    names(df_max) <- c("first", "Bayes_prob_max")
    
    #Markov_mat2 compares for each 3-gram the actual Bayesian prob with the max prob for the first 2-gram
    #   old_Markov_mat$first_two <- as.character(old_Markov_mat$first_two)
    #   df_max$first_two <- as.character(df_max$first_two)
    old_Markov_mat <- data.table(old_Markov_mat)
    df_max <- data.table(df_max)
    
    Markov_mat2 <- merge(old_Markov_mat,df_max, by = "first")
    
    Markov_mat2 <- data.frame(Markov_mat2)
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



TokenizeMarkov <- function(input_txt, ngram, req_freq, samplesize = length(texttotoken), create_token = FALSE) {
  cat("Markov matrix started for ", ngram, "n-gram and ", input_txt, ". \n") 
  if(ngram == 5){
    ngram_sym <-"FiveGr"   
  } else if(ngram == 4){
    ngram_sym <-"FourGr"   
  } else if(ngram == 3)  {
    ngram_sym <-"ThreeGr"   
  }  else if (ngram == 2)  {
    ngram_sym  <- "TwoGr"  
  } else {
    stop("Unkown n-gram",  ngram, ".\n")    
  }
  
  or_txt <- paste("en_US.", input_txt, ".txt", sep="")
  texttotoken <- PlainTextDocument(cap_ston_corp_cl[[or_txt]]$content)[[1]]

  set.seed(123) 
  sample <- sample( length(texttotoken), samplesize)
  texttotoken_sample <- texttotoken[ sample ]
  
  tokenizer <- ngram_tokenizer(ngram)
  if(create_token == TRUE){
    tokenized <- tokenizer(texttotoken_sample)
  }
  cat("Tokenization finalised for ", ngram, "n-gram. \n")
  matrixname <- paste(input_txt,"Tokened", ngram, "Gr.txt", sep = "")
  write.table(as.matrix(tokenized), row.names = FALSE, col.names = FALSE, file = matrixname)
  rm(texttotoken)
  rm(texttotoken_sample)
  rm(tokenized)
  gc()
  
  
  freqmat <- paste(input_txt,"Tokened", ngram, "Gr_uniq.txt",sep= "")  
  system(paste("sort < ", matrixname,  " | uniq -c > ", freqmat,  sep = " "))
  
  
  TokenedGrUn <- LoadNgram(input_txt,ngram_sym, req_freq)
  GrSplitMg <- SplitAndMargeNGr(TokenedGrUn[[2]],ngram_sym)
  rm(TokenedGrUn)
  gc()
  
  name_markov <- paste(input_txt,ngram, "Markov", sep="")
  assign(name_markov, MarkovChain(GrSplitMg,ngram_sym))
  filename <- paste(name_markov,"RData", sep= ".")
  save(list = name_markov, file = filename)
  cat("Markov matrix finalised for ", ngram, "n-gram and ", input_txt, ". \n")  
}
