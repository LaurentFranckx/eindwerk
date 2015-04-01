setwd("D:/coursera/dsc_capstone")


corpusnames <- c("news","twitter","blogs")
#strgtgth <- 3


CreateCorpusList <- function(corpusnames, strglthlist){
  corpuslist <- list()
  for(corpus in corpusnames){
    for(strgtgth in strglthlist){
      load(paste("US.", corpus, strgtgth +1, "Markov.RData" , sep= ""))
      name_df <- paste("US.",corpus, strgtgth +1, "Markov", sep ="" )
      searchcorpus <- get(name_df)
      
      names(searchcorpus$ML1) <- gsub("to_predict","V3", names(searchcorpus$ML1) )
      names(searchcorpus$ML1) <- gsub("V5","V3", names(searchcorpus$ML1) )                                 
      names(searchcorpus$ML2) <- gsub("to_predict","V3", names(searchcorpus$ML2) )
      names(searchcorpus$ML2) <- gsub("V5","V3", names(searchcorpus$ML2))
      names(searchcorpus$ML3) <- gsub("to_predict","V3", names(searchcorpus$ML3) )
      names(searchcorpus$ML3) <- gsub("V5","V3", names(searchcorpus$ML3))
      
      #       cat(names(searchcorpus$ML1), "\n")
      #       cat(names(searchcorpus$ML2), "\n")
      #       cat(names(searchcorpus$ML3), "\n")
      
      searchcorpus$ML1 <- searchcorpus$ML1[  , c("first","V3","Bayes_prob")]
      searchcorpus$ML2 <- searchcorpus$ML2[  , c("first","V3","Bayes_prob")]
      searchcorpus$ML3 <- searchcorpus$ML3[  , c("first","V3","Bayes_prob")]
      
      #   searchmatrix <- paste(corpus,"search", sep="")
      #   assign(searchmatrix, searchcorpus$ML1 ) 
      listtouse <- list(searchcorpus$ML1, searchcorpus$ML2, searchcorpus$ML3) 
      names(listtouse) <- c("ML1","ML2","ML3")
      
      corpuslist[[name_df]] <- listtouse
    }
  }
  return(corpuslist)
  
}


corpuslist <- CreateCorpusList(corpusnames, 1:3)


save(corpuslist, file = "D:/coursera/dsc_capstone/app1/data/corpuslist.RData")




