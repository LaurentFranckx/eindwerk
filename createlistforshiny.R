#setwd("D:/coursera/dsc_capstone")
setwd("D:/coursera/caps_res")

corpusnames <- c("news","twitter","blogs")
#strgtgth <- 3


CreateCorpusList <- function(corpusnames, strglthlist){
  corpuslist <- list()
  for(corpus in corpusnames){
    for(strgtgth in strglthlist){
      load(paste("US.", corpus, strgtgth +1, "Markov.RData" , sep= ""))
      if(strgtgth <4 ){
        name_df <- paste("US.",corpus, strgtgth +1, "Markov", sep ="" )
       } else {
         name_df <- paste(corpus, strgtgth +1, "Markov", sep ="" )
         
       }
      searchcorpus <- get(name_df)
      
      #reduce size of Markov chain with 4 words
      #if(strgtgth == 4 ){
        searchcorpus$ML1 <- searchcorpus$ML1[searchcorpus$ML1$freq > 3  , ]
        searchcorpus$ML2 <- searchcorpus$ML1[searchcorpus$ML2$freq > 3  , ]
        searchcorpus$ML3 <- searchcorpus$ML1[searchcorpus$ML3$freq > 3  , ]
        
      #}
      
      names(searchcorpus$ML1) <- gsub("to_predict","V3", names(searchcorpus$ML1) )
      names(searchcorpus$ML1) <- gsub("V5","V3", names(searchcorpus$ML1) ) 
      names(searchcorpus$ML1) <- gsub("V6","V3", names(searchcorpus$ML1) ) 
      searchcorpus$ML1$first <- as.character(searchcorpus$ML1$first )
      searchcorpus$ML1$V3 <- as.character(searchcorpus$ML1$V3 )
      
      names(searchcorpus$ML2) <- gsub("to_predict","V3", names(searchcorpus$ML2) )
      names(searchcorpus$ML2) <- gsub("V5","V3", names(searchcorpus$ML2))
      names(searchcorpus$ML2) <- gsub("V6","V3", names(searchcorpus$ML2))
      searchcorpus$ML2$first <- as.character(searchcorpus$ML2$first )
      searchcorpus$ML2$V3 <- as.character(searchcorpus$ML2$V3)
      
      names(searchcorpus$ML3) <- gsub("to_predict","V3", names(searchcorpus$ML3) )
      names(searchcorpus$ML3) <- gsub("V5","V3", names(searchcorpus$ML3))
      names(searchcorpus$ML3) <- gsub("V6","V3", names(searchcorpus$ML3))
      searchcorpus$ML3$first <- as.character(searchcorpus$ML3$first )
      searchcorpus$ML3$V3 <- as.character(searchcorpus$ML3$V3 )
      
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
      name_df <- paste("US.",corpus, strgtgth +1, "Markov", sep ="" )
      
      corpuslist[[name_df]] <- listtouse
    }
  }
  return(corpuslist)
  
}


corpuslist <- CreateCorpusList(corpusnames, 1:4)

#with complete 4-gram list, the total corpuslist takes about 810 Mb - too much for Shiny (on disk: 40 Mb)
# when reduced to all 4 grams occuring at least 4 times, 613 Mb
#by transforming factors in characters, one saves a lot of memory: in memort around 164 Mb; on disk 21 Mb (for min freq of 6)


save(corpuslist, file = "D:/coursera/dsc_capstone/app1/data/corpuslist.RData")




