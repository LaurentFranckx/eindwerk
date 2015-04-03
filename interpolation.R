library(plyr)
library(data.table)


setwd("D:/coursera/dsc_capstone")


corpusnames <- c("news","twitter","blogs")
#strgtgth <- 3


MarkovMat4 <- function(corpus){
      load(paste("US.", corpus, "4Markov.RData" , sep= ""))
      name_df <- paste("US.",corpus, "4Markov", sep ="" )
      searchcorpus <- get(name_df)$Markov_mat
      
      #this is the three gram used for the original prediction 
      names(searchcorpus) <- gsub("^first$","ThreeGr", names(searchcorpus) )
      names(searchcorpus) <- gsub("V5","Pred4Gr", names(searchcorpus) )                                 
      names(searchcorpus) <- gsub("freq$","Freq4Gr", names(searchcorpus) )
      names(searchcorpus) <- gsub("freq_first","FreqPred4gr", names(searchcorpus))
      names(searchcorpus) <- gsub("Bayes_prob","Bayes4Gr", names(searchcorpus) )
      searchcorpus$ThreeGr <- as.character(searchcorpus$ThreeGr)
      searchcorpus$Pred4Gr <- as.character(searchcorpus$Pred4Gr)
      searchcorpus$FourGr <- as.character(searchcorpus$FourGr)
      TwoGr <- strsplit(searchcorpus$ThreeGr, " ")
      TwoGr <- sapply(TwoGr, function(x) paste(x[2],x[3], sep = " "))
      searchcorpus$TwoGr <- TwoGr
      searchcorpus$ThreeGr <- gsub(" $", "", searchcorpus$ThreeGr)
      return(searchcorpus)
}

MarkovMat3 <- function(corpus){
  load(paste("US.", corpus, "3Markov.RData" , sep= ""))
  name_df <- paste("US.",corpus, "3Markov", sep ="" )
  searchcorpus <- get(name_df)$Markov_mat
  
  names(searchcorpus) <- gsub("^first$","TwoGr", names(searchcorpus) )
  names(searchcorpus) <- gsub("to_predict","Pred3Gr", names(searchcorpus) )                                 
  names(searchcorpus) <- gsub("freq$","Freq3Gr", names(searchcorpus) )
  names(searchcorpus) <- gsub("freq_first","FreqPred3gr", names(searchcorpus))
  names(searchcorpus) <- gsub("Bayes_prob","Bayes3Gr", names(searchcorpus) )
  searchcorpus$ThreeGr <- as.character(searchcorpus$ThreeGr)
  searchcorpus$Pred3Gr <- as.character(searchcorpus$Pred3Gr)
  searchcorpus$TwoGr <- as.character(searchcorpus$TwoGr)
  searchcorpus$TwoGr <- gsub(" $", "", searchcorpus$TwoGr)
  OneGr <- strsplit(searchcorpus$TwoGr, " ")
  OneGr <- sapply(OneGr, function(x) paste(x[2], sep = " "))
  searchcorpus$OneGr <- OneGr
  searchcorpus <- searchcorpus[  , setdiff(names(searchcorpus),"ThreeGr")]
  return(searchcorpus)
}

MarkovMat2 <- function(corpus){
  load(paste("US.", corpus, "2Markov.RData" , sep= ""))
  name_df <- paste("US.",corpus, "2Markov", sep ="" )
  searchcorpus <- get(name_df)$Markov_mat
  
  names(searchcorpus) <- gsub("^first$","OneGr", names(searchcorpus) )
  names(searchcorpus) <- gsub("V3","Pred2Gr", names(searchcorpus) )                                 
  names(searchcorpus) <- gsub("freq$","Freq2Gr", names(searchcorpus) )
  names(searchcorpus) <- gsub("freq_first","FreqPred2gr", names(searchcorpus))
  names(searchcorpus) <- gsub("Bayes_prob","Bayes2Gr", names(searchcorpus) )
  
  searchcorpus$TwoGr <- as.character(searchcorpus$TwoGr)
  searchcorpus$Pred2Gr <- as.character(searchcorpus$Pred2Gr)
  searchcorpus$OneGr <- as.character(searchcorpus$OneGr)
  searchcorpus$OneGr <- gsub(" $", "", searchcorpus$OneGr)
#   OneGr <- strsplit(searchcorpus$TwoGr, " ")
#   OneGr <- sapply(OneGr, function(x) paste(x[2], sep = " "))
#   searchcorpus$OneGr <- OneGr
  searchcorpus <- searchcorpus[  , setdiff(names(searchcorpus),"TwoGr")]
  return(searchcorpus)
}




MarkovMat4List <- lapply(corpusnames, MarkovMat4)
names(MarkovMat4List) <- corpusnames

MarkovMat3List <- lapply(corpusnames, MarkovMat3)
names(MarkovMat3List) <- corpusnames


MarkovMat2List <- lapply(corpusnames, MarkovMat2)
names(MarkovMat2List) <- corpusnames

samplesize <- 1000
#10000 is too close to the limit
samplefromtable <- sample(nrow(MarkovMat4List[["news"]]),samplesize)
table1 <- MarkovMat4List[["news"]]
table1 <- table1[samplefromtable, ]

#testmat <- merge(data.table(MarkovMat4List[["news"]]),data.table(MarkovMat3List[["news"]]), by = "TwoGr")
testmat <- join(table1,MarkovMat3List[["news"]])
#testmat[testmat$Pred4gr == " a "  , ]

testmat <- testmat[testmat$Pred4Gr == testmat$Pred3Gr  , ]

testmat <- join(testmat,MarkovMat2List[["news"]])
testmat <- testmat[testmat$Pred4Gr == testmat$Pred2Gr  , ]


save(corpuslist, file = "D:/coursera/dsc_capstone/app1/data/corpuslist.RData")




