library(tm)
library(stringr)
load(file = "cap_ston_corp_cl.RData")
load(file = "app1/data/corpuslist.RData")
source("searchwordstest.R")
source("n_gram_tokenizer.R")
source("help_functions.R")
source("app1/searchwords.R")

set.seed(123)
corpuses <- c("news", "twitter", "blogs")
samplesize <- 100
accu_mat <- expand.grid(corpuses,corpuses)
names(accu_mat) <- c("TestCorpus", "TrainCorpus")
accu_mat$accu1 <- c(0)
accu_mat$accu2 <- c(0)
accu_mat$accu3 <- c(0)


system.time(
for(i in 2:4){
  for(TestCorpus in corpuses){
    for(TrainCorpus in corpuses){
      DfToSample <- corpuslist[[paste("US.",TestCorpus,i,"Markov",sep="")]]$ML3
      TextToSampleId <- sample(nrow(DfToSample), samplesize)
      TextToTest <- DfToSample[TextToSampleId  , ]
      CorpusUsedForTest <- TrainCorpus
      TextToTest$first <- as.character(TextToTest$first)
      
      TestResult <- sapply(TextToTest$first, function(x) SearchWrapper(x, CorpusUsedForTest, corpuslist))
      #   TestResult <- as.data.frame(TestResult)
      #  TestResult <- do.call("cbind", TestResult)
      TestResult <- t(TestResult)
      TextToTest <- cbind(TextToTest,TestResult)
      TextToTest[ , 4] <- as.character(TextToTest[ , 4])
      TextToTest[ , 5] <- as.character(TextToTest[ , 5])
      TextToTest[ , 6] <- as.character(TextToTest[ , 6])
      if(any(TextToTest[ , 4] == TextToTest[ , 5]) | any(TextToTest[ , 4] == TextToTest[ , 6])) stop("still duplicate values")
      TextToTest$V3 <- as.character(TextToTest$V3)
      accuracy1num <- sum(TextToTest$V3 == TextToTest[, 4])
      accuracy2num <- sum(TextToTest$V3 == TextToTest[, 5])
      accuracy3num <- sum(TextToTest$V3 == TextToTest[, 6])
      accuracy1 <- accuracy1num/nrow(TextToTest)
      accuracy2 <- (accuracy1num + accuracy2num)/nrow(TextToTest)
      accuracy3 <- (accuracy1num + accuracy2num + accuracy3num)/nrow(TextToTest)
      cat(sprintf("%1.2f"  ,c(accuracy1, accuracy2,accuracy3 ) ), "\n")
      accu_mat[accu_mat$TestCorpus == TestCorpus & accu_mat$TrainCorpus == TrainCorpus,  c("accu1","accu2","accu3")] <- sprintf("%1.2f"  , c(accuracy1, accuracy2,accuracy3 ))
    }
  }
  save(accu_mat, file = paste("accum_mat_",i, "gr_amatch.RData", sep=""))
  cat("Finalised for prediction based on ", i , " words.\n")
}
)



save(accu_mat, file = "accum_mat_2gr.RData")
  
  