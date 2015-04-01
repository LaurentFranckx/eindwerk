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
samplesize <- 50
accu_mat <- matrix(rep(1,9), nrow=3)
accu_mat <- as.data.frame(accu_mat)
row.names(accu_mat) <- corpuses
names(accu_mat) <- corpuses

system.time(
for(TestCorpus in corpuses){
  for(TrainCorpus in setdiff(corpuses,TestCorpus)){
    DfToSample <- corpuslist[[paste("US.",TestCorpus,"4Markov",sep="")]]$ML3
    TextToSampleId <- sample(nrow(DfToSample), samplesize)
    TextToTest <- DfToSample[TextToSampleId  , ]
    CorpusUsedForTest <- TrainCorpus
    TextToTest$first <- as.character(TextToTest$first)
    
      TestResult <- sapply(TextToTest$first, function(x) SearchStrCorpus(x, CorpusUsedForTest, corpuslist))
    
    TextToTest <- cbind(TextToTest,TestResult)
    TextToTest$TestResult <- as.character(TextToTest$TestResult)
    TextToTest$V3 <- as.character(TextToTest$V3)
    accuracy <- sum(TextToTest$V3 == TextToTest$TestResult)/nrow(TextToTest)
    cat(sprintf("%1.2f"  , accuracy))
    accu_mat[TestCorpus,TrainCorpus] <- sprintf("%1.2f"  , accuracy)
  }
}
)




  
  