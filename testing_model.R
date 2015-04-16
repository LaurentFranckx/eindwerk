library(tm)
library(stringr)
load(file = "cap_ston_corp_cl.RData")
load(file = "app1/data/corpuslist.RData")
source("searchwordstest.R")
source("n_gram_tokenizer.R")
source("help_functions.R")
source("app1/searchwords.R")

library(stringdist)

set.seed(123)
corpuses <- c("news", "twitter", "blogs")
samplesize <- 50
accu_mat <- expand.grid(corpuses,corpuses, 3:4, seq(0.0,1.0,by=0.2))
names(accu_mat) <- c("TestCorpus", "TrainCorpus", "i", "k")
accu_mat <- accu_mat[accu_mat$TestCorpus   != accu_mat$TrainCorpus  , ]
accu_mat$accu1 <- c(0)
accu_mat$accu2 <- c(0)
accu_mat$accu3 <- c(0)
options(warn=-1)

accu_manum <- accu_mat
# TestCorpus <- "blogs"
# TrainCorpus <- "news"

#according to validation gives best results, but works very poorly when tested out of sample
#k = 0.15

#k = 0.0

#performs better for the first two words than when k = 0.60
#k = 0.25
#0.15 is to0 low
#k = 0.15

list_of_means <- list()

system.time(
 # for(k in seq(0.1,0.7, by = 0.1)){
 #testing for lower n-gram does not really make sense     
    for(i in 4:5){
      #for(i in 5){   
      cat("started for i = ", i , "\n")
      
      #  for(k in seq(0.13,0.17, by = 0.02)){
      #    cat("started for k = ", k , "\n")
      for(TestCorpus in corpuses){
        resdir <- "D:/coursera/caps_res/"
        if(i < 5){
          DfToSample <- read.table(paste(resdir, "US.",TestCorpus,"Tokened", i,"Gr_uniq.txt",sep=""), stringsAsFactors = FALSE)
        } else {
          DfToSample <- read.table(paste(resdir, TestCorpus,"Tokened", i,"Gr_uniq.txt",sep=""), stringsAsFactors = FALSE)
        }
        
        cat("Started with test set", TestCorpus, ".\n")
        
        TextToSampleId <- sample(nrow(DfToSample), samplesize)
        TextToTest <- DfToSample[TextToSampleId  , ]
        rm(DfToSample)
        gc()
        TextToTest <- TextToTest[, 2]
        testword_vec <- character(length(TextToTest))
        tesstring_vec <- character(length(TextToTest))
        for(j in seq_along(1:length(TextToTest))){
          splitstring <- strsplit(TextToTest[j], " ")[[1]]
          stringlenth <- length(splitstring)
          testword_vec[j] <- splitstring[stringlenth]  
          firstwords <- splitstring[1]
          for(r in 2:(stringlenth-1)) firstwords <- paste(firstwords,splitstring[r], sep= " ")         
          tesstring_vec[j] <- firstwords
        }
        tesstring_vec <- rbind(tesstring_vec,testword_vec)
        TextToTest <- t(rbind(TextToTest,tesstring_vec))  
        colnames(TextToTest) <- c("TextToTest", "first","V3")
        TextToTest <- as.data.frame(TextToTest)
        for(k in seq(0.0,1.0,by=0.2)){
          
        for(TrainCorpus in setdiff(corpuses,TestCorpus)){
          cat("Started with training set", TrainCorpus, ".\n")
          
          CorpusUsedForTest <- TrainCorpus
          TextToTest$first <- as.character(TextToTest$first)
          
          TestResult <- sapply(TextToTest$first, function(x) SearchWrapper(x, CorpusUsedForTest, corpuslist, decrease = k), simplify = TRUE)
          TestResult <- as.data.frame(TestResult)
          #  TestResult <- do.call("cbind", TestResult)
          if(nrow(TestResult) == 3) TestResult <- t(TestResult)
          TextToTest <- cbind(TextToTest,TestResult)
          TestResult[ , 1] <- as.character(TestResult[ , 1])
          TestResult[ , 2] <- as.character(TestResult[ , 2])
          TestResult[ , 2] <- as.character(TestResult[ , 3])
                 
          #    if(any(TextToTest[ , 4] == TextToTest[ , 5]) | any(TextToTest[ , 4] == TextToTest[ , 6])) stop("still duplicate values")
          TextToTest$V3 <- as.character(TextToTest$V3)
          accuracy1num <- sum(TextToTest$V3 == TestResult[, 1], na.rm = TRUE)
          accuracy2num <- sum(TextToTest$V3 == TestResult[, 2], na.rm = TRUE)
          accuracy3num <- sum(TextToTest$V3 == TestResult[, 3], na.rm = TRUE)
          accuracy1 <- accuracy1num/nrow(TextToTest)
          accuracy2 <- (accuracy1num + accuracy2num)/nrow(TextToTest)
          accuracy3 <- (accuracy1num + accuracy2num + accuracy3num)/nrow(TextToTest)
          #           TextToTest_name <- paste("tst_vs_tr",i, TestCorpus,  TrainCorpus, "k",k, sep="_")
          #           assign(TextToTest_name, TextToTest)
          #           save(TextToTest, file = paste(TextToTest_name, "RData", sep="."))
          cat(sprintf("%1.2f"  ,c(accuracy1, accuracy2,accuracy3 ) ), "\n")
          accu_mat[accu_mat$TestCorpus == TestCorpus & accu_mat$TrainCorpus == TrainCorpus & accu_mat$i == i &   accu_mat$k == k,  c("accu1","accu2","accu3")] <- sprintf("%1.2f"  , c(accuracy1, accuracy2,accuracy3 ))
          accu_manum[accu_mat$TestCorpus == TestCorpus & accu_mat$TrainCorpus == TrainCorpus & accu_mat$i == i &   accu_mat$k == k,  c("accu1","accu2","accu3")] <- c(accuracy1, accuracy2,accuracy3 )
          
          
        }
        cat("Finalised for prediction based on discount factor", k , "and testcorpus", TestCorpus, ".\n")
        
        
      }
      
      }
    
#     df_for_colmeans <- accu_manum[accu_manum$TestCorpus !=  accu_manum$TrainCorpus , c("accu1","accu2","accu3")]
#     list_of_means[[ paste("ngr",i, "gr_amatch", "k", k, sep="_") ]] <- colMeans(df_for_colmeans, na.rm = FALSE, dims = 1)
    cat("Finalised for prediction based on ", i , " words.\n")
    }
 
   
)


#name_accumat <- paste("accum_mat_",i, "gr_amatch", "k", k, sep="")
name_accumat <- "accuracy_test"
assign(name_accumat, accu_mat)
save(accu_mat, file = paste(name_accumat, "RData", sep="."))



#df_of_means3 <- do.call("rbind", list_of_means)
#save(accu_mat, file = "accum_mat_2gr.RData")
#save(df_of_means_new, file = "mean_accuracy.RData")  

#save(df_of_means3, file = "mean_accuracy1.RData")  

# df_of_means <- df_of_means2
# save(df_of_means, file = "mean_accuracy.RData")  

  