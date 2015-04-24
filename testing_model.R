library(tm)
library(stringr)
load(file = "cap_ston_corp_cl.RData")
#load(file = "app1/data/corpuslist.RData")
load(file = "app2/data/corpuslist.RData")

source("searchwordstest.R")
source("n_gram_tokenizer.R")
source("help_functions.R")
#source("app1/searchwords.R")
source("app2/searchwordsBlogsNews.R")


library(stringdist)

set.seed(123)
ngramscope <- 4:5
kscope <- seq(0.0,1.0,by=0.2)
#corpuses <- c("news", "twitter", "blogs")
corpuses <- c("twitter", "BlogsNews")

samplesize <- 5
accu_mat <- expand.grid(corpuses,corpuses, ngramscope, kscope)
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
    for(i in ngramscope){
      #for(i in 5){   
      cat("started for i = ", i , "\n")
      
      #  for(k in seq(0.13,0.17, by = 0.02)){
      #    cat("started for k = ", k , "\n")
      for(TestCorpus in corpuses){
        resdir <- "D:/coursera/caps_res/"
#         if(i < 5){
#           DfToSample <- read.table(paste(resdir, "US.",TestCorpus,"Tokened", i,"Gr_uniq.txt",sep=""), stringsAsFactors = FALSE)
#         } else {
           DfToSample <- read.table(paste(resdir, TestCorpus,"Tokened", i,"Gr_uniq.txt",sep=""), stringsAsFactors = FALSE)
#        }
        
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
        for(k in kscope){
          
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
#save(accu_mat, file = paste(name_accumat, "RData", sep="."))

# resutsmodeltesti5 <- read.csv("resutsmodeltesti5.csv")
# 
# results_twitter <- resutsmodeltesti5[resutsmodeltesti5$TrainCorpus== "twitter", ]
# results_blogs <- resutsmodeltesti5[resutsmodeltesti5$TrainCorpus== "blogs", ]
# results_news <- resutsmodeltesti5[resutsmodeltesti5$TrainCorpus== "news", ]

load(file = paste(name_accumat, "RData", sep="."))

accu_mat_twitter <- accu_mat[accu_mat$TrainCorpus== "twitter"&  accu_mat$i ==5, ]
accu_mat_news <- accu_mat[accu_mat$TrainCorpus== "news"&  accu_mat$i ==5, ]
accu_mat_blogs <- accu_mat[accu_mat$TrainCorpus== "blogs"&  accu_mat$i ==5, ]

library(ggplot2)

plot_train_test <- function(train_df, test){
  id_vars <- c("k","TestCorpus")
  meas_vars <- c("accu1","accu2","accu3")
  accu_mat <- train_df[train_df$TestCorpus == test,  ]
  accu_mat_twitter_plot <- accu_mat[ , c(id_vars,"accu1")]
  accu_mat_twitter_plot$words <- c(1)
  names(accu_mat_twitter_plot) <- gsub("accu1","Accuracy",names(accu_mat_twitter_plot))
  accu_mat_twitter_plot2 <- accu_mat[ , c(id_vars, "accu2")]
  accu_mat_twitter_plot2$words <- c(2)
  names(accu_mat_twitter_plot2) <- gsub("accu2","Accuracy",names(accu_mat_twitter_plot2))
  accu_mat_twitter_plot3 <- accu_mat[ , c(id_vars,"accu3")]
  accu_mat_twitter_plot3$words <- c(3)
  names(accu_mat_twitter_plot3) <- gsub("accu3","Accuracy",names(accu_mat_twitter_plot3))
  
  accu_mat_twitter_plot_new <- rbind(accu_mat_twitter_plot, accu_mat_twitter_plot2)
  accu_mat_twitter_plot_new <- rbind(accu_mat_twitter_plot_new, accu_mat_twitter_plot3)
  names(accu_mat_twitter_plot_new) <- gsub("k","Discount_factor",names(accu_mat_twitter_plot_new))
  
  accu_mat_twitter_plot_new$words <- as.factor(accu_mat_twitter_plot_new$words)
  names(accu_mat_twitter_plot_new) <- gsub("words","suggested_words",names(accu_mat_twitter_plot_new))
  return(accu_mat_twitter_plot_new)
  #ggplot(accu_mat_twitter_plot_new, aes(Discount_factor, Accuracy, colour = suggested_words)) + geom_point() + facet_grid(.  ~ TestCorpus)
  #ggplot(accu_mat_twitter_plot_new, aes(Discount_factor, Accuracy, colour = suggested_words)) + geom_point() 
  
}

twitplot <- plot_train_test(accu_mat_twitter, "blogs")
twitplot$TrainCorpus <- "twitter"
newsplot <- plot_train_test(accu_mat_news, "blogs")
newsplot$TrainCorpus <- "news"
blogplot <- plot_train_test(accu_mat_blogs, "news")
blogplot$TrainCorpus <- "blogs"

toplot <- rbind(twitplot,blogplot)
toplot <- rbind(toplot,newsplot)
ggplot(toplot, aes(Discount_factor, Accuracy, colour = suggested_words)) + geom_point() + facet_grid(.  ~ TrainCorpus)



# library(grid)
# grid.newpage()
# pushViewport(viewport(layout = grid.layout(1, 3)))
# vplayout <- function(x, y)
#   viewport(layout.pos.row = x, layout.pos.col = y)
# print(twitplot, vp = vplayout(1, 1))
# print(blogplot, vp = vplayout(1, 2))
# print(newsplot, vp = vplayout(1, 3))


#for twitter: highest accuracy with k = 0.20 and use "blogs"
#for news: highest accuracy with k = 0.40 and use "blogs"
# for blogs: k does not matter, use "news"


#df_of_means3 <- do.call("rbind", list_of_means)
#save(accu_mat, file = "accum_mat_2gr.RData")
#save(df_of_means_new, file = "mean_accuracy.RData")  

#save(df_of_means3, file = "mean_accuracy1.RData")  

# df_of_means <- df_of_means2
# save(df_of_means, file = "mean_accuracy.RData")  

  