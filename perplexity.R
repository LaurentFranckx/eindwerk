library(tm)
library(stringr)
load(file = "cap_ston_corp_cl.RData")
load(file = "app1/data/corpuslist.RData")
source("searchwordstest.R")
source("n_gram_tokenizer.R")
source("help_functions.R")

input_txt <- "blogs"
or_txt <- paste("en_US.", input_txt, ".txt", sep="")
texttotoken <- PlainTextDocument(cap_ston_corp_cl[[or_txt]]$content)[[1]]

# set.seed(123) 
# sample <- sample( length(texttotoken), samplesize)
# texttotoken_sample <- texttotoken[ sample ]
texttotoken_sample <- texttotoken

create_token <- FALSE
ngram <- 1
tokenizer <- ngram_tokenizer(ngram)
if(create_token == TRUE){
  tokenized <- tokenizer(texttotoken_sample)
}

cat("Tokenization finalised for ", ngram, "n-gram. \n")
matrixname <- paste("en_US.",input_txt,"Tokened", ngram, "Gr.txt", sep = "")
write.table(as.matrix(tokenized), row.names = FALSE, col.names = FALSE, file = matrixname)




#here: use Linux code for counts
txttoanalyse <- paste("en_US.",input_txt,sep="")
textname <- paste(txttoanalyse, "Tokened", ngram, "Gr_unique.txt", sep="")
tokendngr <- read.table(textname, stringsAsFactors = FALSE)
names(tokendngr) <- c("freq", ngram)
tokendngr <- tokendngr[order(tokendngr$freq), ]
tokendngr2 <- tokendngr[tokendngr$freq > req_freq, ]




PerplexString <- function(string, corpus, corpuslist){
  str1 <- gsub("[[:punct:]]", " ", as.character(str1))
  str1 <- gsub("  ", " ", as.character(str1))
  uniq_str1 <- strsplit(str1, split = " ")[[1]]
  stringlenth <- length(uniq_str1)
  list_of_probs <- numeric
  firstword <- uniq_str1[[1]]
  ###calculate probability of 1st word
  firstsecond <- paste(uniq_str1[1], uniq_str1[2], "", sep=" ")
  #  ML1_df <- searchcorpus$ML1
  ML1_df <- corpuslist[[paste(corpus, 2, "Markov", sep ="" )]]
  firstsecond_prob <-  ML1_df[ML1_df$first == firstword & ML1_df$V3 == uniq_str1[2], "Bayes_prob"]
  #ML_est <- list()
  #ML_est <- matrix()
#   ML2_df <- corpuslist[[paste(corpus, 3, "Markov", sep ="" )]]
#   secondthird_prob <-  ML1_df[ML1_df$first == firstsecond & ML1_df$V3 == uniq_str1[3], "Bayes_prob"]
   while(i <= stringlenth){
    ML2_df <- corpuslist[[paste(corpus, i, "Markov", sep ="" )]]
    firstsecond <- paste(uniq_str1[i-2], uniq_str1[i-1], "", sep=" ")
    secondthird_prob <-  ML1_df[ML1_df$first == firstsecond & ML1_df$V3 == uniq_str1[i], "Bayes_prob"]
    
  }
  
  
  
}