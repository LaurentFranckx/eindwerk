library(tm)
library(stringr)
library(data.table)

#library(domino)
install.packages("domino", dependencies=TRUE, repos='http://cran.us.r-project.org')
install.packages("stringi", dependencies=TRUE, repos='http://cran.us.r-project.org')

library(stringi)
library(plyr)

source("n_gram_tokenizer.R")
source("help_functions.R")


ngram<- as.numeric(commandArgs(trailingOnly = TRUE)[2])
corpus <- commandArgs(trailingOnly = TRUE)[1]


load(file = "cap_ston_corp_cl.RData")

ngram <- 5
#ngram = 5, req_freq 1 is only observeed
TokenizeMarkov(corpus, ngram= ngram, req_freq = 1, create_token = TRUE)
#TokenizeMarkov(corpus, ngram= ngram, req_freq = 3, create_token = TRUE)





