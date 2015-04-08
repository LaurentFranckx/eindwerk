library(domino)
domino.login("LaurentFranckx", "pruts_dom")
#domino.run("load_whole_corpus.R" )

load(file = "cap_ston_corp_cl.RData")

corpus_list <- c("blogs","news","twitter")

for(corpus in corpus_list){
#  for(ngram in 2:4){
  for(ngram in c(5)){
    domino.run("TokenizeMarkov.R",corpus, ngram )
  }  
}

