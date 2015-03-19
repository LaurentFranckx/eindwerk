library(domino)
domino.login("LaurentFranckx", "pruts_dom")
domino.run("load_whole_corpus.R" )


corpus_list <- c("US.blogs","US.news","US.twitter")

for(corpus in corpus_list){
  for(ngram in 2:4){
    domino.run("TokenizeMarkov.R",corpus, ngram )
  }  
}

