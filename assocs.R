library(tm)


testing <- function(str1){
  str1 <- removeNumbers(tolower(str1))
  str1 <- removePunctuation(str1)
  str1 <- removeWords(str1, stopwords("english"))
  str1 <- stripWhitespace(str1)
  str1 <- stemDocument(str1)
  str1<-unlist(strsplit(str1," "))
  for(i in 1:length(str1)){
    print(str1[i])
    findAssocs(dtm, str1[i],corlimit=0.1)
  }
  #    findAssocs(dtm)
}

load(file = "cap_ston_corp_cl.RData")

set.seed(123)
create_dtm_sample <- TRUE

if(create_dtm_sample == TRUE){
  sample_ind_txt <- list()
  sample_tot <- character()
  samp_size <- 200000
  
  for(i in names(cap_ston_corp_cl)){
    sample <- sample( length(cap_ston_corp_cl[[i]]$content), samp_size)
    sample_txt <- cap_ston_corp_cl[[i]]$content[sample]
    sample_ind_txt[[i]] <- sample_txt
    sample_tot <- c(sample_tot,sample_txt) 
  }
  
}


create_dtm_blogs <- TRUE

if(create_dtm_blogs == TRUE){
  dtm_blogs <- DocumentTermMatrix(Corpus(VectorSource(sample_ind_txt[["en_US.blogs.txt"]])), control = list(global = c(10, Inf)))
  save(dtm_blogs, file = "dtm_blogs.RData")  
} 


dtm <- dtm_blogs


testing()

findAssocs(dtm, "Very early observations on the Bills game: Offense still struggling but the",corlimit=0.1)

str1 <- "observations game struggling"

str1<-unlist(strsplit(str1," "))
for(i in 1:length(str1)){
  print(str1[i])
  findAssocs(dtm, str1[i],corlimit=0.4)
}

library(qdap)
source("dfFindNextWord.R")

resultlist <- function(searchlist, dictionarylist) {
  #results <- list()
  #results <- data.frame(string = c(), search_string = c(), sourceletter = c(), next_word = c())
  results <- c()
  for(string in searchlist){
    dfwords <- dfFindNextWord(string, dictlist = dictionarylist, n = 5)
    #results <- rbind(results, c(string, paste(dfwords$searchstring, collapse = ","), sourceletter, paste(dfwords$next_word, collapse = ", ")))
    #print(dfwords)
    print(dfwords$next_word)
    results <- c(results, list(dfwords$next_word))
  }
  #names(results) <- c("string", "search_string", "sourceletter", "next_word")
  return(results)
}






