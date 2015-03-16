library(tm)
library(domino)
library(stringi)
library(plyr)

#domino.login("LaurentFranckx", "pruts_dom")

source("n_gram_tokenizer.R")


load(file = "cap_ston_corp_cl.RData")

set.seed(123)
create_dtm_sample <- FALSE

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

###################################
#creation of the DTM for the blogs text
###################################

create_dtm_blogs <- FALSE

if(create_dtm_blogs == TRUE){
  dtm_blogs <- DocumentTermMatrix(Corpus(VectorSource(sample_ind_txt[["en_US.blogs.txt"]])), control = list(global = c(10, Inf)))
  save(dtm_blogs, file = "dtm_blogs.RData")  
} 


load(file = "dtm_blogs.RData") 

str(dtm_blogs)
inspect(dtm_blogs[1:10,1:15])

for(i in 1:10){
  cat("The number of words appearing at least ", i , " times is equal to: " , length(findFreqTerms(dtm_blogs, i)) , ".\n")
}
#the following shows that findFreqTerms looks for unique words
NrUniqWrdsBlogs <- length(findFreqTerms(dtm_blogs, 1))
TermsBlogCount <- nTerms(dtm_blogs)
length(unique(findFreqTerms(dtm_blogs, 1)))
FrqTermsUSBlgs <- findFreqTerms(dtm_blogs, 10)

# WordTotBlogs <- apply(as.matrix(dtm_blogs),1, sum)
# WordTotBlogs <- rowSums(as.matrix(dtm_blogs))


##################################""
#### WORKING WITH THE US BLOGS DOCUMENT
####################################"

# dtm_blogsFreq <- termFreq(TextDocument(sample_ind_txt[["en_US.blogs.txt"]]))
USBlogsPT <- PlainTextDocument(cap_ston_corp_cl[["en_US.blogs.txt"]]$content)
str(USBlogsPT)
head(USBlogsPT$content)

# USBlogTokened <- MC_tokenizer(USBlogsPT[[1]])
# str(USBlogTokened)
# scan_tokenizer(cap_ston_corp_cl[[1]])

strsplit_space_tokenizer <- function(x)  unlist(strsplit(as.character(x), "[[:space:]]+"))
USBlogTokened <- strsplit_space_tokenizer(USBlogsPT[[1]])
length(USBlogTokened)/1000
USBlogTokenedUniq <- unique(USBlogTokened)
length(USBlogTokenedUniq)/1000
save(USBlogTokened, file = "USBlogTokened.RData")

#this takes forever, use linux instead
#dtm_blogsFreq <- termFreq(USBlogsPT, control = list(dictionary = FrqTermsUSBlgs))
#load the table with all the unique words as identified by Linux
USBlogFreq <- read.table("en_US_blogsunique.txt", stringsAsFactors = FALSE)
names(USBlogFreq) <- c("freq", "word")
USBlogFreq <- USBlogFreq[order(USBlogFreq$freq), ]
USBlofFeq2 <- USBlogFreq[USBlogFreq$freq > 1, ]
USBlofFeq3 <- USBlogFreq[USBlogFreq$freq > 2, ]
USBlofFeq10 <- USBlogFreq[USBlogFreq$freq > 9, ]
USBlofFeq100 <- USBlogFreq[USBlogFreq$freq > 99, ]
USBlofFeq1000 <- USBlogFreq[USBlogFreq$freq > 999, ]
USBlofFeq10000 <- USBlogFreq[USBlogFreq$freq > 9999, ]


create_token2 <- FALSE

if(create_token2 == TRUE){
  USBlogTokened2Gr <- two_gram_tokenizer(USBlogsPT[[1]])  
}


# user  system elapsed 
# 2998.37   11.08 3267.37 

save(USBlogTokened2Gr, file = "USBlogTokened2Gr.RData")

create_token3 <- FALSE

if(create_token3 == TRUE){
  USBlogTokened3Gr <- three_gram_tokenizer(USBlogsPT[[1]])
}

# user  system elapsed 
# 2661.19   31.88 4500.72

#save(USBlogTokened3Gr, file = "USBlogTokened3Gr.RData")
write.table(as.matrix(USBlogTokened3Gr), row.names = FALSE, col.names = FALSE, file = "USBlogTokened3Gr.txt", )
#USBlogTokened3GrUn <- read.table("USBlogTokened3Gr_unique.txt", stringsAsFactors = FALSE, nrows = 37093446)


#######################""
# read the table with the 3Grams from US blogs and transform them in Markov matrcies
############################""


USBlogTokened3GrUn <- LoadNgram("USBlog","ThreeGr", 3)
USBlog3GrSplitMg <- SplitAndMargeNGr(USBlogTokened3GrUn[[2]],"ThreeGr")
USBlog3GrMarkov <- MarkovChain(USBlog3GrSplitMg,"ThreeGr")




