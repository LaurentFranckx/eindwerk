library(tm)


load(file = "cap_ston_corp_cl.RData")

# cap_ston_dtm <- DocumentTermMatrix(cap_ston_corp_cl, control = list(global = c(1000, Inf)))
# save(cap_ston_dtm, file = "cap_ston_dtm.RData")

# samp_size <- 100000
# sample_1 <- sample( length(cap_ston_corp_cl[[1]]$content), samp_size)
# sample_Corp <- cap_ston_corp_cl[[1]]$content[sample_1]
# system.time(
#   sample_dtm <- DocumentTermMatrix(Corpus(VectorSource(sample_Corp)), control = list(global = c(10, Inf)))
#   
# )
# 
# user  system elapsed 
# 153.15    0.09  154.14 


# samp_size <- 500000
# sample_1 <- sample( length(cap_ston_corp_cl[[1]]$content), samp_size)
# sample_Corp <- cap_ston_corp_cl[[1]]$content[sample_1]
# system.time(
#   sample_dtm <- DocumentTermMatrix(Corpus(VectorSource(sample_Corp)), control = list(global = c(10, Inf)))
#   
# )
# 
# user  system elapsed 
# 588.92    0.84  594.77 

#takes more than hour to run
# system.time(
#   sample_dtm <- DocumentTermMatrix(cap_ston_corp_cl[1], control = list(global = c(10, Inf)))
#   
# )

sample_ind_txt <- list()
sample_tot <- character()
samp_size <- 200000

for(i in names(cap_ston_corp_cl)){
  sample <- sample( length(cap_ston_corp_cl[[i]]$content), samp_size)
  sample_txt <- cap_ston_corp_cl[[i]]$content[sample]
  sample_ind_txt[[i]] <- sample_txt
  sample_tot <- c(sample_tot,sample_txt) 
}

dtm_blogs <- DocumentTermMatrix(Corpus(VectorSource(sample_ind_txt[["en_US.blogs.txt"]])), control = list(global = c(10, Inf)))
save(dtm_blogs, file = "dtm_blogs.RData")
str(dtm_blogs)
inspect(dtm_blogs[1:10,1:15])

for(i in 1:10){
  cat("For min appearance ", i , "length of words vecor: " , length(findFreqTerms(dtm_blogs, i)) , ".\n")
}
#the following shows that findFreqTerms looks for unique words
NrUniqWrdsBlogs <- length(findFreqTerms(dtm_blogs, 1))
TermsBlogCount <- nTerms(dtm_blogs)
length(unique(findFreqTerms(dtm_blogs, 1)))

# WordTotBlogs <- apply(as.matrix(dtm_blogs),1, sum)
# WordTotBlogs <- rowSums(as.matrix(dtm_blogs))

# dtm_blogsFreq <- termFreq(TextDocument(sample_ind_txt[["en_US.blogs.txt"]]))
USBlogsPT <- PlainTextDocument(cap_ston_corp_cl[["en_US.blogs.txt"]]$content)

dtm_blogsFreq <- termFreq(USBlogsPT)




library("Matrix") 

MatrixBlogs <- sparseMatrix(i=dtm_blogs$i, j=dtm_blogs$j, x=dtm_blogs$v,
                                      dims=c(dtm_blogs$nrow, dtm_blogs$ncol))



FreqInBlogs <- findFreqTerms(dtm_blogs, 5)
blogs_dict <- Dict





# system.time(
#   sample_dtm <- DocumentTermMatrix(Corpus(VectorSource(sample_Corp)), control = list(global = c(10, Inf)))
#   
# )



inspect(sample_dtm[1:10,1:15])
inspect(sample_dtm[1:10,10000:10005])


# 
# cap_ston_dtm <- DocumentTermMatrix(cap_ston_corp_cl)
