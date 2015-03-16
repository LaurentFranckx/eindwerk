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


USBlogTokened3GrUn <- read.table("USBlogTokened3Gr_unique.txt", stringsAsFactors = FALSE)
names(USBlogTokened3GrUn) <- c("freq", "ThreeGr")
hist(USBlogTokened3GrUn$freq)
summary(USBlogTokened3GrUn$freq)


USBlogTokened3GrUn <- USBlogTokened3GrUn[order(USBlogTokened3GrUn$freq), ]
#USBlogTokened3GrUn1 <- USBlogTokened3GrUn[USBlogTokened3GrUn$freq > 1, ]
nrow(USBlogTokened3GrUn)/nrow(USBlogTokened3GrUn1)
USBlogTokened3GrUn2 <- USBlogTokened3GrUn[USBlogTokened3GrUn$freq > 2, ]

#USBlogTokened3GrUn2 <- USBlogTokened3GrUn[USBlogTokened3GrUn$freq > 3, ]
# USBlogTokened3GrUn4 <- USBlogTokened3GrUn[USBlogTokened3GrUn$freq > 4, ]
# hist(USBlogTokened3GrUn4$freq)
# summary(USBlogTokened3GrUn4$freq)
rm(USBlogTokened3GrUn)
gc()


# load(file = "USBlogTokened3Gr.RData")


#only split the 3-grams that occur at least 5 times 
# system.time(
# #  USBlogTokened3Gr_split <- sapply(USBlogTokened3Gr[1:1000], stri_split_boundaries, type="word")
# #  USBlogTokened3Gr_split <- stri_split_boundaries(USBlogTokened3Gr[1:100], type="word", simplify = TRUE)
# #  USBlogTokened3GrUn4_split <- stri_split_boundaries(USBlogTokened3GrUn4[sample, "ThreeGr"], simplify = TRUE) 
#   USBlogTokened3GrUn4_split <- stri_split_boundaries(USBlogTokened3GrUn4[, "ThreeGr"], simplify = TRUE)   
#   )
# 
# # user  system elapsed 
# # 0.81    0.03    0.84
# 

#only split the 3-grams that occur at least 3 times 
system.time(
  USBlogTokened3GrUn2_split <- stri_split_boundaries(USBlogTokened3GrUn2[, "ThreeGr"], simplify = TRUE)   
)

# user  system elapsed 
# 2.00    0.05    2.09 


#only split the 3-grams that occur at least 2 times 
# system.time(
#   USBlogTokened3GrUn1_split <- stri_split_boundaries(USBlogTokened3GrUn1[, "ThreeGr"], simplify = TRUE)   
# )

#due to anomaly that occured when splitting USBlogTokened3GrUn1_split
# summary(USBlogTokened3GrUn1_split[,4])
# any(USBlogTokened3GrUn1_split[,4] != "")
# length(which(USBlogTokened3GrUn1_split[,4] != ""))
# USBlogTokened3GrUn1_split[ USBlogTokened3GrUn1_split[,4] != "", ]
# weird_split <- USBlogTokened3GrUn1_split[,4] != ""
# USBlogTokened3GrUn1[weird_split, ]
# USBlogTokened3GrUn1_split <- stri_split_boundaries(USBlogTokened3GrUn1[!(weird_split), "ThreeGr"], simplify = TRUE) 

first_two <- paste(USBlogTokened3GrUn2_split[, 1], USBlogTokened3GrUn2_split[, 2], sep ="")
USBlogTokened3GrUn2_merged <- cbind(first_two, USBlogTokened3GrUn2_split)
USBlogTokened3GrUn2_merged <- cbind(USBlogTokened3GrUn2_merged, USBlogTokened3GrUn2 )
USBlogTokened3GrUn2_merged <- USBlogTokened3GrUn2_merged[ , c("first_two", "V4", "ThreeGr" , "freq" )]
names(USBlogTokened3GrUn2_merged) <- gsub("V4","third_trm",names(USBlogTokened3GrUn2_merged))

# set.seed(123)
# samp_size <- 500000
# sample <- sample( nrow(USBlogTokened3GrUn2_merged), samp_size)
# 
USBlogTokened3GrUn2_merged_toAg <- USBlogTokened3GrUn2_merged[, ]
df_to_aggr <- USBlogTokened3GrUn2_merged_toAg[  , c("freq")]

rm(USBlogTokened3GrUn2_split)
rm(USBlogTokened3GrUn2)
gc()


#### sum the number of times the first two terms occur together
#takes more than two hours with the 3-grams that occur at least 2 times 
# takes more than 1 hour as well with the 3-grams that occur at least three times
# alot of operations take place in cached memory, moreover
system.time(
  USBlogTokened3GrUn2FrqFirst  <-  aggregate(df_to_aggr, by = list(USBlogTokened3GrUn2_merged_toAg$first_two),  FUN = sum )
  
)

names(USBlogTokened3GrUn2FrqFirst) <- c("first_two", "freq_first_2")

Markov_mat <- join(USBlogTokened3GrUn2_merged_toAg, USBlogTokened3GrUn2FrqFirst)
Markov_mat$Bayes_prob <- with(Markov_mat, freq/freq_first_2)
Markov_mat <- Markov_mat[order(Markov_mat$freq_first_2,Markov_mat$freq), ]
head(Markov_mat)
tail(Markov_mat)
summary(Markov_mat)
old_Markov_mat <- Markov_mat

Markov_ML <- list()

for(i in 1:3){
  #find the maximal Bayesian prob for each first "two term"
  df_to_max <- old_Markov_mat[  , c("Bayes_prob")]
  df_max <- aggregate(df_to_max, by = list(old_Markov_mat$first_two) , FUN = max)
  names(df_max) <- c("first_two", "Bayes_prob_max")
  
  #Markov_mat2 compares for each 3-gram the actual Bayesian prob with the max prob for the first 2-gram
#   old_Markov_mat$first_two <- as.character(old_Markov_mat$first_two)
#   df_max$first_two <- as.character(df_max$first_two)
  Markov_mat2 <- join(old_Markov_mat,df_max)
  
  #select all the rows with less than the max Bayes prob for second iteration
  old_Markov_mat <- Markov_mat2[Markov_mat2$Bayes_prob < Markov_mat2$Bayes_prob_max, ]
  
  #this gives, for each first 2-gram, the 3-gram with the highest likelihood
  #in the case of draws for the likelikhood, there will be duplications
  Markov_help <- Markov_mat2[Markov_mat2$Bayes_prob == Markov_mat2$Bayes_prob_max, ]
  Mardupl <- Markov_help[duplicated(Markov_help$first_two),]
  Markov_help <- Markov_help[!(Markov_help$ThreeGr %in% Mardupl$ThreeGr),     ]

  markov_name <- paste("ML",i,sep="")
  Markov_ML[[markov_name]] <- Markov_help
  
  if(nrow(Markov_mat2) != nrow(Markov_help) + nrow(old_Markov_mat) + nrow(Mardupl)) stop("Problem in creation Markow table")
  old_Markov_mat <- rbind(old_Markov_mat,Mardupl) 
  old_Markov_mat  <- old_Markov_mat[  , setdiff(names(old_Markov_mat),"Bayes_prob_max" )]
}

summary(Markov_ML[[2]])
tail(Markov_ML[[3]])
nrow(Markov_ML[[1]])
nrow(Markov_ML[[2]])
nrow(Markov_ML[[3]])





# user  system elapsed 
# 3.35    0.02    3.37 

# try to split all 3-grams, including those that occur only once in the US blogs text
#it is possible to perform this in about 2 minutes, but then it takes your whole memory
# system.time(
#   USBlogTokened3GrUn_split <- stri_split_boundaries(USBlogTokened3GrUn[, "ThreeGr"], simplify = TRUE)   
# )


