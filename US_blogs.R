library(tm)
library(domino)
library(stringi)
library(plyr)

#domino.login("LaurentFranckx", "pruts_dom")

source("n_gram_tokenizer.R")
source("help_functions.R")

load(file = "cap_ston_corp_cl.RData")


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

# strsplit_space_tokenizer <- function(x)  unlist(strsplit(as.character(x), "[[:space:]]+"))
# USBlogTokened <- strsplit_space_tokenizer(USBlogsPT[[1]])
# length(USBlogTokened)/1000
# USBlogTokenedUniq <- unique(USBlogTokened)
# length(USBlogTokenedUniq)/1000
# save(USBlogTokened, file = "USBlogTokened.RData")

#this takes forever, use linux instead
#dtm_blogsFreq <- termFreq(USBlogsPT, control = list(dictionary = FrqTermsUSBlgs))
#load the table with all the unique words as identified by Linux
# USBlogFreq <- read.table("en_US_blogsunique.txt", stringsAsFactors = FALSE)
# names(USBlogFreq) <- c("freq", "word")
# USBlogFreq <- USBlogFreq[order(USBlogFreq$freq), ]
# USBlofFeq2 <- USBlogFreq[USBlogFreq$freq > 1, ]
# USBlofFeq3 <- USBlogFreq[USBlogFreq$freq > 2, ]
# USBlofFeq10 <- USBlogFreq[USBlogFreq$freq > 9, ]
# USBlofFeq100 <- USBlogFreq[USBlogFreq$freq > 99, ]
# USBlofFeq1000 <- USBlogFreq[USBlogFreq$freq > 999, ]
# USBlofFeq10000 <- USBlogFreq[USBlogFreq$freq > 9999, ]
# 
# 
# create_token2 <- TRUE
# if(create_token2 == TRUE){
#   USBlogTokened2Gr <- two_gram_tokenizer(USBlogsPT[[1]])  
# }
# # user  system elapsed 
# # 2998.37   11.08 3267.37 
# save(USBlogTokened2Gr, file = "USBlogTokened2Gr.RData")

create_token3 <- TRUE

if(create_token3 == TRUE){
  USBlogTokened3Gr <- three_gram_tokenizer(USBlogsPT[[1]])
}

write.table(as.matrix(USBlogTokened3Gr), row.names = FALSE, col.names = FALSE, file = "USBlogTokened3Gr.txt", )
system("sort < D:/coursera/dsc_capstone/USBlogTokened3Gr.txt | uniq -c > D:/coursera/dsc_capstone/USBlogTokenedThreeGr_unique.txt
")
USBlogTokened3GrUn <- LoadNgram("USBlog","ThreeGr", 3)
USBlog3GrSplitMg <- SplitAndMargeNGr(USBlogTokened3GrUn[[2]],"ThreeGr")
USBlog3GrMarkov <- MarkovChain(USBlog3GrSplitMg,"ThreeGr")
save(USBlog3GrMarkov,"USBlog3GrMarkov.RData")



