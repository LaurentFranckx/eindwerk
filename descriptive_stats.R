
load(file = "cap_ston_corp_cl.RData")
library("poweRlaw")

##################################""
#### WORKING WITH THE US BLOGS DOCUMENT
####################################"

# dtm_blogsFreq <- termFreq(TextDocument(sample_ind_txt[["en_US.blogs.txt"]]))
USBlogsPT <- PlainTextDocument(cap_ston_corp_cl[["en_US.blogs.txt"]]$content)
#str(USBlogsPT)
#head(USBlogsPT$content)

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

TestFreqTable <- read.table("US.twitterunique.txt", stringsAsFactors = FALSE)
names(TestFreqTable) <- c("freq", "word")
TestFreqTable <- TestFreqTable[order(TestFreqTable$freq, decreasing = TRUE), ]
summary(TestFreqTable$freq)
# USBlofFeq2 <- USBlogFreq[USBlogFreq$freq > 1, ]
# USBlofFeq3 <- USBlogFreq[USBlogFreq$freq > 2, ]
# USBlofFeq10 <- USBlogFreq[USBlogFreq$freq > 9, ]
# USBlofFeq100 <- USBlogFreq[USBlogFreq$freq > 99, ]
# USBlofFeq1000 <- USBlogFreq[USBlogFreq$freq > 999, ]
# USBlofFeq10000 <- USBlogFreq[USBlogFreq$freq > 9999, ]

freq_count <- TestFreqTable$freq
FreqPow <- displ$new(freq_count)
FreqPow$getXmin()
#USBlogFreqPowPars <- estimate_pars(USBlogFreqPow, pars = NULL)
FreqPowMin <- estimate_xmin(FreqPow, pars = NULL)
FreqPow$setXmin(FreqPowMin)


# USBlogFreqPowEst <- USBlogFreqPow$setPars(USBlogFreqPowPars$pars)
# USBlogFreqPow$getPars()
# USBlogFreqPow$setPars(2)

bootstrap_p(FreqPow)

#plot(FreqPow)
summary(FreqPow$internal$freq)
summary(FreqPow$internal$values)
summary(freq_count)
y <- FreqPow$internal$freq
x <- FreqPow$internal$values
df <- cbind(x,y)
head(df)
tail(df)
plot(log(x),log(y))

TestFreqTable[ TestFreqTable$freq == max(TestFreqTable$freq) , ]

test <- dist_cdf(FreqPow)

system.time(
  TestFreqTable$sum_of_freq <- cumsum(freq_count) 
  )


total_count <- sum(freq_count)


system.time(
  TestFreqTable$sum_of_shares <- TestFreqTable$sum_of_freq/total_count
)


TestFreqTable[TestFreqTable$sum_of_shares > 0.5, ][1, ]








