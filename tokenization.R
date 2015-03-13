# wordslist <- list()
#  
#  
# twitter_US <- readLines("D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/en_US.twitter.txt")
# blogs_US <- readLines("D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/en_US.blogs.txt")
# news_US <- readLines("D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/en_US.news.txt")
# # 
# # 
# wordslist[["twitter_US"]] <- twitter_US
# wordslist[["blogs_US"]] <- blogs_US
# wordslist[["news_US"]] <- news_US
# save(wordslist, file = "wordslist.RData")

#load(file = "wordslist.RData")
library(tm)

# EN_corp_news <- Corpus(VectorSource(wordslist[["news_US"]])) 
# save(EN_corp_news, file = "EN_corp_news.RData")
load(file = "EN_corp_news.RData")
inspect(EN_corp_news[1:3])
inspect(EN_corp_news[77259])
inspect(EN_corp_news[77255:77265])

# EN_corp_blogs <- Corpus(VectorSource(wordslist[["blogs_US"]])) 
# save(EN_corp_blogs, file = "EN_corp_blogs.RData")
load(file = "EN_corp_blogs.RData")
inspect(EN_corp_blogs[899280:899290])
# rm(EN_corp_blogs)
# gc()


# EN_corp_twitter_1 <- Corpus(VectorSource((wordslist[["twitter_US"]])[1:1000000])) 
# save(EN_corp_twitter_1, file = "EN_corp_twitter_1.RData")
load(file = "EN_corp_twitter_1.RData")
# rm(EN_corp_twitter_1)
# gc()


# EN_corp_twitter_2 <- Corpus(VectorSource((wordslist[["twitter_US"]])[1000000:2000000])) 
# save(EN_corp_twitter_2, file = "EN_corp_twitter_2.RData")
load(file = "EN_corp_twitter_2.RData")
# rm(EN_corp_twitter_2)
# gc()

# EN_corp_twitter_3 <- Corpus(VectorSource((wordslist[["twitter_US"]])[2000000:2360148])) 
# save(EN_corp_twitter_3, file = "EN_corp_twitter_3.RData")
load(file = "EN_corp_twitter_3.RData")
inspect(EN_corp_twitter_3[1:10])

download.file("http://www.bannedwordlist.com/lists/swearWords.txt", "swearWords.txt" )
swearwords <- readLines("swearWords.txt")


banned_words <- readLines("D:/coursera/dsc_capstone/full-list-of-bad-words-banned-by-google-txt-file/banned.txt")

EN_corp_twitter_3_cl <- tm_map(EN_corp_twitter_3, removeNumbers)
EN_corp_twitter_3_cl <- tm_map(EN_corp_twitter_3_cl, removePunctuation)
EN_corp_twitter_3_cl <- tm_map(EN_corp_twitter_3_cl, removeWords, banned_words)
EN_corp_twitter_3_cl <- tm_map(EN_corp_twitter_3_cl, stripWhitespace)
inspect(EN_corp_twitter_3_cl[1:10])







