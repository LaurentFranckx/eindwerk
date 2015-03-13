wordslist <- list()


twitter_US <- readLines("D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/en_US.twitter.txt")
blogs_US <- readLines("D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/en_US.blogs.txt")
news_US <- readLines("D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/en_US.news.txt")


wordslist[["twitter_US"]] <- twitter_US
wordslist[["blogs_US"]] <- blogs_US
wordslist[["news_US"]] <- news_US

wordslist_max <- sapply(wordslist, function(x) max(sapply(x,nchar)))


# twitter_US_nchar <- sapply(twitter_US, nchar)
# max_twitter <- max(twitter_US_nchar)

find_word <- function(word){
  find_in_x <- function(x){
    pos <- grep(word, x)
    if (length(pos) > 0) {
      return(TRUE) 
    } else {
      return(FALSE) }    
  }
 return(find_in_x) 
}

find_love <- find_word(" love ")
twitter_love <- sapply(twitter_US, find_love, USE.NAMES = FALSE)
love_found <- twitter_US[twitter_love]

find_hate <- find_word(" hate ")
twitter_hate <- sapply(twitter_US, find_hate, USE.NAMES = FALSE)
hate_found <- twitter_US[twitter_hate]

hate_found[1:10]
length(love_found)/length(hate_found)

find_biostats <- find_word("biostats")
twitter_biostats <- sapply(twitter_US, find_biostats, USE.NAMES = FALSE)
biostats_found <- twitter_US[twitter_biostats]

kick <- "A computer once beat me at chess, but it was no match for me at kickboxing"
find_kick <- find_word(kick)
twitter_kicks <- sapply(twitter_US, find_kick, USE.NAMES = FALSE)
twitter_US[twitter_kicks]













