library(tm)
library(stringr)
folder_txt <- "D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/"
txts <- list.files(folder_txt, "txt")
#loading in binary format is too slow
#cap_ston_corp <- Corpus(DirSource(folder_txt, encoding = "UTF-8", mode = "binary"))
#cap_ston_corp <- Corpus(DirSource(folder_txt, encoding = "UTF-8"))
cap_ston_corp <- Corpus(DirSource(folder_txt, encoding = "UTF-8"))


banned_words <- readLines("D:/coursera/dsc_capstone/full-list-of-bad-words-banned-by-google-txt-file/banned.txt")

replace_utf_by_latin <- content_transformer(function(x) iconv(x, from="UTF-8", to="latin1", sub=" "))
replace_by_empty <- content_transformer(function(x, pattern) gsub(pattern," ",x))


cap_ston_corp_cl <- tm_map(cap_ston_corp, removeNumbers)
#cap_ston_corp_cl <- tm_map(cap_ston_corp_cl, removePunctuation)
# cap_ston_corp_cl <- tm_map(cap_ston_corp_cl, str_replace_all, pattern = "[[:punct:]]", replacement = " ")
# cap_ston_corp_cl <- tm_map(cap_ston_corp_cl, str_replace_all, pattern = "^[:alpha:]", replacement = " ")
cap_ston_corp_cl <- tm_map(cap_ston_corp_cl, replace_utf_by_latin)
#cap_ston_corp_cl <- tm_map(cap_ston_corp_cl, replace_by_empty, "[[:punct:]]")
#cap_ston_corp_cl <- tm_map(cap_ston_corp_cl, replace_by_empty, "^[:alpha:]")

cap_ston_corp_cl <- tm_map(cap_ston_corp_cl, removeWords, banned_words)
cap_ston_corp_cl <- tm_map(cap_ston_corp_cl, stripWhitespace)

save(cap_ston_corp_cl, file = "cap_ston_corp_cl.RData")


