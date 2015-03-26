library(stringdist)
library(plyr)

corpusnames <- c("news","twitter","blogs")
strgtgth <- 3
corpuslist <- list()
for(corpus in corpusnames){
  load(paste("US.", corpus, strgtgth +1, "Markov.RData" , sep= ""))
  searchcorpus <- get(paste("US.",corpus, strgtgth +1, "Markov", sep ="" ))
#   searchmatrix <- paste(corpus,"search", sep="")
#   assign(searchmatrix, searchcorpus$ML1 ) 
  corpuslist[[corpus]] <- searchcorpus$ML1
}

freq_tables <- list()

for(name in corpusnames){
  TestFreqTable <- read.table(paste("US.", name , "unique.txt", sep=""), stringsAsFactors = FALSE)
  names(TestFreqTable) <- c("freq", "word")
  freq_tables[[name]]  <- TestFreqTable
}

freq_tables_df <- do.call(rbind, freq_tables)
freq_tables_df_sum <-  aggregate(freq_tables_df$freq, by = list(freq_tables_df$word), FUN = sum)
names(freq_tables_df_sum) <- c("word","freq" )
#freq_tables_df_sum$freq <- as.numeric(freq_tables_df_sum$freq)

str1 <- "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"
str1 <- gsub("[[:punct:]]", " ", as.character(str1))
str1 <- gsub("  ", " ", as.character(str1))
uniq_str1 <- unique(strsplit(str1, split = " ")[[1]])
uniq_str1 <- as.data.frame(uniq_str1)
names(uniq_str1) <- "word"
uniq_str_freq <- join(uniq_str1, freq_tables_df_sum)
uniq_str_freq <- uniq_str_freq[order(uniq_str_freq$freq), ]
uniq_str_freq_low <- as.character(uniq_str_freq[1:3, "word"]) 

SearchStrCorpus(str1, "US.twitter")
# No results found in this iteration. 
# New search string is: and I'd.
#          V3 Bayes_prob
# 285817   be  0.4545455
# 104627 love  0.3333333
# 52665  like  0.2121212

#ML1_df$V5 <- as.character(ML1_df$V5)
suggestions <- c("sleep","eat", "give", "die" )

ClosestMatchStrSug <- function(suggest){
  sleep <- sapply(corpuslist, function(x)  x[x$V5 == suggest, ] , simplify = FALSE  )
  sleep <- do.call(rbind,sleep)
  stringdist(str1, sleep[1, "FourGr" ])
  distsleep <- sapply(sleep[, "first" ] ,  stringdist, str1 )
  sleep <- sleep[distsleep == min(distsleep),]
  return(sleep)
}

lapply(suggestions, ClosestMatchStrSug)


suggestions <- c("sleep","eat", "give", "die" )

for(suggest in suggestions){
  txtfile <- paste("D:/coursera/caps_res/",suggest,".txt",sep="")
  assign(paste(suggest,"_txt",sep="") , readLines(txtfile))
}



sleep_txt[stringdist(str1, sleep_txt) == min(stringdist(str1, sleep_txt))]
uniq_str1_paste <- Reduce(paste, uniq_str1)

#sleep_txt[stringdist(uniq_str1_paste, sleep_txt) == min(stringdist(uniq_str1_paste, sleep_txt))]
sleep_txt[stringdist(uniq_str1_paste, sleep_txt) == min(stringdist(uniq_str1_paste, sleep_txt))]
eat_txt[stringdist(uniq_str1_paste, eat_txt) == min(stringdist(uniq_str1_paste, eat_txt))]
give_txt[stringdist(uniq_str1_paste, give_txt) == min(stringdist(uniq_str1_paste, give_txt))]
die_txt[stringdist(uniq_str1_paste, die_txt) == min(stringdist(uniq_str1_paste, die_txt))]

sleep_txt[stringdist(uniq_str_freq_low, sleep_txt) == min(stringdist(uniq_str_freq_low, sleep_txt))]
eat_txt[stringdist(uniq_str_freq_low, eat_txt) == min(stringdist(uniq_str_freq_low, eat_txt))]
give_txt[stringdist(uniq_str_freq_low, give_txt) == min(stringdist(uniq_str_freq_low, give_txt))]
die_txt[stringdist(uniq_str_freq_low, die_txt) == min(stringdist(uniq_str_freq_low, die_txt))]



DistSuggestToCorpus <- function(suggest){
  sleep <- sapply(corpuslist, function(x)  x[x$V5 == suggest, ] , simplify = FALSE  )
  sleep <- do.call(rbind,sleep)
  stringdist(str1, sleep[1, "FourGr" ])
  distsleep <- stringdist(sleep[, "first" ] , str1 )
  distsleep2 <- stringdist(sleep[, "first" ] ,  uniq_str_freq_low )
  returnlist <- list(sleep[distsleep == min(distsleep), "FourGr"],sleep[distsleep2 == min(distsleep2), "FourGr"] )
  names(returnlist) <- c("Complete string", "Most frequent words")
  return(returnlist)
}

MinDistMarkov <- lapply(suggestions, DistSuggestToCorpus)
names(MinDistMarkov) <- suggestions





ML1_df[ML1_df$V5 == "sleep", ]
eat_twit <- ML1_df[ML1_df$V5 == "eat", ]
eat_give <- ML1_df[ML1_df$V5 == "give", ]
eat_die <- ML1_df[ML1_df$V5 == "die", ]


#############################################"
#############################################"
#############################################"
Question 2
str2 <- "Guy at my table's wife got up to go to the bathroom and I asked about dessert and he started telling me about his"
horticultural
spiritual
marital
financial

SearchStrCorpus(str2, "US.twitter")
# 330949  new 0.28971963
# 143404 work 0.14018692
# 85108  book 0.07476636

SearchStrCorpus(str2, "US.blogs")
# 634312 life 0.08431373
# 384305  own 0.06862745
# 290287  new 0.04509804

#############################################"
#############################################"
#############################################"

Question 3
str3 <- "I'd give anything to see arctic monkeys this"
weekend
morning
decade
month


SearchStrCorpus(str3, "US.twitter")
# No results found in this iteration. 
# New search string is: monkeys this.
# No results found in this iteration. 
# New search string is:  this .
# V3 Bayes_prob
# 618731      is 0.06275629
# 577791 weekend 0.05824556
# 554454 morning 0.05597813

SearchStrCorpus(str3, "US.blogs")
# No results found in this iteration. 
# New search string is: monkeys this.
# No results found in this iteration. 
# New search string is:  this .
# V3 Bayes_prob
# 743827   is 0.07640535
# 693360 year 0.02888031
# 662585 week 0.02829437



#############################################"
#############################################"
#############################################"



Question 4
str4 <- "Talking to your mom has the same effect as a hug and helps reduce your"
stress
hunger
sleepiness
happiness

SearchStrCorpus(str4, "US.twitter")
# 627086 favorite 0.02128423
# 586143     life 0.02004669
# 562803      own 0.01638330


SearchStrCorpus(str4, "US.blogs")
# No results found in this iteration. 
# New search string is: reduce your.
# V3 Bayes_prob
# 160167 risk          1


SearchStrCorpus(str4, "US.news")
# No results found in this iteration. 
# New search string is: reduce your.
# V3 Bayes_prob
# 117280 risk          1




#############################################"
#############################################"
#############################################"
Question 5
str5 <- "When you were in Holland you were like 1 inch away from me but you hadn't time to take a"
picture
walk
look
minute

SearchStrCorpus(str5, "US.twitter")
V3 Bayes_prob
311110    nap 0.14473684
126933  break 0.09688995
86664  shower 0.08851675

SearchStrCorpus(str5, "US.blogs")
V3 Bayes_prob
445684 picture 0.09771310
213170    look 0.08939709
151575   break 0.07207207

SearchStrCorpus(str5, "US.news")
V3 Bayes_prob
390304 lead 0.12768362
145731 look 0.06892655
95764   job 0.03276836


#############################################"
#############################################"
#############################################"
Question 6
str6 <- "I'd just like all of these questions answered, a presentation of evidence, and a jury to settle the"
matter
case
incident
account

SearchStrCorpus(str6, "US.twitter")
No results found in this iteration. 
New search string is: settle the.
V3 Bayes_prob

SearchStrCorpus(str6, "US.blogs")
No results found in this iteration. 
New search string is: settle the.
V3 Bayes_prob
133955  bill  0.5555556
3153   issue  0.4444444

SearchStrCorpus(str6, "US.news")
V3 Bayes_prob
255403    case       0.68
34354  dispute       0.16
109362   issue       0.16



#############################################"
#############################################"
#############################################"
Question 7
str7 <- "I can't deal with unsymetrical things. I can't even hold an uneven number of bags of groceries in each"
toe
finger
arm
hand

SearchStrCorpus(str7, "US.twitter")
No results found in this iteration. 
New search string is: in each.
V3 Bayes_prob
308788      of  0.2117647
124086 other's  0.1647059
68662    other  0.1294118

SearchStrCorpus(str7, "US.blogs")
No results found in this iteration. 
New search string is: in each.
          V3 Bayes_prob
644494    of 0.22323049
394156 other 0.12341198
299820    sc 0.06352087

SearchStrCorpus(str7, "US.news")
No results found in this iteration. 
New search string is: in each.
              V3 Bayes_prob
690679        of 0.50328515
402862 direction 0.07227332
305175     other 0.04204993



#############################################"
#############################################"
#############################################"
Question 8
str8 <- "Every inch of you is perfect from the bottom to the"
top
center
middle
side

SearchStrCorpus(str8, "US.twitter")
       V3 Bayes_prob
16770 top          1

SearchStrCorpus(str8, "US.blogs")
        V3 Bayes_prob
101573 top          1

SearchStrCorpus(str8, "US.news")
No results found in this iteration. 
New search string is: to the.
           V3 Bayes_prob
909072 public 0.01444881
619355      U 0.01309968
519756  state 0.01301264

#############################################"
#############################################"
#############################################"
Question 9
str9 <- "I’m thankful my childhood was filled with imagination and bruises from playing"
weekly
outside
inside
daily

SearchStrCorpus(str9, "US.twitter")
No results found in this iteration. 
New search string is: from playing.
               V3 Bayes_prob
165987        the  0.7647059
19783  basketball  0.2352941

SearchStrCorpus(str9, "US.blogs")
No results found in this iteration. 
New search string is: from playing.
        V3 Bayes_prob
91922 with          1


SearchStrCorpus(str9, "US.news")
No results found in this iteration. 
New search string is: from playing.
         V3 Bayes_prob
334771   in  0.4166667
86560   for  0.1666667
537544 with  0.1666667

#############################################"
#############################################"
#############################################"
Question 10
str10 <- "I like how the same people are in almost all of Adam Sandler's"
str10 <- "I like how the same people are in almost all of Adam Sandler"
str10 <- "I like how the same people are in almost all of Adam Sandlers"
stories
pictures
movies
novels

SearchStrCorpus(str10, "US.twitter")
SearchStrCorpus(str10, "US.blogs")
SearchStrCorpus(str10, "US.news")



egrep " Adam Sandler[:punct:]*" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt > D:/coursera/caps_res/sandler.txt
egrep " stories[:punct:]*" D:/coursera/caps_res/sandler.txt > D:/coursera/caps_res/storiessandler.txt
egrep " pictures[:punct:]*" D:/coursera/caps_res/sandler.txt > D:/coursera/caps_res/picturessandler.txt
egrep " movies[:punct:]*" D:/coursera/caps_res/sandler.txt > D:/coursera/caps_res/moviessandler.txt
egrep " novels[:punct:]*" D:/coursera/caps_res/sandler.txt > D:/coursera/caps_res/novelssandler.txt

****
choice: movies


