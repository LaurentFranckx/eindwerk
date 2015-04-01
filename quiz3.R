library(stringdist)
library(plyr)

corpusnames <- c("news","twitter","blogs")
#strgtgth <- 3
corpuslist <- list()
for(corpus in corpusnames){
  for(strgtgth in 1:3){
    load(paste("US.", corpus, strgtgth +1, "Markov.RData" , sep= ""))
    name_df <- paste("US.",corpus, strgtgth +1, "Markov", sep ="" )
    searchcorpus <- get(name_df)
    #   searchmatrix <- paste(corpus,"search", sep="")
    #   assign(searchmatrix, searchcorpus$ML1 ) 
    corpuslist[[name_df]] <- searchcorpus$ML1  
  }
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

str1 <- "When you breathe, I want to be the air for you. I'll be there for you, I'd live and I'd"

SearchStrCorpus(str1, "US.twitter")
# No results found in this iteration. 
# New search string is: and I'd.
#          V3 Bayes_prob
# 285817   be  0.4545455
# 104627 love  0.3333333
# 52665  like  0.2121212

# SearchStrCorpus(str1, "US.blogs")
# No results found in this iteration. 
# New search string is: and I'd.
#          V3 Bayes_prob
# 486194 like  0.2848101
# 244463   be  0.1455696
# 158697 love  0.1202532


grep "there for you*.*live.*and.*sleep[:punct:]*"  D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt > D:/coursera/caps_res/sleep.txt
grep "there for you*.*live.*and.*eat[:punct:]*" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt > D:/coursera/caps_res/eat.txt
grep "there for you*.*live.*and.*give[:punct:]*" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt > D:/coursera/caps_res/give.txt
grep "there for you*.*live.*and.*die[:punct:]*" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt >  D:/coursera/caps_res/die.txt


first chosen: give
wrong

str1 <- "I would live and"
SearchStrCorpus(str1, "US.twitter")
> SearchStrCorpus(str1, "US.twitter")
No results found in this iteration. 
New search string is: live and.
V3 Bayes_prob
330905 learn  0.2056075
143408  work  0.1401869
85306    die  0.1214953

SearchStrCorpus(str1, "US.blogs")
> SearchStrCorpus(str1, "US.blogs")
No results found in this iteration. 
New search string is: live and.
V3 Bayes_prob
527956  work 0.24770642
283030     I 0.08715596
194111 learn 0.07339450

suggestions <- c("sleep","eat", "give", "die" )

str1 <- "I live and"
SearchStrCorpus(str1, "US.twitter")
grep "live and sleep" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt >  D:/coursera/caps_res/liveandsleep.txt
grep "live and eat" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt >  D:/coursera/caps_res/liveandeat.txt
grep "live and give" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt >  D:/coursera/caps_res/liveandgive.txt
grep "live and die" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt >  D:/coursera/caps_res/liveanddie.txt

grep "live and sleep" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt | D:/coursera/caps_res/liveandsleep.txt -l
grep "live and eat" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt |   D:/coursera/caps_res/liveandeat.txt -l
grep "live and give" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt |  D:/coursera/caps_res/liveandgive.txt -l
grep "live and die" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt |  D:/coursera/caps_res/liveanddie.txt -l


grep -c "live and sleep" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt
grep -c "live and eat" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
grep -c "live and give" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
grep -c "live and die" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 


##2nd trial use die

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

grep "tell[ing]*.*about.*horticultural[:punct:]*"  D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt > D:/coursera/caps_res/horticultural.txt
grep "tell[ing]*.*about.*spiritual[:punct:]*" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt > D:/coursera/caps_res/spiritual.txt
grep "tell[ing]*.*about.*marital[:punct:]*" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt > D:/coursera/caps_res/marital.txt
grep "tell[ing]*.*about.*financial[:punct:]*" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt >  D:/coursera/caps_res/financial.txt

chose: financial
wrong

SearchStrCorpus("telling me about", "US.blogs")
V3 Bayes_prob
280269 the      0.225
66643    a      0.175
22940  her      0.150


SearchStrCorpus("telling me about", "US.twitter")
> SearchStrCorpus("telling me about", "US.twitter")
V3 Bayes_prob
200986 the  0.3333333
33482  her  0.2000000
101631  it  0.2000000

SearchStrCorpus("started telling me", "US.blogs")
> SearchStrCorpus("started telling me", "US.blogs")
V3 Bayes_prob
87737 about          1


SearchStrCorpus("started telling me", "US.twitter")
No results found in this iteration. 
New search string is: telling me.
V3 Bayes_prob
485148   to  0.2114695
287933 that  0.1326165
219997    I  0.1129032


grep -c "telling me about horticultural" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt
grep -c "telling me about spiritual" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
grep -c "telling me about marital" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
grep -c "telling me about financial" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 

0 counts for all 

grep -c "about horticultural" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt > D:/coursera/caps_res/abouthorticultural.txt
grep -c "about spiritual" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt > D:/coursera/caps_res/aboutspir
grep -c "about marital" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt >  D:/coursera/caps_res/aboumar.txt
grep -c "about financial" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt > D:/coursera/caps_res/aboutfinanc.txt

egrep -c "about (his|her) horticultural" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "about (his|her) spiritual" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "about (his|her) marital" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "about (his|her) financial" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 


#chose: spiritual


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

SearchStrCorpus(str3, "US.news")
> SearchStrCorpus(str3, "US.news")
No results found in this iteration. 
New search string is: monkeys this.
No results found in this iteration. 
New search string is:  this .
V3 Bayes_prob
738983 year 0.11072143
684377   is 0.05623749
651905 week 0.05304512

egrep "see.*this weekend[:punct:]*" D:/coursera/caps_res/seethis.txt > D:/coursera/caps_res/weekend.txt
egrep "see.*this morning[:punct:]*" D:/coursera/caps_res/seethis.txt > D:/coursera/caps_res/morning.txt
egrep "see.*this decade[:punct:]*" D:/coursera/caps_res/seethis.txt > D:/coursera/caps_res/decade.txt
egrep "see.*this month[:punct:]*" D:/coursera/caps_res/seethis.txt > D:/coursera/caps_res/month.txt

#chosen morning

egrep -c "see.*this weekend[:punct:]*" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "see.*this morning[:punct:]*" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "see.*this decade[:punct:]*" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt  
egrep -c "see.*this month[:punct:]*" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 

#choose weekend 


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

egrep "reduce[:punct:]*" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt > D:/coursera/caps_res/reduce.txt
egrep " stress[:punct:]*" D:/coursera/caps_res/reduce.txt > D:/coursera/caps_res/stress.txt
egrep " hunger[:punct:]*" D:/coursera/caps_res/reduce.txt > D:/coursera/caps_res/hunger.txt
egrep " sleepiness[:punct:]*" D:/coursera/caps_res/reduce.txt > D:/coursera/caps_res/sleepiness.txt
egrep " happiness[:punct:]*" D:/coursera/caps_res/reduce.txt > D:/coursera/caps_res/happiness.txt


#chose stress
CORRECT

#############################################"
#############################################"
#############################################"
Question 5
str5 <- "When you were in Holland you were like 1 inch away from me but you hadn't time to take a"
str5 <- gsub("[[:punct:]]", " ", as.character(str5))
str5 <- gsub("  ", " ", as.character(str5))
uniq_str1 <- unique(strsplit(str5, split = " ")[[1]])
uniq_str1 <- as.data.frame(uniq_str1)
names(uniq_str1) <- "word"
uniq_str_freq <- join(uniq_str1, freq_tables_df_sum)
uniq_str_freq <- uniq_str_freq[order(uniq_str_freq$freq), ]
uniq_str_freq_low <- as.character(uniq_str_freq[1:3, "word"]) 





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

egrep "time to take[:punct:]*" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt > D:/coursera/caps_res/timetotake.txt
egrep " picture[:punct:]*" D:/coursera/caps_res/timetotake.txt > D:/coursera/caps_res/pictme.txt
egrep " walk[:punct:]*" D:/coursera/caps_res/timetotake.txt > D:/coursera/caps_res/waltime.txt
egrep " look[:punct:]*" D:/coursera/caps_res/timetotake.txt > D:/coursera/caps_res/looktime.txt
egrep " minute[:punct:]*" D:/coursera/caps_res/timetotake.txt > D:/coursera/caps_res/minutetime.txt


#choice look
egrep -c "to take a picture" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "to take a walk" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "to take a look" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt  
egrep -c "to take a minute" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 

egrep -c "time to take a picture" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "time to take a walk" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "time to take a look" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt  
egrep -c "time to take a minute" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 

egrep -c "hadn't time to take a picture" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "hadn't time to take a walk" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "hadn't time to take a look" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt  
egrep -c "hadn't time to take a minute" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
0 count


egrep -c "no time to take a picture" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "no time to take a walk" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "no time to take a look" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt  
egrep -c "no time to take a minute" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt
0 count 

egrep -c "take a picture" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "take a walk" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "take a look" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt  
egrep -c "take a minute" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 

#still confirms look...


egrep -c "the time to take a picture" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "the time to take a walk" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "the time to take a look" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt  
egrep -c "the time to take a minute" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
0 count

egrep -c "no time for a picture" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "no time for a walk" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "no time for a look" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt  
egrep -c "no time for a minute" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt
0 count

egrep -c "time for a picture" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "time for a walk" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "time for a look" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt  
egrep -c "time for a minute" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt
#suggests walk

egrep -c "hadn't time for a picture" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "hadn't time for a walk" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "hadn't time for a look" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt  
egrep -c "hadn't time for a minute" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt
#zero count


egrep -c "time .* picture" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "time .* walk" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "time .* look" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt  
egrep -c "time .* minute" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt
confirm look


egrep -c "hadn't time .* picture" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "hadn't time .* walk" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "hadn't time .* look" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt  
egrep -c "hadn't time .* minute" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt

egrep -c "no time .* picture" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "no time .* walk" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "no time .* look" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt  
egrep -c "no time .* minute" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt

egrep -c "inch .* picture" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "inch .* walk" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "inch .* look" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt  
egrep -c "inch .* minute" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt
#suggest minutes

egrep -c "hadn.* picture" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "hadn.* walk" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "hadn.* look" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt  
egrep -c "hadn.* minute" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt
suggests look

egrep -c "had.*time.*picture" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "had.*time.*walk" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "had.*time.*look" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt  
egrep -c "had.*time.*minute" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt
suggests look


egrep -c "you.*time.*picture" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "you.*time.*walk" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "you.*time.*look" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt  
egrep -c "you.*time.*minute" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt


egrep -c "you.*time.{0,5}picture" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "you.*time.{0,5}walk" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "you.*time.{0,5}look" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt  
egrep -c "you.*time.{0,5}minute" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt


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

egrep "settle the[:punct:]*" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt > D:/coursera/caps_res/settle.txt
egrep " matter[:punct:]*" D:/coursera/caps_res/settle.txt > D:/coursera/caps_res/settlemat.txt
egrep " case[:punct:]*" D:/coursera/caps_res/settle.txt > D:/coursera/caps_res/settlecase.txt
egrep " incident[:punct:]*" D:/coursera/caps_res/settle.txt > D:/coursera/caps_res/settleinc.txt
egrep " account[:punct:]*" D:/coursera/caps_res/settle.txt > D:/coursera/caps_res/settleacc.txt

#chose case


egrep "settle the matter" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt > D:/coursera/caps_res/settlematter.txt
egrep "settle the case" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt > D:/coursera/caps_res/settlecase.txt
egrep -c "settle the incident" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt  
egrep -c "settle the account" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
#still case


egrep -c "to settle the matter" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "to settle the case" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "to settle the incident" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt  
egrep -c "to settle the account" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
#still case

egrep -c "jury to settle the matter" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "jury to settle the case" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "jury to settle the incident" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt  
egrep -c "jury to settle the account" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
#still case

egrep -c "a jury to settle the matter" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "a jury to settle the case" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "a jury to settle the incident" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt  
egrep -c "a jury to settle the account" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
gives all zeros

egrep -c "evidence.*jury.*matter" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "evidence.*jury.*case" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "evidence.*jury.*incident" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt  
egrep -c "evidence.*jury.*account" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 


egrep -c "like.*to settle the matter" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "like.*to settle the case" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "like.*to settle the incident" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt  
egrep -c "like.*to settle the account" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 

egrep -c "like.*settle the matter" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "like.*settle the case" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "like.*settle the incident" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt  
egrep -c "like.*settle the account" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 

egrep -c "like.*settle.*matter" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "like.*settle.*case" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "like.*settle.*incident" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt  
egrep -c "like.*settle.*account" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 

egrep -c "settle.*matter" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "settle.*case" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "settle.*incident" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt  
egrep -c "settle.*account" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 

egrep -c "settle.* matter " D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "settle.* case " D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "settle.* incident " D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt  
egrep -c "settle.* account " D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 

egrep -c "settle .{0,5} matter " D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "settle .{0,5} case " D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "settle .{0,5} incident " D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt  
egrep -c "settle .{0,5} account " D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
still more fore case


egrep -c "matter settled" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "case settled" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep -c "incident settled" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt  
egrep -c "account settled" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
#again, case higher



#only alternative is matter but always lower count
egrep -c "jury to settle " D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
0 count

egrep -c "jury .* settle " D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep "jury .* settle " D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 

egrep -c "answered .* settle " D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 
egrep "answered .* settle " D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt 

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

egrep "bags[:punct:]*" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt > D:/coursera/caps_res/bags.txt
egrep " hold[:punct:]*" D:/coursera/caps_res/bags.txt > D:/coursera/caps_res/holdbags.txt
egrep " toe[:punct:]*" D:/coursera/caps_res/holdbags.txt > D:/coursera/caps_res/toeholdbags.txt
egrep " finger[:punct:]*" D:/coursera/caps_res/holdbags.txt > D:/coursera/caps_res/fingholdbags.txt
egrep " arm[:punct:]*" D:/coursera/caps_res/holdbags.txt > D:/coursera/caps_res/armholdbags.txt
egrep " hand[:punct:]*" D:/coursera/caps_res/holdbags.txt > D:/coursera/caps_res/handholdbags.txt


#chose hand
CORRECT

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


###
chose top
CORRECT

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

egrep "playing[:punct:]*" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt > D:/coursera/caps_res/playing.txt
egrep " bruises[:punct:]*" D:/coursera/caps_res/playing.txt > D:/coursera/caps_res/playbruises.txt
egrep " imagination[:punct:]*" D:/coursera/caps_res/playing.txt > D:/coursera/caps_res/playimag.txt
egrep "weekly[:weekly:]*" D:/coursera/caps_res/playing.txt > D:/coursera/caps_res/weekplaying.txt
egrep "outside[:punct:]*" D:/coursera/caps_res/playing.txt > D:/coursera/caps_res/outplaying.txt
egrep "inside[:punct:]*" D:/coursera/caps_res/playing.txt > D:/coursera/caps_res/inplaying.txt
egrep "daily[:punct:]*" D:/coursera/caps_res/playing.txt > D:/coursera/caps_res/dailplaying.txt


###########
chose outside
CORRECT

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
CORRECT


