


SearchStrCorpus("The guy in front of me just bought a pound of bacon, a bouquet, and a case of", "US.blogs")
V3 Bayes_prob
297863  the       0.66
79572  beer       0.16
31676    if       0.10

SearchStrCorpus("The guy in front of me just bought a pound of bacon, a bouquet, and a case of", "US.twitter")
240394  the  0.8529412
62424  beer  0.1470588




SearchStrCorpus("You're the reason why I smile everyday. Can you follow me please? It would mean the", "US.blogs")
New search string is: mean the.
V3 Bayes_prob
417104      world  0.2340426
182623       same  0.1702128
104092 difference  0.1489362


SearchStrCorpus("You're the reason why I smile everyday. Can you follow me please? It would mean the", "US.twitter")
273298 world 0.97575758
90984  WORLD 0.02424242


SearchStrCorpus("Hey sunshine, can you follow me and make me the", "US.blogs")
743661        most 0.11892676
491050 opportunity 0.04931109
394448       other 0.04350979


SearchStrCorpus("Hey sunshine, can you follow me and make me the", "US.twitter")
V3 Bayes_prob
199547 happiest  0.8275862
32081        th  0.1724138


SearchStrCorpus("Very early observations on the Bills game: Offense still struggling but the", "US.blogs")
851744  fact 0.02101028
598085 truth 0.02078677
500435  rest 0.01989271

SearchStrCorpus("Very early observations on the Bills game: Offense still struggling but the", "US.twitter")
563714 best 0.04728790
364838 only 0.02990264
295240  key 0.02781641

SearchStrCorpus("Very early observations on the Bills game: Offense still struggling but the", "US.news")
816561 company 0.01787063
527063    most 0.01258495
427681    best 0.01233325

egrep "struggling" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt > D:/coursera/caps_res/struggling.txt
egrep "game" D:/coursera/caps_res/struggling.txt > D:/coursera/caps_res/strugglinggame.txt
egrep "[oO]ffense" D:/coursera/caps_res/strugglinggame.txt > D:/coursera/caps_res/q4.txt
egrep "defense" D:/coursera/caps_res/q4.txt > D:/coursera/caps_res/defense.txt
egrep "players" D:/coursera/caps_res/q4.txt > D:/coursera/caps_res/players.txt
egrep "referee" D:/coursera/caps_res/q4.txt
egrep "crowd" D:/coursera/caps_res/q4.txt



SearchStrCorpus("Go on a romantic date at the", "US.news")
900907        end 0.05673823
611193       time 0.05275381
511597 University 0.03128344

SearchStrCorpus("Go on a romantic date at the", "US.blogs")
952867  end 0.09069912
698902 same 0.06541115
600948 time 0.06342720


SearchStrCorpus("Go on a romantic date at the", "US.twitter")
689013    end 0.06470246
489279   same 0.05825460
418824 moment 0.02565710

egrep "romantic" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt > D:/coursera/caps_res/romantic.txt
egrep "date" D:/coursera/caps_res/romantic.txt > D:/coursera/caps_res/romanticdate.txt
egrep "grocery" D:/coursera/caps_res/romanticdate.txt > D:/coursera/caps_res/grocery.txt
egrep "movies" D:/coursera/caps_res/romanticdate.txt > D:/coursera/caps_res/movies.txt
egrep "beach" D:/coursera/caps_res/romanticdate.txt
egrep "mall" D:/coursera/caps_res/romanticdate.txt


SearchStrCorpus("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my", "US.blogs")
260654  own  0.5483871
52576  mind  0.2258065
168823  way  0.2258065


SearchStrCorpus("Well I'm pretty sure my granny has some old bagpipes in her garage I'll dust them off and be on my", "US.twitter")
240352  way  0.3970588
62358   own  0.1176471
102878 show  0.1176471




SearchStrCorpus("Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some", "US.blogs")
V3 Bayes_prob
250221 time          1

SearchStrCorpus("Ohhhhh #PointBreak is on tomorrow. Love that film and haven't seen it in quite some", "US.twitter")
V3 Bayes_prob
125112 time          1



SearchStrCorpus("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little", "US.blogs")
422436 brother  0.1836735
187259  sister  0.1326531
108018    girl  0.1122449


SearchStrCorpus("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little", "US.twitter")
No results found in this iteration. 
New search string is: his little.
V3 Bayes_prob
58988 brother          1


SearchStrCorpus("After the ice bucket challenge Louis will push his long wet hair out of his eyes with his little", "US.news")
268113 brother  0.5238095
43911     girl  0.2380952
533932  sister  0.2380952


SearchStrCorpus("Be grateful for the good times and keep the faith during the", "US.blogs")
842950  day 0.11344113
589347 week 0.05507832
491753    s 0.03537140



SearchStrCorpus("Be grateful for the good times and keep the faith during the", "US.twitter")
544917  day 0.18822480
346346 week 0.08563782
277055 game 0.06690455


SearchStrCorpus("Be grateful for the good times and keep the faith during", "US.twitter")
No results found in this iteration. 
New search string is: faith during.
No results found in this iteration. 
New search string is:  during .
V3 Bayes_prob
354193 the 0.42857143
313807   a 0.07699019
291026  my 0.05169029

SearchStrCorpus("Be grateful for the good times and keep the faith during", "US.blogs")


egrep "good times" D:/coursera/dsc_capstone/Coursera-SwiftKey/final/en_US/*.txt > D:/coursera/caps_res/goodtimes.txt
egrep "bad" D:/coursera/caps_res/goodtimes.txt > D:/coursera/caps_res/goodandbadtimes.txt
egrep "worse" D:/coursera/caps_res/goodtimes.txt > D:/coursera/caps_res/goodandworsetimes.txt
egrep "sad" D:/coursera/caps_res/goodtimes.txt > D:/coursera/caps_res/goodandsadtimes.txt
egrep "hard" D:/coursera/caps_res/goodtimes.txt > D:/coursera/caps_res/goodandhardtimes.txt
egrep "during" D:/coursera/caps_res/goodandbadtimes.txt  > D:/coursera/caps_res/duringgoodandbadtimes.txt
egrep "during" D:/coursera/caps_res/goodandhardtimes.txt > D:/coursera/caps_res/duringgoodandhardtimes.txt


SearchStrCorpus("If this isn't the cutest thing you've ever seen, then you must be", "US.blogs")
286367    a  0.4186047
71133  able  0.2325581
25703   the  0.1162791


SearchStrCorpus("If this isn't the cutest thing you've ever seen, then you must be", "US.twitter")
258167         a 0.25233645
77583         so 0.14018692
40858  following 0.08411215

SearchStrCorpus("If this isn't the cutest thing you've ever seen, then you must be", "US.news")

SearchStrCorpus("you must be", "US.blogs")
286367    a  0.4186047
71133  able  0.2325581
25703   the  0.1162791


SearchStrCorpus("you must be", "US.twitter")
258167         a 0.25233645
77583         so 0.14018692
40858  following 0.08411215


SearchStrCorpus("you must be", "US.news")
111567  a          1

SearchStrCorpus("must be", "US.blogs")
803975    a 0.10738255
550663  the 0.04737465
453361 able 0.02724043


SearchStrCorpus("must be", "US.twitter")
584560   a 0.14592934
385408 the 0.07168459
315534  so 0.03789042

SearchStrCorpus("must be", "US.news")
744681        a 0.07127430
455842 approved 0.04823614
357127     done 0.04391649

markov3twit <- US.twitter3Markov[[1]]


markov3twit[markov3twit$first == "must be" , ]

markov3twit[markov3twit$first == "must be " &markov3twit$to_predict == "insane", ]
markov3twit[markov3twit$first == "must be " &markov3twit$to_predict == "insensitive", ]
markov3twit[markov3twit$first == "must be " &markov3twit$to_predict == "callous", ]
markov3twit[markov3twit$first == "must be " &markov3twit$to_predict == "asleep", ]


TestFreqTable <- read.table("US.twitterunique.txt", stringsAsFactors = FALSE)
names(TestFreqTable) <- c("freq", "word")
teststring <- unlist(strsplit("If this isn't the cutest thing you've ever seen, then you must be", " "))
TestFreqTable[TestFreqTable$word %in% teststring, ]



SearchStrCorpus("Buffy the Vampire", "US.twitter")


SearchStrCorpus("on my favourite", "US.twitter")
SearchStrCorpus("on favourite", "US.twitter")
SearchStrCorpus("favourite", "US.twitter")
SearchStrCorpus("on my favorite", "US.twitter")




SearchStrCorpus("on my favourite", "US.blogs")
SearchStrCorpus("on favourite", "US.blogs")

SearchStrCorpus("on my favourite", "US.news")


SearchStrCorpus("let's grab", "US.twitter")
SearchStrCorpus("let's grab", "US.twitter")

V3 Bayes_prob
95762       a        0.5
505097 coffee        0.5

SearchStrCorpus("My guests should get here by", "US.twitter")
No results found in this iteration. 
New search string is: here by.
V3 Bayes_prob
228224    the  0.3428571
59843      my  0.2285714
19823  myself  0.1428571

SearchStrCorpus("guests by", "US.twitter")
No results found in this iteration. 
New search string is:  by .
V3 Bayes_prob
543275 the 0.16570969
502390   a 0.05351401
479108 and 0.02861736

SearchStrCorpus("get by", "US.twitter")
V3 Bayes_prob
242089      with  0.2926829
70100        the  0.2682927
26415  achieving  0.1219512


SearchStrCorpus("arrive by", "US.twitter")



library(stringdist)





