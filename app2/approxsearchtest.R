library(stringdist)


strgtgth <- 3
corpus <- "twitter"
name_df <- paste("US.",corpus, strgtgth +1, "Markov", sep ="" )
Markovchains    <- corpuslist[[name_df]]
ML1_df <- Markovchains$ML1
ML1_df$V3 <- as.character(ML1_df$V3)
#maybe no need to return bayes prob in real application
#  ML1_est <- ML1_df[ML1_df$first == search_string, c("V3", "Bayes_prob")
tail(ML1_df)

search_string <- "one of the "
ML1_est <- ML1_df[ML1_df$first == search_string, "V3"]
#amatch(search_string,ML1_df)
ML1_df[amatch(search_string,ML1_df$first),]
ML1_df[amatch("one of those",ML1_df$first),]
ML1_df[amatch("one of those ",ML1_df$first),]
ML1_df[amatch("one of these ",ML1_df$first),]

ML1_df[amatch("just another day ",ML1_df$first),]


df_for_sample <- corpuslist[["US.news4Markov"]]$ML1
sample_el <- sample(nrow(df_for_sample),50)
gr_to_sample <- df_for_sample[sample_el  , "first"]
gr_to_sample <- as.character(gr_to_sample)


illus_amatch <- sapply(gr_to_sample, function(x) amatch(x,ML1_df$first, method = "jw",maxDist=0.1))
#illus_amatch <- as.character(illus_amatch)
ML1_amatch  <- ML1_df[illus_amatch,  ]
#ML1_amatch  <- ML1_df[illus_amatch, ]
ML1_amatch <- cbind(as.character(ML1_amatch[,"first"]),gr_to_sample)
