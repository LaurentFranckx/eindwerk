
load(file = "cap_ston_corp_cl.RData")

TokenizeMarkov("US.blogs", ngram= 2, req_freq = 3, samplesize = 10^4, create_token = TRUE)
TokenizeMarkov("US.blogs", ngram= 3, req_freq = 3, samplesize = 10^4, create_token = TRUE)
TokenizeMarkov("US.blogs", ngram= 4, req_freq = 3, samplesize = 10^4, create_token = TRUE)

TokenizeMarkov("US.news", ngram= 2, req_freq = 3, samplesize = 10^4, create_token = TRUE)
TokenizeMarkov("US.news", ngram= 3, req_freq = 3, samplesize = 10^4, create_token = TRUE)
TokenizeMarkov("US.news", ngram= 4, req_freq = 3, samplesize = 10^4, create_token = TRUE)

TokenizeMarkov("US.twitter", ngram= 2, req_freq = 3, samplesize = 10^4, create_token = TRUE)
TokenizeMarkov("US.twitter", ngram= 3, req_freq = 3, samplesize = 10^4, create_token = TRUE)
TokenizeMarkov("US.twitter", ngram= 4, req_freq = 3, samplesize = 10^4, create_token = TRUE)





