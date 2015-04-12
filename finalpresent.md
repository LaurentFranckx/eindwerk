Next Word Prediction with R
========================================================
author: me 
date: 

Central objectives and constraints of this project:
========================================================

- Generate a "next" word prediction for an English sentence 
- Underlying prediction algorythm should be accurate 
- Data size for on-line implementation should not exceed Shiny limits
- Response to user input should be sufficiently quick 


Data: sources and approach 
========================================================

- Three datasets of English sentences, based on Twitter, news articles and blogs
- Preliminary data analysis: important differences between three datasets requires separate analysis 
- Stopwords are integral part of sentence content and have been kept
- No stemming: users want complete word, not its stem
- Very high number of words and low order n-grams with low count: eliminate to improve speed without decreasing accuracy 


Data: description of algorithm
========================================================

![alt text](algor_flow.PNG)






========================================================

![plot of chunk unnamed-chunk-1](finalpresent-figure/unnamed-chunk-1-1.png) 

- Key element: how many 'next' words are predicted
- Discount factors matters but not in consistent way
- Moving from prediction based on 1-grams to 2-grams brings clearer benefits than moving to 3-grams


