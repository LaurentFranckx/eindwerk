library(plyr)
library(data.table)


setwd("D:/coursera/dsc_capstone")


corpusnames <- c("news","twitter","blogs")
#strgtgth <- 3


MarkovMat4 <- function(corpus){
      load(paste("US.", corpus, "4Markov.RData" , sep= ""))
      name_df <- paste("US.",corpus, "4Markov", sep ="" )
      searchcorpus <- get(name_df)$Markov_mat
      
      #this is the three gram used for the original prediction 
      names(searchcorpus) <- gsub("^first$","ThreeGr", names(searchcorpus) )
      names(searchcorpus) <- gsub("V5","Pred4Gr", names(searchcorpus) )                                 
      names(searchcorpus) <- gsub("freq$","Freq4Gr", names(searchcorpus) )
      names(searchcorpus) <- gsub("freq_first","FreqPred4gr", names(searchcorpus))
      names(searchcorpus) <- gsub("Bayes_prob","Bayes4Gr", names(searchcorpus) )
      searchcorpus$ThreeGr <- as.character(searchcorpus$ThreeGr)
      searchcorpus$Pred4Gr <- as.character(searchcorpus$Pred4Gr)
      searchcorpus$FourGr <- as.character(searchcorpus$FourGr)
      TwoGr <- strsplit(searchcorpus$ThreeGr, " ")
      TwoGr <- sapply(TwoGr, function(x) paste(x[2],x[3], sep = " "))
      searchcorpus$TwoGr <- TwoGr
      searchcorpus$ThreeGr <- gsub(" $", "", searchcorpus$ThreeGr)
      return(searchcorpus)
}

MarkovMat3 <- function(corpus){
  load(paste("US.", corpus, "3Markov.RData" , sep= ""))
  name_df <- paste("US.",corpus, "3Markov", sep ="" )
  searchcorpus <- get(name_df)$Markov_mat
  
  names(searchcorpus) <- gsub("^first$","TwoGr", names(searchcorpus) )
  names(searchcorpus) <- gsub("to_predict","Pred3Gr", names(searchcorpus) )                                 
  names(searchcorpus) <- gsub("freq$","Freq3Gr", names(searchcorpus) )
  names(searchcorpus) <- gsub("freq_first","FreqPred3gr", names(searchcorpus))
  names(searchcorpus) <- gsub("Bayes_prob","Bayes3Gr", names(searchcorpus) )
  searchcorpus$ThreeGr <- as.character(searchcorpus$ThreeGr)
  searchcorpus$Pred3Gr <- as.character(searchcorpus$Pred3Gr)
  searchcorpus$TwoGr <- as.character(searchcorpus$TwoGr)
  searchcorpus$TwoGr <- gsub(" $", "", searchcorpus$TwoGr)
  OneGr <- strsplit(searchcorpus$TwoGr, " ")
  OneGr <- sapply(OneGr, function(x) paste(x[2], sep = " "))
  searchcorpus$OneGr <- OneGr
  searchcorpus <- searchcorpus[  , setdiff(names(searchcorpus),"ThreeGr")]
  return(searchcorpus)
}

MarkovMat2 <- function(corpus){
  load(paste("US.", corpus, "2Markov.RData" , sep= ""))
  name_df <- paste("US.",corpus, "2Markov", sep ="" )
  searchcorpus <- get(name_df)$Markov_mat
  
  names(searchcorpus) <- gsub("^first$","OneGr", names(searchcorpus) )
  names(searchcorpus) <- gsub("V3","Pred2Gr", names(searchcorpus) )                                 
  names(searchcorpus) <- gsub("freq$","Freq2Gr", names(searchcorpus) )
  names(searchcorpus) <- gsub("freq_first","FreqPred2gr", names(searchcorpus))
  names(searchcorpus) <- gsub("Bayes_prob","Bayes2Gr", names(searchcorpus) )
  
  searchcorpus$TwoGr <- as.character(searchcorpus$TwoGr)
  searchcorpus$Pred2Gr <- as.character(searchcorpus$Pred2Gr)
  searchcorpus$OneGr <- as.character(searchcorpus$OneGr)
  searchcorpus$OneGr <- gsub(" $", "", searchcorpus$OneGr)
#   OneGr <- strsplit(searchcorpus$TwoGr, " ")
#   OneGr <- sapply(OneGr, function(x) paste(x[2], sep = " "))
#   searchcorpus$OneGr <- OneGr
  searchcorpus <- searchcorpus[  , setdiff(names(searchcorpus),"TwoGr")]
  return(searchcorpus)
}




MarkovMat4List <- lapply(corpusnames, MarkovMat4)
names(MarkovMat4List) <- corpusnames

MarkovMat3List <- lapply(corpusnames, MarkovMat3)
names(MarkovMat3List) <- corpusnames


MarkovMat2List <- lapply(corpusnames, MarkovMat2)
names(MarkovMat2List) <- corpusnames

###calculate the weights for twitter
#with sample of 1000, 4000, 6000, all weights should go to highest n-gram
samplesize <- 4000
#from 8000 is too close to the limit
samplefromtable <- sample(nrow(MarkovMat4List[["twitter"]]),samplesize)
table1 <- MarkovMat4List[["twitter"]]
table1 <- table1[samplefromtable, ]

#testmat <- merge(data.table(MarkovMat4List[["news"]]),data.table(MarkovMat3List[["twitter"]]), by = "TwoGr")
testmat <- join(table1,MarkovMat3List[["twitter"]])
#testmat[testmat$Pred4gr == " a "  , ]

testmat <- testmat[testmat$Pred4Gr == testmat$Pred3Gr  , ]

testmat <- join(testmat,MarkovMat2List[["twitter"]])
testmat <- testmat[testmat$Pred4Gr == testmat$Pred2Gr  , ]

FrGrForVal <- testmat$FourGr

#validate on blogs
ValidDF <- MarkovMat4List[["news"]]
ValidDF2 <- ValidDF[ValidDF$FourGr %in% FrGrForVal, c("FourGr", "Freq4Gr") ]
names(ValidDF2) <- gsub( "Freq4Gr" , "Freq4GrVal", names(ValidDF2))
anyDuplicated(ValidDF2$FourGr)

testmat2 <- join(testmat, ValidDF2)
testmat2[is.na(testmat2) ]  <- 0

#http://cran.r-project.org/web/packages/lpSolve/lpSolve.pdf
library(lpSolve)
library(maxLik)
library(nloptr)

testmat2$Bayes1Gr <- with(testmat2,Freq2Gr/sum(Freq2Gr) )

# lambda4 <- sum(with(testmat2, log(Bayes4Gr) * Freq4GrVal))
# lambda3 <- sum(with(testmat2, log(Bayes3Gr) * Freq4GrVal))
# lambda2 <- sum(with(testmat2, log(Bayes2Gr) * Freq4GrVal))
# lambda1 <- sum(with(testmat2, log(Bayes1Gr) * Freq4GrVal))

# f.obj <- c(lambda4, lambda3, lambda2, lambda1)
# f.con <- matrix (c(1, 1, 1,1,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1), nrow=5, byrow=TRUE)
# f.dir <- c("=", ">=",">=",">=",">=")
# f.rhs <- c(1,0,0,0,0)
# 
# loglik <- function(param) {
#   lambda1 <- param[1]
#   lambda2 <- param[2]
#   lambda3 <- param[3]
#   lambda4 <- param[4]  
#   ll <- with(testmat2, sum(Freq4GrVal * (lambda4 * log(Bayes4Gr) + lambda3 * log(Bayes3Gr) + lambda2 * log(Bayes2Gr) + lambda1 * log(Bayes1Gr))  ))
#   ll
# }


# A <- matrix(c(1, 1,1,1), 1, 4)
# B <- 1
# C <- matrix (c(1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1), nrow=4, byrow=TRUE)
# D <- rep(0,4)
# A <- matrix (c(-1, -1,-1,-1,1,0,0,0,0,1,0,0,0,0,1,0,0,0,0,1), nrow=5, byrow=TRUE)
# B  <- c(1,rep(0,4))

#res <- maxNM(f, start=c(1,1), constraints=list(ineqA=A, ineqB=B),
# 
# maxLik(loglik, grad = NULL, hess = NULL, start= rep(0.25,4), method= "NR", print.level=2, constraints=list(ineqA=A,ineqB=B
#                                                                     #          , ineqA= C, ineqB= D
#                                                                     )
#        )
# 
# 
# maxBFGS(loglik, start= rep(0.25,4), constraints=list(ineqA=A,ineqB=B
#                                                                                                             #          , ineqA= C, ineqB= D
# )
# )
# 
# 

#
# f(x) = x1*x4*(x1 + x2 + x3) + x3
#


eval_f <- function(x) {
#  ll <- with(testmat2, sum(Freq4GrVal * (x[4] * log(Bayes4Gr) + x[3] * log(Bayes3Gr) + x[2] * log(Bayes2Gr) + x[1] * log(Bayes1Gr))  ))
  ll <- with(testmat2, sum(Freq4GrVal * log(x[4] * Bayes4Gr + x[3] * Bayes3Gr + x[2] * Bayes2Gr + x[1] * Bayes1Gr)  ))
  return(list("objective"= -ll, "gradient"= c(with(testmat2, -sum(Freq4GrVal * Bayes1Gr/(x[4] * Bayes4Gr + x[3] * Bayes3Gr + x[2] * Bayes2Gr + x[1] * Bayes1Gr))),
                                             with(testmat2, -sum(Freq4GrVal*  Bayes2Gr/(x[4] * Bayes4Gr + x[3] * Bayes3Gr + x[2] * Bayes2Gr + x[1] * Bayes1Gr))),
                                             with(testmat2, -sum(Freq4GrVal * Bayes3Gr /(x[4] * Bayes4Gr + x[3] * Bayes3Gr + x[2] * Bayes2Gr + x[1] * Bayes1Gr))),
                                             with(testmat2, -sum(Freq4GrVal*  Bayes4Gr/(x[4] * Bayes4Gr + x[3] * Bayes3Gr + x[2] * Bayes2Gr + x[1] * Bayes1Gr) ))                                             
                                             )))
}

# constraint functions
# inequalities
# eval_g_ineq <- function( param ) {
#   constr <- c(-param[1],-param[2],-param[3],-param[4]  )
#   grad <- matrix(c(- 1,0,0,0,
#            0, - 1,0,0,
#            0,0, - 1,0,
#            0,0,0, - 1 ), nrow =4)
#   return( list( "constraints"=constr, "jacobian"=grad ) )
# }

lb <- c( 0, 0, 0, 0 )
# equalities
eval_g_eq <- function( param ) {
  constr <- param[1] + param[2] + param[3] + param[4] -1
  grad <- c( 1,
             1,
             1,
             1 )
  return( list( "constraints"=constr, "jacobian"=grad ) )
}

x0 <- rep(0.25,4)

local_opts <- list( "algorithm" = "NLOPT_LD_MMA",
                    "xtol_rel" = 1.0e-7 )

opts <- list( "algorithm" = "NLOPT_LD_AUGLAG",
              "xtol_rel" = 1.0e-7,
              "maxeval" = 1000
             ,"local_opts" = local_opts 
              )

res <- nloptr( x0=x0,
               eval_f=eval_f,
#               eval_g_ineq=eval_g_ineq,
                lb = lb, 
               eval_g_eq=eval_g_eq, opts = opts)

res$solution

# with tweets for training, blogs/news for validation give same result
# > res$solution
# [1] 0 0 0 1
