

library(shiny)
library(quantmod)

source("searchwords.R")


# assign("%&%",  function (a, b) paste(a, b, sep = ""))
# catn = function(...) cat(..., "\n")
# wasClicked =  function(button) {
#          if(exists("input"))  (!is.null(button) ) {    if(button > 0) {        
#            return(TRUE)        }      }
#          return(FALSE)
#        }
# 
# 
# source("debugTools.R", local=TRUE)

corpusnames <- c("news","twitter","blogs")
#strgtgth <- 3
corpuslist <- list()
for(corpus in corpusnames){
  for(strgtgth in 1:3){
    load(file.path("data",paste("US.", corpus, strgtgth +1, "Markov.RData" , sep= "")))
    name_df <- paste("US.",corpus, strgtgth +1, "Markov", sep ="" )
    searchcorpus <- get(name_df)
    #   searchmatrix <- paste(corpus,"search", sep="")
    #   assign(searchmatrix, searchcorpus$ML1 ) 
    corpuslist[[name_df]] <- searchcorpus  
  }
}






# Define server logic required to draw a histogram
shinyServer(
  function(input, output, session) {
  #probably will need renderTable
   output$text1 <- renderText({ 
#  output$text1 <- renderTable({ 
#     dataInput <- reactive({
#       getSymbols(input$searchstring, input$corpus)
#     })
#     result <-       SearchStrCorpus(dataInput(), corpuslist)
    if(grepl("[[:space:]]+$", input$searchstring)){
      result <-       SearchStrCorpus(input$searchstring, input$corpus, corpuslist)
    } else {
      paste("Push the return buton to get a suggestion for a next word.")
    }
#    paste("You want complete ",input$searchstring, "based on ", input$corpus)
  })



  }




)
