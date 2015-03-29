

library(shiny)
library(quantmod)

source("searchwords.R")



# assign("%&%",  function (a, b) paste(a, b, sep = ""))
# catn = function(...) cat(..., "\n")
# wasClicked =  function(button) {
#   if(exists("input"))  
#     if(!is.null(button) ) {
#       if(button > 0) {        
#         return(TRUE)
#       }
#     }
#   return(FALSE)
# }
# 
# rValues = reactiveValues() 
# thisSession <<- session
# 
# source("debugTools.R", local=TRUE)


load(file = "data/corpuslist.RData")

# Define server logic required to draw a histogram
shinyServer(
  function(input, output, session) {
  #probably will need renderTable
   output$text1 <- renderText({ 
#   output$text1 <- renderTable({ 
#     dataInput <- reactive({
#       getSymbols(input$searchstring, input$corpus)
#     })
#     result <-       SearchStrCorpus(dataInput(), corpuslist)
     #or use length(strsplit(str1, split = " ")[[1]]) -1 to find number of blanks
    if(grepl("[[:space:]]+$", input$searchstring)){
      result <-       SearchStrCorpus(input$searchstring, input$corpus, corpuslist)
    } else {
      paste("Enter a space to get a suggestion for a next word.")
    }
#    paste("You want complete ",input$searchstring, "based on ", input$corpus)
  })



  }




)
