

library(shiny)
library(quantmod)
library(stringdist)


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
#     values <- reactiveValues()
#     values$predictions  <- SearchWrapper("", input$corpus, corpuslist)
    
#    predictions <- paste("Enter a space to get a suggestion for a next word.")
    
    
    predictions <- reactive({
    #observe({
 #     searchstring <- input$searchstring
      if(grepl("[[:space:]]+$", input$searchstring)){
  #    if(input$newsearch){
        searchstring <- gsub("[[:space:]]+$", " ", input$searchstring)
        if (input$corpus == "news") {
           k <- 0.4
        } else if (input$corpus == "blogs") {
           k <- 0 
        } else {
          k <- 0.2
        }
        
        predictions <- SearchWrapper(searchstring, input$corpus, corpuslist, decrease = k)
   #    }
        # selectInput("result", "Choose the next word", result)
        # checkboxGroupInput("result", "Choose the next word", result)
#        result[[1]] 
       } else {
         #predictions <- "Enter a space to get a suggestion for a next word."
         predictions <- " "
       }
    
      
    })
     
  #probably will need renderTable
   output$text1 <- renderText({ 
#     output$text1 <- renderUI({  
#   output$text1 <- renderTable({ 
#     dataInput <- reactive({
#       getSymbols(input$searchstring, input$corpus)
#     })
#     result <-       SearchStrCorpus(dataInput(), corpuslist)
     #or use length(strsplit(str1, split = " ")[[1]]) -1 to find number of blanks
#     if(grepl("[[:space:]]+$", input$searchstring)){
#       searchstring <- gsub("[[:space:]]+$", " ", input$searchstring)
#       result <-       SearchWrapper(searchstring, input$corpus, corpuslist)
     # selectInput("result", "Choose the next word", result)
     # checkboxGroupInput("result", "Choose the next word", result)
     predictions()[[1]]
     #predictions
#     } else {
#       paste("Enter a space to get a suggestion for a next word.")
#     }
#    paste("You want complete ",input$searchstring, "based on ", input$corpus)
  })

output$text2 <- renderText({ 
  if (length(predictions()) > 1 & input$threewords) {
    results <- predictions()[[2]]
  } else {
    results <- NULL
  }
  
  results 
})

output$text3 <- renderText({ 
  if (length(predictions()) > 2 & input$threewords) {
    results <- predictions()[[3]]
  } else {
    results <- NULL
  }
   
  results
})


observe({
  if(input$action == 0)   return()  
  x <- input$action
  isolate(updateTextInput(session, "searchstring", value = ""))
    
  })

observe({
  if(input$pred1 == 0)   return()      
    firstval <- isolate(input$searchstring)
      isolate(updateTextInput(session, "searchstring", value = paste(firstval, predictions()[[1]], " ", sep = "")))
})


observe({
 if(input$pred2 == 0)   return()      
  firstval <- isolate(input$searchstring)
  isolate(updateTextInput(session, "searchstring", value = paste(firstval, predictions()[[2]], " ", sep = "")))
})

observe({
  if(input$pred3 == 0)   return()      
  firstval <- isolate(input$searchstring)
  isolate(updateTextInput(session, "searchstring", value = paste(firstval, predictions()[[3]]," ", sep = "")))
})


  }

)








