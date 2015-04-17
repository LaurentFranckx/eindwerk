library(shiny)

style <-   "

.form-group.shiny-input-container {
      width: 100%
    }

#searchstring {
      width: 100%
    }
"
    

  
shinyUI(fluidPage(  
tags$head(tags$style(type="text/css", style)),
fluidRow(column(12,titlePanel("Next word prediction"))),
fluidRow(column(12,h3("Some guidance"))),
fluidRow(column(12,
        helpText("This is a prototype app for 'next word' prediction. 
        In order to run a test, first select the type of text you are writing (tweet, news article, blog) - the app has been optimized for each type of text.
              "))),
fluidRow(column(12,
        helpText("Then start writing your sentence."))),
fluidRow(column(12, strong("The app will start searching for the best
              prediction of the next word as soon as you hit the 'space' button.")  
              )),
fluidRow(column(12,
              "Beneath the current panel, you will get a suggestion for the next word.
              If you click on this word, it will automatically be added to your sentence.
              If this is not the word you intended to write next, keep on typing in the input field.
              ")),
fluidRow(column(12,
                helpText("Note that you can overrule the default setting of 'single' word prediction by clicking 
                in the checkbox 'Give three word suggestions'.
              You will then get a list with three suggestions for the next word.
              If you click on the word of your choice, it will automatically be added to your sentence.
              If the 'correct' next word does not appear in the list, keep on typing in the input field.
              You can get back to 'single' word prediction at any time by unclicking the checkbox. 
              "))),

fluidRow(column(12,         
             "Please note that, if you add a new word to your sentence before the suggestions appear, 
              the app will relaunch its search based on your most recent input, and this may cause a sligthly longer 
               execution time. 
             The app has been optimised for predictions based on the four most recent words you have typed."
             )),


fluidRow(column(12,         
              "You can empty the input field by clicking one 'Reset input text' at any time. 
              ")),

fluidRow(column(12,
                helpText("It is possible that, at some stage, your screen will turn grey and stop reacting. Reloading the page should solve the problem.
If the problems persists,  or if you encounter any other performance problems, please contact me at franckx.laurent@gmail.com . 
              "))),

fluidRow(column(6,
          radioButtons("corpus", 
                       label = h3("Chosen text style"), 
                       choices = list("twitter" = "twitter", 
                                      "news" = "news", "blogs" = "blogs"),
                       selected = "twitter")
          )),
fluidRow(column(6,checkboxInput("threewords", label = "Give three word suggestions", value = FALSE))),
fluidRow(column(3,actionButton("action", label = "Reset input text"))),

fluidRow(column(12, textInput("searchstring", label = h3("Text input"), 
                              value = "Enter text..."
                              ))) ,        
fluidRow(column(3, actionButton(inputId = "pred1",  label = textOutput("text1"), icon = NULL) )  ) , 
fluidRow(column(3, actionButton(inputId = "pred2",  label = textOutput("text2"), icon = NULL)) )  ,
fluidRow( column(3,actionButton(inputId = "pred3",  label = textOutput("text3"), icon = NULL)) )   

#width = 12
)

)

