library(shiny)

shinyUI(fluidPage(
  tags$head(tags$style(
    type="text/css",
    "#text {width: 100%}
    .btn {
    display:inline-block;
    width: 100%;
    }
    "
  )),
fluidRow(column(12,titlePanel("Next word prediction"))),
fluidRow(column(12,h3("Some guidance"))),
fluidRow(column(12,
        helpText("This is a prototype app for 'next word' prediction. 
        In order to run a test, first select the type of text you are writing (tweet, news article, blog).
              Then start writing your sentence - the app will start searching for the best
              prediction of the next word as soon as you hit the 'space' button. 
              Beneath the current panel, you will get a list with three suggestions for the next word.
              If you click on the word of your choice, it will automatically be added to your sentence.
              If the 'correct' next word does not appear in the list, keep on typing in the input field.
              
             Please note that, if you add a new word to your sentence before the suggestions appear, 
              the app will relaunch its search based on your most recent input, and this may cause a sligthly longer 
               execution time. 
             The app has been optimised for predictions based on the three most recent words you have typed.
              You can empty the input field by clicking one 'Reset input text' at any time. 
              "))),
fluidRow(column(6,
          radioButtons("corpus", 
                       label = h3("Preferred corpus"), 
                       choices = list("twitter" = "twitter", 
                                      "news" = "news", "blogs" = "blogs"),
                       selected = "twitter")
          )),
fluidRow(column(3,actionButton("action", label = "Reset input text"))),
fluidRow(column(12, textInput("searchstring", label = h3("Text input"), value = "Enter text..."))) ,        
fluidRow(column(3, actionButton(inputId = "pred1",  label = textOutput("text1"), icon = NULL) )  ) ,
fluidRow(column(3, actionButton(inputId = "pred2",  label = textOutput("text2"), icon = NULL)) )  ,
fluidRow( column(3,actionButton(inputId = "pred3",  label = textOutput("text3"), icon = NULL)) )  
#width = 12
)

)

