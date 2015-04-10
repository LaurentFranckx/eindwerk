library(shiny)

shinyUI(fluidPage(
  titlePanel("Next word prediction"),
  
sidebarLayout(
    sidebarPanel(
        h3("Some guidance"),
        helpText("This is a prototype app for 'next word' prediction. 
        In order to run a test, first select the type of text you are writing (tweet, news article, blog).
              Then start writing your sentence - the app will start searching for the best
              prediction of the next word as soon as you hit the 'space' button. 
              Beneath the current panel, you will get a drop down list with three suggestions for the next word.
             Please note that, if you add a new word to your sentence before the suggestions appear, 
              the app will relaunch its search based on your most recent input. 
             The app has been optimised for predictions based on the three most recent words you have typed.
              Also note that, although the app suggests the next word, you still need to fill in the next word manually in the 
              input field. 
              "),
        fluidRow(
          radioButtons("corpus", 
                       label = h3("Preferred corpus"), 
                       choices = list("twitter" = "twitter", 
                                      "news" = "news", "blogs" = "blogs"),
                       selected = "twitter")
          ),
        fluidRow(actionButton("action", label = "Reset input text")),
#                 actionButton("newsearch", label = "Search next word"))  ,
        fluidRow(textInput("searchstring", label = h3("Text input"), value = "Enter text...")) ,
        
#        checkboxGroupInput("outputval", "Chosen output", choices = outval),
        
#         textInput("chosen_out", label = h3("Chosen output"), 
#                   value = uiOutput("text1")),
fluidRow( actionButton(inputId = "pred1",  label = textOutput("text1"), icon = NULL) )   ,
fluidRow( actionButton(inputId = "pred2",  label = textOutput("text2"), icon = NULL))   ,
fluidRow( actionButton(inputId = "pred3",  label = textOutput("text3"), icon = NULL))   ,
width = 10
),

mainPanel(
)


)
))

