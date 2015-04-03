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
              On the right panel, you will get a drop down list with three suggestions for the next word.
             Please note that, if you add a new word to your sentence before the suggestions appear, 
              the app will relaunch its search based on your most recent input. 
             The app has been optimised for predictions based on the three most recent words you have typed.
              Also note that, although the app suggests the next word, you still need to fill in the next word manually in the 
              input field. 
              "),
          radioButtons("corpus", 
                              label = h3("Preferred corpus"), 
                              choices = list("twitter" = "twitter", 
                                             "news" = "news", "blogs" = "blogs"),
                              selected = "twitter"),
 
 
           textInput("searchstring", label = h3("Text input"), 
                     value = "Enter text...")
 ) ,  


mainPanel(
#   uiOutput("debugTools"),
#   textOutput("text1")
  uiOutput("text1")
  
)

)

))