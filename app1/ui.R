library(shiny)

shinyUI(fluidPage(
  titlePanel("Next word prediction"),
  
sidebarLayout(
    sidebarPanel(
        h3("Some guidance"),
        helpText("First select the type of text you are writing (tweet, news article, blog).
              Then start writing your sentence - the app will start searching for the best
              prediction of the next word as soon as you hit the 'space' button. 
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