library(shiny)

shinyUI(fluidPage(
  titlePanel("Basic widgets"),
  
sidebarLayout(
    sidebarPanel(
           checkboxGroupInput("corpus", 
                              label = h3("Preferred corpus"), 
                              choices = list("twitter" = "twitter", 
                                             "news" = "news", "blogs" = "blogs"),
                              selected = "twitter"),
 
           h3("Help text"),
           helpText("Note: help text isn't a true widget,", 
                    "but it provides an easy way to add text to",
                    "accompany other widgets."), 
           textInput("searchstring", label = h3("Text input"), 
                     value = "Enter text...")
 ) ,  


mainPanel(
#  uiOutput("debugTools"),
  textOutput("text1")
  
)

)

))