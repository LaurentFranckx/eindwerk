library(shiny)

predict <- function(foo_or_bar) { ifelse(foo_or_bar, 'Foo', 'Bar') } 
style <- "

.form-group.shiny-input-container {
width: 100%
}


#foo25  {
width: 25%  
}

#foo50  {
width: 50%  
}

#foo75  {
width: 75%  
}
"

runApp(shinyApp(
  ui = shinyUI(fluidPage(
    tags$head(tags$style(type="text/css", style)),
    fluidRow(
      column(
        # Column 12 can use a whole width of the page
        width = 12,
        textInput('foo25', 'I am foo25 inside column12'),
        textInput('foo50', 'I am foo50 inside column12'),
        textInput('foo75', 'I am foo75 inside column12'),
        textInput('foo', 'I am some other foo inside column12')
      ),
      column(width = 9,
             # Width of the child is limited by the width of the parent element
             textInput('bar', 'I am some bar  inside column9')
      ),
      column(width = 5,
             # Same as above
             textInput('fobar', 'I am some foobar  inside column5')
      )
    )
  )),
  
  server = shinyServer(function(input, output, session) {
  })
))
