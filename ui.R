library(shiny)

shinyUI(fluidPage(theme = "bootstrap-united.css",
  titlePanel("Coursera Data Science Capstone: Next Word Predictor"),
  sidebarPanel(
    h3("Instruction"), 
    p("Input a sentence into the text field, or select a pre-defined sentence from the dropdown list"),
    p('You will then see the list of predicted next words, ranked in order from highest match') 
  ),
  mainPanel(
    textInput("entry",
              h5("Input your sentence"),
              ""),
    selectInput("sentence", "Examples:",
                c("How are you","Today is a","How may I","You should be","I am thinking")),
    numericInput("n",
                 h5("Desired numbers of predictions"), 
                 value = 5),
    br(),
    span(h4(textOutput('sent')),style = "color:dark blue"),
    dataTableOutput('table')
    
    
  )
)
)