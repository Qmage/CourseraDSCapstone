library(shiny)
source('mymodel.R')
profanity <- read.csv("profanity.csv", header=FALSE, stringsAsFactors=FALSE)
profanity <- profanity$V1
load('ngrams.RData')

shinyServer(function(input, output, clientData, session) {
  
  observe({
    updateTextInput(session, "entry",
                           value = input$sentence
                           )
    })
  
  predictionOutput <- reactive({
    predictNextWord(input$entry,profanity,unigramDF,bigramDF,trigramDF,fourgramDF,fivegramDF,maxResults = input$n)
  })
  
  output$sent <- renderText({
    if(input$entry == ""){
      return("Please enter a text sentence")
    }else{
      return(paste0("Your input text is - \"",input$entry,"\""))
    }
  })
  
  output$table <- renderDataTable({predictionOutput()})
    
})