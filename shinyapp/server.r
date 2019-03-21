
library(shiny)
library(data.table)

source("prediction_model.R");

shinyServer(
  
  function(input, output, session) {
    
    observe({
      
      input_text <- input$text
      input_pred <- input$predictions
      
      output$predicted_word <- renderDataTable(
        data.table("Predicted words" = predicted_word(tolower(as.character(
          input_text)), 
          input_pred)),
        options = list(
          searching = FALSE, 
          paging = FALSE, 
          info=FALSE)
      )
      
    })
    
  }
)

