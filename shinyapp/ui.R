

library(shiny)
library(markdown)
library(shinythemes)

shinyUI(
  navbarPage("Data Science Specialization SwiftKey Capstone",
             theme = shinytheme("flatly"),
             tabPanel("Result",
                      titlePanel("Text Prediction"),
                      sidebarLayout(
                        sidebarPanel(
                          textInput("text",label = h4("Type a few words"), value = "Blue"),
                          sliderInput("predictions", "Number of predictions", 1, 8, 5, 1)
                          ),
                        mainPanel(dataTableOutput("predicted_word") )
                        )
                      ),
             tabPanel("About", 
                      titlePanel("About"),
                      fluidRow(
                        column(8, includeMarkdown("about.md"))
                        )
                      )
             )
  )

 
